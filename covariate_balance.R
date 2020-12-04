library("wCorr")
library("parallel")
library(fst)
library(data.table)
library("xgboost")

load("./covariates2016_temp.RData")

# Fit GPS model using linear regression
GPS_mod <- lm(pm25_ensemble ~ mean_bmi + smoke_rate + hispanic + pct_blk + medhouseholdincome + medianhousevalue +
              poverty + education + popdensity + pct_owner_occ + summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
              as.factor(year) + as.factor(region), covariates)
covariates$GPS <- dnorm(covariates$pm25_ensemble,
                        mean = predict(GPS_mod,covariates),
                        sd = summary(GPS_mod)$sigma)

# Calculate the weight
Nm <- dnorm(covariates$pm25_ensemble,
            mean = mean(covariates$pm25_ensemble, na.rm = TRUE),
            sd = sd(covariates$pm25_ensemble, na.rm = TRUE))
covariates$IPW <- Nm / (covariates$GPS)
covariates$IPW[covariates$IPW > 10] <- 10

# Fit GPS model using Xgboost machine
GPS_mod2 <- xgboost(data = data.matrix(covariates[, c(4:19)]),
                    label = covariates$pm25_ensemble,
                    nrounds = 50)
covariates$GPS2 <- dnorm(covariates$pm25_ensemble,
                         mean = predict(GPS_mod2, data.matrix(covariates[, c(4:19)])),
                         sd = sd(covariates$pm25_ensemble - predict(GPS_mod2, data.matrix(covariates[, c(4:19)]))))

# Calculate the weight
Nm <- dnorm(covariates$pm25_ensemble,
            mean = mean(covariates$pm25_ensemble, na.rm = TRUE),
            sd = sd(covariates$pm25_ensemble, na.rm = TRUE))
covariates$IPW2 <- Nm / (covariates$GPS2)
covariates$IPW2[covariates$IPW2 > 10] <- 10

# Matching function based on linear GPS
matching.fun.dose.l1.caliper2 <- function(simulated.data,
                                          GPS_mod,
                                          a,
                                          delta_n=1,
                                          scale) {
  ## cosmetic changes only
  simulated.data[["treat"]] <- simulated.data[["pm25_ensemble"]]
  p.a <- dnorm(a, mean = predict(GPS_mod, simulated.data), sd = summary(GPS_mod)[["sigma"]])
  ## calculate min and max once, cache result
  treat.min <- min(simulated.data[["treat"]], na.rm = TRUE)
  treat.max <- max(simulated.data[["treat"]], na.rm = TRUE)
  GPS.min <- min(simulated.data[["GPS"]], na.rm = TRUE)
  GPS.max <- max(simulated.data[["GPS"]], na.rm = TRUE)
  ## using transform instead of $ is mostly cosmetic
  simulated.data <- transform(simulated.data,
                              std.treat = (treat - treat.min) / (treat.max - treat.min),
                              std.GPS = (GPS - GPS.min) / (GPS.max - GPS.min))
  std.a <- (a - treat.min) / (treat.max - treat.min)
  std.p.a <- (p.a - GPS.min) / (GPS.max - GPS.min)
  ## this subsetting doesn't depend on i, and therefore doesn't need to be done on each iteration
  simulated.data.subset <- simulated.data[abs(simulated.data[["treat"]] - a) <= (delta_n/2), ]
  ## doing the subtraction with `outer` is faster than looping over with sapply or parSapply
  if (nrow(simulated.data.subset) >= 1) {
    wm <- sapply(1:length(std.p.a), function(iter) {
      return(apply(abs(outer(simulated.data.subset[["std.GPS"]], std.p.a[iter], `-`)) * scale,
                   2,
                   function(x) which.min(abs(simulated.data.subset[["std.treat"]] - std.a) * (1 - scale) + x))
      )
    })
    dp <- simulated.data.subset[wm, ]
  } else {dp <- data.frame(matrix(rep(NA, ncol(simulated.data)), nrow = 1))}
  dp$pm25 <- a
  return(dp)
  gc()
}

# Matching function based on Xgboost GPS
matching.fun.dose.l1.caliper_xgb <- function(simulated.data,
                                          GPS_mod,
                                          a,
                                          delta_n = 1,
                                          scale) {
  ## cosmetic changes only
  simulated.data[["treat"]] <- simulated.data[["pm25_ensemble"]]
  p.a <- dnorm(a,
               mean = predict(GPS_mod2, data.matrix(simulated.data[, c(4:19)])),
               sd = sd(simulated.data$pm25_ensemble - predict(GPS_mod2, data.matrix(simulated.data[, c(4:19)]))))
  
  ## calculate min and max once, cache result
  treat.min <- min(simulated.data[["treat"]], na.rm = TRUE)
  treat.max <- max(simulated.data[["treat"]], na.rm = TRUE)
  GPS.min <- min(simulated.data[["GPS2"]], na.rm = TRUE)
  GPS.max <- max(simulated.data[["GPS2"]], na.rm = TRUE)
  ## using transform instead of $ is mostly cosmetic
  simulated.data <- transform(simulated.data,
                              std.treat = (treat - treat.min) / (treat.max - treat.min),
                              std.GPS = (GPS2 - GPS.min) / (GPS.max - GPS.min))
  std.a <- (a - treat.min) / (treat.max - treat.min)
  std.p.a <- (p.a - GPS.min) / (GPS.max - GPS.min)
  ## this subsetting doesn't depend on i, and therefore doesn't need to be done on each iteration
  simulated.data.subset <- simulated.data[abs(simulated.data[["treat"]] - a) <= (delta_n / 2), ]
  ## doing the subtraction with `outer` is faster than looping over with sapply or parSapply
  if (nrow(simulated.data.subset) >= 1) {
    wm <- sapply(1:length(std.p.a), function(iter) {
      return(apply(abs(outer(simulated.data.subset[["std.GPS"]], std.p.a[iter], `-`)) * scale,
                   2,
                   function(x) which.min(abs(simulated.data.subset[["std.treat"]] - std.a) * (1 - scale) + x))
      )
    })
    dp <- simulated.data.subset[wm, ]
  } else {dp <- data.frame(matrix(rep(NA, ncol(simulated.data)), nrow = 1))}
  dp$pm25 <- a
  return(dp)
  gc()
}

a.vals <- seq(min(covariates$pm25_ensemble), max(covariates$pm25_ensemble), length.out = 50)
delta_n <- (a.vals[2] - a.vals[1])

# Create matched data based on two GPS
match_data <- rbindlist(mclapply(a.vals + delta_n/2,
                                 matching.fun.dose.l1.caliper2,
                                 simulated.data = covariates,
                                 GPS_mod = GPS_mod,
                                 delta_n = delta_n,
                                 scale = 1,
                                 mc.cores = 8))
match_data1 <- subset(match_data[complete.cases(match_data), ],
                      pm25_ensemble < quantile(covariates$pm25_ensemble, 0.99) & 
                        pm25_ensemble > quantile(covariates$pm25_ensemble, 0.01))

match_data_xgb <- rbindlist(mclapply(a.vals + delta_n/2,
                                     matching.fun.dose.l1.caliper_xgb,
                                     simulated.data = covariates,
                                     GPS_mod = GPS_mod2,
                                     delta_n = delta_n,
                                     scale = 1,
                                     mc.cores = 8))
match_data2 <- subset(match_data_xgb[complete.cases(match_data_xgb), ],
                      pm25_ensemble < quantile(covariates$pm25_ensemble, 0.99) &
                        pm25_ensemble > quantile(covariates$pm25_ensemble, 0.01))

# Saved the weighted and matched results
save(covariates, match_data1,match_data2, file = "./match_data2016_temp.RData")

# Caculate absolute correlation for each covariates to assess covariate balance
cor_origin <- c(abs(polyserial(covariates$pm25_ensemble, covariates$year_fac)),
                abs(polyserial(covariates$pm25_ensemble, covariates$region)),
                abs(cor(covariates$pm25_ensemble, covariates$mean_bmi, method = c("spearman"))),
                abs(cor(covariates$pm25_ensemble, covariates$smoke_rate, method = c("spearman"))),
                abs(cor(covariates$pm25_ensemble, covariates$hispanic, method = c("spearman"))),
                abs(cor(covariates$pm25_ensemble, covariates$pct_blk, method = c("spearman"))),
                abs(cor(covariates$pm25_ensemble, covariates$medhouseholdincome, method = c("spearman"))),
                abs(cor(covariates$pm25_ensemble, covariates$medianhousevalue, method = c("spearman"))),
                abs(cor(covariates$pm25_ensemble, covariates$poverty, method = c("spearman"))),
                abs(cor(covariates$pm25_ensemble, covariates$education, method = c("spearman"))),
                abs(cor(covariates$pm25_ensemble, covariates$popdensity, method = c("spearman"))),
                abs(cor(covariates$pm25_ensemble, covariates$pct_owner_occ, method = c("spearman"))),
                abs(cor(covariates$pm25_ensemble, covariates$summer_tmmx, method = c("spearman"))),
                abs(cor(covariates$pm25_ensemble, covariates$summer_rmax, method = c("spearman"))),
                abs(cor(covariates$pm25_ensemble, covariates$winter_tmmx, method = c("spearman"))),
                abs(cor(covariates$pm25_ensemble, covariates$winter_rmax, method = c("spearman"))))

cor_weight <- c(abs(weightedCorr(weights=covariates$IPW, covariates$pm25_ensemble, covariates$year_fac, method = c("Polyserial"))),
                abs(weightedCorr(weights=covariates$IPW, covariates$pm25_ensemble, covariates$region, method = c("Polyserial"))),
                abs(weightedCorr(weights=covariates$IPW, covariates$pm25_ensemble, covariates$mean_bmi, method = c("spearman"))),
                abs(weightedCorr(weights=covariates$IPW, covariates$pm25_ensemble, covariates$smoke_rate, method = c("spearman"))),
                abs(weightedCorr(weights=covariates$IPW, covariates$pm25_ensemble, covariates$hispanic, method = c("spearman"))),
                abs(weightedCorr(weights=covariates$IPW, covariates$pm25_ensemble, covariates$pct_blk, method = c("spearman"))),
                abs(weightedCorr(weights=covariates$IPW, covariates$pm25_ensemble, covariates$medhouseholdincome, method = c("spearman"))),
                abs(weightedCorr(weights=covariates$IPW, covariates$pm25_ensemble, covariates$medianhousevalue, method = c("spearman"))),
                abs(weightedCorr(weights=covariates$IPW, covariates$pm25_ensemble, covariates$poverty, method = c("spearman"))),
                abs(weightedCorr(weights=covariates$IPW, covariates$pm25_ensemble, covariates$education, method = c("spearman"))),
                abs(weightedCorr(weights=covariates$IPW, covariates$pm25_ensemble, covariates$popdensity, method = c("spearman"))),
                abs(weightedCorr(weights=covariates$IPW, covariates$pm25_ensemble, covariates$pct_owner_occ, method = c("spearman"))),
                abs(weightedCorr(weights=covariates$IPW, covariates$pm25_ensemble, covariates$summer_tmmx, method = c("spearman"))),
                abs(weightedCorr(weights=covariates$IPW, covariates$pm25_ensemble, covariates$summer_rmax, method = c("spearman"))),
                abs(weightedCorr(weights=covariates$IPW, covariates$pm25_ensemble, covariates$winter_tmmx, method = c("spearman"))),
                abs(weightedCorr(weights=covariates$IPW, covariates$pm25_ensemble, covariates$winter_rmax, method = c("spearman"))))


cor_weight2 <- c(abs(weightedCorr(weights=covariates$IPW2, covariates$pm25_ensemble, covariates$year_fac, method = c("Polyserial"))),
                 abs(weightedCorr(weights=covariates$IPW2, covariates$pm25_ensemble, covariates$region, method = c("Polyserial"))),
                 abs(weightedCorr(weights=covariates$IPW2, covariates$pm25_ensemble, covariates$mean_bmi, method = c("spearman"))),
                 abs(weightedCorr(weights=covariates$IPW2, covariates$pm25_ensemble, covariates$smoke_rate, method = c("spearman"))),
                 abs(weightedCorr(weights=covariates$IPW2, covariates$pm25_ensemble, covariates$hispanic, method = c("spearman"))),
                 abs(weightedCorr(weights=covariates$IPW2, covariates$pm25_ensemble, covariates$pct_blk, method = c("spearman"))),
                 abs(weightedCorr(weights=covariates$IPW2, covariates$pm25_ensemble, covariates$medhouseholdincome, method = c("spearman"))),
                 abs(weightedCorr(weights=covariates$IPW2, covariates$pm25_ensemble, covariates$medianhousevalue, method = c("spearman"))),
                 abs(weightedCorr(weights=covariates$IPW2, covariates$pm25_ensemble, covariates$poverty, method = c("spearman"))),
                 abs(weightedCorr(weights=covariates$IPW2, covariates$pm25_ensemble, covariates$education, method = c("spearman"))),
                 abs(weightedCorr(weights=covariates$IPW2, covariates$pm25_ensemble, covariates$popdensity, method = c("spearman"))),
                 abs(weightedCorr(weights=covariates$IPW2, covariates$pm25_ensemble, covariates$pct_owner_occ, method = c("spearman"))),
                 abs(weightedCorr(weights=covariates$IPW2, covariates$pm25_ensemble, covariates$summer_tmmx, method = c("spearman"))),
                 abs(weightedCorr(weights=covariates$IPW2, covariates$pm25_ensemble, covariates$summer_rmax, method = c("spearman"))),
                 abs(weightedCorr(weights=covariates$IPW2, covariates$pm25_ensemble, covariates$winter_tmmx, method = c("spearman"))),
                 abs(weightedCorr(weights=covariates$IPW2, covariates$pm25_ensemble, covariates$winter_rmax, method = c("spearman"))))


cor_matched <- c(abs(polyserial(match_data1$pm25_ensemble, match_data1$year_fac)),
                 abs(polyserial(match_data1$pm25_ensemble, match_data1$region)),
                 abs(cor(match_data1$pm25_ensemble, match_data1$mean_bmi, method = c("spearman"))),
                 abs(cor(match_data1$pm25_ensemble, match_data1$smoke_rate, method = c("spearman"))),
                 abs(cor(match_data1$pm25_ensemble, match_data1$hispanic, method = c("spearman"))),
                 abs(cor(match_data1$pm25_ensemble, match_data1$pct_blk, method = c("spearman"))),
                 abs(cor(match_data1$pm25_ensemble, match_data1$medhouseholdincome, method = c("spearman"))),
                 abs(cor(match_data1$pm25_ensemble, match_data1$medianhousevalue, method = c("spearman"))),
                 abs(cor(match_data1$pm25_ensemble, match_data1$poverty, method = c("spearman"))),
                 abs(cor(match_data1$pm25_ensemble, match_data1$education, method = c("spearman"))),
                 abs(cor(match_data1$pm25_ensemble, match_data1$popdensity, method = c("spearman"))),
                 abs(cor(match_data1$pm25_ensemble, match_data1$pct_owner_occ, method = c("spearman"))),
                 abs(cor(match_data1$pm25_ensemble, match_data1$summer_tmmx, method = c("spearman"))),
                 abs(cor(match_data1$pm25_ensemble, match_data1$summer_rmax, method = c("spearman"))),
                 abs(cor(match_data1$pm25_ensemble, match_data1$winter_tmmx, method = c("spearman"))),
                 abs(cor(match_data1$pm25_ensemble, match_data1$winter_rmax, method = c("spearman"))))

cor_matched2 <- c(abs(polyserial(match_data2$pm25_ensemble, match_data2$year_fac)),
                  abs(polyserial(match_data2$pm25_ensemble, match_data2$region)),
                  abs(cor(match_data2$pm25_ensemble, match_data2$mean_bmi, method = c("spearman"))),
                  abs(cor(match_data2$pm25_ensemble, match_data2$smoke_rate, method = c("spearman"))),
                  abs(cor(match_data2$pm25_ensemble, match_data2$hispanic, method = c("spearman"))),
                  abs(cor(match_data2$pm25_ensemble, match_data2$pct_blk, method = c("spearman"))),
                  abs(cor(match_data2$pm25_ensemble, match_data2$medhouseholdincome, method = c("spearman"))),
                  abs(cor(match_data2$pm25_ensemble, match_data2$medianhousevalue, method = c("spearman"))),
                  abs(cor(match_data2$pm25_ensemble, match_data2$poverty, method = c("spearman"))),
                  abs(cor(match_data2$pm25_ensemble, match_data2$education, method = c("spearman"))),
                  abs(cor(match_data2$pm25_ensemble, match_data2$popdensity, method = c("spearman"))),
                  abs(cor(match_data2$pm25_ensemble, match_data2$pct_owner_occ, method = c("spearman"))),
                  abs(cor(match_data2$pm25_ensemble, match_data2$summer_tmmx, method = c("spearman"))),
                  abs(cor(match_data2$pm25_ensemble, match_data2$summer_rmax, method = c("spearman"))),
                  abs(cor(match_data2$pm25_ensemble, match_data2$winter_tmmx, method = c("spearman"))),
                  abs(cor(match_data2$pm25_ensemble, match_data2$winter_rmax, method = c("spearman"))))

save(cor_origin, cor_weight, cor_weight2, cor_matched, cor_matched2, file = "./correlations.RData")
