# All types of statistical models implemented for the paper
library("survival")
library("gnm")
library("parallel")
require(doParallel)
library(data.table)
library(fst)
require("xgboost")


dir_data = '/nfs/home/X/xwu/shared_space/ci3_xwu/National_Causal/data2016_temp/'
dir_out = '/nfs/home/X/xwu/shared_space/ci3_xwu/National_Causal/data2016_temp/'


load(paste0(dir_data,"covariates.RData"))
load(paste0(dir_data,"aggregate_data.RData"))
aggregate_data<-merge(aggregate_data,covariates,by=c("zip","year"),all.x=T)


# Cox Proportional Hazard
Cox_raw <- coxph(Surv(followup_year,followup_year_plus_one,dead)~pm25_ensemble + 
             mean_bmi + smoke_rate + hispanic + pct_blk +
             medhouseholdincome + medianhousevalue +
             poverty + education + popdensity + pct_owner_occ +
             summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
             as.factor(year) + as.factor(region)
             +strata(as.factor(entry_age_break))+strata(as.factor(sex))+strata(as.factor(race))+strata(as.factor(dual))
           , data=national_merged2016,
           ties = c("efron"),na.action = na.omit)
Cox <- summary(Cox_raw)
save(Cox ,file=paste0(dir_out,"Cox.RData"))


# Cox-equvalent conditional Poisson Regression
gnm_raw<-gnm(dead~  pm25_ensemble + 
               mean_bmi + smoke_rate + hispanic + pct_blk +
               medhouseholdincome + medianhousevalue +
               poverty + education + popdensity + pct_owner_occ +
               summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
               as.factor(year) + as.factor(region)
               +offset(log(time_count)),
               eliminate= (as.factor(sex):as.factor(race):as.factor(dual):as.factor(entry_age_break):as.factor(followup_year)), 
               data=aggregate_data,family=poisson(link="log"))
Poisson<-summary(gnm_raw)
exp(10*Poisson$coefficients[1])
save(Poisson,file=paste0(dir_out,"Poisson.RData"))

# Weighting by GPS 
aggregate_data$IPW[aggregate_data$IPW>10]<-10

IPTW_gnm_raw<-gnm(dead~  pm25_ensemble
               +offset(log(time_count)),
               eliminate=(as.factor(sex):as.factor(race):as.factor(dual):as.factor(entry_age_break):as.factor(followup_year)),
               data=aggregate_data,family=poisson(link="log"),weights= IPW)
IPW_gnm<-summary(IPTW_gnm_raw)
exp(10*IPW_gnm$coefficients[1])
save(IPW_gnm,file=paste0(dir_out,"IPW.RData"))


# Adjustment by GPS 
a.vals <- seq(min(aggregate_data$pm25_ensemble),max(aggregate_data$pm25_ensemble),length.out = 50)
GPScova.fun.dose<-function(simulated.data,GPS_mod,a,model1=flexible_model1){
  simulated.data$year_fac <- as.factor(simulated.data$year)
  simulated.data$region <- as.factor(simulated.data$region)
  p.a <- dnorm(a,mean = predict(GPS_mod,data.matrix(simulated.data[feature_names])),
               sd=sd(simulated.data$pm25_ensemble-predict(GPS_mod,data.matrix(simulated.data[feature_names]))))
  
  data.a<-data.frame(cbind(cbind(pm25_ensemble=a,GPS=p.a),simulated.data[,c(1:7,9)]))
  data.a$dead <- predict(model1,data.a,type="response")

  data.a<-aggregate(cbind(data.a$dead,data.a$time_count,data.a$pm25_ensemble) , by=list(
    data.a$sex,data.a$race,data.a$dual,data.a$entry_age_break,data.a$followup_year), FUN=mean)
  colnames(data.a)[1:8]<-c("sex","race","dual","entry_age_break","followup_year","dead","time_count","pm25_ensemble")
  return(data.a)
}
cl<-makeCluster(8)
registerDoParallel(cl)
flexible_model1<-bam(dead~ pm25_ensemble + GPS + pm25_ensemble*GPS + I(GPS^2) +
                           as.factor(sex)+as.factor(race)+as.factor(dual)+as.factor(entry_age_break)+as.factor(followup_year)+
                           offset(log(time_count)),data=aggregate_data,family=poisson(link="log"),
                           chunk.size=5000,cluster=cl) 
stopCluster(cl)


delta_n <-a.vals[2]-a.vals[1]
GPScova<-data.table(Reduce(rbind,mclapply(a.vals+delta_n/2,GPScova.fun.dose,
                                          simulated.data=aggregate_data,GPS_mod=GPS_mod,model1=flexible_model1,mc.cores=1)))

GPScova_model<-summary(gnm(log(dead)~ pm25_ensemble + offset(log(time_count)), 
		eliminate= (as.factor(sex):as.factor(race):as.factor(dual):as.factor(entry_age_break):as.factor(followup_year)),data=GPScova))
exp(10*GPScova_model$coefficients[1])

GPScova_gam<-gam(log(dead)~ s(pm25_ensemble) + as.factor(sex)+as.factor(race)+as.factor(dual)+as.factor(entry_age_break)+as.factor(followup_year)
                 + offset(log(time_count)), data=GPScova)

save(GPScova,GPScova_model,GPScova_gam,file=paste0(dir_out,"GPScova.RData"))

# Matching by GPS
# We utilize parallel computing to accelarate matching
process <- c(0:49)[as.integer(as.character(commandArgs(trailingOnly = TRUE))) + 1]

load("/nfs/home/X/xwu/shared_space/ci3_xwu/National_Causal/data2016_temp/covariates.RData")

f <- list.files("/nfs/home/X/xwu/shared_space/ci3_xwu/National_Causal/data2016_temp/FST_data",
                pattern = "\\.fst",
                full.names = TRUE)

aggregate_data.list <- mclapply(f,read_fst,mc.cores=mc_cores)
aggregate_data<-rbindlist(aggregate_data.list)

# Matching on single exposure level a, a.vals selects the caliper
a.vals <- seq(min(aggregate_data$pm25_ensemble),max(aggregate_data$pm25_ensemble),length.out = 50)
delta_n<-a.vals[2]-a.vals[1]

# Matching on single exposure level a
matching.fun.dose.l1.caliper2 <- function(simulated.data,
                                          GPS_mod,
                                          a,
                                          delta_n=1,
                                          scale=1)
{
  ## cosmetic changes only
  simulated.data[["treat"]] <- simulated.data[["pm25_ensemble"]]
  simulated.data[["year_fac"]] <- as.factor(simulated.data[["year"]])
  simulated.data[["region"]] <- as.factor(simulated.data[["region"]])
  p.a <- dnorm(a,mean = predict(GPS_mod,data.matrix(simulated.data[feature_names])),
          sd=mod_sd)
  
  ## calculate min and max once, cache result
  treat.min <- min(simulated.data[["treat"]],na.rm=T)
  treat.max <- max(simulated.data[["treat"]],na.rm=T)
  GPS.min <- min(simulated.data[["GPS"]],na.rm=T)
  GPS.max <- max(simulated.data[["GPS"]],na.rm=T)
  if (nrow(simulated.data)>1){
  simulated.data <- transform(simulated.data,
                              std.treat = (treat - treat.min) / (treat.max - treat.min),
                              std.GPS = (GPS - GPS.min) / (GPS.max - GPS.min))
  std.a <- (a - treat.min) / (treat.max - treat.min)
  std.p.a <- (p.a - GPS.min) / (GPS.max - GPS.min)
  }else{
    simulated.data <- transform(simulated.data,
                                std.treat = treat ,
                                std.GPS = GPS )
    std.a <- a 
    std.p.a <- p.a 
  }
  simulated.data.subset <- simulated.data[abs(simulated.data[["treat"]] - a) <= (delta_n/2), ]
  ## doing the subtraction with `outer` is faster than looping over with sapply or parSapply
  if (nrow(simulated.data.subset)>=1){
      wm <- sapply(1:length(std.p.a),function(iter){
        return(apply(abs(outer(simulated.data.subset[["std.GPS"]], std.p.a[iter], `-`)) * scale,
                     2,
                     function(x) which.min(abs(simulated.data.subset[["std.treat"]] - std.a) * (1 - scale) + x))
      )
    })
    dp <- simulated.data.subset[wm, c("dead", "time_count")]
  }else{dp <-cbind(NA,NA)}
  E.a <- apply(dp, 2, sum, na.rm = T)
  return(c(simulated.data[1,3:7], E.a[1], E.a[2], a))
  gc()
}

# Function to implement the matching under each strata
par.match.noerrer<-function(a_i=a_i,data.list,GPS_mod=GPS_mod,delta_n=delta_n,scale=scale){
   matching_noerror_level <- data.table(Reduce(rbind,mclapply(1:length(data.list),function(i,a_i=a_i,GPS_mod=GPS_mod,delta_n=delta_n,scale=scale){
    return(matching.fun.dose.l1.caliper2(simulated.data=data.list[[i]],GPS_mod=GPS_mod,a=a_i,delta_n=delta_n,scale=scale))
    },GPS_mod=GPS_mod,a_i=a_i,delta_n=delta_n,scale=scale,mc.cores=mc_cores)))
  colnames(matching_noerror_level) <-c("sex","race","dual","entry_age_break","followup_year","dead","time_count","pm25_ensemble")
  return(matching_noerror_level)
  gc()
}

match.noerrer<-par.match.noerrer(a.vals[process+1]+delta_n/2,data.list=aggregate_data.list,GPS_mod=GPS_mod,delta_n=delta_n,scale=1)

# Save the matched data
saveRDS(match.noerrer,paste0(dir_data,"Output/",process,".rds"))

# Load matched data
f <- list.files(paste0(dir_data,"Output/"), pattern = "\\.rds", full.names = TRUE)

matching<-rbindlist(lapply(f, readRDS))
matching2<-subset(matching,time_count>0)
matching2<-as.data.frame(lapply(matching2, unlist))

# Fit univariate Poisson on matched data
matching_gnm2<-summary(gnm(dead~ pm25_ensemble+offset(log(time_count))
                            , eliminate = (as.factor(sex):as.factor(race):as.factor(dual):as.factor(entry_age_break):as.factor(followup_year)),
                          data=matching2,family=poisson(link="log")))

matching_gnm<-summary(gnm(dead~ pm25_ensemble+offset(log(time_count))
                            , eliminate = (as.factor(sex):as.factor(race):as.factor(dual):as.factor(entry_age_break):as.factor(followup_year)),
                          data=subset(matching2,pm25_ensemble > quantile(matching2$pm25_ensemble,0.01)
                                          &pm25_ensemble < quantile(matching2$pm25_ensemble,0.99)),family=poisson(link="log")))
exp(10*matching_gnm$coefficients[1])
exp(10*matching_gnm2$coefficients[1])

save(matching2,matching_gnm,matching_gnm2,file=paste0(dir_out,"Matching.RData"))



