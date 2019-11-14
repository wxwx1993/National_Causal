# Bootstrap on zip-code cluster to obtain robust CIs account for spatial correlation
library("mgcv")
require(parallel)
library(data.table)
library(fst)
library("dplyr")
library("xgboost")

load("/nfs/home/X/xwu/shared_space/ci3_xwu/National_Causal/data2016/covariates.RData")
feature_names <- GPS_mod$feature_names
rm(list= ls()[!(ls() %in% c("feature_names"))])
mc_cores = 16

load("//nfs/home/X/xwu/shared_space/ci3_xwu/National_Causal/data2016/aggregate_data.RData")

# Choose fix caliper same as main analyese
a.vals <- seq(min(aggregate_data$pm25_ensemble),max(aggregate_data$pm25_ensemble),length.out = 50)
delta_n<-a.vals[2]-a.vals[1]

# Matching on single exposure level a
matching.fun.dose.l1.caliper2 <- function(simulated.data,
                                          GPS_mod,
                                          mod_sd,
                                          a,
                                          delta_n=1,
                                          scale)
{
  simulated.data[["treat"]] <- simulated.data[["pm25_ensemble"]]
  simulated.data[["year_fac"]] <- as.factor(simulated.data[["year"]])
  simulated.data[["region"]] <- as.factor(simulated.data[["region"]])
  p.a <- dnorm(a,mean = predict(GPS_mod,data.matrix(simulated.data[,feature_names])),
               sd=mod_sd)
  ## calculate min and max once, cache result
  treat.min <- min(simulated.data[["treat"]],na.rm=T)
  treat.max <- max(simulated.data[["treat"]],na.rm=T)
  GPS.min <- min(simulated.data[["GPS"]],na.rm=T)
  GPS.max <- max(simulated.data[["GPS"]],na.rm=T)
  ## using transform instead of $ is mostly cosmetic
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

  wm <- apply(abs(outer(simulated.data.subset[["std.GPS"]], std.p.a, `-`)) * scale,
              2,
              function(x) which.min(abs(simulated.data.subset[["std.treat"]] - std.a) * (1 - scale) + x)
  )
  dp <- simulated.data.subset[wm, c("dead", "time_count")]
  E.a <- apply(dp, 2, sum, na.rm = T)
  return(c(simulated.data[1,3:7], E.a[1], E.a[2], a))
  gc()
}

# Function to implement the matching under each strata
par.match.noerrer<-function(a_i=a_i,data.list,GPS_mod=GPS_mod,mod_sd=mod_sd,delta_n=delta_n,scale=scale){
  #matching_noerror_level <- NULL
  matching_noerror_level <- data.table(Reduce(rbind,mclapply(1:length(data.list),function(i,a_i=a_i,GPS_mod=GPS_mod,mod_sd=mod_sd,delta_n=delta_n,scale=scale){
    return(matching.fun.dose.l1.caliper2(simulated.data=data.list[[i]],GPS_mod=GPS_mod,mod_sd=mod_sd,a=a_i,delta_n=delta_n,scale=scale))
  },GPS_mod=GPS_mod,mod_sd=mod_sd,a_i=a_i,delta_n=delta_n,scale=scale,mc.cores=mc_cores)))
  colnames(matching_noerror_level) <-c("sex","race","dual","entry_age_break","followup_year","dead","time_count","pm25_ensemble")
  return(matching_noerror_level)
  gc()
}

aggregate_data.list<-split(aggregate_data, list(aggregate_data$zip))
num_uniq_zip <- length(unique(aggregate_data$zip))

match_coefs_boots<-NULL
for (boots_id in 1:500){
  set.seed(boots_id)
  zip_sample<-sample(1:num_uniq_zip,floor(2*sqrt(num_uniq_zip)),replace=T) 
  aggregate_data_boots<-data.frame(Reduce(rbind,aggregate_data.list[zip_sample]))
  
  covariates_boots<-aggregate(aggregate_data_boots[,c(10,12:26)], by=list(aggregate_data_boots$zip,aggregate_data_boots$year), FUN=min)
  colnames(covariates_boots)[1:2]<-c("zip","year")
  covariates_boots$year_fac <- as.factor(covariates_boots$year)
  covariates_boots$region <- as.factor(covariates_boots$region)
  covariates_boots<-subset(covariates_boots[complete.cases(covariates_boots) ,])
  
  GPS_mod <-xgboost(data = data.matrix(covariates_boots[,feature_names]), label = covariates_boots$pm25_ensemble,nrounds=50)
  mod_sd = sd(covariates_boots$pm25_ensemble-predict(GPS_mod,data.matrix(covariates_boots[,feature_names])))
  covariates_boots$GPS<-dnorm(covariates_boots$pm25_ensemble,mean = predict(GPS_mod,data.matrix(covariates_boots[,feature_names])),
                              sd=mod_sd)
  Nm<-dnorm(covariates_boots$pm25_ensemble,mean=mean(covariates_boots$pm25_ensemble,na.rm=T),sd=sd(covariates_boots$pm25_ensemble,na.rm=T))
  covariates_boots$IPW<-Nm/(covariates_boots$GPS)
  covariates_boots<-covariates_boots[,c("zip","year","IPW","GPS")]
  aggregate_data_boots<-left_join(aggregate_data_boots,covariates_boots,by=c("zip","year"))
  aggregate_data_boots.list <- split(aggregate_data_boots,list(aggregate_data_boots$sex,aggregate_data_boots$race, aggregate_data_boots$dual,aggregate_data_boots$entry_age_break,aggregate_data_boots$followup_year))
  aggregate_data_boots.list<-aggregate_data_boots.list[lapply(aggregate_data_boots.list,nrow)>0]
  
  
matching <- rbindlist(lapply(a.vals+delta_n/2,function(a){
  par.match.noerrer(a_i=a,data.list=aggregate_data_boots.list,GPS_mod=GPS_mod,mod_sd=mod_sd,delta_n=delta_n,scale=1)
}))
# Obtain matched data
matching2<-subset(matching,time_count>0)
options(stringsAsFactors = FALSE)
matching2<-as.data.frame(lapply(matching2, unlist))

  ######## Main models ########
matching_model<-summary(glm(dead~ pm25_ensemble + as.factor(sex)+as.factor(race)+as.factor(dual)+as.factor(entry_age_break)+as.factor(followup_year)+offset(log(time_count))
                            , data=matching2,family=poisson(link="log")))
match_coefs_boots<-c(match_coefs_boots,matching_model$coefficients[2])
rm(aggregate_data_boots);rm(covariates_boots)
}


save(num_uniq_zip,match_coefs_boots_all,file="/nfs/home/X/xwu/shared_space/ci3_xwu/National_Causal/data2016/Boots/match_coefs_boots.RData")


load("/nfs/home/X/xwu/shared_space/ci3_xwu/National_Causal/data2016_temp/Matching.RData")

exp(10*(matching_gnm2$coefficients[1]-1.96*sd(match_coefs_boots_all) *sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip)))
exp(10*(matching_gnm2$coefficients[1]+1.96*sd(match_coefs_boots_all) *sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip)))

