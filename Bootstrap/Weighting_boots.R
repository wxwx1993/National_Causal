# Bootstrap on zip-code cluster to obtain robust CIs account for spatial correlation
library("mgcv")
library("survival")
library("haven")
library("parallel")
library("dplyr")
library(data.table)
library("xgboost")
library("gnm")

dir_data = '/nfs/home/X/xwu/shared_space/ci3_xwu/National_Causal/data2016_temp/'
dir_out = '/nfs/home/X/xwu/shared_space/ci3_xwu/National_Causal/data2016_temp/Boots/'

load(paste0(dir_data,"covariates.RData"))
load(paste0(dir_data,"aggregate_data.RData"))

# Weighting by GPS
cl=makeCluster(25,outfile='')
registerDoParallel(cl)
aggregate_data.list<-split(aggregate_data, list(aggregate_data$zip))
num_uniq_zip <- length(unique(aggregate_data$zip))

IPTW_coefs_boots<-NULL
for (boots_id in 1:500){
  set.seed(boots_id)
  zip_sample<-sample(1:num_uniq_zip,floor(2*sqrt(num_uniq_zip)),replace=T) 
  aggregate_data_boots<-data.frame(Reduce(rbind,aggregate_data.list[zip_sample]))
  
  covariates_boots<-aggregate(aggregate_data_boots[,c(10:25)], by=list(aggregate_data_boots$zip,aggregate_data_boots$year), FUN=min)
  colnames(covariates_boots)[1:2]<-c("zip","year")
  covariates_boots$year_fac <- as.factor(covariates_boots$year)
  covariates_boots$region <- as.factor(covariates_boots$region)
  covariates_boots<-subset(covariates_boots[complete.cases(covariates_boots) ,])
  
  GPS_mod <-xgboost(data = data.matrix(covariates_boots[feature_names]), label = covariates_boots$pm25_ensemble,nrounds=50)
  covariates_boots$GPS<-dnorm(covariates_boots$pm25_ensemble,mean = predict(GPS_mod,data.matrix(covariates_boots[feature_names])),
                              sd=sd(covariates_boots$pm25_ensemble-predict(GPS_mod,data.matrix(covariates_boots[feature_names]))))
  Nm<-dnorm(covariates_boots$pm25_ensemble,mean=mean(covariates_boots$pm25_ensemble,na.rm=T),sd=sd(covariates_boots$pm25_ensemble,na.rm=T))
  covariates_boots$IPW<-Nm/(covariates_boots$GPS)
  covariates_boots<-covariates_boots[,c("zip","year","IPW","GPS")]
  aggregate_data_boots<-left_join(aggregate_data_boots,covariates_boots,by=c("zip","year"))
  
  ######## Main models ########
  aggregate_data_boots$IPW[aggregate_data_boots$IPW>10]<-10
  IPTW_loglinear<-glm(dead~  pm25_ensemble +as.factor(sex)+as.factor(race)+as.factor(dual)+as.factor(entry_age_break)+as.factor(followup_year)
                      +offset(log(time_count)),data=aggregate_data_boots,family=poisson(link="log"),weights= IPW)
  IPTW_coefs_boots<-c(IPTW_coefs_boots,IPTW_loglinear$coefficients[2])
rm(aggregate_data_boots);rm(covariates_boots)
}

save(num_uniq_zip,IPTW_coefs_boots,file=paste0(dir_out,"IPTW_coefs_boots.RData"))

load(paste0(dir_data,"IPW.RData"))


exp(10*(IPW_gnm$coefficients[1]-1.96*sd(IPTW_coefs_boots) *sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip)))
exp(10*(IPW_gnm$coefficients[1]+1.96*sd(IPTW_coefs_boots) *sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip)))


