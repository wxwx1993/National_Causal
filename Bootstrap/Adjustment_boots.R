# Bootstrap on zip-code cluster to obtain robust CIs account for spatial correlation
library("mgcv")
library("dplyr")
library("parallel")
library(data.table)
library("xgboost")
library("gnm")

dir_data = '/nfs/home/X/xwu/shared_space/ci3_xwu/National_Causal/data2016_temp/'
dir_out = '/nfs/home/X/xwu/shared_space/ci3_xwu/National_Causal/data2016_temp/Boots/'

load(paste0(dir_data,"covariates.RData"))
load(paste0(dir_data,"aggregate_data.RData"))

# Adjustment by GPS
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

cl=makeCluster(16, outfile='')
registerDoParallel(cl)
aggregate_data.list<-split(aggregate_data, list(aggregate_data$zip))
num_uniq_zip <- length(unique(aggregate_data$zip))

HI_coefs_boots<-NULL
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
  
  a.vals <- seq(min(aggregate_data$pm25_ensemble),max(aggregate_data$pm25_ensemble),length.out = 50)

registerDoParallel(cl)
flexible_model1<-gnm(dead~ pm25_ensemble + GPS + pm25_ensemble*GPS + I(GPS^2) + offset(log(time_count))
                  , eliminate= (as.factor(sex):as.factor(race):as.factor(dual):as.factor(entry_age_break):as.factor(followup_year))
		              , data=aggregate_data_boots,family=poisson(link="log")) 

delta_n <-a.vals[2]-a.vals[1]
GPScova<-data.table(Reduce(rbind,mclapply(a.vals+delta_n/2,GPScova.fun.dose,
                    simulated.data=aggregate_data_boots,GPS_mod=GPS_mod,model1=flexible_model1,mc.cores=8)))


GPScova_model<-summary(gnm(log(dead)~ pm25_ensemble + offset(log(time_count)), 
		eliminate= (as.factor(sex):as.factor(race):as.factor(dual):as.factor(entry_age_break):as.factor(followup_year)),data=GPScova))
HI_coefs_boots<-c(HI_coefs_boots,GPScova_model$coefficients[1])
rm(aggregate_data_boots);rm(covariates_boots)
}
stopCluster(cl)

save(num_uniq_zip,HI_coefs_boots,file=paste0(dir_out,"HI_coefs_boots.RData"))


load(paste0(dir_data,"GPScova.RData"))

exp(10*(GPScova_model$coefficients[1]+1.96*sd(HI_coefs_boots) *sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip)))
exp(10*(GPScova_model$coefficients[1]-1.96*sd(HI_coefs_boots) *sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip)))


