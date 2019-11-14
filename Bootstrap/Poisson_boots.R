# Bootstrap on zip-code cluster to obtain robust CIs account for spatial correlation
library("mgcv")
library("parallel")
library("dplyr")
require(parallel)
library(data.table)


dir_data = '/nfs/home/X/xwu/shared_space/ci3_xwu/National_Causal/data2016_temp/'
dir_out = '/nfs/home/X/xwu/shared_space/ci3_xwu/National_Causal/data2016_temp/Boots/'

load(paste0(dir_data,"aggregate_data.RData"))

# poisson
cl=makeCluster(25,outfile='')
registerDoParallel(cl)
aggregate_data.list<-split(aggregate_data, list(aggregate_data$zip))
num_uniq_zip <- length(unique(aggregate_data$zip))

loglinear_coefs_boots<-NULL
for (boots_id in 1:500){
  set.seed(boots_id)
  zip_sample<-sample(1:num_uniq_zip,floor(2*sqrt(num_uniq_zip)),replace=T) 
  aggregate_data_boots<-data.frame(Reduce(rbind,aggregate_data.list[zip_sample]))
  
  gnm_raw<-gnm(dead~  pm25_ensemble + 
                 mean_bmi + smoke_rate + hispanic + pct_blk +
                 medhouseholdincome + medianhousevalue +
                 poverty + education + popdensity + pct_owner_occ + summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
                 as.factor(year) + as.factor(region)
               +offset(log(time_count)),eliminate= (as.factor(sex):as.factor(race):as.factor(dual):as.factor(entry_age_break):as.factor(followup_year)), data=aggregate_data_boots,family=poisson(link="log"))
  
  loglinear_coefs_boots<-c(loglinear_coefs_boots,summary(gnm_raw)$coefficients[1])
  rm(aggregate_data_boots)
}
stopCluster(cl)

save(loglinear_coefs_boots,file=paste0(dir_out,"loglinear_coefs_boots.RData"))

load(paste0(dir_data,"Poisson.RData"))

exp(10*(Poisson$coefficients[1]-1.96*sd(loglinear_coefs_boots) *sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip)))
exp(10*(Poisson$coefficients[1]+1.96*sd(loglinear_coefs_boots) *sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip)))

