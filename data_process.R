# Generate all required data from different data sources (see Table S1).
library(fst)
library(data.table)
library(parallel)
library("xgboost")
library("dplyr")
library("foreign")

f <- list.files("/nfs/nsaph_ci3/ci3_health_data/medicare/mortality/1999_2016/wu/cache_data/merged_by_year_v2",
                pattern = "\\.fst",
                full.names = TRUE)

myvars <- c("year","zip","sex","race","age","dual","entry_age_break","statecode",
            "followup_year","followup_year_plus_one","dead","pm25_ensemble",
            "mean_bmi","smoke_rate","hispanic","pct_blk","medhouseholdincome","medianhousevalue",
            "poverty","education","popdensity", "pct_owner_occ","summer_tmmx","winter_tmmx","summer_rmax","winter_rmax")
national_merged2016 <- rbindlist(lapply(f,
                        read_fst,
                        columns = myvars,
                        as.data.table=TRUE))

national_merged2016<-as.data.frame(national_merged2016)

national_merged2016$zip <- sprintf("%05d", national_merged2016$zip)

NORTHEAST=c("NY","MA","PA","RI","NH","ME","VT","CT","NJ")  
SOUTH=c("DC","VA","NC","WV","KY","SC","GA","FL","AL","TN","MS","AR","MD","DE","OK","TX","LA")
MIDWEST=c("OH","IN","MI","IA","MO","WI","MN","SD","ND","IL","KS","NE")
WEST=c("MT","CO","WY","ID","UT","NV","CA","OR","WA","AZ","NM")

national_merged2016$region=ifelse(national_merged2016$state %in% NORTHEAST, "NORTHEAST", 
                                  ifelse(national_merged2016$state %in% SOUTH, "SOUTH",
                                         ifelse(national_merged2016$state %in% MIDWEST, "MIDWEST",
                                                ifelse(national_merged2016$state %in% WEST, "WEST",
                                                       NA))))

national_merged2016 <- national_merged2016[complete.cases(national_merged2016[,c(1:27)]) ,]
#> dim(national_merged2016)
#[1] 573370257        27

# Main analysis
covariates<-aggregate(national_merged2016[,c(12:27)], by=list(national_merged2016$zip,national_merged2016$year), FUN=min)
colnames(covariates)[1:2]<-c("zip","year")

covariates<-subset(covariates[complete.cases(covariates) ,])
covariates$year_fac <- as.factor(covariates$year)
covariates$region <- as.factor(covariates$region)

save(covariates,file="./balance/covariates.RData")

# Fit GPS model via Xgboost machine
GPS_mod <-xgboost(data = data.matrix(covariates[,c(4:19)]), label = covariates$pm25_ensemble,nrounds=50)
mod_sd<- sd(covariates$pm25_ensemble-predict(GPS_mod,data.matrix(covariates[,c(4:19)])))
feature_names <- GPS_mod$feature_names
covariates$GPS<-dnorm(covariates$pm25_ensemble,mean = predict(GPS_mod,data.matrix(covariates[,c(4:19)])),
                      sd=sd(covariates$pm25_ensemble-predict(GPS_mod,data.matrix(covariates[,c(4:19)]))))
Nm<-dnorm(covariates$pm25_ensemble,mean=mean(covariates$pm25_ensemble,na.rm=T),sd=sd(covariates$pm25_ensemble,na.rm=T))
covariates$IPW<-Nm/(covariates$GPS)
covariates<-covariates[,c("zip","year","IPW","GPS")]
save(GPS_mod,mod_sd,feature_names,covariates,file="./covariates.RData")

# Generate count data for each individual characteristics and follow-up year
national_merged2016$time_count<-national_merged2016$followup_year_plus_one-national_merged2016$followup_year
dead_personyear<-aggregate(cbind(national_merged2016$dead,national_merged2016$time_count), by=list(national_merged2016$zip,national_merged2016$year,
                                                                                                   national_merged2016$sex,national_merged2016$race,national_merged2016$dual,national_merged2016$entry_age_break,national_merged2016$followup_year), FUN=sum)
confounders<-aggregate(national_merged2016[,c(12:27)], by=list(national_merged2016$zip,national_merged2016$year,
                                                               national_merged2016$sex,national_merged2016$race,national_merged2016$dual,national_merged2016$entry_age_break,national_merged2016$followup_year), FUN=min)
aggregate_data<-merge(dead_personyear,confounders
                      ,by=c("Group.1", "Group.2", "Group.3", "Group.4", "Group.5", "Group.6", "Group.7"))
colnames(aggregate_data)[8:9]<-c("dead","time_count")
colnames(aggregate_data)[1:7]<-c("zip","year","sex","race","dual","entry_age_break","followup_year")
aggregate_data<-subset(aggregate_data[complete.cases(aggregate_data) ,])

save(aggregate_data,file="./aggregate_data.RData")


load("./covariates.RData")
load("./aggregate_data.RData")
aggregate_data<-merge(aggregate_data,covariates,by=c("zip","year"),all.x=T)

aggregate_data.list <- split(aggregate_data,list(aggregate_data$sex,aggregate_data$race, aggregate_data$dual,aggregate_data$entry_age_break,aggregate_data$followup_year))
aggregate_data.list<-aggregate_data.list[lapply(aggregate_data.list,nrow)>0]

mclapply(aggregate_data.list,function(data){
  write_fst(data,paste0("./FST_data/",
                        data$sex[1],data$race[1], data$dual[1],data$entry_age_break[1],data$followup_year[1],".fst"))
},mc.cores=16)


# For analyses, exclude year as a confounder

# Fit GPS model via Xgboost machine
load("./balance/covariates.RData")

GPS_mod <-xgboost(data = data.matrix(covariates[,c(4:18)]), label = covariates$pm25_ensemble,nrounds=50)
mod_sd <- sd(covariates$pm25_ensemble-predict(GPS_mod,data.matrix(covariates[,c(4:18)])))
feature_names <- GPS_mod$feature_names
covariates$GPS<-dnorm(covariates$pm25_ensemble,mean = predict(GPS_mod,data.matrix(covariates[,c(4:18)])),
                      sd=sd(covariates$pm25_ensemble-predict(GPS_mod,data.matrix(covariates[,c(4:18)]))))
Nm<-dnorm(covariates$pm25_ensemble,mean=mean(covariates$pm25_ensemble,na.rm=T),sd=sd(covariates$pm25_ensemble,na.rm=T))
covariates$IPW<-Nm/(covariates$GPS)
covariates<-covariates[,c("zip","year","IPW","GPS")]

save(GPS_mod,mod_sd,feature_names,covariates,file="./covariates_noyear.RData")

load("./covariates_noyear.RData")
load("./aggregate_data.RData")
aggregate_data<-merge(aggregate_data,covariates,by=c("zip","year"),all.x=T)

aggregate_data.list <- split(aggregate_data,list(aggregate_data$sex,aggregate_data$race, aggregate_data$dual,aggregate_data$entry_age_break,aggregate_data$followup_year))
aggregate_data.list<-aggregate_data.list[lapply(aggregate_data.list,nrow)>0]

dir.create(file.path("./FST_data_noyear"), showWarnings = FALSE)
mclapply(aggregate_data.list,function(data){
  write_fst(data,paste0("./FST_data_noyear/",
                        data$sex[1],data$race[1], data$dual[1],data$entry_age_break[1],data$followup_year[1],".fst"))
},mc.cores=16)

# For analyses, exclude meteorological variables as confounders
load("./balance/covariates.RData")

GPS_mod <-xgboost(data = data.matrix(covariates[,c(4:13,18:19)]), label = covariates$pm25_ensemble,nrounds=50)
mod_sd <- sd(covariates$pm25_ensemble-predict(GPS_mod,data.matrix(covariates[,c(4:13,18:19)])))
feature_names <- GPS_mod$feature_names
covariates$GPS<-dnorm(covariates$pm25_ensemble,mean = predict(GPS_mod,data.matrix(covariates[,c(4:13,18:19)])),
                      sd=sd(covariates$pm25_ensemble-predict(GPS_mod,data.matrix(covariates[,c(4:13,18:19)]))))
Nm<-dnorm(covariates$pm25_ensemble,mean=mean(covariates$pm25_ensemble,na.rm=T),sd=sd(covariates$pm25_ensemble,na.rm=T))
covariates$IPW<-Nm/(covariates$GPS)
covariates<-covariates[,c("zip","year","IPW","GPS")]

save(GPS_mod,mod_sd,feature_names,covariates,file="./covariates_notemp.RData")

load("./covariates_notemp.RData")
load("./aggregate_data.RData")
aggregate_data<-merge(aggregate_data,covariates,by=c("zip","year"),all.x=T)

aggregate_data.list <- split(aggregate_data,list(aggregate_data$sex,aggregate_data$race, aggregate_data$dual,aggregate_data$entry_age_break,aggregate_data$followup_year))
aggregate_data.list<-aggregate_data.list[lapply(aggregate_data.list,nrow)>0]

dir.create(file.path("./FST_data_notemp"), showWarnings = FALSE)
mclapply(aggregate_data.list,function(data){
  write_fst(data,paste0("./FST_data_notemp/",
                        data$sex[1],data$race[1], data$dual[1],data$entry_age_break[1],data$followup_year[1],".fst"))
},mc.cores=16)

# Save the complete data
write.foreign(national_merged2016, datafile="./national_merged2016.csv", 
              codefile="./national_merged2016.sas",
              package = c("SAS"))
