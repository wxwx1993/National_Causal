# Bootstrap on zip-code cluster to obtain robust CIs account for spatial correlation
library("mgcv")
library("survival")
library("parallel")
library("dplyr")
library(fst)
library(data.table)

f <- list.files("/nfs/nsaph_ci3/ci3_health_data/medicare/mortality/1999_2016/wu/cache_data/merged_by_year_v2",
                pattern = "\\.fst",
                full.names = TRUE)

myvars <- c("year","zip","sex","race","age","dual","entry_age_break","statecode",
            "followup_year","followup_year_plus_one","dead","pm25_ensemble",
            "mean_bmi","smoke_rate","hispanic","pct_blk","medhouseholdincome","medianhousevalue",
            "poverty","education","popdensity", "pct_owner_occ",
            "summer_tmmx","winter_tmmx","summer_rmax","winter_rmax")
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


load("//nfs/home/X/xwu/shared_space/ci3_xwu/National_Causal/data2016_temp/aggregate_data.RData")
all_zip <- unique(aggregate_data$zip)
rm(aggregate_data)
num_uniq_zip <- length(all_zip)

# Save the bootstrapped data to accelarate computing
dir.create(file.path("/nfs/home/X/xwu/shared_space/ci3_nsaph/XiaoWu/Cox_boots2016_temp"), showWarnings = FALSE)
lapply(1:500,function(boots_id){
  set.seed(boots_id)
  zip_sample<-sample(1:num_uniq_zip,floor(2*sqrt(num_uniq_zip)),replace=T) 
  national_merged2016_boots<-subset(national_merged2016, zip %in% all_zip[zip_sample]) 
  write_fst(national_merged2016_boots,paste0("/nfs/home/X/xwu/shared_space/ci3_nsaph/XiaoWu/Cox_boots2016_temp/",
                        boots_id,".fst"))
})


Cox_coefs_boots<-NULL
for (boots_id in 1:500){
  set.seed(boots_id)
  national_merged2016_boots<- read_fst(paste0("/nfs/home/X/xwu/shared_space/ci3_nsaph/XiaoWu/Cox_boots2016_temp/",
                                       boots_id,".fst"))
    ######## Main models ########
  Cox<-coxph(Surv(followup_year,followup_year_plus_one,dead)~pm25_ensemble + 
               mean_bmi + smoke_rate + hispanic + pct_blk +
               medhouseholdincome + medianhousevalue +
               poverty + education + popdensity + pct_owner_occ +
               summer_tmmx + winter_tmmx + summer_rmax + winter_rmax +
               as.factor(year) + as.factor(region)
             +strata(as.factor(entry_age_break))+strata(as.factor(sex))+strata(as.factor(race))+strata(as.factor(dual))
             , data=national_merged2016_boots,
             ties = c("efron"),na.action = na.omit)
  Cox_coefs_boots<-c(Cox_coefs_boots,summary(Cox)$coefficients[1])
  rm(national_merged2016_boots)
}

save(num_uniq_zip,Cox_coefs_boots,file="/nfs/home/X/xwu/shared_space/ci3_xwu/National_Causal/data2016_temp/Boots/Cox_coefs_boots.RData")

load("/nfs/home/X/xwu/shared_space/ci3_xwu/National_Causal/data2016_temp/Cox.RData")

exp(log(1.066)+10*1.96*sd(Cox_coefs_boots) *sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip))
exp(log(1.066)-10*1.96*sd(Cox_coefs_boots) *sqrt(2*sqrt(num_uniq_zip))/sqrt(num_uniq_zip))

