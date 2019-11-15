# Calculate the number of death saved among elderly in the US for one decade due to compliance of air quality policy 
library("parallel")


# Load data
dir_data = '/nfs/home/X/xwu/shared_space/ci3_xwu/National_Causal/data2016_temp/'
dir_out = '/nfs/home/X/xwu/shared_space/ci3_xwu/National_Causal/data2016_temp/'

load(paste0(dir_data,"covariates.RData"))
load(paste0(dir_data,"aggregate_data.RData"))

# Caculate the number of death and person-time in each zip code and year
dead_personyear<-aggregate(cbind(aggregate_data$dead,aggregate_data$time_count), by=list(aggregate_data$zip,aggregate_data$year), FUN=sum)
pm <-aggregate(aggregate_data$pm25_ensemble, by=list(aggregate_data$zip,aggregate_data$year), FUN=min)
zip_data<-merge(dead_personyear,pm,by=c("Group.1", "Group.2"))
colnames(zip_data)[1:5]<-c("zip","year","dead","time_count","pm25_ensemble")

# Choose one decade
zip_data2016 <- subset(zip_data, pm25_ensemble >12 & year>2006, year<=2016)

# Calculate the number of death saved due to compliance of NAAQS (PM2.5 <= 12 mug/m^3)
sum(zip_data2016$dead/zip_data2016$time_count*(1-(1/1.068)^((zip_data2016$pm25_ensemble-12)/10))*zip_data2016$time_count)
sum(zip_data2016$dead/zip_data2016$time_count*(1-(1/1.054)^((zip_data2016$pm25_ensemble-12)/10))*zip_data2016$time_count)
sum(zip_data2016$dead/zip_data2016$time_count*(1-(1/1.083)^((zip_data2016$pm25_ensemble-12)/10))*zip_data2016$time_count)


# Calculate the additional number of death saved due to compliance of WHO standard (PM2.5 <= 10 mug/m^3) compared to current NAAQS (PM2.5 <= 12 mug/m^3)
zip_data2016_10 <- subset(zip_data, pm25_ensemble >10 & year>2006, year<=2016)
zip_data2016_10[zip_data2016_10$pm25_ensemble >12, ]$pm25_ensemble = 12

sum(zip_data2016_10$dead/zip_data2016_10$time_count*(1-(1/1.231)^((zip_data2016_10$pm25_ensemble-10)/10))*zip_data2016_10$time_count)
sum(zip_data2016_10$dead/zip_data2016_10$time_count*(1-(1/1.180)^((zip_data2016_10$pm25_ensemble-10)/10))*zip_data2016_10$time_count)
sum(zip_data2016_10$dead/zip_data2016_10$time_count*(1-(1/1.284)^((zip_data2016_10$pm25_ensemble-10)/10))*zip_data2016_10$time_count)


               