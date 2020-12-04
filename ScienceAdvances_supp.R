# Generate Figures for the supplementary materials
library("ggplot2")
library(grid) 
library(pBrackets) 
library(gridExtra)

pdf("abs_corr_total_2016_noyear.pdf", width = 17 / 1.8, height = 11 / 1.8)
load("~/Dropbox/National_Causal/Causal/correlations2016_temp_noyear.RData")
order <- order(cor_origin)
balance <- data.frame(matrix(NA, nrow = 16 * 3, ncol = 0))
balance$Covariates <- rep(1:16, 3)
balance$Correlation <- c(sort(cor_origin), cor_matched2[order], cor_weight2[order])
balance$covariates_name <- rep(c("Year", "Region", "Avg BMI", "% Ever Smoker", "% Hispanic", "% Black","Med Hhold Inc","Med Hm Val",
                                 "% < Poverty", "% < High School Edu", "Popn Dens", "% Hm Owner Occ",
                                 "Summer Temperature", "Summer Humidity", "Winter Temperature", "Winter Humidity")[order], 3)
balance$Implementations <- c(rep("unadjusted", 16), rep("matching", 16), rep("weighting", 16))
balance$Implementations <- factor(balance$Implementations, levels = c("unadjusted", "matching", "weighting"))

p1 <- ggplot(balance, aes(x = Correlation, y = Covariates, colour = Implementations)) + 
  scale_y_discrete(limit = 1:16,
                   labels = c("Calender Year", "Census Region", "Mean BMI", "% Ever Smoked", "% Hispanic", "% Black",
                              "Median Household Income", "Median Home Value",
                              "% Below Poverty Level", "% Below High School Education", "Population Density", "% Owner-occupied Housing",
                              "Summer Temperature", "Summer Humidity", "Winter Temperature", "Winter Humidity")[order]) + 
  geom_point() +
  geom_path() +
  geom_vline(xintercept = 0.1) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 10), legend.position = c(0.7, 0.2),
        axis.text.y = element_text(angle = 45, hjust = 1)) +
  ggtitle("Entire Medicare Enrollees (2000-2016)") + 
  xlab("Absolute Correlation") +
  xlim(0, 0.7)

load("~/Dropbox/National_Causal/Causal/correlations2016_temp_low_noyear.RData")
order <- order(cor_origin)
balance <- data.frame(matrix(NA, nrow = 16 * 3,ncol = 0))
balance$Covariates <- rep(1:16, 3)
balance$Correlation <- c(sort(cor_origin), cor_matched2[order], cor_weight2[order])

balance$covariates_name <- rep(c("Year", "Region", "Avg BMI", "% Ever Smoker", "% Hispanic", "% Black", "Med Hhold Inc", "Med Hm Val",
                                 "% < Poverty", "% < High School Edu", "Popn Dens", "% Hm Owner Occ",
                                 "Summer Temperature", "Summer Humidity", "Winter Temperature", "Winter Humidity")[order], 3)
balance$Implementations <- c(rep("unadjusted", 16), rep("matching", 16), rep("weighting", 16))
balance$Implementations <- factor(balance$Implementations, levels = c("unadjusted", "matching", "weighting"))


p2 <- ggplot(balance, aes(x = Correlation, y = Covariates, colour = Implementations)) + 
  scale_y_discrete(limit = 1:16,
                   labels = c("Calender Year", "Census Region", "Mean BMI", "% Ever Smoked", "% Hispanic", "% Black",
                              "Median Household Income", "Median Home Value",
                              "% Below Poverty Level", "% Below High School Education", "Population Density", "% Owner-occupied Housing",
                              "Summer Temperature", "Summer Humidity", "Winter Temperature", "Winter Humidity")[order]) +
  geom_point() +
  geom_path() +
  geom_vline(xintercept = 0.1) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 10), legend.position = c(0.7, 0.2),
        axis.text.y = element_text(angle = 45, hjust = 1)) +
  ggtitle(c(expression(paste("Medicare Enrollees Exposed to PM"[2.5] <= 12 , mu, g/m^3)))) + 
  xlab("Absolute Correlation") +
  xlim(0, 0.5)
grid.arrange(p1, p2, nrow = 1)
dev.off()


pdf("abs_corr_total_2016_notemp.pdf", width = 17 / 1.8, height = 11 / 1.8)
load("~/Dropbox/National_Causal/Causal/correlations2016_temp_notemp.RData")
order <- order(cor_origin)
balance <- data.frame(matrix(NA, nrow = 16 * 3, ncol = 0))
balance$Covariates <- rep(1:16, 3)
balance$Correlation <- c(sort(cor_origin), cor_matched2[order], cor_weight2[order])
balance$covariates_name <- rep(c("Year", "Region", "Avg BMI", "% Ever Smoker", "% Hispanic", "% Black", "Med Hhold Inc", "Med Hm Val",
                                 "% < Poverty", "% < High School Edu", "Popn Dens", "% Hm Owner Occ",
                                 "Summer Temperature", "Summer Humidity", "Winter Temperature", "Winter Humidity")[order], 3)
balance$Implementations <- c(rep("unadjusted", 16), rep("matching", 16), rep("weighting", 16))
balance$Implementations <- factor(balance$Implementations, levels = c("unadjusted", "matching", "weighting"))

p1 <- ggplot(balance, aes(x = Correlation, y = Covariates, colour = Implementations)) + 
  scale_y_discrete(limit = 1:16,
                   labels = c("Calender Year", "Census Region", "Mean BMI", "% Ever Smoked", "% Hispanic", "% Black",
                              "Median Household Income", "Median Home Value",
                              "% Below Poverty Level", "% Below High School Education", "Population Density", "% Owner-occupied Housing",
                              "Summer Temperature", "Summer Humidity", "Winter Temperature", "Winter Humidity")[order]) + 
  geom_point() +
  geom_path() +
  geom_vline(xintercept = 0.1) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 10), legend.position = c(0.7, 0.2),
        axis.text.y = element_text(angle = 45, hjust = 1)) +
  ggtitle("Entire Medicare Enrollees (2000-2016)") + 
  xlab("Absolute Correlation") +
  xlim(0, 0.5)

load("~/Dropbox/National_Causal/Causal/correlations2016_temp_low_notemp.RData")
order <- order(cor_origin)
balance <- data.frame(matrix(NA, nrow = 16 * 3, ncol = 0))
balance$Covariates <- rep(1:16,3)
balance$Correlation <- c(sort(cor_origin),cor_matched2[order],cor_weight2[order])

balance$covariates_name <- rep(c("Year","Region","Avg BMI","% Ever Smoker","% Hispanic","% Black","Med Hhold Inc","Med Hm Val",
                                 "% < Poverty","% < High School Edu","Popn Dens","% Hm Owner Occ","Summer Temperature","Summer Humidity","Winter Temperature","Winter Humidity")[order],3)
balance$Implementations <- c(rep("unadjusted",16), rep("matching",16), rep("weighting",16))
balance$Implementations <- factor(balance$Implementations, levels = c("unadjusted", "matching", "weighting"))


p2<- ggplot(balance, aes(x=Correlation, y=Covariates, colour=Implementations)) + 
  scale_y_discrete(limit = 1:16,
                   labels = c("Calender Year","Census Region","Mean BMI","% Ever Smoked","% Hispanic","% Black","Median Household Income","Median Home Value",
                              "% Below Poverty Level","% Below High School Education","Population Density","% Owner-occupied Housing","Summer Temperature","Summer Humidity","Winter Temperature","Winter Humidity")[order])+

  geom_point() +
  geom_path() +
  geom_vline(xintercept = 0.1) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5,size=10),legend.position = c(0.7, 0.2),
        axis.text.y = element_text(angle = 45, hjust = 1)) +
  ggtitle(c(expression(paste("Medicare Enrollees Exposed to PM"[2.5] <= 12 ,mu, g/m^3)))) + 
  xlab("Absolute Correlation") +
  xlim(0, 0.5)
grid.arrange(p1, p2, nrow = 1)

dev.off()


pdf("abs_corr_total_2012.pdf",width = 17/1.8, height = 11/1.8)
load("~/Dropbox/National_Causal/Causal/correlations2012_temp.RData")
order <- order(cor_origin)
balance <- data.frame(matrix(NA,nrow=16*3,ncol = 0))
balance$Covariates <- rep(1:16,3)
balance$Correlation <- c(sort(cor_origin),cor_matched2[order],cor_weight2[order])
balance$covariates_name <- rep(c("Year","Region","Avg BMI","% Ever Smoker","% Hispanic","% Black","Med Hhold Inc","Med Hm Val",
                                 "% < Poverty","% < High School Edu","Popn Dens","% Hm Owner Occ","Summer Temperature","Summer Humidity","Winter Temperature","Winter Humidity")[order],3)
balance$Implementations <- c(rep("unadjusted",16), rep("matching",16), rep("weighting",16))
balance$Implementations <- factor(balance$Implementations, levels = c("unadjusted", "matching", "weighting"))


p1 <- ggplot(balance, aes(x=Correlation, y=Covariates, colour=Implementations)) + 
  scale_y_discrete(limit = 1:16,
                   labels = c("Calender Year","Census Region","Mean BMI","% Ever Smoked","% Hispanic","% Black","Median Household Income","Median Home Value",
                              "% Below Poverty Level","% Below High School Education","Population Density","% Owner-occupied Housing","Summer Temperature","Summer Humidity","Winter Temperature","Winter Humidity")[order]) + 
  geom_point() +
  geom_path() +
  geom_vline(xintercept = 0.1) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5,size=10),legend.position = c(0.7, 0.2),
        axis.text.y = element_text(angle = 45, hjust = 1)) +
  ggtitle("Entire Medicare Enrollees (2000-2012)") + 
  xlab("Absolute Correlation") +
  xlim(0, 0.5)

load("~/Dropbox/National_Causal/Causal/correlations2012_temp_low.RData")
order <- order(cor_origin)
balance <- data.frame(matrix(NA,nrow=16*3,ncol = 0))
balance$Covariates <- rep(1:16,3)
balance$Correlation <- c(sort(cor_origin),cor_matched2[order],cor_weight2[order])

balance$covariates_name <- rep(c("Year","Region","Avg BMI","% Ever Smoker","% Hispanic","% Black","Med Hhold Inc","Med Hm Val",
                                 "% < Poverty","% < High School Edu","Popn Dens","% Hm Owner Occ","Summer Temperature","Summer Humidity","Winter Temperature","Winter Humidity")[order],3)

balance$Implementations <- c(rep("unadjusted",16), rep("matching",16), rep("weighting",16))
balance$Implementations <- factor(balance$Implementations, levels = c("unadjusted", "matching", "weighting"))


p2<- ggplot(balance, aes(x=Correlation, y=Covariates, colour=Implementations)) + 
  scale_y_discrete(limit = 1:16,
                   labels = c("Calender Year","Census Region","Mean BMI","% Ever Smoked","% Hispanic","% Black","Median Household Income","Median Home Value",
                              "% Below Poverty Level","% Below High School Education","Population Density","% Owner-occupied Housing","Summer Temperature","Summer Humidity","Winter Temperature","Winter Humidity")[order])+

  geom_point() +
  geom_path() +
  geom_vline(xintercept = 0.1) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5,size=10),legend.position = c(0.7, 0.2),
        axis.text.y = element_text(angle = 45, hjust = 1)) +
  ggtitle(c(expression(paste("Medicare Enrollees Exposed to PM"[2.5] <= 12 ,mu, g/m^3)))) + 
  xlab("Absolute Correlation") +
  xlim(0, 0.5)
grid.arrange(p1, p2, nrow = 1)

dev.off()


pdf("abs_corr_total_2012_noyear.pdf",width = 17/1.8, height = 11/1.8)
load("~/Dropbox/National_Causal/Causal/correlations2012_temp_noyear.RData")
order <- order(cor_origin)
balance <- data.frame(matrix(NA,nrow=16*3,ncol = 0))
balance$Covariates <- rep(1:16,3)
balance$Correlation <- c(sort(cor_origin),cor_matched2[order],cor_weight2[order])
balance$covariates_name <- rep(c("Year","Region","Avg BMI","% Ever Smoker","% Hispanic","% Black","Med Hhold Inc","Med Hm Val",
                                 "% < Poverty","% < High School Edu","Popn Dens","% Hm Owner Occ","Summer Temperature","Summer Humidity","Winter Temperature","Winter Humidity")[order],3)
balance$Implementations <- c(rep("unadjusted",16), rep("matching",16), rep("weighting",16))
balance$Implementations <- factor(balance$Implementations, levels = c("unadjusted", "matching", "weighting"))


p1 <- ggplot(balance, aes(x=Correlation, y=Covariates, colour=Implementations)) + 
  scale_y_discrete(limit = 1:16,
                   labels = c("Calender Year","Census Region","Mean BMI","% Ever Smoked","% Hispanic","% Black","Median Household Income","Median Home Value",
                              "% Below Poverty Level","% Below High School Education","Population Density","% Owner-occupied Housing","Summer Temperature","Summer Humidity","Winter Temperature","Winter Humidity")[order]) + 
  geom_point() +
  geom_path() +
  geom_vline(xintercept = 0.1) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5,size=10),legend.position = c(0.7, 0.2),
        axis.text.y = element_text(angle = 45, hjust = 1)) +
  ggtitle("Entire Medicare Enrollees (2000-2012)") + 
  xlab("Absolute Correlation") +
  xlim(0, 0.5)

load("~/Dropbox/National_Causal/Causal/correlations2012_temp_low_noyear.RData")
order <- order(cor_origin)
balance <- data.frame(matrix(NA,nrow=16*3,ncol = 0))
balance$Covariates <- rep(1:16,3)
balance$Correlation <- c(sort(cor_origin),cor_matched2[order],cor_weight2[order])

balance$covariates_name <- rep(c("Year","Region","Avg BMI","% Ever Smoker","% Hispanic","% Black","Med Hhold Inc","Med Hm Val",
                                 "% < Poverty","% < High School Edu","Popn Dens","% Hm Owner Occ","Summer Temperature","Summer Humidity","Winter Temperature","Winter Humidity")[order],3)
balance$Implementations <- c(rep("unadjusted",16), rep("matching",16), rep("weighting",16))
balance$Implementations <- factor(balance$Implementations, levels = c("unadjusted", "matching", "weighting"))


p2<- ggplot(balance, aes(x=Correlation, y=Covariates, colour=Implementations)) + 
  scale_y_discrete(limit = 1:16,
                   labels = c("Calender Year","Census Region","Mean BMI","% Ever Smoked","% Hispanic","% Black","Median Household Income","Median Home Value",
                              "% Below Poverty Level","% Below High School Education","Population Density","% Owner-occupied Housing","Summer Temperature","Summer Humidity","Winter Temperature","Winter Humidity")[order])+

  geom_point() +
  geom_path() +
  geom_vline(xintercept = 0.1) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5,size=10),legend.position = c(0.7, 0.2),
        axis.text.y = element_text(angle = 45, hjust = 1)) +
  ggtitle(c(expression(paste("Medicare Enrollees Exposed to PM"[2.5] <= 12 ,mu, g/m^3)))) + 
  xlab("Absolute Correlation") +
  xlim(0, 0.5)
grid.arrange(p1, p2, nrow = 1)

dev.off()


data <- data.frame(method = c(rep(c("Cox", "Poisson", "Matching", "Weighting", "Adjustment"),2)),
                   HR = c(5.9, 5.5, 5.5, 6.7, 4.7, 
                          36.7, 34.2, 27.1, 29.8, 23.3
                   )/100+1,
                   lower_CI = c(5.1, 4.8, 4.2, 5.6, 3.7,
                                33.1, 30.8, 24.1, 25.4, 17.6
                   )/100+1,
                   upper_CI = c(6.7, 6.3, 6.8, 7.9, 5.7,
                                40.4, 37.7, 30.1, 34.4, 29.2
                   )/100+1,
                   Methods = c(1:5,1:5),
                   type = c(rep("Adjust for Year (2012)",5),rep("Low level",5)))
bracketsGrob <- function(...){ 
  l <- list(...) 
  e <- new.env() 
  e$l <- l 
  grid:::recordGrob( { 
    do.call(grid.brackets, l) 
  }, e) 
} 


pdf("HR_sep2012.pdf",width = 20, height = 8.5)

p1 <- ggplot(data[1:5,], aes(x=Methods, y=HR, colour= type),size= 5) + 
  ylab("Hazard Ratios (%)") +
  geom_point(aes(size = 1)) +
  geom_errorbar(aes(ymin=lower_CI, ymax=upper_CI), width=.2,size=1) +
  geom_vline(xintercept = 2.5, linetype="dashed", size=0.8)  +
  annotate(geom = "text", x = seq_len(nrow(data)), y = 1-0.75/100, label = data$method, size = 8) +
  annotate(geom = "text", x = c(3), y = 1+21/100, label = c("Entire Medicare Enrollees (2000-2012)"), size = 8) +
  annotate(geom = "text", x = c(c(1.5+5 * (0),4+5 * (0))), y = 1-1.75/100, label = c(rep(c("Regression"),1),rep(c("Causal"),1)), size = 8) +
  coord_cartesian(ylim = c(1, 1.2), xlim = c(0.5, 5.5), expand = FALSE, clip = "off")  +
  geom_segment(aes(x = 2.5, y = 1-2/100, xend = 2.5, yend = 1),colour="black") +
  theme_bw() +
  theme(plot.margin = unit(c(3, 2, 4, 1), "lines"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=16),
        axis.title.y = element_text(size = 20),
        legend.position = "none")

p2<- ggplot(data[6:10,], aes(x=Methods, y=HR),size= 5) + 
  ylab("Hazard Ratios (%)") +
  geom_point(aes(size = 1), colour="darkblue") +
  geom_errorbar(aes(ymin=lower_CI, ymax=upper_CI), width=.2,size=1, colour="darkblue") +
  geom_vline(xintercept = 2.5, linetype="dashed", size=0.8)  +
  annotate(geom = "text", x = seq_len(nrow(data)), y = 1-1.875/100, label = data$method, size = 8) +
  annotate(geom = "text", x = c(3), y = 1 + 52.5 / 100, 
           label = c(expression(paste("Medicare Enrollees Exposed to PM"[2.5] <= 12 , mu, g/m^3, "(2000-2012)"))), size = 8) +
  annotate(geom = "text", x = c(c(1.5 + 5 * (0), 4 + 5 * (0))), y = 1 - 4.375 / 100, label = c(rep(c("Regression"), 1), rep(c("Causal"), 1)), size = 8) +
  coord_cartesian(ylim = c(1, 1.50), xlim = c(0.5, 5.5), expand = FALSE, clip = "off")  +
  geom_segment(aes(x = 2.5, y = 1 - 2 / 100, xend = 2.5, yend = 1), colour = "black") +
  theme_bw() +
  theme(plot.margin = unit(c(3, 2, 4, 1), "lines"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        legend.position = "none")

grid.arrange(p1, p2, nrow = 1)
dev.off()



load("./2012_temp/covariates2012_temp.RData")
covariates2012 <- covariates
load("./boots/2016_temp/covariates2016_temp.RData")
covariates2016 <- covariates
load("./boots/2016_temp_low/covariates2016_temp_low.RData")
covariates2016_low <- covariates
load("./boots/2012_temp_low/covariates2012_temp_low.RData")
covariates2012_low <- covariates

covariates2012$Cohort <- 'Years: 2000-2012'
covariates2016$Cohort <- 'Years: 2000-2016'
covariates2016_low$Cohort <- 'Years: 2000-2016 (Low Level)'
covariates2012_low$Cohort <- 'Years: 2000-2012 (Low Level)'

GPS <- rbind(covariates2012, covariates2016, covariates2016_low, covariates2012_low)

pdf("gps_boost3.pdf",width = 11 / 1.8, height = 8.5 / 1.8)
ggplot(GPS, aes(x=GPS, fill = Cohort)) + 
  geom_density(alpha = 0.2) +
  xlab("Values of estimated GPS") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),legend.position = c(0.2, 0.8))
dev.off()



