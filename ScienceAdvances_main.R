# Generate Figures for the main text
library("ggplot2")
library(grid) 
library(pBrackets) 
library(gridExtra)
data <- data.frame(method = c(rep(c("Cox", "Poisson", "Matching", "Weighting", "Adjustment"),2)),
                   HR = c(6.6, 6.2, 6.8, 7.6, 7.2, 
                          36.9, 34.7, 26.1, 26.8, 23.1
                          )/100+1,
                   lower_CI = c(5.8, 5.5, 5.4, 6.5, 6.1,
                                34.0, 32.0, 23.3, 23.7, 18.0
                                )/100+1,
                   upper_CI = c(7.4, 6.9, 8.3, 8.8, 8.2,
                                39.9, 37.5, 28.9, 30.0, 28.4
                                )/100+1,
                   Methods = c(1:5,1:5),
                   type = c(rep("Adjust for Year (2016)",5),rep("Low level",5)))

bracketsGrob <- function(...){ 
  l <- list(...) 
  e <- new.env() 
  e$l <- l 
  grid:::recordGrob( { 
    do.call(grid.brackets, l) 
  }, e) 
} 

# Figure 3
pdf("HR_sep2.pdf",width = 20, height = 8.5)

p1 <- ggplot(data[1:5,], aes(x=Methods, y=HR, colour= type),size= 5) + 
  ylab("Hazard Ratios (%)") +
  geom_point(aes(size = 1)) +
  geom_errorbar(aes(ymin=lower_CI, ymax=upper_CI), width=.2,size=1) +
  geom_vline(xintercept = 2.5, linetype="dashed", size=0.8)  +
  annotate(geom = "text", x = seq_len(nrow(data)), y = 1-0.75/100, label = data$method, size = 8) +
  annotate(geom = "text", x = c(3), y = 1+21/100, label = c("Entire Medicare Enrollees (2000-2016)"), size = 8) +
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
  annotate(geom = "text", x = c(3), y = 1+52.5/100, label = c(expression(paste("Medicare Enrollees Exposed to PM"[2.5] <= 12 ,mu, g/m^3,"(2000-2016)"))), size = 8) +
  annotate(geom = "text", x = c(c(1.5+5 * (0),4+5 * (0))), y = 1-4.375/100, label = c(rep(c("Regression"),1),rep(c("Causal"),1)), size = 8) +
  coord_cartesian(ylim = c(1, 1.50), xlim = c(0.5, 5.5), expand = FALSE, clip = "off")  +
  geom_segment(aes(x = 2.5, y = 1-2/100, xend = 2.5, yend = 1),colour="black") +
  theme_bw() +
  theme(plot.margin = unit(c(3, 2, 4, 1), "lines"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=16),
        axis.title.y = element_text(size = 20),
        legend.position = "none")

grid.arrange(p1, p2, nrow = 1)

dev.off()

# Figure 2
pdf("abs_corr_total_2016.pdf",width = 17/1.8, height = 11/1.8)
load("~/Dropbox/National_Causal/Causal/correlations2016_temp.RData")
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
  ggtitle("Entire Medicare Enrollees (2000-2016)") + 
  xlab("Absolute Correlation") +
  xlim(0, 0.5)

load("~/Dropbox/National_Causal/Causal/correlations2016_temp_low.RData")
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

