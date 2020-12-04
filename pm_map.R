# Generate Figure 1, the long-term PM2.5 maps for the continental US
library(tidyverse)
library(sp)
library("raster")

# Load data
pm_2016 <- readRDS("./National_Causal/Maps/PredictionStep2_Annual_PM25_USGrid_20160101_20161231.rds")
pm <- readRDS("./National_Causal/Maps/USGridSite.rds")
pm$pm_2016 <- as.numeric(pm_2016)
states <- shapefile("cb_2017_us_county_500k.shp")
pm <- as.data.frame(pm)
coordinates(pm) <- ~Lon + Lat
proj4string(pm) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
states <- spTransform(states, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
sp_points2016 <- pm[states,]

data2016 <- cbind.data.frame(coordinates(sp_points2016), pm_2016 = sp_points2016$pm_2016)
names(data2016) <- c("x", "y", "pm_2016")
data2016[data2016$pm_2016 > 16, ]$pm_2016 = 16

png("pm_2016.jpeg", height = 1024 * 0.6, width = 1024)
ggplot() +
  geom_point(data = data2016,
             aes(x = x, y = y, color = pm_2016),
             size=0.15) +
  xlim(-125, -65) + 
  ylim(25, 50) +
  scale_color_gradient2(expression(paste("PM"[2.5])), low  = "#445694", mid = "#f2f4fb", high = "#8b0000", midpoint = 8,
                        breaks = c(0, 4, 8, 12, 16),
                        labels = c("0", "4", "8", "12", "16+"), limits = c(0, 16), na.value = "white") +
  labs(title = expression(paste("Annual Average of PM"[2.5], " per ", mu, g/m^3, " in 2016"))) +
  coord_map() +
  theme_minimal() +
  theme(plot.title = element_text(size = 24, hjust = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        line = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal", 
        legend.text = element_text(angle = 60,  size = 20),
        legend.text.align = 0.75,
        legend.title = element_text(size = 24),
        legend.key.width = unit(150, "points"),
        panel.grid.major = element_line(colour = "transparent"))
dev.off()

pm_2000 <- readRDS("./National_Causal/Maps/PredictionStep2_Annual_PM25_USGrid_20000101_20001231.rds")
pm <- readRDS("./National_Causal/Maps/USGridSite.rds")
pm$pm_2000 <- as.numeric(pm_2000)
states <- shapefile("cb_2017_us_county_500k.shp")
pm <- as.data.frame(pm)
coordinates(pm) <- ~Lon+Lat
proj4string(pm) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
states <- spTransform(states, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
sp_points2000 <- pm[states, ]

data2000 <- cbind.data.frame(coordinates(sp_points2000), sp_points2000$pm_2000)
names(data2000) <- c("x", "y", "pm_2000")
data2000[data2000$pm_2000 > 16, ]$pm_2000 = 16

png("pm_2000.jpeg", height = 1024 * 0.6*2, width = 1024 * 2)
ggplot() +
  geom_point(data = data2000,
             aes(x = x, y = y, color = pm_2000),
             size = 0.15) +
  xlim(-125,-65) +
  ylim(25,50) +
  scale_color_gradient2(expression(paste("PM"[2.5])), low  = "#445694", mid="#f2f4fb", high = "#8b0000", midpoint = 8,
                        breaks = c(0, 4, 8, 12, 16),
                        labels = c("0", "4", "8", "12", "16+"), limits = c(0, 16), na.value = "white") +
  labs(title = expression(paste("Annual Average of PM"[2.5], " per ", mu, g/m^3, " in 2000"))) +
  coord_map() +
  theme_minimal() +
  theme(plot.title = element_text(size = 24 * 2, hjust = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        line = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal", 
        legend.text = element_text(angle = 60,  size = 20 * 2),
        legend.text.align = 0.75,
        legend.title = element_text(size = 24 * 2),
        legend.key.width = unit(150*2, "points"),
        panel.grid.major = element_line(colour = "transparent"))
dev.off()



