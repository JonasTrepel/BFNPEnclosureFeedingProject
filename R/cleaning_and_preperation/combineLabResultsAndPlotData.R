#join plant and soil results 

rm(list = ls())

library(data.table)
library(ggplot2)
library(tidyverse)
library(sf)
library(gridExtra)
library(glmmTMB)
library(MuMIn)
library(segmented)
library(mapview)
library(terra)
## load plot data 

dt <- fread("data/spatial_data/plot_locations/Cleaned_Plot_Data_BFNP.csv")
dt$Landcover <- gsub("l", "L", dt$Landcover)
dt$Landcover <- gsub("f", "F", dt$Landcover)

table(dt$Distance_Level)

dt[Distance_Level == 0, Distance_Enclosure := -100 ]
dt[Distance_Level == 1, Distance_Enclosure := 100 ]
dt[Distance_Level == 2, Distance_Enclosure := 800 ]
dt[Distance_Level == 3, Distance_Enclosure := 2100 ]
#mapview(dt)

cn <- fread("data/raw_data/lab_results/Plant_CN_May2023.csv")
cn <- cn[,.(Plot_ID, N, C)]
cn$Plot_ID <- gsub("-", "_", cn$Plot_ID)
icp <- fread("data/raw_data/lab_results/Plant_ICP_May2023.csv")
icp <- icp[,.(Plot_ID, K, P, Mg, Ca, Na)]
icp$Plot_ID <- gsub("-", "_", icp$Plot_ID)

names(dt)
dt$Type

dt.p <- dt[Type == "Vegetation"]

dt.res.p <- left_join(dt.p, cn, by = "Plot_ID")
dt.res.p <- left_join(dt.res.p, icp, by = "Plot_ID")

dt.res.p
dt.res.p[, Units := "K, P, Mg, Ca and Na are mg/g dry weight; N and C is %"]


## soil 
soil.res <- fread("data/raw_data/lab_results/Soil_May2023.csv")
soil.res$Plot_ID <- gsub("-", "_", soil.res$Plot_ID)

names(dt)
dt$Type

dt.s <- dt[Type == "Soil"]

dt.res.s <- left_join(dt.s, soil.res, by = "Plot_ID")

dt.res.s[, Units := "Al, Mg, Na, Ca, K, Acidity & ECEC are cmolc/kg dry weight; P is mg/kg, N and C is %"]


names(dt.res.p)

names(dt.res.s)
setnames(dt.res.s, c( "Exchang_Al", "Exchange_Mg", "Exchange_Ca", "Exchange_Na", "Exchange_K"),
         c("Al", "Mg", "Ca", "Na", "K"))

dt.res.p[, ECEC := NA] 
dt.res.p[, Base_saturation := NA] 
dt.res.p[, Exchange_Acidity := NA] 
dt.res.p[, Al := NA] 

setdiff(names(dt.res.p), names(dt.res.s))

dt.res <- rbind(dt.res.p, dt.res.s)

dt.res[, Mountain_Or_Enclosure := ifelse(grepl("M", dt$Site_ID), "Mountain", "Enclosure")]

table(dt.res$Mountain_Or_Enclosure)

ggplot() +
  geom_point(data = dt.res[Mountain_Or_Enclosure == "Enclosure" & Type == "Vegetation",], aes(x = Distance_Level, y = N))

dt.sf <- st_as_sf(dt.res, 
                  coords = c("X", "Y"), 
                  crs = 4326)
mapview(dt.sf)

dt <- dt.res[,-c("garmin_elevation", "ID", "U2018_CLC2018_V2020_20u1",
             "ID.1", "Priority")]

mean(dt[Type == "Vegetation" & !Distance_Enclosure > 0, ]$P)
quantile(dt[Type == "Vegetation" & !Distance_Enclosure > 0, ]$P)

sd(dt[Type == "Vegetation" & !Distance_Enclosure > 0, ]$P)


## add hight above nearest drainage 
hand <- rast("data/spatial_data/HAND_BFNP_30m.tif")
plot(hand)

dt.sf.trans <- st_transform(dt.sf, crs = st_crs(hand))
hand.ex <- terra::extract(hand, # the rast layers
                          vect(dt.sf.trans), # the spatial polygons, which you have to convert to a terra format
                          mean, na.rm = T) # since we're dealing with polygons we need to summarize per overlapping pixel somehow

hand.ex # 
setDT(hand.ex)

dt.sf.hand <- cbind(dt.sf, hand.ex)
dt.sf.hand <- dt.sf.hand %>% rename(HAND = b1) 
glimpse(dt.sf.hand)

dt.fin <- dt.sf.hand %>% 
  as.data.table() %>%
  mutate(geometry = NULL, 
         ID = NULL)

fwrite(dt.fin, "data/spatial_data/plotAndLabDataMay2023/Plots_and_lab_results_enclosures_and_mountains_2023.csv")


st_write(dt.sf.hand, "data/spatial_data/plotAndLabDataMay2023/Plots_and_lab_results_enclosures_and_mountains_2023.shp")

