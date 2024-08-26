### Complile and prepare model data
rm(list = ls())

library(data.table)
library(ggplot2)
library(tidyverse)
library(sf)
library(mapview)
library(gridExtra)
library(glmmTMB)
library(MuMIn)
library(segmented)
library(MetBrewer)
library(mgcv)


## load data -------------------------


### get exclosure meta data -------

### find a way to also include the densities 

enclosure.locs <- st_read("data/spatial_data/Winter_enclosures/winter_enclosures_NPBF.shp") %>% 
  filter(id == 1) %>% 
  mutate(id2 = 1:nrow(.), 
         Site_ID = case_when(
           id2 %in% c(5) ~ "A",
           id2 %in% c(7) ~ "B",
           id2 %in% c(1) ~ "C",
           id2 %in% c(3) ~ "D",
           id2 %in% c(6, 2, 4) ~ "pre"),
         enclosure_name = case_when(
           Site_ID == "A" ~ "Ahornschachten",
           Site_ID == "B" ~ "Buchenau",
           Site_ID == "C" ~ "Neuhüttenwiese",
           Site_ID == "D" ~ "Riedlhäng"
         ),
         enclosure_age = case_when(
           Site_ID == "A" ~ 2023-1993,
           Site_ID == "B" ~ 2023-1992,
           Site_ID == "C" ~ 2023-1975,
           Site_ID == "D" ~ 2023-1975
         ),
         deer_number = case_when(
           Site_ID == "A" ~ 31,
           Site_ID == "B" ~ 61,
           Site_ID == "C" ~ 114,
           Site_ID == "D" ~ 84
         ),
         area_ha = as.numeric(st_area(.)/10000), 
         deer_density = deer_number/area_ha
  ) %>% filter(Site_ID != "pre")


enclosure.meta <- enclosure.locs %>% 
  as.data.table() %>% 
  mutate(geometry = NULL, 
         id = NULL, id2 = NULL)


### load plot data and lab results 

dt.sf <- read_sf("data/spatial_data/plotAndLabDataMay2023/Plots_and_lab_results_enclosures_and_mountains_2023.shp") 

XY <- as.data.frame(st_coordinates(dt.sf))

coords <- dt.sf %>% 
  mutate(X = XY$X, 
         Y = XY$Y) %>% as.data.table() %>% mutate(geometry = NULL) %>% dplyr::select(c(Plot_ID, X, Y))

### get actual distance to enclosures 

enclosure.locs
mapview(enclosure.locs)

dt.sf.en <- dt.sf %>% filter(Site_ID %in% c("A", "B", "C", "D"))

mapview(enclosure.locs) + mapview(dt.sf.en)

dist <- st_distance(dt.sf.en, enclosure.locs)

dt.dist <- data.table(
  Plot_ID = unique(dt.sf.en$Plot_ID), 
  dist_C =  dist[,1],
  dist_D =  dist[,2],
  dist_A =  dist[,3],
  dist_B =  dist[,4]
) %>% pivot_longer(cols = c(dist_C, dist_D, dist_A, dist_B)) %>% 
  group_by(Plot_ID) %>% 
  summarize(min_dist_enclosure = as.numeric(min(value)))


dt.sf.en2 <- dt.sf.en %>% left_join(dt.dist)
mapview(dt.sf.en2, zcol = "min_dist_enclosure")

quantile(dt.dist$min_dist_enclosure)

### extract bark beetle and soil type stuff 

dt.sf.en

deadWood <- read_sf("data/spatial_data/Bodentypen und Totholz/Totholz.shp")
#mapview(deadWood, zcol = "Change_Yea")
st_crs(deadWood)
min(deadWood$Change_Yea, na.rm = T)

dt.sf.tr <- st_transform(dt.sf.en, crs = st_crs(deadWood))

dt.sf.tr1 <- st_join(dt.sf.tr, deadWood)
mapview(dt.sf.tr1, zcol = "Totholz")


soilType <- read_sf("data/spatial_data/Bodentypen und Totholz/Übersichts-Bodenkarte_LfU.shp")
glimpse(soilType)
mapview(soilType, zcol = "K_LEGENDE")


dt.sf.tr2 <- st_join(dt.sf.tr1, soilType)
summary(dt.sf.tr2)
table(dt.sf.tr2$K_LEGENDE)
mapview(dt.sf.tr2, zcol = "L_LEGENDE")


barkBeetleAndSoil <- dt.sf.tr2 %>% 
  dplyr::select(L_LEGENDE, K_LEGENDE, Change_Yea, Not_Before, Not_After, Plot_ID) %>%
  rename(
    soilTypeLegendL = L_LEGENDE, 
    soilTypeLegendK = K_LEGENDE, 
    deadWoodChangeYear = Change_Yea, 
    deadWoodNotBefore = Not_Before, 
    deadWoodNotAfter = Not_After, 
    plot_id = Plot_ID) %>% 
  as.data.table() %>% 
  mutate(deadWood = ifelse(is.na(deadWoodChangeYear), "No deadwood", "Deadwood"), 
         deadWoodChangeYear = ifelse(is.na(deadWoodChangeYear), 1980, deadWoodChangeYear), 
         geometry = NULL) %>% unique() %>% 
  group_by(plot_id) %>% slice_max(deadWoodChangeYear)
  

n_distinct(barkBeetleAndSoil$plot_id)
### load lab results and combine

dt.res <- fread("data/spatial_data/plotAndLabDataMay2023/Plots_and_lab_results_enclosures_and_mountains_2023.csv") %>% 
  left_join(coords) %>% 
  mutate(flag = ifelse(Notes %in% c(
    "Very close to road; soil 20m deeper in forest",
    "Very close to road",
    "Close to building",
    "Recent clearcut; be careful with soil compaction",
    "Close to annoyingly well used sreet",
    "Close to gravel road",
    "Very close (5m) to fertilized meadow/n/n",
    "Close to fertilized meadow (25m)",
    "Soil wet",
    "Close to forest road",
    "Rather wet soil"
  ), "exclude", "ok")) %>% 
  rename(distance_level = Distance_Level, 
         type = Type) %>% 
  dplyr::select(-Distance_Enclosure) %>% 
  mutate(
    distance_enclosure = case_when(
      distance_level == 0 ~ -100, 
      distance_level == 1 ~ 100, 
      distance_level == 2 ~ 900, 
      distance_level == 3 ~ 2100), 
    tier = ifelse(distance_level %in% c("Control", "Treatment"), "Mountains", "Enclosures"), 
    distance_enclosure_scaled = as.numeric(scale(distance_enclosure)), 
    elevation_scaled = as.numeric(scale(Elevation)), 
    hand_scaled = as.numeric(scale(HAND)), ## Hand = height above nearest drainage 
    Site_ID = as.factor(Site_ID)
  ) %>% rename(elevation = Elevation, 
               habitat_type = Habitat_Ty) %>% 
  left_join(enclosure.meta) %>% 
  mutate(deer_density_scaled = as.numeric(scale(deer_density)), 
         deer_number_scaled = as.numeric(scale(deer_number)), 
         enclosure_age_scaled = as.numeric(scale(enclosure_age)), 
         Transect_ID = paste0(Site_ID, Transect_ID)
         ) %>% 
  left_join(dt.dist) %>% 
  mutate(min_dist_enclosure_scaled = as.numeric(scale(min_dist_enclosure)))


names(dt.res) <- tolower(names(dt.res))

transect_start_elevation <- dt.res %>% dplyr::filter(distance_level == 0) %>% dplyr::select(transect_id, elevation) %>% unique() %>% 
  rename(transect_start_elevation = elevation)

dt.res2 <- dt.res %>% left_join(transect_start_elevation) %>% 
  mutate(elevation_rel_enc = elevation - transect_start_elevation, 
         cn = c/n, 
         np = n/p, 
         elevation_rel_enc_scaled = as.numeric(scale(elevation_rel_enc))) %>% 
  left_join(barkBeetleAndSoil) %>% 
  mutate(
    soilTypeLegendK_scaled = soilTypeLegendK, 
    deadWoodChangeYear_scaled = as.numeric(scale(deadWoodChangeYear)), 
    deadWood_scaled = deadWood
  )

fwrite(dt.res2,"data/clean_data/bfnp_enclosure_model_data.csv")

table(dt.res2$deadWoodChangeYear)
table(dt.res2$deadWood)
table(dt.res2$soilTypeLegendK)


