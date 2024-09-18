### create grids around the enclosures 

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
library(ggspatial)
###

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

bfnp <- read_sf("data/spatial_data/Outline/WDPA_WDOECM_Jul2024_Public_667_shp_0/WDPA_WDOECM_Jul2024_Public_667_shp-polygons.shp") %>% summarize() %>% st_transform(crs = 4326)

grids <- bfnp %>% 
  st_transform(crs = "EPSG:25832") %>% 
  st_make_grid(cellsize = c(100, 100)) %>% 
  st_as_sf() %>% st_transform(crs = "EPSG:4326") %>% 
  mutate(gridID = paste0("grid", 1:nrow(.)))
mapview(grids) 
dist <- st_distance(grids, enclosure.locs)

gridCovRaw <- data.table(
  gridID = unique(grids$gridID), 
  dist_C =  as.numeric(dist[,1]),
  dist_D =  as.numeric(dist[,2]),
  dist_A =  as.numeric(dist[,3]),
  dist_B =  as.numeric(dist[,4])
) %>% mutate(
  Site_ID = case_when(
    dist_A <= 2500 ~ "A",  
    dist_B <= 2500 ~ "B", 
    dist_C <= 2500 ~ "C", 
    dist_D <= 2500 ~ "D"
  )
) %>% filter(!is.na(Site_ID)) %>% 
  pivot_longer(cols = c(dist_C, dist_D, dist_A, dist_B)) %>% 
  group_by(Site_ID, gridID) %>% 
  summarize(min_dist_enclosure = as.numeric(min(value))) %>% 
  left_join(enclosure.meta) %>% 
  left_join(grids) %>% 
  st_as_sf()

mapview(gridCovRaw)

deadWood <- read_sf("data/spatial_data/Bodentypen und Totholz/Totholz.shp")
#mapview(deadWood, zcol = "Change_Yea")
st_crs(deadWood)
min(deadWood$Change_Yea, na.rm = T)

dt.sf.tr <- st_transform(gridCovRaw, crs = st_crs(deadWood))

dt.sf.tr1 <- st_join(dt.sf.tr, deadWood) 
mapview(dt.sf.tr1, zcol = "Totholz")

barkBeetle <- dt.sf.tr1 %>% 
  dplyr::select(Change_Yea, Not_Before, Not_After, gridID) %>%
  rename(
    deadWoodChangeYear = Change_Yea, 
    deadWoodNotBefore = Not_Before, 
    deadWoodNotAfter = Not_After) %>% 
  as.data.table() %>% 
  mutate(deadWood = ifelse(is.na(deadWoodChangeYear), "No deadwood", "Deadwood"), 
         deadWoodChangeYear = ifelse(is.na(deadWoodChangeYear), 1980, deadWoodChangeYear), 
         x = NULL) %>% unique() %>% 
  group_by(gridID) %>% slice_max(deadWoodChangeYear)


## elevation 

dem <- terra::rast("data/spatial_data/DEM/ElevationBFNP30m.tif")
plot(dem)

gridCovRawTrans <- st_transform(gridCovRaw, crs = st_crs(dem))
dem.ex <- terra::extract(dem, # the rast layers
                         vect(gridCovRawTrans), # the spatial polygons, which you have to convert to a terra format
                         mean, na.rm = T) # since we're dealing with polygons we need to summarize per overlapping pixel somehow

dem.ex # 
setDT(extraction)

gridCovRaw$elevation <- dem.ex[,2]
mapview(gridCovRaw, zcol = "elevation")


gridCov <- gridCovRaw %>%
  left_join(barkBeetle) %>% 
  mutate(
  min_dist_enclosure_scaled = as.numeric(scale(min_dist_enclosure)), 
  elevation_scaled = as.numeric(scale(elevation)), 
  deer_density_scaled = as.numeric(scale(deer_density)), 
  deadWoodChangeYear_scaled = as.numeric(scale(deadWoodChangeYear))
  
)
  
### load model data 

dt.mod <- fread("data/clean_data/bfnp_enclosure_model_data.csv") %>% 
  dplyr::filter(!flag == "exclude") %>% 
  mutate(enclosure_name = as.factor(enclosure_name))

dt.veg <- dt.mod[type == 'Vegetation' & tier == 'Enclosures', ]


library(mgcv)
vn <- gam(n ~ s(min_dist_enclosure_scaled, k = 3) + elevation_scaled + s(min_dist_enclosure_scaled, by = deer_density_scaled) + deadWoodChangeYear_scaled, data = dt.veg, select = TRUE, method = "REML")
summary(vn)

gridCov$vegNPred <- as.numeric(predict(vn, newdata = gridCov))

mapview(gridCov, zcol = "vegNPred")


p <- ggplot() +
  geom_sf(data= bfnp) +
  geom_sf(data = gridCov, aes(fill = vegNPred, color = vegNPred), alpha = 0.8) +
  scale_fill_viridis_c(option = "C") +
  scale_color_viridis_c(option = "C") +
  annotation_scale( location = "bl",bar_cols = c("grey60", "white")) +
  geom_sf(data = enclosure.locs, fill = "transparent", linewidth = 1.1) +
  theme_void() +
  labs(color = "Predicted\nVegetation N\n(mg/g)", fill = "Predicted\nVegetation N\n(mg/g)")
p

ggsave(plot = p, "builds/plots/predictedPlantN.png", dpi = 600)
  

