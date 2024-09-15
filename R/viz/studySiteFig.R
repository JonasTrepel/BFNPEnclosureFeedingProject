library(readr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(mgcv)
library(visreg)
library(MetBrewer)
library(data.table)
library(ggridges)
library(gridExtra)
library(interactions)
library(glmmTMB)
library(sf)
library(ggspatial)

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


### load plot data and lab results 
dt.mod <- fread("data/clean_data/bfnp_enclosure_model_data.csv") %>% 
  dplyr::filter(!flag == "exclude") %>% 
  mutate(enclosure_name = as.factor(enclosure_name))

dt.sf <- read_sf("data/spatial_data/plotAndLabDataMay2023/Plots_and_lab_results_enclosures_and_mountains_2023.shp") 
xy <- st_coordinates(dt.sf)
dt.sf.coords <- dt.sf %>% cbind(xy) %>% filter(Mnt_O_E == "Enclosure") %>%  mutate(flag = ifelse(Notes %in% c(
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
), "exclude", "ok"))

bfnp <- read_sf("data/spatial_data/Outline/WDPA_WDOECM_Jul2024_Public_667_shp_0/WDPA_WDOECM_Jul2024_Public_667_shp-polygons.shp") %>% summarize() %>% st_transform(crs = 4326)
mapview::mapview(bfnp)


p.loc <- ggplot() +
  geom_sf(data = bfnp, fill = "grey95" ) +
  annotation_scale( location = "bl",bar_cols = c("grey60", "white")) +
  geom_point(data=dt.sf.coords, aes(x = X, y = Y, color = flag), size = 3, alpha = .7) +
  geom_sf(data = enclosure.locs, aes(fill = enclosure_name)) +
  scale_fill_met_d(name = "Egypt") +
  scale_color_manual(values = c("ok" = "black", "exclude" = "grey40"), guide = "none") +
  theme_void() +
  labs(x = "Longitude", y = "Latitude", fill = "Enclosure") +
  theme(legend.position = c(0.15, 0.2))
p.loc

ggsave(plot = p.loc, "builds/plots/plot_locs.png", dpi = 600)
library(rnaturalearth)

europe <- ne_countries(scale = 50, continent = "Europe") %>% st_transform(crs = 4326)
mapview::mapview(europe)

p.europe <- ggplot() +
  geom_sf(data = europe, fill = "grey95", color = "grey50") + 
  geom_sf(data = bfnp, fill = "black" ) +
  xlim(c(-10, 25)) +
  ylim(c(37, 65)) +
  theme_void()
p.europe
ggsave(plot = p.europe, "builds/plots/europe.png", dpi = 600)


### Correlation figure 
names(dt.mod)
dt.mod <- fread("data/clean_data/bfnp_enclosure_model_data.csv") %>% 
  dplyr::filter(!flag == "exclude") %>% 
  mutate(enclosure_name = as.factor(enclosure_name)) %>%
  rename(Elevation = elevation, 
         HAND = hand,
         `Distance to Enclosure` = min_dist_enclosure, 
         `Relative Elevation` = elevation_rel_enc, 
         `Deadwood year` = deadWoodChangeYear) %>% 
  filter(mountain_or_enclosure == "Enclosure") %>%
  dplyr::select(Elevation, HAND, `Distance to Enclosure`, `Relative Elevation`,`Deadwood year`) 
library(GGally)
p.pair <- ggpairs(dt.mod) + theme(axis.text = element_text(angle = 45))
ggsave(plot = p.pair, "builds/plots/correlations.png", dpi = 600, height = 7, width = 7)
