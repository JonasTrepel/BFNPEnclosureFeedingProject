
rm(list = ls())

library(data.table)
library(ggplot2)
library(tidyverse)
library(sf)
library(gridExtra)
library(glmmTMB)
library(terra)
library(segmented)
library(MetBrewer)

dt.sf.raw <- read_sf("data/clean_data/enclosure_nutrients/Plots_and_lab_results_enclosures_and_mountains_2023.shp") 

XY <- as.data.frame(st_coordinates(dt.sf.raw))

coords <- dt.sf.raw %>% 
  mutate(X = XY$X, 
         Y = XY$Y) %>% as.data.table() %>% mutate(geometry = NULL) %>% dplyr::select(c(Plot_ID, X, Y))

dt <- fread("data/clean_data/enclosure_nutrients/Plots_and_lab_results_enclosures_and_mountains_2023.csv") %>% 
  left_join(coords)


table(dt$Distance_Level)

#excluide potential spurious samples
dt.res <- dt[!Notes %in% c(
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
)] %>% 
  rename(distance_level = Distance_Level, 
         type = Type) %>% 
  mutate(
    distance_enclosure = case_when(
      distance_level == 0 ~ -100, 
      distance_level == 1 ~ 100, 
      distance_level == 2 ~ 900, 
      distance_level == 3 ~ 2100), 
    distance_level = case_when(
      .default = distance_level, 
      distance_level == "Control"~ "Low density", 
      distance_level == "Treatment"~ "High density"
    ))



dt.soil.en.melt <- melt(dt.res[Mountain_Or_Enclosure == "Enclosure" & type == "Soil", ], id.vars = c("fid", "Plot_ID", "Habitat_Ty","Notes",  "Other_Note",              
                                    "Timestamp","Site_ID", "distance_level", "distance_enclosure", "Transect_ID",
                                     "type", "Landcover", "Elevation", "HAND", "Lab_ID", "X", "Y", "Units", "Mountain_Or_Enclosure"))
dt.soil.en.melt <- dt.soil.en.melt[variable %in% c("N", "C", "P", "K", "Ca", "Mg", "Na")]
## Soil points 

en.soil <- ggplot() +
  geom_boxplot(data = dt.soil.en.melt, aes(x = as.factor(distance_level), y = value),
            alpha = .5, fill = "grey90", outlier.shape = NULL)  +
  geom_jitter(data = dt.soil.en.melt[!is.na(value), ], aes(x = as.factor(distance_level), y = value, color = Site_ID),
              alpha = .5)  +
  scale_color_viridis_d() +
  facet_wrap(~ variable, scales = "free", ncol = 4) +
  scale_x_discrete(breaks = (as.factor(dt.res$distance_level)),
                     labels = (dt.res$distance_enclosure))+
  labs(y = "Soil nutrient concentration", x = "Distance to enclosure (m)", title = "Soil", subtitle = "Enclosures", 
       color = "Enclosure ID") +
  theme_bw() +
  theme(plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5), 
        legend.position = "none")
en.soil


dt.veg.en.melt <- melt(dt.res[Mountain_Or_Enclosure == "Enclosure" & type == "Vegetation", ], id.vars = c("fid", "Plot_ID", "Habitat_Ty","Notes",  "Other_Note",              
                                                                                                     "Timestamp","Site_ID", "distance_level", "distance_enclosure", "Transect_ID",
                                                                                                     "type", "Landcover", "Elevation", "Lab_ID", "X", "Y", "Units", "Mountain_Or_Enclosure"))
dt.veg.en.melt <- dt.veg.en.melt[variable %in% c("N", "C", "P", "K", "Ca", "Mg", "Na")]

en.veg <- ggplot() +
  geom_boxplot(data = dt.veg.en.melt, aes(x = as.factor(distance_level), y = value),
               alpha = .5, fill = "grey90", outlier.shape = NULL)  +
  geom_jitter(data = dt.veg.en.melt[!is.na(value), ], aes(x = as.factor(distance_level), y = value, color = Site_ID),
              alpha = .5)  +
  scale_color_viridis_d() +
  facet_wrap(~ variable, scales = "free", ncol = 4) +
  scale_x_discrete(breaks = (as.factor(dt.res$distance_level)),
                   labels = (dt.res$distance_enclosure))+
  labs(y = "Grass nutrient concentration", x = "Distance to enclosure (m)", title = "Vegetation", subtitle = "Enclosures", 
       color = "Enclosure ID") +
  theme_bw() +
  theme(plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5), 
        legend.position = "bottom")
en.veg


enclosures <- grid.arrange(en.soil, en.veg, heights = c(1, 1.1))
ggsave(plot = enclosures, "builds/exploratory_plots/enclosure_nutrients.png", dpi = 600, height = 12, width = 9)

## Summer sites  

dt.soil.mount.melt <- melt(dt.res[Mountain_Or_Enclosure == "Mountain" & type == "Soil", ], id.vars = c("fid", "Plot_ID", "Habitat_Ty","Notes",  "Other_Note",              
                                                                                                     "Timestamp","Site_ID", "distance_level", "distance_enclosure", "Transect_ID",
                                                                                                     "type", "Landcover", "Elevation", "Lab_ID", "X", "Y", "Units", "Mountain_Or_Enclosure"))
dt.soil.mount.melt <- dt.soil.mount.melt[variable %in% c("N", "C", "P", "K", "Ca", "Mg", "Na")]

mount.soil <- ggplot() +
  geom_boxplot(data = dt.soil.mount.melt, aes(x = as.factor(distance_level), y = value),
               alpha = .5, fill = "grey90", outlier.shape = NULL)  +
  geom_jitter(data = dt.soil.mount.melt[!is.na(value), ], aes(x = as.factor(distance_level), y = value, color = Site_ID),
              alpha = .5)  +
  scale_color_viridis_d() +
  facet_wrap(~ variable, scales = "free", ncol = 4) +
  labs(y = "Soil nutrient concentration", x =  "",
       title = "Soil",
       color = "Site",
       subtitle = "Summer sites (mountains)") +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5))

mount.soil


dt.veg.mount.melt <- melt(dt.res[Mountain_Or_Enclosure == "Mountain" & type == "Vegetation", ], id.vars = c("fid", "Plot_ID", "Habitat_Ty","Notes",  "Other_Note",              
                                                                                                       "Timestamp","Site_ID", "distance_level", "distance_enclosure", "Transect_ID",
                                                                                                       "type", "Landcover", "Elevation", "Lab_ID", "X", "Y", "Units", "Mountain_Or_Enclosure"))
dt.veg.mount.melt <- dt.soil.mount.melt[variable %in% c("N", "C", "P", "K", "Ca", "Mg", "Na")]


mount.veg <- ggplot() +
  geom_boxplot(data = dt.soil.mount.melt, aes(x = as.factor(distance_level), y = value),
               alpha = .5, fill = "grey90", outlier.shape = NULL)  +
  geom_jitter(data = dt.soil.mount.melt[!is.na(value), ], aes(x = as.factor(distance_level), y = value, color = Site_ID),
              alpha = .5)  +
  scale_color_viridis_d() +
  facet_wrap(~ variable, scales = "free", ncol = 4) +
  labs(y = "Grass nutrient concentration", x = "",
       title = "Vegetation",
       color = "Site",
       subtitle = "Summer sites (mountains)") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5))

mount.veg



mountains <- grid.arrange(mount.soil, mount.veg, heights = c(1, 1.1))

ggsave(plot = mountains, "builds/exploratory_plots/mountain_nutrients.png", dpi = 600, height = 12, width = 9)



## spatial plots 


library(sf)
dt.sf <- read_sf("data/clean_data/enclosure_nutrients/Plots_and_lab_results_enclosures_and_mountains_2023.shp")

dt.sf <- dt.sf[dt.sf$Plot_ID %in% c(unique(dt.res$Plot_ID)), ]

enclosures <- st_read("data/Winter_enclosures/winter_enclosures_NPBF.shp") %>% 
  filter(id == 1) %>% 
  mutate(id2 = 1:nrow(.), 
         Site_ID = case_when(
           id2 %in% c(5) ~ "A",
           id2 %in% c(7) ~ "B",
           id2 %in% c(1) ~ "C",
           id2 %in% c(3) ~ "D",
           id2 %in% c(6, 2, 4) ~ "pre"),
         area_ha = as.numeric(st_area(.)/10000)
  )
mapview::mapview(enclosures)

sites <- ggplot() +
  geom_sf(data = enclosures[!enclosures$Site_ID == "pre", ], aes(), fill = "black") +
  geom_sf(data = dt.sf[dt.sf$Mnt_O_E == "Enclosure", ], aes(color = Site_ID), size = 1.5, alpha = 0.5) +
  geom_sf(data = dt.sf[dt.sf$Mnt_O_E == "Mountain", ], aes(color = Site_ID, shape = Dstnc_L), size = 1.5, alpha = 0.5) +
  scale_color_viridis_d()+
  scale_fill_viridis_d()+
  labs(color = "Site ID", 
       shape = "Control/Treatment ") + 
  theme_classic() + 
  theme(panel.grid = element_line(color = "grey95"))
sites

ggsave(plot = sites, "builds/exploratory_plots/site_locs.png", dpi = 600, height = 6, width = 8)



## extract tree cover 

tc <- rast("data/clean_data/spatial_data/tree_cover_BFNP_2018.tif")
plot(tc)
range(values(tc), na.rm = TRUE)


dt.sf.buff <- st_buffer(dt.sf, 10)
dt.sf.buff$ID <- NULL
names(dt.sf.buff)
mapview::mapview(dt.sf.buff)

dt.sf.buff <- st_transform(dt.sf.buff, crs = crs(tc))

tc.ext <- terra::extract(tc, 
                         vect(dt.sf.buff),
                         mean, na.rm = T)

dt.sf.buff2 <- cbind(tc.ext, dt.sf.buff) %>% mutate(geometry = NULL) %>% as.data.table()

dt.sf.buff3 <- unique(dt.sf.buff2[, .(Plot_ID, tree_cover_BFNP_2018)])
dt.res2 <- dt.res %>% 
  left_join(dt.sf.buff3)


p.tc <- ggplot() +
  geom_boxplot(data = dt.res2[distance_level %in% c("High density", "Low density"), ], aes(x = as.factor(distance_level), y = tree_cover_BFNP_2018),
               alpha = .5, fill = "grey90", outlier.shape = NULL)  +
  geom_jitter(data = dt.res2[distance_level %in% c("High density", "Low density"), ], aes(x = as.factor(distance_level), y = tree_cover_BFNP_2018),
              alpha = .5, size = 3, width = 0.2)  +
  scale_color_viridis_d(option = "D") +
  labs(y = "Tree cover (%)", x = "High or low deer density",
       color = "Site") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5))

ggsave(plot = p.tc, "builds/exploratory_plots/tree_cover_summer_sites.png", dpi = 600, height = 6, width = 8)

