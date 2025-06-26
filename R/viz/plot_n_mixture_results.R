#Vizualize N mixture results 
library(data.table)
library(tidyverse)
library(terra)
library(sf)
library(mapview)
library(sjPlot)
library(unmarked)
library(patchwork)
library(gridExtra)
library(MetBrewer)

r_deer <- rast("builds/model_outputs/red_deer_map_mean.tif")
mod <- readRDS("builds/model_outputs/p3_1_2_1_nopred.Rds")
sf_encl <- st_read("data/spatial_data/Winter_enclosures/winter_enclosures_NPBF.shp") %>% 
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
  ) %>% filter(Site_ID != "pre") %>% 
  st_transform(crs = "EPSG:25833")


sf_ct <- st_read("data/n_mixture_data/CT_locations/CT_locations_25833.shp")
mapview(sf_ct)
sf_grid <- st_read("data/n_mixture_data/grid_BFNP.shp")
mapview(sf_grid)
# Predict distance to winter enclosure 

get_mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

var_raw <- read.csv("data/n_mixture_data/vars_prediction.csv") 

var <- read.csv("data/n_mixture_data/vars_prediction.csv") %>% 
  mutate(veg_coverage_10m_2019_2020_25833 = median(veg_coverage_10m_2019_2020_25833), 
         DTM_final_25833 = median(DTM_final_25833), 
         buffer35_VG_mean = median(buffer35_VG_mean), 
         ht_cropped = get_mode(ht_cropped)
  )
dt_pred <- predict(mod, newdata = var, type = "state", appendData = TRUE)

p_pred <- ggplot(dt_pred, aes(x = dist_wint, y = Predicted)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(
    x = "Distance to Enclosure (m)",
    y = "Predicted Abundance",
    title = "b)"
  ) +
  theme_minimal() +
  theme(    panel.grid.minor = element_blank(),
            axis.text.y = element_text(size = 12),
            axis.text.x = element_text(size = 11),
            plot.title = element_text(face = "bold", hjust = 0))
  
p_pred

# estimates 

m_summ <- summary(mod)

dt_est <- m_summ$state %>%
  as.data.frame() %>% 
  rownames_to_column(var = "term") %>% 
  rename(estimate = Estimate) %>% 
  mutate(ci_lb = estimate - 1.96*SE, 
         ci_ub = estimate + 1.96*SE, 
         p = round(`P(>|z|)`, 3)) %>% 
  mutate( clean_term = case_when(
    term == "(Intercept)" ~ "Intercept",
    term == "ht_croppeddeadwood" ~ "Habitat: Deadwood",
    term == "ht_croppeddec_stand" ~ "Habitat: Deciduous Stand",
    term == "ht_croppedmix_stand" ~ "Habitat: Mixed Stand",
    term == "ht_croppedopen" ~ "Habitat: Open Habitat",
    term == "scale(veg_coverage_10m_2019_2020_25833)" ~ "Canopy Cover",
    term == "scale(DTM_final_25833)" ~ "Elevation",
    term == "scale(buffer35_VG_mean)" ~ "Vegetation Density",
    term == "scale(dist_wint)" ~ "Distance to Enclosure",
    TRUE ~ term
  ))


p_est <- dt_est %>%
  filter(clean_term != "Intercept") %>% 
  ggplot(aes(x = estimate, y = reorder(clean_term, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_pointrange(
    aes(xmin = ci_lb, xmax = ci_ub),
    size = 0.9,
    linewidth = 1.1,
    color = "grey50",
    alpha = 0.75
  ) +
  geom_pointrange(
    data = dt_est %>% filter(clean_term == "Distance to Enclosure"),
    aes(xmin = ci_lb, xmax = ci_ub),
    size = 0.9,
    linewidth = 1.1,
    color = "darkred",
    alpha = 0.9
  ) +
  labs(
    x = "Estimate",
    y = NULL,
    title = "a)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 11),
    plot.title = element_text(face = "bold", hjust = 0)
  )

p_est        

# dt_r_deer <- as.data.frame(r_deer, xy = T)
# p_map <- ggplot() +
#   geom_sf(data = sf_encl, color = "red") +
#   geom_tile(data = dt_r_deer, aes(x = x, y = y, fill = Predicted, color = Predicted)) +
#   geom_sf(data = sf_encl, color = "red") +
#   scale_color_viridis_c() +
#   scale_fill_viridis_c() +
#   theme_void() +
#   theme(legend.position = c(0.7, 0.7))
#   
# p_map
# 
# p_est_pred_stack <- p_est / p_pred
# p_all <- p_map | p_est_pred_stack
# p_all


p_est_pred <- p_est | p_pred
ggsave(plot = p_est_pred, "builds/plots/n_mixture_estimates_and_pred.png", dpi = 600, height = 4, width = 9)


# Plot camera trap distribution 
p_ct <- ggplot() +
  geom_sf(data = sf_grid %>% mutate(group = "all") %>% group_by(group) %>% summarize) +
  geom_sf(data = sf_ct, aes(color = "Camera Trap")) +
  scale_color_manual(values = c("Camera Trap" = "black")) +
  geom_sf(data = sf_encl, aes(fill = enclosure_name)) +
  scale_fill_met_d(name = "Egypt") +
  labs(fill = "Enclosure Name", color = "") +
  theme_void() +
  theme(legend.position = c(0.7, 0.7))

p_ct
ggsave(plot = p_ct, "builds/plots/camera_trap_distribution.png", dpi = 600)
