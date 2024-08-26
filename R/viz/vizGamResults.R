### visualize the distance to exclosure relationships wherever appropriate 
library(data.table)
library(tidyverse)
library(MuMIn)
library(tidyr)
library(broom)
library(brms)
library(tidybayes)


dt.mod <- fread("data/clean_data/bfnp_enclosure_model_data.csv") %>% 
  dplyr::filter(!flag == "exclude") %>% 
  mutate(enclosure_name = as.factor(enclosure_name))
names(dt.mod)

foreach.results <- readRDS("builds/model_outputs/gam_all_res.Rds")

names(foreach.results)
pred <- foreach.results$pred %>% unique()
bm.spec <- foreach.results$bm.spec %>% unique()


#Vegetation 
veg.ids <- unique(bm.spec[sphere == "Vegetation", ]$formula_id)
dt.pred.veg <- pred[formula_id %in% c(veg.ids) & clean_var == "min_dist_enclosure", ]  %>% 
  mutate(nutrient = str_to_sentence(response), 
  significance = case_when(
    response == "n" ~ "significant", 
    response == "c" ~ "non significant", 
    response == "p" ~ "non significant", 
    response == "k" ~ "significant", 
    response == "mg" ~ "significant", 
    response == "ca" ~ "significant", 
    response == "na" ~ "significant",
    response == "cn" ~ "significant",
    response == "np" ~ "non significant"
  ),
  var_value = as.numeric(var_value)) %>% left_join(bm.spec) %>% 
  mutate(nutrient = case_when(
    .default = nutrient, 
    response == "cn" ~ "C:N", 
    response == "np" ~ "N:P"
  ))



pp <- ggplot() +
  geom_point(data = dt.pred.veg, aes(x = var_value, y = response_value, color = enclosure_name), alpha = 0.4, size = 2) +
  geom_ribbon(data = dt.pred.veg[random_effect == TRUE, ], aes(x = var_value, ymin =  ci.lb, ymax = ci.ub, fill = enclosure_name), alpha = 0.5) +
  geom_ribbon(data = dt.pred.veg[random_effect == FALSE, ], aes(x = var_value, ymin =  ci.lb, ymax = ci.ub), alpha = 0.5, fill = "grey50") +
  geom_line(data = dt.pred.veg[random_effect == TRUE, ], aes(x = var_value, y =  fit, color = enclosure_name, linetype = significance), linewidth = 1.1) +
  geom_line(data = dt.pred.veg[random_effect == FALSE, ], aes(x = var_value, y =  fit, linetype = significance), color = "grey5", linewidth = 1.1) +
  # scale_color_manual(values=c("non significant" = "#88A0DC", "significantly negative" = "#63396C","significantly positive" = "#ED9D34")) +
  scale_fill_met_d(name = "Egypt") +
  scale_color_met_d(name = "Egypt") +
  facet_wrap(~nutrient, scales = "free", ncol = 4) +
  scale_linetype_manual(values = c("significant" = "solid", "non significant" = "dotted")) + 
  labs(x = paste0("Distance to Enclosure (m)"), y = paste0("Nutrient Concentration"), color = "Enclosure", linetype = "Significance", fill = "Enclosure", 
       subtitle = "Vegetation Nutrients", title = "a)") +
  theme_bw() +
  theme(legend.position = "bottom", 
        plot.title = element_text(face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 14), 
        panel.grid = element_blank()
        )
pp

#Soil 
soil.ids <- unique(bm.spec[sphere == "Soil", ]$formula_id)
dt.pred.soil <- pred[formula_id %in% c(soil.ids) & clean_var == "min_dist_enclosure", ]  %>% 
  mutate(nutrient = str_to_sentence(response), 
         significance = case_when(
           response == "n" ~ "significant", 
           response == "c" ~ "non significant", 
           response == "p" ~ "non significant", 
           response == "k" ~ "non significant", 
           response == "mg" ~ "non significant", 
           response == "ca" ~ "non significant", 
           response == "na" ~ "significant",
           response == "cn" ~ "non significant",
           response == "np" ~ "non significant",
           response == "al" ~ "significant"),
         var_value = as.numeric(var_value),
         ) %>% left_join(bm.spec) %>% 
  mutate(nutrient = case_when(
    .default = nutrient, 
    response == "cn" ~ "C:N", 
    response == "np" ~ "N:P"
  )) %>% filter(response != "al")

dt.pred.soil$var_value = round(dt.pred.soil$var_value, 0)

ps <- ggplot() +
  geom_point(data = dt.pred.soil, aes(x = var_value, y = response_value, color = enclosure_name), alpha = 0.4, size = 2) +
  geom_ribbon(data = dt.pred.soil[random_effect == TRUE, ], aes(x = var_value, ymin =  ci.lb, ymax = ci.ub, fill = enclosure_name), alpha = 0.5) +
  geom_ribbon(data = dt.pred.soil[random_effect == FALSE, ], aes(x = var_value, ymin =  ci.lb, ymax = ci.ub), alpha = 0.5, fill = "grey50") +
  geom_line(data = dt.pred.soil[random_effect == TRUE, ], aes(x = var_value, y =  fit, color = enclosure_name, linetype = significance), linewidth = 1.1) +
  geom_line(data = dt.pred.soil[random_effect == FALSE, ], aes(x = var_value, y =  fit, linetype = significance), color = "grey5", linewidth = 1.1) +
  # scale_color_manual(values=c("non significant" = "#88A0DC", "significantly negative" = "#63396C","significantly positive" = "#ED9D34")) +
  scale_fill_met_d(name = "Egypt") +
  scale_color_met_d(name = "Egypt") +
  facet_wrap(~nutrient, scales = "free", ncol = 4) +
  scale_linetype_manual(values = c("significant" = "solid", "non significant" = "dotted")) + 
  labs(x = paste0("Distance to Enclosure (m)"), y = paste0("Nutrient Concentration"), color = "Enclosure", linetype = "Significance", fill = "Enclosure", 
       subtitle = "Soil Nutrients", title = "b)") +
  theme_bw() +
  theme(legend.position = "bottom", 
        plot.title = element_text(face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 14), 
        panel.grid = element_blank()
  )
ps

##combine

pcomb <- grid.arrange(pp, ps, ncol = 1, heights = c(1, 1))
ggsave(plot = pcomb, "builds/plots/gam_dist_preds.png", dpi = 600, height = 12, width = 10)

### check the effect of deadwood: 

#Vegetation 
veg.ids <- unique(bm.spec[sphere == "Vegetation", ]$formula_id)
dt.pred.veg.dw <- pred[formula_id %in% c(veg.ids) & clean_var == "deadWoodChangeYear", ]  %>% 
  mutate(nutrient = str_to_sentence(response), 
         significance = ifelse(response %in% c("n", "c", "p", "k", "mg", "na", "cn"), "significant", "non significant"), 
         var_value = as.numeric(var_value)) %>% left_join(bm.spec) %>% 
  mutate(nutrient = case_when(
    .default = nutrient, 
    response == "cn" ~ "C:N", 
    response == "np" ~ "N:P"
  ))



pd.veg <- ggplot() +
  geom_point(data = dt.pred.veg.dw, aes(x = var_value, y = response_value, color = enclosure_name), alpha = 0.4, size = 2) +
  geom_ribbon(data = dt.pred.veg.dw[random_effect == TRUE, ], aes(x = var_value, ymin =  ci.lb, ymax = ci.ub, fill = enclosure_name), alpha = 0.5) +
  geom_ribbon(data = dt.pred.veg.dw[random_effect == FALSE, ], aes(x = var_value, ymin =  ci.lb, ymax = ci.ub), alpha = 0.5, fill = "grey50") +
   geom_line(data = dt.pred.veg.dw[random_effect == TRUE, ], aes(x = var_value, y =  fit, color = enclosure_name, linetype = significance), linewidth = 1.1) +
   geom_line(data = dt.pred.veg.dw[random_effect == FALSE, ], aes(x = var_value, y =  fit, linetype = significance), color = "grey5", linewidth = 1.1) +
  scale_fill_met_d(name = "Egypt") +
  scale_color_met_d(name = "Egypt") +
  facet_wrap(~nutrient, scales = "free", ncol = 4) +
  scale_linetype_manual(values = c("significant" = "solid", "non significant" = "dotted")) + 
  labs(title= "a)", x = paste0("Deadwood year"), y = paste0("Nutrient content (mg/g)"), color = "Enclosure", linetype = "Significance", fill = "Enclosure", 
       subtitle = "Vegetation Nutrients") +
  theme_bw() +
  theme(legend.position = "bottom", 
        plot.title = element_text(face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 14), 
        panel.grid = element_blank()
  )
pd.veg

#Soil 
soil.ids <- unique(bm.spec[sphere == "Soil", ]$formula_id)
dt.pred.soil.dw <- pred[formula_id %in% c(soil.ids) & clean_var == "deadWoodChangeYear", ]  %>% 
  mutate(nutrient = str_to_sentence(response), 
         significance = ifelse(response %in% c(), "significant", "non significant"), 
         var_value = as.numeric(var_value)) %>% left_join(bm.spec) %>% 
  mutate(nutrient = case_when(
    .default = nutrient, 
    response == "cn" ~ "C:N", 
    response == "np" ~ "N:P"
  ))



pd.soil <- ggplot() +
  geom_point(data = dt.pred.soil.dw, aes(x = var_value, y = response_value, color = enclosure_name), alpha = 0.4, size = 2) +
  geom_ribbon(data = dt.pred.soil.dw[random_effect == TRUE, ], aes(x = var_value, ymin =  ci.lb, ymax = ci.ub, fill = enclosure_name), alpha = 0.5) +
  geom_ribbon(data = dt.pred.soil.dw[random_effect == FALSE, ], aes(x = var_value, ymin =  ci.lb, ymax = ci.ub), alpha = 0.5, fill = "grey50") +
  geom_line(data = dt.pred.soil.dw[random_effect == TRUE, ], aes(x = var_value, y =  fit, color = enclosure_name, linetype = significance), linewidth = 1.1) +
  geom_line(data = dt.pred.soil.dw[random_effect == FALSE, ], aes(x = var_value, y =  fit, linetype = significance), color = "grey5", linewidth = 1.1) +
  scale_fill_met_d(name = "Egypt") +
  scale_color_met_d(name = "Egypt") +
  facet_wrap(~nutrient, scales = "free", ncol = 4) +
  scale_linetype_manual(values = c("significant" = "solid", "non significant" = "dotted")) + 
  labs(title= "b)", x = paste0("Deadwood year"), y = paste0("Nutrient content (mg/g)"), color = "Enclosure", linetype = "Significance", fill = "Enclosure", 
       subtitle = "Soil Nutrients") +
  theme_bw() +
  theme(legend.position = "bottom", 
        plot.title = element_text(face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 14), 
        panel.grid = element_blank()
  )
pd.soil


pcomb.dw <- grid.arrange(pd.veg, pd.soil, ncol = 1, heights = c(1, 0.65))

ggsave(plot = pcomb.dw, "builds/plots/gam_deadwood_cont_preds.png", dpi = 600, height = 10, width = 10)


