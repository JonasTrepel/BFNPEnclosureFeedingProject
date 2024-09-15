### visualize the distance to exclosure relationships wherever appropriate 
library(data.table)
library(tidyverse)
library(MuMIn)
library(tidyr)
library(broom)
library(brms)
library(tidybayes)
library(gridExtra)


dt.mod <- fread("data/clean_data/bfnp_enclosure_model_data.csv") %>% 
  dplyr::filter(!flag == "exclude") %>% 
  mutate(enclosure_name = as.factor(enclosure_name))
names(dt.mod)

foreach.results <- readRDS("builds/model_outputs/gam_all_res.Rds")

names(foreach.results)
pred <- foreach.results$pred %>% unique()
predInt <- foreach.results$pred.int %>% unique()
bm.spec <- foreach.results$bm.spec %>% unique()


#Vegetation 
veg.ids <- unique(bm.spec[sphere == "Vegetation", ]$formula_id)
dt.pred.veg <- pred[formula_id %in% c(veg.ids) & clean_var == "min_dist_enclosure", ]  %>% 
  mutate(nutrient = str_to_sentence(response), 
  significance = case_when(
    response == "n" ~ "significant", 
    response == "c" ~ "non significant", 
    response == "p" ~ "significant", 
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
           response == "cn" ~ "significant",
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
         var_value = as.numeric(var_value), 
         var_value = 2023-var_value) %>%
  left_join(bm.spec) %>% 
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
  labs(title= "a)", x = paste0("Year Since Tree Dieback"), y = paste0("Nutrient content (mg/g)"), color = "Enclosure", linetype = "Significance", fill = "Enclosure", 
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
         significance = ifelse(response %in% c("p", "k", "ca"), "significant", "non significant"), 
         var_value = as.numeric(var_value),
         var_value = 2023-var_value) %>%
  left_join(bm.spec) %>% 
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
  labs(title= "b)", x = paste0("Year Since Tree Dieback"), y = paste0("Nutrient content (mg/g)"), color = "Enclosure", linetype = "Significance", fill = "Enclosure", 
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

### Interaction with Deer Density and Numbers 

#Vegetation # deer density 
veg.ids <- unique(bm.spec[sphere == "Vegetation", ]$formula_id)
dt.pred.veg.deerD <- predInt[formula_id %in% c(veg.ids) & clean_var == "min_dist_enclosure" & moderator == "deer_density", ]  %>% 
  mutate(nutrient = str_to_sentence(response), 
         significance = ifelse(response %in% c("n", "cn"), "significant", "non significant"), 
         var_value = as.numeric(var_value)) %>%
  left_join(bm.spec) %>% 
  mutate(nutrient = case_when(
    .default = nutrient, 
    response == "cn" ~ "C:N", 
    response == "np" ~ "N:P"
  ))



pd.veg.deerD <- ggplot() +
  geom_point(data = dt.pred.veg.deerD, aes(x = var_value, y = response_value, color = enclosure_name), alpha = 0.4, size = 2) +
  
  geom_ribbon(data = dt.pred.veg.deerD[random_effect == TRUE, ], aes(x = var_value, ymin =  ci.lb.low, ymax = ci.ub.low, fill = enclosure_name), alpha = 0.5) +
  geom_ribbon(data = dt.pred.veg.deerD[random_effect == FALSE, ], aes(x = var_value, ymin =  ci.lb.low, ymax = ci.ub.low), alpha = 0.5, fill = "grey50") +
  geom_line(data = dt.pred.veg.deerD[random_effect == TRUE, ], aes(x = var_value, y =  fit.low, color = enclosure_name, linewidth = significance, linetype = "low"), linewidth = 1.1) +
  geom_line(data = dt.pred.veg.deerD[random_effect == FALSE, ], aes(x = var_value, y =  fit.low, linewidth = significance, linetype = "low"), color = "grey5", linewidth = 1.1) +
  
  geom_ribbon(data = dt.pred.veg.deerD[random_effect == TRUE, ], aes(x = var_value, ymin =  ci.lb.high, ymax = ci.ub.high, fill = enclosure_name), alpha = 0.5) +
  geom_ribbon(data = dt.pred.veg.deerD[random_effect == FALSE, ], aes(x = var_value, ymin =  ci.lb.high, ymax = ci.ub.high), alpha = 0.5, fill = "grey50") +
  geom_line(data = dt.pred.veg.deerD[random_effect == TRUE, ], aes(x = var_value, y =  fit.high, color = enclosure_name, linewidth = significance, linetype = "high"), linewidth = 1.1) +
  geom_line(data = dt.pred.veg.deerD[random_effect == FALSE, ], aes(x = var_value, y =  fit.high, linewidth = significance, linetype = "high"), color = "grey5", linewidth = 1.1) +
  scale_fill_met_d(name = "Egypt") +
  scale_color_met_d(name = "Egypt") +
  facet_wrap(~nutrient, scales = "free", ncol = 4) +
  scale_linetype_manual(values = c("low" = "dashed", "high" = "dotted")) + 
  scale_linewidth_manual(values = c("significant" = 1.5, "non significant" = 0.1)) + 
  labs(title= "a)", x = paste0("Distance to enclosure"), y = paste0("Nutrient content (mg/g)"), color = "Enclosure", linetype = "Deer Density", fill = "Enclosure", 
       subtitle = "Deer Density Interaction (Veg.)") +
  theme_bw() +
  theme(legend.position = "bottom", 
        plot.title = element_text(face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 14), 
        panel.grid = element_blank(),
        legend.box="vertical",
        legend.margin=margin()
  )+ guides(color=guide_legend(nrow=2,byrow=TRUE))
pd.veg.deerD

# deer numbers
veg.ids <- unique(bm.spec[sphere == "Vegetation", ]$formula_id)
dt.pred.veg.deerN <- predInt[formula_id %in% c(veg.ids) & clean_var == "min_dist_enclosure" & moderator == "deer_number", ]  %>% 
  mutate(nutrient = str_to_sentence(response), 
         significance = ifelse(response %in% c("na"), "significant", "non significant"), 
         var_value = as.numeric(var_value)) %>%
  left_join(bm.spec) %>% 
  mutate(nutrient = case_when(
    .default = nutrient, 
    response == "cn" ~ "C:N", 
    response == "np" ~ "N:P"
  ))



pd.veg.deerN <- ggplot() +
  geom_point(data = dt.pred.veg.deerN, aes(x = var_value, y = response_value, color = enclosure_name), alpha = 0.4, size = 2) +
  
  geom_ribbon(data = dt.pred.veg.deerN[random_effect == TRUE, ], aes(x = var_value, ymin =  ci.lb.low, ymax = ci.ub.low, fill = enclosure_name), alpha = 0.5) +
  geom_ribbon(data = dt.pred.veg.deerN[random_effect == FALSE, ], aes(x = var_value, ymin =  ci.lb.low, ymax = ci.ub.low), alpha = 0.5, fill = "grey50") +
  geom_line(data = dt.pred.veg.deerN[random_effect == TRUE, ], aes(x = var_value, y =  fit.low, color = enclosure_name, linewidth = significance, linetype = "low"), linewidth = 1.1) +
  geom_line(data = dt.pred.veg.deerN[random_effect == FALSE, ], aes(x = var_value, y =  fit.low, linewidth = significance, linetype = "low"), color = "grey5", linewidth = 1.1) +
  
  geom_ribbon(data = dt.pred.veg.deerN[random_effect == TRUE, ], aes(x = var_value, ymin =  ci.lb.high, ymax = ci.ub.high, fill = enclosure_name), alpha = 0.5) +
  geom_ribbon(data = dt.pred.veg.deerN[random_effect == FALSE, ], aes(x = var_value, ymin =  ci.lb.high, ymax = ci.ub.high), alpha = 0.5, fill = "grey50") +
  geom_line(data = dt.pred.veg.deerN[random_effect == TRUE, ], aes(x = var_value, y =  fit.high, color = enclosure_name, linewidth = significance, linetype = "high"), linewidth = 1.1) +
  geom_line(data = dt.pred.veg.deerN[random_effect == FALSE, ], aes(x = var_value, y =  fit.high, linewidth = significance, linetype = "high"), color = "grey5", linewidth = 1.1) +
  scale_fill_met_d(name = "Egypt") +
  scale_color_met_d(name = "Egypt") +
  facet_wrap(~nutrient, scales = "free", ncol = 4) +
  scale_linetype_manual(values = c("low" = "dashed", "high" = "dotted")) + 
  scale_linewidth_manual(values = c("significant" = 1.5, "non significant" = 0.1)) + 
  labs(title= "b)", x = paste0("Distance to enclosure"), y = paste0("Nutrient content (mg/g)"), color = "Enclosure", linetype = "Deer Number", fill = "Enclosure", 
       subtitle = "Deer Number Interaction (Veg.)") +
  theme_bw() +
  theme(legend.position = "bottom", 
        plot.title = element_text(face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 14), 
        panel.grid = element_blank(),
        legend.box="vertical",
        legend.margin=margin()
  )+guides(color=guide_legend(nrow=2,byrow=TRUE))
pd.veg.deerN

### SOIL 

## deer density 
soil.ids <- unique(bm.spec[sphere == "Soil", ]$formula_id)
dt.pred.soil.deerD <- predInt[formula_id %in% c(soil.ids) & clean_var == "min_dist_enclosure" & moderator == "deer_density", ]  %>% 
  mutate(nutrient = str_to_sentence(response), 
         significance = ifelse(response %in% c("c"), "significant", "non significant"), 
         var_value = as.numeric(var_value)) %>%
  left_join(bm.spec) %>% 
  mutate(nutrient = case_when(
    .default = nutrient, 
    response == "cn" ~ "C:N", 
    response == "np" ~ "N:P"
  ))



pd.soil.deerD <- ggplot() +
  geom_point(data = dt.pred.soil.deerD, aes(x = var_value, y = response_value, color = enclosure_name), alpha = 0.4, size = 2) +
  
  geom_ribbon(data = dt.pred.soil.deerD[random_effect == TRUE, ], aes(x = var_value, ymin =  ci.lb.low, ymax = ci.ub.low, fill = enclosure_name), alpha = 0.5) +
  geom_ribbon(data = dt.pred.soil.deerD[random_effect == FALSE, ], aes(x = var_value, ymin =  ci.lb.low, ymax = ci.ub.low), alpha = 0.5, fill = "grey50") +
  geom_line(data = dt.pred.soil.deerD[random_effect == TRUE, ], aes(x = var_value, y =  fit.low, color = enclosure_name, linewidth = significance, linetype = "low"), linewidth = 1.1) +
  geom_line(data = dt.pred.soil.deerD[random_effect == FALSE, ], aes(x = var_value, y =  fit.low, linewidth = significance, linetype = "low"), color = "grey5", linewidth = 1.1) +
  
  geom_ribbon(data = dt.pred.soil.deerD[random_effect == TRUE, ], aes(x = var_value, ymin =  ci.lb.high, ymax = ci.ub.high, fill = enclosure_name), alpha = 0.5) +
  geom_ribbon(data = dt.pred.soil.deerD[random_effect == FALSE, ], aes(x = var_value, ymin =  ci.lb.high, ymax = ci.ub.high), alpha = 0.5, fill = "grey50") +
  geom_line(data = dt.pred.soil.deerD[random_effect == TRUE, ], aes(x = var_value, y =  fit.high, color = enclosure_name, linewidth = significance, linetype = "high"), linewidth = 1.1) +
  geom_line(data = dt.pred.soil.deerD[random_effect == FALSE, ], aes(x = var_value, y =  fit.high, linewidth = significance, linetype = "high"), color = "grey5", linewidth = 1.1) +
  scale_fill_met_d(name = "Egypt") +
  scale_color_met_d(name = "Egypt") +
  facet_wrap(~nutrient, scales = "free", ncol = 4) +
  scale_linetype_manual(values = c("low" = "dashed", "high" = "dotted")) + 
  scale_linewidth_manual(values = c("significant" = 1.5, "non significant" = 0.1)) + 
  labs(title= "c)", x = paste0("Distance to enclosure"), y = paste0("Nutrient content (mg/g)"), color = "Enclosure", linetype = "Deer Density", fill = "Enclosure", 
       subtitle = "Deer Density Interaction (Soil)") +
  theme_bw() +
  theme(legend.position = "bottom", 
        plot.title = element_text(face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 14), 
        panel.grid = element_blank(),
        legend.box="vertical",
        legend.margin=margin()
  )+guides(fill=guide_legend(nrow=2,byrow=TRUE))
pd.soil.deerD

#deer numbers
soil.ids <- unique(bm.spec[sphere == "Soil", ]$formula_id)
dt.pred.soil.deerN <- predInt[formula_id %in% c(soil.ids) & clean_var == "min_dist_enclosure" & moderator == "deer_number", ]  %>% 
  mutate(nutrient = str_to_sentence(response), 
         significance = ifelse(response %in% c("p", "mg"), "significant", "non significant"), 
         var_value = as.numeric(var_value)) %>%
  left_join(bm.spec) %>% 
  mutate(nutrient = case_when(
    .default = nutrient, 
    response == "cn" ~ "C:N", 
    response == "np" ~ "N:P"
  ))



pd.soil.deerN <- ggplot() +
  geom_point(data = dt.pred.soil.deerN, aes(x = var_value, y = response_value, color = enclosure_name), alpha = 0.4, size = 2) +
  
  geom_ribbon(data = dt.pred.soil.deerN[random_effect == TRUE, ], aes(x = var_value, ymin =  ci.lb.low, ymax = ci.ub.low, fill = enclosure_name), alpha = 0.5) +
  geom_ribbon(data = dt.pred.soil.deerN[random_effect == FALSE, ], aes(x = var_value, ymin =  ci.lb.low, ymax = ci.ub.low), alpha = 0.5, fill = "grey50") +
  geom_line(data = dt.pred.soil.deerN[random_effect == TRUE, ], aes(x = var_value, y =  fit.low, color = enclosure_name, linewidth = significance, linetype = "low"), linewidth = 1.1) +
  geom_line(data = dt.pred.soil.deerN[random_effect == FALSE, ], aes(x = var_value, y =  fit.low, linewidth = significance, linetype = "low"), color = "grey5", linewidth = 1.1) +
  
  geom_ribbon(data = dt.pred.soil.deerN[random_effect == TRUE, ], aes(x = var_value, ymin =  ci.lb.high, ymax = ci.ub.high, fill = enclosure_name), alpha = 0.5) +
  geom_ribbon(data = dt.pred.soil.deerN[random_effect == FALSE, ], aes(x = var_value, ymin =  ci.lb.high, ymax = ci.ub.high), alpha = 0.5, fill = "grey50") +
  geom_line(data = dt.pred.soil.deerN[random_effect == TRUE, ], aes(x = var_value, y =  fit.high, color = enclosure_name, linewidth = significance, linetype = "high"), linewidth = 1.1) +
  geom_line(data = dt.pred.soil.deerN[random_effect == FALSE, ], aes(x = var_value, y =  fit.high, linewidth = significance, linetype = "high"), color = "grey5", linewidth = 1.1) +
  scale_fill_met_d(name = "Egypt") +
  scale_color_met_d(name = "Egypt") +
  facet_wrap(~nutrient, scales = "free", ncol = 4) +
  scale_linetype_manual(values = c("low" = "dashed", "high" = "dotted")) + 
  scale_linewidth_manual(values = c("significant" = 1.5, "non significant" = 0.1)) + 
  labs(title= "d)", x = paste0("Distance to enclosure"), y = paste0("Nutrient content (mg/g)"), color = "Enclosure", linetype = "Deer Number", fill = "Enclosure", 
       subtitle = "Deer Number Interaction (Soil)") +
  theme_bw() +
  theme(legend.position = "bottom", 
        plot.title = element_text(face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 14), 
        panel.grid = element_blank(),
        legend.box="vertical",
        legend.margin=margin() 
  ) +guides(fill=guide_legend(nrow=2,byrow=TRUE))
pd.soil.deerN

p.comb.DeerVeg <- grid.arrange(pd.veg.deerD, pd.veg.deerN, widths = c(2, 1))
p.comb.DeerSoil <- grid.arrange(pd.soil.deerD, pd.soil.deerN, widths = c(2, 2))


pcomb.Deer <- grid.arrange(p.comb.DeerVeg, p.comb.DeerSoil, ncol = 1, heights = c(1, 1))

ggsave(plot = pcomb.Deer, "builds/plots/gam_deer_interaction_preds.png", dpi = 600, height = 9, width = 11)

