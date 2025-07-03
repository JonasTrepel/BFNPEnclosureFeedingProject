
library(data.table)
library(tidyverse)
library(MuMIn)
library(tidyr)
library(broom)
library(brms)
library(tidybayes)
library(mgcv)

dt_mod <- fread("data/clean_data/bfnp_enclosure_model_data.csv") %>% 
  dplyr::filter(!flag == "exclude") %>% 
  mutate(enclosure_name = as.factor(enclosure_name)) %>% 
  unique()
names(dt_mod)

foreach_results <- readRDS("builds/model_outputs/gam_all_res.Rds")

names(foreach_results)
pred <- foreach_results$pred %>% unique()
bm_spec <- foreach_results$bm.spec %>% unique()


res <- foreach_results$res


### run all best models again to get significace 

#### Vegetation #######
dt_veg <- dt_mod[type == 'Vegetation' & tier == 'Enclosures', ]

###### N

vn0 <- gam(n ~ 1, data = dt_veg, select = TRUE, method = "REML")
vn0
AICc.vn0 <- unname(as.numeric(AICc(vn0)))
bm_spec[, .(sphere, response, vars)]
vn <- gam(n ~ s(min_dist_enclosure_scaled, k = 3) + deadWoodChangeYear_scaled,
          data = dt_veg, select = TRUE, method = "REML")
r.squaredGLMM(vn)
summary(vn)
par(mfrow = c(2, 2))
gam.check(vn)

AICc.vn <- unname(as.numeric(AICc(vn)))
AICc.vn - AICc.vn0

###### C
vc0 <- gam(c ~ 1, data = dt_veg, select = TRUE, method = "REML")
vc0
AICc.vc0 <- unname(as.numeric(AICc(vc0)))

vc <- gam(c ~ deadWoodChangeYear_scaled,
          data = dt_veg, select = TRUE, method = "REML")
summary(vc)
par(mfrow = c(2, 2))
gam.check(vc)

AICc.vc<- unname(as.numeric(AICc(vc)))
AICc.vc - AICc.vc0


###### P
vp0 <- gam(p ~ 1, data = dt_veg, select = TRUE, method = "REML")
vp0
AICc.vp0 <- unname(as.numeric(AICc(vp0)))

vp <- gam(p ~ s(min_dist_enclosure_scaled, k = 3) + deadWoodChangeYear_scaled,
          data = dt_veg, select = TRUE, method = "REML")
summary(vp)
par(mfrow = c(2, 2))
gam.check(vp)

AICc.vp<- unname(as.numeric(AICc(vp)))
AICc.vp - AICc.vp0

###### K
vk0 <- gam(k ~ 1, data = dt_veg, select = TRUE, method = "REML")
vk0
AICc.vk0 <- unname(as.numeric(AICc(vk0)))

vk <- gam(k ~ min_dist_enclosure_scaled + s(enclosure_name, bs = 're') + deadWoodChangeYear_scaled,
          data = dt_veg, select = TRUE, method = "REML")
summary(vk)
par(mfrow = c(2, 2))
gam.check(vk)

AICc.vk<- unname(as.numeric(AICc(vk)))
AICc.vk - AICc.vk0

 
###### Mg
vmg0 <- gam(mg ~ 1, data = dt_veg, select = TRUE, method = "REML")
vmg0
AICc.vmg0 <- unname(as.numeric(AICc(vmg0)))

vmg <- gam(mg ~ min_dist_enclosure_scaled + s(enclosure_name, bs = 're'),
           data = dt_veg, select = TRUE, method = "REML")
summary(vmg)
par(mfrow = c(2, 2))
gam.check(vmg)

AICc.vmg<- unname(as.numeric(AICc(vmg)))
AICc.vmg - AICc.vmg0

###### Ca
vca0 <- gam(ca ~ 1, data = dt_veg, select = TRUE, method = "REML")
vca0
AICc.vca0 <- unname(as.numeric(AICc(vca0)))

vca <- gam(ca ~ elevation_scaled + deadWoodChangeYear_scaled,
           data = dt_veg, select = TRUE, method = "REML")
summary(vca)
par(mfrow = c(2, 2))
gam.check(vca)

AICc.vca<- unname(as.numeric(AICc(vca)))
AICc.vca - AICc.vca0

###### Na
vna0 <- gam(na ~ 1, data = dt_veg, select = TRUE, method = "REML")
vna0
AICc.vna0 <- unname(as.numeric(AICc(vna0)))

vna <- gam(na ~ min_dist_enclosure_scaled,
           data = dt_veg, select = TRUE, method = "REML")
summary(vna)
par(mfrow = c(2, 2))
gam.check(vna)

AICc.vna<- unname(as.numeric(AICc(vna)))
AICc.vna - AICc.vna0


###### C:N
vcn0 <- gam(cn ~ 1, data = dt_veg, select = TRUE, method = "REML")
vcn0
AICc.vcn0 <- unname(as.numeric(AICc(vcn0)))

vcn <- gam(cn ~ s(min_dist_enclosure_scaled, k = 3) + deadWoodChangeYear_scaled,
           data = dt_veg, select = TRUE, method = "REML")
summary(vcn)
par(mfrow = c(2, 2))
gam.check(vcn)

AICc.vcn<- unname(as.numeric(AICc(vcn)))
AICc.vcn - AICc.vcn0

###### N:P
vnp0 <- gam(np ~ 1, data = dt_veg, select = TRUE, method = "REML")
vnp0
AICc.vnp0 <- unname(as.numeric(AICc(vnp0)))

vnp <- gam(np ~ hand_scaled + s(enclosure_name, bs = 're'), data = dt_veg, select = TRUE, method = "REML")
summary(vnp)
par(mfrow = c(2, 2))
gam.check(vnp)

AICc.vnp<- unname(as.numeric(AICc(vnp)))
AICc.vnp - AICc.vnp0


########### soil ##########

dt_soil <- dt_mod[type == 'Soil' & tier == 'Enclosures', ]

###### N
bm_spec
sn0 <- gam(n ~ 1, data = dt_soil, select = TRUE, method = "REML")
sn0
AICc.sn0 <- unname(as.numeric(AICc(sn0)))

sn <- gam(n ~ s(min_dist_enclosure_scaled, k = 3) + elevation_scaled,
          data = dt_soil, select = TRUE, method = "REML")
summary(sn)
par(mfrow = c(2, 2))
gam.check(sn)

AICc.sn <- unname(as.numeric(AICc(sn)))
AICc.sn - AICc.sn0

###### C
sc0 <- gam(c ~ 1, data = dt_soil, select = TRUE, method = "REML")
sc0
AICc.sc0 <- unname(as.numeric(AICc(sc0)))

sc <- gam(c ~ s(min_dist_enclosure_scaled, k = 3) + hand_scaled,
          data = dt_soil, select = TRUE, method = "REML")
summary(sc)
par(mfrow = c(2, 2))
gam.check(sc)

AICc.sc<- unname(as.numeric(AICc(sc)))
AICc.sc - AICc.sc0


###### P
sp0 <- gam(p ~ 1, data = dt_soil, select = TRUE, method = "REML")
sp0
AICc.sp0 <- unname(as.numeric(AICc(sp0)))
bm_spec
sp <- gam(p ~ elevation_scaled + deadWoodChangeYear_scaled, data = dt_soil, select = TRUE, method = "REML")
summary(sp)
par(mfrow = c(2, 2))
gam.check(vp)

AICc.sp<- unname(as.numeric(AICc(sp)))
AICc.sp - AICc.sp0

###### K
sk0 <- gam(k ~ 1, data = dt_soil, select = TRUE, method = "REML")
sk0
AICc.sk0 <- unname(as.numeric(AICc(sk0)))

sk <- gam(k ~ 1, data = dt_soil, select = TRUE, method = "REML")
summary(sk)
par(mfrow = c(2, 2))
gam.check(sk)

AICc.sk<- unname(as.numeric(AICc(sk)))
AICc.sk - AICc.sk0


###### Mg
smg0 <- gam(mg ~ 1, data = dt_soil, select = TRUE, method = "REML")
smg0
AICc.smg0 <- unname(as.numeric(AICc(smg0)))

smg <- gam(mg ~ s(min_dist_enclosure_scaled, k = 3),
           data = dt_soil, select = TRUE, method = "REML")
summary(smg)
par(mfrow = c(2, 2))
gam.check(smg)

AICc.smg <- unname(as.numeric(AICc(smg)))
AICc.smg - AICc.smg0

###### Ca
sca0 <- gam(ca ~ 1, data = dt_soil, select = TRUE, method = "REML")
sca0
AICc.sca0 <- unname(as.numeric(AICc(sca0)))

sca <- gam(ca ~ s(enclosure_name, bs = 're'),
           data = dt_soil, select = TRUE, method = "REML")
summary(sca)
par(mfrow = c(2, 2))
gam.check(sca)

AICc.sca<- unname(as.numeric(AICc(sca)))
AICc.sca - AICc.sca0

###### Na
sna0 <- gam(na ~ 1, data = dt_soil, select = TRUE, method = "REML")
sna0
AICc.sna0 <- unname(as.numeric(AICc(sna0)))

sna <- gam(na ~ min_dist_enclosure_scaled,
           data = dt_soil, select = TRUE, method = "REML")
summary(sna)
r.squaredGLMM(sna)
par(mfrow = c(2, 2))
gam.check(sna)

AICc.sna<- unname(as.numeric(AICc(sna)))
AICc.sna - AICc.sna0

###### C:N
scn0 <- gam(cn ~ 1, data = dt_soil, select = TRUE, method = "REML")
scn0
AICc.scn0 <- unname(as.numeric(AICc(scn0)))

scn <- gam(cn ~ min_dist_enclosure_scaled + soilTypeLegendK_scaled,
  data = dt_soil, select = TRUE, method = "REML")
summary(scn)
r.squaredGLMM(scn)

par(mfrow = c(2, 2))
gam.check(scn)

AICc.scn<- unname(as.numeric(AICc(scn)))
AICc.scn - AICc.scn0

###### N:P
snp0 <- gam(np ~ 1, data = dt_soil, select = TRUE, method = "REML")
snp0
AICc.snp0 <- unname(as.numeric(AICc(snp0)))

snp <- gam(np ~ min_dist_enclosure_scaled + elevation_scaled,
           data = dt_soil, select = TRUE, method = "REML")
summary(snp)
par(mfrow = c(2, 2))
gam.check(snp)

AICc.snp <- unname(as.numeric(AICc(snp)))
AICc.snp - AICc.snp0


###### Al
sla0 <- gam(al ~ 1, data = dt_soil, select = TRUE, method = "REML")
sla0
AICc.sla0 <- unname(as.numeric(AICc(sla0)))

sla <- gam(al ~ min_dist_enclosure_scaled + s(enclosure_name, bs = 're'), data = dt_soil, select = TRUE, method = "REML")
summary(sla)
par(mfrow = c(2, 2))
gam.check(sla)

AICc.sla <- unname(as.numeric(AICc(sla)))
AICc.sla - AICc.sla0

############## All models #########
model_list <- list(
  vn = vn, 
  vp = vp, 
  vc = vc, 
  vk = vk, 
  vmg = vmg, 
  vca = vca, 
  vna = vna, 
  vcn = vcn, 
  vnp = vnp, 
  sn = sn, 
  sp = sp, 
  sc = sc, 
  sk = sk, #intercept only 
  smg = smg, 
  sca = sca, 
  sna = sna, 
  scn = scn, 
  snp = snp
)

soil_plots <- list()
plant_plots <- list()
dt_opr <- data.frame()
obs_vs_pred_list <- list() 

for(i in 1:length(model_list)){
  
  m <- model_list[[i]]
  
  model_name <- names(model_list)[i]
  
  
  ### Observed vs predicted and residuals ------
  if(startsWith(model_name, "v")) {
    dt_sub <- dt_veg
  } else if(startsWith(model_name, "s")) {
    dt_sub <- dt_soil
  }
  
  
  tmp_opr <- data.frame(
    observed = m$y,
    predicted = predict(m),
    residuals = residuals(m), 
    model_name = model_name) %>% 
    mutate(clean_name = case_when(
      model_name == "vn"  ~ "Vegetation N",
      model_name == "vp"  ~ "Vegetation P",
      model_name == "vc"  ~ "Vegetation C",
      model_name == "vk"  ~ "Vegetation K",
      model_name == "vmg" ~ "Vegetation Mg",
      model_name == "vca" ~ "Vegetation Ca",
      model_name == "vna" ~ "Vegetation Na",
      model_name == "vcn" ~ "Vegetation C:N",
      model_name == "vnp" ~ "Vegetation N:P",
      model_name == "sn"  ~ "Soil N",
      model_name == "sp"  ~ "Soil P",
      model_name == "sc"  ~ "Soil C",
      model_name == "sk"  ~ "Soil K",
      model_name == "smg" ~ "Soil Mg",
      model_name == "sca" ~ "Soil Ca",
      model_name == "sna" ~ "Soil Na",
      model_name == "scn" ~ "Soil C:N",
      model_name == "snp" ~ "Soil N:P"
    ))
  
  range_vals <- range(c(tmp_opr$observed, tmp_opr$predicted), na.rm = TRUE)
  
  p_op <- tmp_opr %>% ggplot() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    geom_point(aes(x = observed, y = predicted), alpha = 0.6) +
    labs(x = "Observed", y = "Predicted", title = unique(tmp_opr$clean_name)) +
    xlim(range_vals) +
    ylim(range_vals) +
    theme_minimal()
  
  p_op
  obs_vs_pred_list[[model_name]] <- p_op
  
  dt_opr <- rbind(dt_opr, tmp_opr)
  
  #### Check concurivity ---------
  
  plot_title = case_when(
    model_name == "vn"  ~ "Vegetation N",
    model_name == "vp"  ~ "Vegetation P",
    model_name == "vc"  ~ "Vegetation C",
    model_name == "vk"  ~ "Vegetation K",
    model_name == "vmg" ~ "Vegetation Mg",
    model_name == "vca" ~ "Vegetation Ca",
    model_name == "vna" ~ "Vegetation Na",
    model_name == "vcn" ~ "Vegetation C:N",
    model_name == "vnp" ~ "Vegetation N:P",
    model_name == "sn"  ~ "Soil N",
    model_name == "sp"  ~ "Soil P",
    model_name == "sc"  ~ "Soil C",
    model_name == "sk"  ~ "Soil K",
    model_name == "smg" ~ "Soil Mg",
    model_name == "sca" ~ "Soil Ca",
    model_name == "sna" ~ "Soil Na",
    model_name == "scn" ~ "Soil C:N",
    model_name == "snp" ~ "Soil N:P"
  )
  

  # conc <- tryCatch({
  #   concurvity(m, full = F)   }, error = function(e) {
  #   message("Concurvity computation failed: ", e$message)
  #   NULL
  # })  
  # 
  # if(is.null(conc)){next}
  # 
  # colnames(conc_mat) <- rownames(conc_mat)
  # print(conc$estimate)  # matrix of concurvity values
  # conc_df <- reshape2::melt(conc_mat)
  # colnames(conc_df) <- c("term_1", "term_2", "Concurvity")
  # 
  # p_conc = ggplot(conc_df, aes(x=term_1, y=term_2, fill=Concurvity)) +
  #   geom_tile() +
  #   scale_fill_viridis_c() +    
  #   theme_minimal() +
  #   labs(title = paste0("Concurvity ", plot_title)) +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # print(p_conc)
  

  conc <- tryCatch({
    concurvity(m)
  }, error = function(e) {
    message("Concurvity computation failed: ", e$message)
    NULL
  })  
  
  if(is.null(conc)){next}
  
  conc_dt <- conc %>%
    as.data.frame() %>% 
    rownames_to_column(var = "what") %>% 
    filter(what == "estimate") %>% 
    dplyr::select(-c(para, what)) %>% 
    reshape2::melt(variable.name = "term", value.name = "concurvity")
  
  p_conc <- ggplot(conc_dt, aes(x = concurvity, y = term)) +
    geom_col(fill = "olivedrab") +
    labs(x = "Concurvity", y = "Term") +
    theme_minimal() +
    geom_vline(xintercept = 1) +
    labs(title = paste0("Concurvity ", plot_title))
  
  print(p_conc)
  
  if(startsWith(model_name, "v")) {
    plant_plots[[model_name]] <- p_conc
  } else if(startsWith(model_name, "s")) {
    soil_plots[[model_name]] <- p_conc
  }

}


library(patchwork)
### plot 

plant_combined <- wrap_plots(plant_plots, ncol = 2) + 
  plot_annotation(title = "a) Vegetation models")
print(plant_combined)


soil_combined <- wrap_plots(soil_plots, ncol = 2) + 
  plot_annotation(title = "b) Soil models")
print(soil_combined)

p_conc_all <- plant_combined / soil_combined
print(p_conc_all)
ggsave(plot = p_conc_all, "builds/plots/gam_concurvity_plots.png",
       width = 10, height = 10, dpi = 600)

# plot obs vs pred 
op_combined <- wrap_plots(obs_vs_pred_list, ncol = 5)
print(op_combined)  
ggsave(plot = op_combined, "builds/plots/gam_obs_vs_pred_plots.png",
       width = 10, height = 8, dpi = 600)

#residuals 

p_resid <- dt_opr %>% 
  ggplot() +
  geom_histogram(aes(x = residuals, y = after_stat(count))) +
  facet_wrap(~clean_name, scales = "free") +
  labs(x = "Residuals", y = "Count")
p_resid
ggsave(plot = p_resid, "builds/plots/gam_residual_plots.png",
       width = 10, height = 8, dpi = 600)
