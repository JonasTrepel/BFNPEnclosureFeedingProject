
library(data.table)
library(tidyverse)
library(MuMIn)
library(tidyr)
library(broom)
library(brms)
library(tidybayes)
library(MuMIn)

dt.mod <- fread("data/clean_data/bfnp_enclosure_model_data.csv") %>% 
  dplyr::filter(!flag == "exclude") %>% 
  mutate(enclosure_name = as.factor(enclosure_name))
names(dt.mod)

foreach.results <- readRDS("builds/model_outputs/gam_all_res.Rds")

names(foreach.results)
pred <- foreach.results$pred %>% unique()
bm.spec <- foreach.results$bm.spec %>% unique()


res <- foreach.results$res


### run all best models again to get significace 

#### Vegetation #######
dt.veg <- dt.mod[type == 'Vegetation' & tier == 'Enclosures', ]

###### N

vn0 <- gam(n ~ 1, data = dt.veg, select = TRUE, method = "REML")
vn0
AICc.vn0 <- unname(as.numeric(AICc(vn0)))
bm.spec[, .(sphere, response, vars)]
vn <- gam(n ~ s(min_dist_enclosure_scaled) + elevation_scaled + s(min_dist_enclosure_scaled, by = deer_density_scaled) + deadWoodChangeYear_scaled, data = dt.veg, select = TRUE, method = "REML")
summary(vn)

AICc.vn <- unname(as.numeric(AICc(vn)))
AICc.vn - AICc.vn0

###### C
vc0 <- gam(c ~ 1, data = dt.veg, select = TRUE, method = "REML")
vc0
AICc.vc0 <- unname(as.numeric(AICc(vc0)))

vc <- gam(c ~ hand_scaled + s(min_dist_enclosure_scaled, by = deer_number_scaled) + deadWoodChangeYear_scaled + soilTypeLegendK_scaled, data = dt.veg, select = TRUE, method = "REML")
summary(vc)

AICc.vc<- unname(as.numeric(AICc(vc)))
AICc.vc - AICc.vc0


###### P
vp0 <- gam(p ~ 1, data = dt.veg, select = TRUE, method = "REML")
vp0
AICc.vp0 <- unname(as.numeric(AICc(vp0)))

vp <- gam(p ~ s(min_dist_enclosure_scaled) + hand_scaled + s(enclosure_name, bs = 're') + s(enclosure_name, min_dist_enclosure_scaled, bs = 're') + deadWoodChangeYear_scaled, data = dt.veg, select = TRUE, method = "REML")
summary(vp)

AICc.vp<- unname(as.numeric(AICc(vp)))
AICc.vp - AICc.vp0

###### K
vk0 <- gam(k ~ 1, data = dt.veg, select = TRUE, method = "REML")
vk0
AICc.vk0 <- unname(as.numeric(AICc(vk0)))

vk <- gam(k ~ s(min_dist_enclosure_scaled) + s(enclosure_name, bs = 're') + deadWoodChangeYear_scaled, data = dt.veg, select = TRUE, method = "REML")
summary(vk)

AICc.vk<- unname(as.numeric(AICc(vk)))
AICc.vk - AICc.vk0

 
###### Mg
vmg0 <- gam(mg ~ 1, data = dt.veg, select = TRUE, method = "REML")
vmg0
AICc.vmg0 <- unname(as.numeric(AICc(vmg0)))

vmg <- gam(mg ~ s(min_dist_enclosure_scaled) + s(enclosure_name, bs = 're') + deadWoodChangeYear_scaled, data = dt.veg, select = TRUE, method = "REML")
summary(vmg)

AICc.vmg<- unname(as.numeric(AICc(vmg)))
AICc.vmg - AICc.vmg0

###### Ca
vca0 <- gam(ca ~ 1, data = dt.veg, select = TRUE, method = "REML")
vca0
AICc.vca0 <- unname(as.numeric(AICc(vca0)))

vca <- gam(ca ~ min_dist_enclosure_scaled + s(enclosure_name, bs = 're') + soilTypeLegendK_scaled, data = dt.veg, select = TRUE, method = "REML")
summary(vca)

AICc.vca<- unname(as.numeric(AICc(vca)))
AICc.vca - AICc.vca0

###### Na
vna0 <- gam(na ~ 1, data = dt.veg, select = TRUE, method = "REML")
vna0
AICc.vna0 <- unname(as.numeric(AICc(vna0)))

vna <- gam(na ~ min_dist_enclosure_scaled + s(enclosure_name, bs = 're') + deadWoodChangeYear_scaled, data = dt.veg, select = TRUE, method = "REML")
summary(vna)

AICc.vna<- unname(as.numeric(AICc(vna)))
AICc.vna - AICc.vna0


###### C:N
vcn0 <- gam(cn ~ 1, data = dt.veg, select = TRUE, method = "REML")
vcn0
AICc.vcn0 <- unname(as.numeric(AICc(vcn0)))

vcn <- gam(cn ~ s(min_dist_enclosure_scaled) + elevation_scaled + s(min_dist_enclosure_scaled, by = deer_density_scaled) + deadWoodChangeYear_scaled + soilTypeLegendK_scaled, data = dt.veg, select = TRUE, method = "REML")
summary(vcn)

AICc.vcn<- unname(as.numeric(AICc(vcn)))
AICc.vcn - AICc.vcn0

###### N:P
vnp0 <- gam(np ~ 1, data = dt.veg, select = TRUE, method = "REML")
vnp0
AICc.vnp0 <- unname(as.numeric(AICc(vnp0)))

vnp <- gam(np ~ hand_scaled + s(enclosure_name, bs = 're') + s(enclosure_name, min_dist_enclosure_scaled, bs = 're'), data = dt.veg, select = TRUE, method = "REML")
summary(vnp)

AICc.vnp<- unname(as.numeric(AICc(vnp)))
AICc.vnp - AICc.vnp0


########### soil ##########

dt.soil <- dt.mod[type == 'Soil' & tier == 'Enclosures', ]

###### N
bm.spec
sn0 <- gam(n ~ 1, data = dt.soil, select = TRUE, method = "REML")
sn0
AICc.sn0 <- unname(as.numeric(AICc(sn0)))

sn <- gam(n ~ s(min_dist_enclosure_scaled) + elevation_scaled, data = dt.soil, select = TRUE, method = "REML")
summary(sn)

AICc.sn <- unname(as.numeric(AICc(sn)))
AICc.sn - AICc.sn0

###### C
sc0 <- gam(c ~ 1, data = dt.soil, select = TRUE, method = "REML")
sc0
AICc.sc0 <- unname(as.numeric(AICc(sc0)))

sc <- gam(c ~ s(min_dist_enclosure_scaled) + hand_scaled + soilTypeLegendK_scaled, data = dt.soil, select = TRUE, method = "REML")
summary(sc)

AICc.sc<- unname(as.numeric(AICc(sc)))
AICc.sc - AICc.sc0


###### P
sp0 <- gam(p ~ 1, data = dt.soil, select = TRUE, method = "REML")
sp0
AICc.sp0 <- unname(as.numeric(AICc(sp0)))
bm.spec
sp <- gam(p ~ elevation_scaled + s(min_dist_enclosure_scaled, by = deer_number_scaled) + deadWoodChangeYear_scaled, data = dt.soil, select = TRUE, method = "REML")
summary(sp)

AICc.sp<- unname(as.numeric(AICc(sp)))
AICc.sp - AICc.sp0

###### K
vk0 <- gam(k ~ 1, data = dt.soil, select = TRUE, method = "REML")
vk0
AICc.vk0 <- unname(as.numeric(AICc(vk0)))

vk <- gam(k ~ deadWoodChangeYear_scaled, data = dt.soil, select = TRUE, method = "REML")
summary(vk)

AICc.vk<- unname(as.numeric(AICc(vk)))
AICc.vk - AICc.vk0


###### Mg
smg0 <- gam(mg ~ 1, data = dt.soil, select = TRUE, method = "REML")
smg0
AICc.smg0 <- unname(as.numeric(AICc(smg0)))

smg <- gam(mg ~ s(min_dist_enclosure_scaled) + elevation_scaled + s(min_dist_enclosure_scaled, by = deer_number_scaled), data = dt.soil, select = TRUE, method = "REML")
summary(smg)

AICc.smg <- unname(as.numeric(AICc(smg)))
AICc.smg - AICc.smg0

###### Ca
sca0 <- gam(ca ~ 1, data = dt.soil, select = TRUE, method = "REML")
sca0
AICc.sca0 <- unname(as.numeric(AICc(sca0)))

sca <- gam(ca ~ s(enclosure_name, bs = 're') + deadWoodChangeYear_scaled, data = dt.soil, select = TRUE, method = "REML")
summary(sca)

AICc.sca<- unname(as.numeric(AICc(sca)))
AICc.sca - AICc.sca0

###### Na
sna0 <- gam(na ~ 1, data = dt.soil, select = TRUE, method = "REML")
sna0
AICc.sna0 <- unname(as.numeric(AICc(sna0)))

sna <- gam(na ~ s(min_dist_enclosure_scaled) + hand_scaled + s(min_dist_enclosure_scaled, by = deer_density_scaled), data = dt.soil, select = TRUE, method = "REML")
summary(sna)

AICc.sna<- unname(as.numeric(AICc(sna)))
AICc.sna - AICc.sna0

###### C:N
scn0 <- gam(cn ~ 1, data = dt.soil, select = TRUE, method = "REML")
scn0
AICc.scn0 <- unname(as.numeric(AICc(scn0)))

scn <- gam(cn ~ s(min_dist_enclosure_scaled) + hand_scaled + s(min_dist_enclosure_scaled, by = deer_density_scaled) + s(enclosure_name, min_dist_enclosure_scaled, bs = 're') + soilTypeLegendK, data = dt.soil, select = TRUE, method = "REML")
summary(scn)

AICc.scn<- unname(as.numeric(AICc(scn)))
AICc.scn - AICc.scn0

###### N:P
snp0 <- gam(np ~ 1, data = dt.soil, select = TRUE, method = "REML")
snp0
AICc.snp0 <- unname(as.numeric(AICc(snp0)))

snp <- gam(np ~ elevation_scaled + s(enclosure_name, min_dist_enclosure_scaled, bs = 're'), data = dt.soil, select = TRUE, method = "REML")
summary(snp)

AICc.snp <- unname(as.numeric(AICc(snp)))
AICc.snp - AICc.snp0


###### Al
snp0 <- gam(al ~ 1, data = dt.soil, select = TRUE, method = "REML")
snp0
AICc.snp0 <- unname(as.numeric(AICc(snp0)))

snp <- gam(al ~ min_dist_enclosure_scaled + elevation_rel_enc_scaled + s(enclosure_name, bs = 're') + soilTypeLegendK_scaled, data = dt.soil, select = TRUE, method = "REML")
summary(snp)

AICc.snp <- unname(as.numeric(AICc(snp)))
AICc.snp - AICc.snp0

