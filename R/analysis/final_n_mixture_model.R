# RED DEER N-MIXTURE MODEL

#remove(list=ls())

# N-MIXTURE MODEL

library("readr")
library("unmarked")
library("AICcmodavg")
library("dplyr")
library("nmixgof")
library(tidyverse)
library(data.table)

occ_length_3<-read.csv("data/n_mixture_data/red_deer_occasion_lentgh_count_3days.csv") 
colnames(occ_length_3)[which(names(occ_length_3) == "X")] <- "CT_id"  

# load dataframes with predictors
vars<-read.csv("data/n_mixture_data/CT_variables_25833.csv") 

# combine predictors and occasion length tables (probably not needed but in this way I am sure to follow Lwin script)
occ_length_3_vars<-left_join(occ_length_3, vars, by="CT_id") 

###### 1. create unmarked data frame ######

### 3 days occasion length

# format data

y3<-occ_length_3_vars[,c(2:58)]         #Count Matrix for repeated count

siteCovs3 <- occ_length_3_vars[,c(118:134)] %>% ##Site covariates for abundance and detection
  mutate(buffer35_VG_mean = buffer35_VG_mean*(-1), #need tp be inverted to ensure larger values - higher density 
         buffer10_VG_mean = -buffer10_VG_mean*(-1))

obsCovs3<-list( effort=occ_length_3_vars[,c(59:115)]) #Observation effort for detection


# unmarked df

umf3 <- unmarkedFramePCount(y = y3,   ##Count Matrix for repeated count
                            siteCovs = siteCovs3,   ##Site covariates for abundance and detection
                            obsCovs = obsCovs3 ) ##Observation effort for detection



###### 2. run final model #####


p3.1.2.1=pcount(~ scale(effort) + scale(buffer10_VG_mean)        #detection variable
                ~ ht_cropped +                                     #abundance variables
                  scale(veg_coverage_10m_2019_2020_25833) +
                  scale(DTM_final_25833) +
                  scale(buffer35_VG_mean) +
                  scale(dist_wint),                           
                K=120,                                             #high K value
                data=umf3)

summary(p3.1.2.1) #AIC: 5514.421 
chat(p3.1.2.1) #2.694068
saveRDS(p3.1.2.1, file="builds/model_outputs/p3_1_2_1_nopred.Rds")
load("builds/model_outputs/p3_1_2_1_nopred.RData")

###### 3. Adjust estimate with quasi-likelihood approach #####

m_summ <- summary(p3.1.2.1)

dt_est_state <- m_summ$state %>%
  as.data.frame() %>% 
  rownames_to_column(var = "term") %>% 
  rename(estimate = Estimate) %>% 
  mutate(adj_se = round(SE * sqrt(chat(p3.1.2.1)), 3), #correct for overdispersion
         adj_ci_ub = estimate + 1.96*adj_se, 
         adj_ci_lb = estimate - 1.96*adj_se,
         p_adj = round(2 * (1 - pnorm(abs(estimate / adj_se))), 3),
         z = round(z, 3),
         ci_lb = estimate - 1.96*SE, 
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
  ), 
  Component = "Abundance")


dt_est_det <- m_summ$det %>%
  as.data.frame() %>% 
  rownames_to_column(var = "term") %>% 
  rename(estimate = Estimate) %>% 
  mutate(adj_se = round(SE * sqrt(chat(p3.1.2.1)), 3), #correct for overdispersion
         adj_ci_ub = estimate + 1.96*adj_se, 
         adj_ci_lb = estimate - 1.96*adj_se,
         p_adj = round(2 * (1 - pnorm(abs(estimate / adj_se))), 3),
         z = round(z, 3),
         ci_lb = estimate - 1.96*SE, 
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
    term == "scale(effort)" ~ "Sampling Effort",
    term == "scale(buffer10_VG_mean)" ~ "Vegetation Density",
    
    TRUE ~ term
  ), 
  Component = "Detection")


dt_est <- rbind(dt_est_det, dt_est_state) %>%
  mutate(est_ci = paste0(
    round(estimate, 3), 
    " (", 
    round(adj_ci_lb, 3), 
    "; ",
    round(adj_ci_ub, 3), 
    ")")) %>% 
  select(
    Component = Component,
    Term = clean_term,
    `Estimate (CI)` = est_ci,
    SE = adj_se,
    z,
    p = p_adj)

fwrite(dt_est, "builds/model_outputs/estimate_table.csv")
# Calculate overdispersion parameter
chat(p3.1.2.1)

# Extract abundance part of the model
abundance_summary <- summary(p3.1.2.1)$state

# Extract standard errors from abundance part
se <- abundance_summary[, "SE"]

# Adjust standard errors for over-dispersion 
adjusted_se <- se * sqrt(chat(p3.1.2.1)) 

# Create a summary table with adjusted standard errors 
summary_table <- data.frame(
  Estimate = abundance_summary[, "Estimate"],
  Adjusted_SE = adjusted_se,
  Z_value = abundance_summary[, "Estimate"] / adjusted_se,
  P_value = 2 * (1 - pnorm(abs(abundance_summary[, "Estimate"] / adjusted_se)))
)
print(summary_table)
