##############################################################
######## Bavarian Forest Enclosures Models GAMS ##########
##############################################################
####### JULY 2024 ########
###### Jonas Trepel ######


source("R/functions/partialPred.R")

library(data.table)
library(tidyverse)
library(MuMIn)
library(tidyr)
library(broom)
library(mgcv)
library(brms)
library(tidybayes)
library(tictoc)

dt.mod <- fread("data/clean_data/bfnp_enclosure_model_data.csv") %>% 
  dplyr::filter(!flag == "exclude") %>% 
  mutate(enclosure_name = as.factor(enclosure_name))
names(dt.mod)

dt.cor <- dt.mod %>% 
  dplyr::select(elevation_scaled, hand_scaled, distance_enclosure_scaled, min_dist_enclosure_scaled, deer_number_scaled, enclosure_age_scaled, deer_density_scaled, elevation_rel_enc_scaled) %>%
  filter(complete.cases(.))
library(ggcorrplot)
corr <- round(cor(dt.cor), 1)

ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)

ggplot() +
  geom_point(data = dt.mod, aes(y = distance_enclosure, x = min_dist_enclosure))

ggplot() +
  geom_point(data = dt.mod, aes(y = min_dist_enclosure, x = min_dist_enclosure))

#### build model guide 



responses<- c("n",
              "c",
              "k",
              "p",
              "mg",
              "ca",
              "na", 
              "cn", 
              "np", 
              "al")


names(dt.mod %>% dplyr::select(contains("scaled")))
vars <- c("s(min_dist_enclosure_scaled)",
          "min_dist_enclosure_scaled",
          "hand_scaled",
          "elevation_scaled",
          "elevation_rel_enc_scaled",
          "s(min_dist_enclosure_scaled, by = deer_density_scaled)",
          "s(min_dist_enclosure_scaled, by = deer_number_scaled)",
          # "s(min_dist_enclosure_scaled, by = elevation_scaled)",
          # "s(min_dist_enclosure_scaled, by = hand_scaled)",
          "s(enclosure_name, bs = 're')",
          "s(enclosure_name, min_dist_enclosure_scaled, bs = 're')", 
          "deadWoodChangeYear_scaled", 
          "soilTypeLegendK_scaled"
          
)


# Function to generate all combinations of the variables
generate.combinations <- function(vars, max.n) {
  all.combs <- c()  # Initialize an empty vector to store combinations
  
  # Loop through different sizes of combinations 
  for (i in 1:max.n) {
    comb <- combn(vars, i, simplify = FALSE)  # Generate combinations of size i
    all.combs <- c(all.combs, comb)  # Append to all_combinations
  }
  
  return(all.combs)
}

# Generate all combinations
combinations <- generate.combinations(vars, max.n = 5)

c.comb <- sapply(combinations, function(x) paste(x, collapse = " + "))


#### define data tiers -------------------
subsets <- c("type == 'Soil' & tier == 'Enclosures'", "type == 'Vegetation' & tier == 'Enclosures'")

sphere = c("Soil", "Vegetation")

dt.mod$tier

dt.tier <- data.table(
  subset = subsets, 
  sphere = sphere, 
  n = 0)

for(i in 1:nrow(dt.tier)){
  subset <- dt.tier[i, ]$subset
  dt.sub <- dt.mod %>% filter(eval(parse(text = subset)))
  
  nr <- nrow(dt.sub)
  dt.tier[i, ]$n <- nr
  
}

## build guides
guide.raw.raw <- CJ(vars = c.comb, 
                    response = responses, 
                    sphere = sphere) %>% 
  mutate(formula_id = paste0("formula_", 1:nrow(.)), 
         n_var =  sapply(vars, function(x) length(unlist(strsplit(x, " \\+ ")))), 
         formula = paste0(response, " ~ ", vars)) %>% 
  left_join(dt.tier) 

inter.var <- c(1)
guide.inter <-  CJ(vars = inter.var, 
                   response = responses, 
                   sphere = sphere) %>% 
  mutate(formula_id = paste0("intercept_formula_", 1:nrow(.)), 
         n_var = 0, 
         formula = paste0(response, " ~ ", vars)) %>% 
  left_join(dt.tier)


guide.raw <- rbind(guide.raw.raw, guide.inter)
#### check correlations ---------------
vars
vars.clean <- gsub("s\\(", "", vars)
vars.clean <- gsub("\\)", "", vars.clean)
vars.clean <- gsub("min_dist_enclosure_scaled, by = ", "", vars.clean)
vars.clean <- data.table(
  vars.clean = vars.clean) %>%
  filter(!grepl("enclosure_name", vars.clean)) %>%
  #filter(!grepl("deadWood", vars.clean)) %>%
  filter(!grepl("soilType", vars.clean)) %>%
  pull() %>%
  unique()
vars.clean

pair.combs <- combn(vars.clean, 2, simplify = FALSE)  

dt.corr <- data.table(
  var1 = character(), 
  var2 = character(), 
  corr = numeric(), 
  exclude_if = character()
)

exclusions <- c()

for(i in 1:length(pair.combs)){
  
  dt.tmp <- data.table(
    var1 = pair.combs[[i]][1], 
    var2 = pair.combs[[i]][2], 
    corr = NA,
    exclude_if = NA
  )
  
  var1 <- dt.mod %>% dplyr::select(all_of(dt.tmp$var1)) %>% pull()
  var2 <- dt.mod %>% dplyr::select(all_of(dt.tmp$var2)) %>% pull()
  
  cor.ob <- cor.test(var1, var2)
  
  corr <- unname(cor.ob$estimate)
  
  dt.tmp$corr <- corr
  
  exclusions.tmp <- c()
  
  if(abs(corr) >= 0.6){
    
    dt.tmp$exclude_if <- paste0("grepl('", dt.tmp$var1, "', formula) & grepl('", dt.tmp$var2,"', formula)")
    
    exclusions.tmp <- guide.raw %>% filter(eval(parse(text = dt.tmp$exclude_if))) %>% dplyr::select(formula_id) %>% pull
    
  }
  
  dt.corr <- rbind(dt.corr, dt.tmp)
  
  exclusions <- c(exclusions, exclusions.tmp)
  
  exclusions <- unique(exclusions)
  
  print(i)
}

guide.raw2 <- guide.raw %>% filter(!formula_id %in% c(exclusions)) 

guide.raw2[grepl("min_dist_enclosure_scaled, by = elevation_scaled", formula) ,]

guide.raw2[grepl("min_dist_enclosure_scaled \\|", formula) & grepl("1 \\|", formula),]


guide <- guide.raw2 %>% 
  mutate(interaction = ifelse(grepl("by", vars), TRUE, FALSE), 
         exclude = case_when(
           .default = "no", 
           grepl("by = deer_number_scaled", formula) & grepl("\\|", formula) ~ "exclude", ### by including an interaction with deer density and numbers basically account for 
           grepl("by = deer_density_scaled", formula) & grepl("\\|", formula) ~ "exclude", ### by including an interaction with deer density and numbers basically account for 
           grepl("min_dist_enclosure_scaled \\|", formula) & grepl("1 \\|", formula)  ~ "exclude",
           grepl("min_dist_enclosure_scaled \\+", formula) & grepl("s\\(min_dist_enclosure_scaled", formula)  ~ "exclude",
           grepl("s\\(min_dist_enclosure_scaled\\) \\+ min_dist_enclosure_scaled", formula) ~ "exclude",
           interaction == TRUE & !(grepl("min_dist_enclosure_scaled \\+", formula) | 
                                     grepl("hand_scaled \\+", formula) |
                                     grepl("elevation_scaled \\+", formula) |
                                     grepl("min_dist_enclosure_scaled \\) \\+", formula)) ~ "exclude",
           n_var > n/10 ~ "exclude"),
  ) %>% 
  filter(exclude == "no") %>% mutate(model_group = paste0(response, "_", sphere)) %>% 
  mutate(random_effect = ifelse(grepl("'re'", vars), TRUE,FALSE)) %>% 
  filter(!(sphere == "Vegetation" & response == "al"))

unique(guide$vars)
unique(guide[response == "al", ]$sphere)



res <- data.table()
pred <- data.table()
pred.int <- data.table()

bm.spec.out <- data.table()
res.out <- data.table()
pred.out <- data.table()
pred.int.out <- data.table()


#guide <- guide %>% sample_n(10)

model_group <- "c_Vegetation"
#guide <- guide %>% filter(formula_id %in% c(bmfs))
tic()
for(model_group in unique(guide$model_group)){
                             
                             print(model_group)
                             
                             model.group <- model_group
                             
                             guide <- guide %>% data.table() %>% as_tibble() %>% as.data.table() 
                             
                             filter.resp <- unique(guide[model_group == model.group, ]$response)
                             
                             guide.sub <- guide %>% filter(model_group %in% c(model.group))
                             
                             subset <- unique(guide[model_group == model.group, ]$subset)
                             
                             dt.sub <- dt.mod %>% filter(eval(parse(text = subset)))
                             
                             #guide.sub <- guide.sub %>% sample_n(1)
                             
                             for(i in 1:nrow(guide.sub)){
                               
                               formula <- as.formula(guide.sub[i,]$formula)
                               
                               m <- tryCatch(
                                 {gam(formula, data = dt.sub, select = TRUE, method = "REML")},
                                 error = function(e) {cat("Model", i, "failed: ", e$message, "\n") 
                                   return(NULL) })
                               
                               if(is.null(m)){next}
                               
                               m.sum <- summary(m)
                               
                               
                               
                               tmp <- data.frame(r_squared = NA, 
                                                 aicc = NA, 
                                                 aic = NA,
                                                 bic = NA,
                                                 formula = NA, 
                                                 response = NA, 
                                                 deviance_expl = NA)
                               
                               
                               tmp <- tmp %>% 
                                 mutate(
                                   r_squared = m.sum$r.sq, 
                                   aicc = AICc(m), 
                                   aic = AIC(m),
                                   bic = BIC(m),
                                   formula = guide.sub[i,]$formula, 
                                   response = guide.sub[i,]$response, 
                                   model_group = guide.sub[i,]$model_group,
                                   deviance_expl = m.sum$dev.expl,
                                   formula_id = guide.sub[i,]$formula_id, 
                                   sphere = guide.sub[i,]$sphere,
                                   interaction = guide.sub[i,]$interaction,
                                   vars = guide.sub[i,]$vars, 
                                   random_effect = guide.sub[i,]$random_effect
                                 )
                               
                        
                               
                               res <- rbind(res, tmp)
                               
                               print(paste0(i, "/", nrow(guide.sub), " model group: ", model_group))
                             }
                             
                             bm.spec <- res %>% filter(model_group == model.group) %>%
                               slice_min(aicc) %>% 
                               unique() 
                             
                             formula.bm <- as.formula(bm.spec$formula)
                             
                             
                             m <- tryCatch(
                               {gam(formula.bm, data = dt.sub, select = TRUE, method = "REML")},
                               error = function(e) {cat("Model", i, "failed: ", e$message, "\n") 
                                 return(NULL) })
                             
                             summary(m)
                             bm.spec
                             
                             var.names <- bm.spec %>% dplyr::select(vars) %>% 
                               mutate(sep_vars  = str_split(string = vars, pattern = "\\+")) %>% 
                               dplyr::select(sep_vars) %>% 
                               pull() %>% 
                               map(~ .x[!grepl("by", .x)]) %>% 
                               map(~ .x[!grepl("\\|", .x)]) %>% 
                               unlist() %>% unique()
                             var.names <-  gsub("s\\(", "", var.names)
                             var.names <-  gsub(",bs='re'", "", var.names)
                             var.names <-  gsub(", bs='re'", "", var.names)
                             var.names <-  gsub("\\) ", "", var.names)
                             var.names <-  gsub("\\)", "", var.names)
                             var.names <-  gsub("enclosure_name,", "", var.names)
                             var.names <-  gsub("bs='re'", "", var.names)
                             var.names <-  gsub(" ", "", var.names)
                             var.names <-  gsub("1", "", var.names)

                             
                             var.names <- var.names %>% as.data.frame() %>% filter(!grepl("'re'", .)) %>% pull()
                             
                             var.names <- unique(var.names)
                             if(length(var.names) == 1){if(var.names == ""){var.names <- "intercept"}}
                             
                             moderators <- bm.spec %>% dplyr::select(vars) %>% 
                               mutate(sep_vars  = str_split(string = vars, pattern = "\\+")) %>% 
                               dplyr::select(sep_vars) %>% 
                               pull() %>%
                               map(~ .x[grepl("by", .x)]) %>%
                               unlist() %>% 
                               as.data.table() %>% 
                               mutate(moderators_raw  = str_split(string = ., pattern = "by")) %>%
                               dplyr::select(moderators_raw) %>% pull() %>% 
                               map(~ .x[!grepl("min_dist_enclosure_scaled", .x)]) %>% unlist()
                             
                             moderators <-  gsub("\\) ", "", moderators)
                             moderators <-  gsub("\\)", "", moderators)
                             
                             moderators <-  gsub("\\=", "", moderators)
                             moderators <-  gsub(" ", "", moderators)
                             
                             
                             ### loop through vars and get pred

                             if(length(var.names) > 0 & !any(grepl("intercept", var.names))){
                               
                               for(j in 1:length(var.names)){
                                 
                                 var <- var.names[j] 
                                 
                                 marg.tmp <- partialPred(model = m, response = filter.resp,
                                                    var = var,
                                                    data = dt.sub, newdata = dt.sub %>% dplyr::select(-c(all_of(filter.resp)))) 
                                 

                                 marg.tmp <- marg.tmp %>% rename(var_value = paste0(gsub("_scaled", "", var))) %>% mutate(term = var,
                                                                                                                          clean_var = gsub("log_", "", term),
                                                                                                                          clean_var = gsub("_scaled", "", clean_var), 
                                                                                                                          response_value = dt.sub %>% dplyr::select(c(all_of(filter.resp)))%>% pull(), 
                                                                                                                          enclosure_name = dt.sub$enclosure_name)
                                 
                                 ggplot() + geom_line(aes(x = marg.tmp$var_value, y = marg.tmp$fit))
                                 
                                 if(j==1){
                                   marg <- marg.tmp}else{
                                     marg <- rbind(marg, marg.tmp)}
                               }
                               
                               tmp.pred <- marg %>% 
                                 mutate(model_group = model.group, 
                                        response = filter.resp,
                                        formula_id = bm.spec$formula_id
                                 )
                               
                               pred <- rbind(tmp.pred, pred)
                               
                             }
                             
                             if(bm.spec$interaction == TRUE){
                               
                               for(k in 1:length(moderators)){
                                 
                                 var.int <- "min_dist_enclosure_scaled"
                                 
                                 moderator = moderators[k]
                                 
                                 
                                 marg.tmp <- partialPred(model = m, response = filter.resp,
                                                    var = var, interaction = TRUE, moderator = moderator,
                                                    data = dt.sub, newdata = dt.sub %>% dplyr::select(-c(all_of(filter.resp))))
                                 
                                 moderator.clean <-  gsub("_scaled", "", moderator)
                                 
                                 marg.tmp.int <- marg.tmp %>% rename(var_value = paste0(gsub("_scaled", "", var))) %>% mutate(term = var,
                                                                                                                              clean_var = gsub("log_", "", term),
                                                                                                                              clean_var = gsub("_scaled", "", clean_var),
                                                                                                                              enclosure_name = dt.sub$enclosure_name,
                                                                                                                              response_value = dt.sub %>% dplyr::select(c(all_of(filter.resp)))%>% pull(),
                                                                                                                              moderator_value = dt.sub %>% dplyr::select(c(all_of(moderator.clean)))%>% pull())
                                 if(k==1){
                                   marg.int <- marg.tmp.int}else{
                                     marg.int <- rbind(marg.int, marg.tmp.int)}
                               }
                               
                               tmp.pred.int <- marg.int %>% 
                                 mutate(model_group = model.group, 
                                        response = filter.resp,
                                        formula_id = bm.spec$formula_id)
                               
                               pred.int <- rbind(tmp.pred.int, pred.int)
                               
                             }
                             
                             bm.spec.out <- rbind(bm.spec, bm.spec.out)
                             res.out <- rbind(res, res.out)
                             pred.out <- rbind(pred, pred.out)
                             pred.int.out <- rbind(pred, pred.int.out)
                             
                             
                           }

print("loop done")
toc()
foreach.results <- list(bm.spec = bm.spec.out, res = res.out, pred = pred.out, pred.int = pred.int.out)
saveRDS(foreach.results, "builds/model_outputs/gam_all_res.Rds")



bm.spec <- foreach.results$bm.spec %>% unique()
pred <- foreach.results$pred %>% unique()

bm.spec[sphere == "Soil"]

unique(guide$model_group)
