# RED DEER N-MIXTURE MODEL

remove(list=ls())

# N-MIXTURE MODEL

library("readr")
library("unmarked")
library("AICcmodavg")
library("dplyr")


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

siteCovs3 <- occ_length_3_vars[,c(118:134)]      ##Site covariates for abundance and detection

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
#chat(p3.1.2.1) #2.694068
saveRDS(p3.1.2.1, file="builds/model_outputs/p3_1_2_1_nopred.Rds")
load("builds/model_outputs/p3_1_2_1_nopred.RData")
