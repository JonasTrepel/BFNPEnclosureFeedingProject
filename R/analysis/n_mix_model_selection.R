# RED DEER N-MIXTURE MODEL
# N-MIXTURE MODEL

library("readr")
library("unmarked")
library("AICcmodavg")
library("dplyr")
library("nmixgof")



occ_length_3<-read.csv("data/n_mixture_data/n_red_deer_occasion_lentgh_count_3days.csv") 
colnames(occ_length_3)[which(names(occ_length_3) == "X")] <- "CT_id"  

# load df with different occasion lengths 
occ_length_7<-read.csv("data/n_mixture_data/red_deer_occasion_lentgh_count_7days.csv") 
occ_length_5<-read.csv("data/n_mixture_data/red_deer_occasion_lentgh_count_5days.csv") 
occ_length_3<-read.csv("data/n_mixture_data/red_deer_occasion_lentgh_count_3days.csv") 

#change CT_id name
colnames(occ_length_7)[which(names(occ_length_7) == "X")] <- "CT_id"  
colnames(occ_length_5)[which(names(occ_length_5) == "X")] <- "CT_id"  
colnames(occ_length_3)[which(names(occ_length_3) == "X")] <- "CT_id"  

# load dataframes with predictors
vars<-read.csv("data/n_mixture_data/CT_variables_25833.csv") 

# combine predictors and occasion length tables (probably not needed but in this way I am sure to follow Lwin script)
occ_length_7_vars<-left_join(occ_length_7, vars, by="CT_id") 
occ_length_5_vars<-left_join(occ_length_5, vars, by="CT_id") 
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

### 5 days occasion length

# format data

y5<-occ_length_5_vars[,c(2:35)]         #Count Matrix for repeated count

siteCovs5 <- occ_length_5_vars[,c(72:88)]      ##Site covariates for abundance and detection

obsCovs5<-list( effort=occ_length_5_vars[,c(36:69)]) #Observation effort for detection


# unmarked df

umf5 <- unmarkedFramePCount(y = y5,   ##Count Matrix for repeated count
                            siteCovs = siteCovs5,   ##Site covariates for abundance and detection
                            obsCovs = obsCovs5 ) ##Observation effort for detection

### 7 days occasion length

# format data

y7<-occ_length_7_vars[,c(2:26)]         #Count Matrix for repeated count

siteCovs7 <- occ_length_7_vars[,c(54:70)]      ##Site covariates for abundance and detection

obsCovs7<-list( effort=occ_length_7_vars[,c(27:51)]) #Observation effort for detection


# unmarked df

umf7 <- unmarkedFramePCount(y = y7,   ##Count Matrix for repeated count
                            siteCovs = siteCovs7,   ##Site covariates for abundance and detection
                            obsCovs = obsCovs7 ) ##Observation effort for detection

###### 2. select best occasion length on global model #####

# same method to choose best occ_length (basing on global) as in Can et al. (2019) and Wang et al. (2019) 

# calculate K value for each occ. length as in Kery et al. (2018) - max K value (max(Ci,t)) + 100

max(y7,na.rm = TRUE) #20 + 100 = 120
max(y5,na.rm = TRUE) #20 + 100 = 120
max(y3,na.rm = TRUE) #10 + 100 = 110
# => use K = 120 for all


# 7 days
p7_test_nopred=pcount(~ scale(effort) +
                        scale(ruggedness_25833) +
                        scale(buffer10_VG_mean)                  #detection variables
                      ~ ht_cropped +                             #abundance variables
                        scale(veg_coverage_10m_2019_2020_25833) +
                        scale(DTM_final_25833) +
                        scale(ruggedness_25833) +
                        scale(buffer35_VG_mean) +
                        scale(dist_wint),                          
                      K=120,                                     #high K value
                      data=umf7)

summary(p7_test_nopred)  #AIC:  4499.473
save(p7_test_nopred, file="builds/model_outputs/p7_test_nopred.RData")
load("builds/model_outputs/p7_test_nopred.RData")


# 5 days
p5_test_nopred=pcount( ~ scale(effort) +
                         scale(ruggedness_25833) +
                         scale(buffer10_VG_mean)                  #detection variables
                       ~ ht_cropped +                             #abundance variables
                         scale(veg_coverage_10m_2019_2020_25833) +
                         scale(DTM_final_25833) +
                         scale(ruggedness_25833) +
                         scale(buffer35_VG_mean) +
                         scale(dist_wint),                           
                       K=120,                                      #high K value
                       data=umf5)

summary(p5_test_nopred) #AIC: 4901.769
save(p5_test_nopred, file="builds/model_outputs/p5_test_nopred.RData")
load("builds/model_outputs/p5_test_nopred.RData")

# 3 days
p3_test_nopred=pcount(~ scale(effort) +
                        scale(ruggedness_25833) +
                        scale(buffer10_VG_mean)                  #detection variables
                      ~ ht_cropped +                             #abundance variables
                        scale(veg_coverage_10m_2019_2020_25833) +
                        scale(DTM_final_25833) +
                        scale(ruggedness_25833) +
                        scale(buffer35_VG_mean) +
                        scale(dist_wint),                           
                      K=120,                                       #high K value
                      data=umf3)

summary(p3_test_nopred)  #AIC: 5516.119
save(p3_test_nopred, file="builds/model_outputs/p3_test_nopred.RData")
load("builds/model_outputs/p3_test_nopred.RData")


# run GoF to see which occ. length provides the best fit (c-hat statistic)

# 7 days
gof7=Nmix.gof.test(p7_test_nopred,nsim=1000)  
gof7
chat(p7_test_nopred)

#Observed chi-square statistic = 10243.37    #higher value indicates a larger discrepancy between observed and expected values.
#Number of bootstrap samples = 100
#P-value = 0                               #significant -> not very good sign

#Quantiles of bootstrapped statistics:
#  0%  25%  50%  75% 100% 
#2607 2727 2786 2861 2998                  #The observed chi-square statistic (7709.718) is far greater than the maximum bootstrapped statistic (3044), suggesting that the model does not adequately describe the data.

#Estimate of c-hat = 3.67                  #High overdispersion

# 5 days
gof5=Nmix.gof.test(p5_test_nopred,nsim=1000) 
gof5
chat(p5_test_nopred)

#Observed chi-square statistic = 12056.75
#Number of bootstrap samples = 100
#P-value = 0

#Quantiles of bootstrapped statistics:
#  0%  25%  50%  75% 100% 
#3564 3700 3772 3839 4109 

#Estimate of c-hat = 3.2 

# 3 days
gof3=Nmix.gof.test(p3_test_nopred,nsim=1000)  
gof3
chat(p3_test_nopred)

#Observed chi-square statistic = 16466.87 
#Number of bootstrap samples = 100
#P-value = 0

#Quantiles of bootstrapped statistics:
#  0%  25%  50%  75% 100% 
#5708 5967 6066 6175 6370 

#Estimate of c-hat = 2.71             #lowest c-hat 

## BEST FITNESS PROVIDED BY 3 DAYS OCCASION LENGTH

###### 3. run multiple model with 3-days occ length #####


# global model with only rug as detection var
p3.1.1=pcount(~ scale(effort) + scale(ruggedness_25833)        #detection variable
              ~ ht_cropped +                              #abundance variables
                scale(veg_coverage_10m_2019_2020_25833) +
                scale(DTM_final_25833) +
                scale(ruggedness_25833) +
                scale(buffer35_VG_mean) +
                scale(dist_wint),                           
              K=120,                                       #high K value
              data=umf3)

summary(p3.1.1)  #AIC: 5514.161
chat(p3.1.1) #2.68953
save(p3.1.1, file="builds/model_outputs/p3_1_1_nopred.RData")

# global model with only veg dens as detection var
p3.1.2=pcount(~ scale(effort) + scale(buffer10_VG_mean)        #detection variable
              ~ ht_cropped +                              #abundance variables
                scale(veg_coverage_10m_2019_2020_25833) +
                scale(DTM_final_25833) +
                scale(ruggedness_25833) +
                scale(buffer35_VG_mean) +
                scale(dist_wint),                           
              K=120,                                       #high K value
              data=umf3)

summary(p3.1.2) #AIC: 5514.174
chat(p3.1.2) #2.698736
save(p3.1.2, file="builds/model_outputs/p3_1_2_nopred.RData")

# veg dens as detection and no rug in abundance
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
save(p3.1.2.1, file="builds/model_outputs/p3_1_2_1_nopred.RData")
load("builds/model_outputs/p3_1_2_1_nopred.RData")

# veg dens as detection and no rug nor dist_wint in abundance
p3.1.2.2=pcount(~ scale(effort) + scale(buffer10_VG_mean)        #detection variable
                ~ ht_cropped +                                     #abundance variables
                  scale(veg_coverage_10m_2019_2020_25833) +
                  scale(DTM_final_25833) +
                  scale(buffer35_VG_mean),                           
                K=120,      
                #high K value
                data=umf3)

summary(p3.1.2.2) #AIC: 5526.645  
chat(p3.1.2.2) #2.622983
save(p3.1.2.2, file="builds/model_outputs/p3.1.2.2_nopred.RData")

# remove also habitat from abundance
p3.1.2.3=pcount(~ scale(effort) + scale(buffer10_VG_mean)        #detection variable
                ~                                                #abundance variables
                  scale(veg_coverage_10m_2019_2020_25833) +
                  scale(DTM_final_25833) +
                  scale(buffer35_VG_mean),                           
                K=120,                                             #high K value
                data=umf3)

summary(p3.1.2.3)
chat(p3.1.2.3) #2.683242
save(p3.1.2.3, file="builds/model_outputs/p3.1.2.3_nopred.RData")


###### 4. run model selection with qAICc #####

#load models to compare
load("builds/model_outputs/p3_1_1_nopred.RData")
load("builds/model_outputs/p3_1_2_nopred.RData")
load("builds/model_outputs/p3_1_2_1_nopred.RData")
load("builds/model_outputs/p3.1.2.2_nopred.RData")
load("builds/model_outputs/p3.1.2.3_nopred.RData")

#check summary 
summary(p3.1.1)
summary(p3.1.2)
summary(p3.1.2.1)
summary(p3.1.2.2)
summary(p3.1.2.3)

#make model list
Models <- list(p3.1.1,p3.1.2,
               p3.1.2.1,p3.1.2.2,p3.1.2.3)

Names <- c("p3.1.1","p3.1.2","p3.1.2.1","p3.1.2.2","p3.1.2.3")

aictab(cand.set = Models, modnames = Names, c.hat =2.6)  # best one = p3.1.2.1


##### 5. check residuals #####

residqq(p3.1.2.1) 
residfit(p3.1.2.1) 
residcov(p3.1.2.1)
chat(p3.1.2.1)

gofp3.1.2.1=Nmix.gof.test(p3.1.2.1,nsim=1000)  
gofp3.1.2.1

