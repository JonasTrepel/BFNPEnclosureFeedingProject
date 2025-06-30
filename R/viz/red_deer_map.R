# RED DEER PREDICTION MAP

# THE RASTER IS BASED ON EPSG 25833 AS A COORDINATE REFERENCE SYSTEM
library("readr")
library("unmarked")
library("ggplot2")
library(dplyr)
library(sf)
library(terra)
library(raster)
library(AICcmodavg)

#load model
load("builds/model_outputs/p3_1_2_1_nopred.RData")
summary(p3.1.2.1)

#load variables for predictions
var<-read.csv("data/n_mixture_data/vars_prediction.csv")


##### 1. run predictions ####

predicted_abundance <- predict(p3.1.2.1, newdata = var, type = "state", se.fit = TRUE)
print(predicted_abundance)

overall_abundance <- sum(predicted_abundance$Predicted)
print(overall_abundance) #38474.69

mean_abundance <- mean(predicted_abundance$Predicted)
print(mean_abundance) #5.662206

#accounting for chat
lambdapred<-modavgPred(cand.set = list(p3.1.2.1), newdata= var, parm.type = "lambda", type= "response", c.hat = 2.69)
print(lambdapred)


# Calculate overall abundance (sum of predicted lambda values)
overall_abundance <- sum(lambdapred$mod.avg.pred)
print(overall_abundance) #38474.69

# Calculate mean abundance (average abundance per site)
mean_abundance <- mean(lambdapred$mod.avg.pred)
print(mean_abundance) #5.662206

##### 2. work on raster ####

#load grid cell
grid_BFNP <- st_read("data/n_mixture_data/grid_BFNP.shp")  # The base extent
grid_BFNP<-grid_BFNP[,c(1,9)]  #keep useful column


#combine prediction with grid_BFNP
BFNP_pred<-cbind(grid_BFNP, lambdapred)

#keep only useful column
BFNP_Pred_mean<-BFNP_pred[,c(3,12)]  #raster with mean prediction
BFNP_Pred_se<-BFNP_pred[,c(4,12)]  #raster with SE - this is needed to plot the standard error but I do not think it is relevant in this case

#RASTERIZE MEAN VALUE 

# Convert the sf object to a SpatVector
spat_vector_mean <- vect(BFNP_Pred_mean)
plot(spat_vector_mean)

# Rasterize SpatVector
template_raster <- rast("data/n_mixture_data/DTM_final_25833.tif")  #this is just a template to base the extent of the deer raster
rasterized_mean <- rasterize(spat_vector_mean, template_raster, field = "mod.avg.pred", fun = mean)
plot(rasterized_mean)

#RASTERIZE STANDARD ERROR (not necessarily needed)

# Convert the sf object to a SpatVector
spat_vector_SE <- vect(BFNP_Pred_se)
plot(spat_vector_SE)

# Rasterize SpatVector
rasterized_SE <- rasterize(spat_vector_SE, template_raster, field = "uncond.se", fun = mean)
plot(rasterized_SE)

###### 3. download rasters #####

writeRaster(rasterized_mean, "builds/model_outputs/red_deer_map_mean.tif", overwrite = TRUE)
writeRaster(rasterized_SE, "builds/model_outputs/red_deer_map_SE.tif", overwrite = TRUE)

