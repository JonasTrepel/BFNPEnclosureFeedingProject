# BFNPEnclosureFeedingProject

This project contains data and scripts for the manuscript "Intensive Feeding Modifies Nutrient Patterns in a Strictly Protected Area" 

## Main dataset
The main dataset is "bfnp_enclosure_model_data.csv" and can be found in "data/clean_data/". The relevant columns can be described as follows: 

plot_id: unique ID of each sampling plot

site_id: unique ID of each site (e.g., enclosure)

transect_id: unique ID of each transect

notes: notes made during the sampling 

type: whether the sample is a vegetation or soil sample

elevation: elevation above sea level in meter

n: nitrogen concentration 

c: carbon concentration 

k: potassium consentration

p: phosphorous concentration 

mg: magnesium concentration 

ca: calcium concentration 

na: sodium concentration 

cn: C/N ratio

np: N/P ratio

units: units of the concentrations

mountain_or_enclosure: whether the sample is part of the enclosure study ("enclosure")

x: longitude 

y: latitude

flag: inidicates if point should be excluded or not

distance_enclosure: idealized distance to the enclosure

min_distance_enclosure: realized minimal distance to the enclosure

deer_number: average deer number in the respective enclosure

deer_density: deer density in the enclosure

soilTypeLegendK: soil type (numeric)

soilTypeLegendL: soil type (descriptive)

elevation_rel_enc: elevation relative to the first plot on the transect (in the enclosure)

deadWoodChangeYear: year the tree cover turned to deadwood

..._scaled: variables ending on "_scaled" have been scaled and centred using the scale() function.  

## Scripts 
Scripts for data cleaning and preparation are stored at: "R/cleaning_and_preperation/"

Scripts for the analysis are stored at: "R/analysis/"

Script for vizualizaitons and figures are at: "R/viz/"

Scripts for functions are stored at: "R/functions/".

Note that we did not upload most of the spatial data as this would exceed GitHubs storage capacities. Please don't hesitate to reach out if you have any questions (jonas.trepel@bio.au.dk)!
