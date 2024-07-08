#Load libraries
library(terra)
library(dplyr)

# Set up
home_dir  <<- "//166.2.126.25/TreeMap/"
setwd(home_dir)


# Load a landfire raster as a template to match
lf_raster<- rast("./01_Data/02_Landfire/LF_220/Vegetation/EVC/LF2022_EVC_220_CONUS/Tif/LC22_EVC_220.tif")

# Load the landfire zones 
lf_zones<- vect("./01_Data/02_Landfire/LF_zones/Landfire_zones/refreshGeoAreas_041210.shp")

# Reproject the landfire zones to match the lf raster
lf_zones<- terra::project(lf_zones, lf_raster)
lf_zone_nums<- sort(lf_zones$ZONE_NUM)


# Load all LandFire rasters ----
evc<- rast("./01_Data/02_Landfire/LF_230/Vegetation/EVC/LF2022_EVC_230_CONUS/Tif/LC22_EVC_230.tif")
evh<- rast("./01_Data/02_Landfire/LF_230/Vegetation/EVH/LF2022_EVH_230_CONUS/Tif/LC22_EVH_230.tif")
evt<- rast("./01_Data/02_Landfire/LF_230/Vegetation/EVT/LF2022_EVT_230_CONUS/Tif/LC22_EVT_230.tif")
#
aspect<- rast("./01_Data/02_Landfire/LF_220/Topo/Asp/LF2020_Asp_220_CONUS/Tif/LC20_Asp_220.tif")
elevation<- rast("./01_Data/02_Landfire/LF_220/Topo/Elev/LF2020_Elev_220_CONUS/Tif/LC20_Elev_220.tif")
slope<- rast("./01_Data/02_Landfire/LF_220/Topo/SlpD/LF2020_SlpD_220_CONUS/Tif/LC20_SlpD_220.tif")

# Set up variables to reclassify ----

# EVC codes that represent forest
# represented as range start, range end, desired end value
evc_forest_codes_mat <- matrix(c(
  -9999,100,NA,
  110,119,15, 
  120,129,25, 
  130,139,35, 
  140,149,45, 
  150,159,55,
  160,169,65,
  170,179,75, 
  180,189,85, 
  190,199,95,
  200,399,NA), 
  nrow = 11, ncol = 3, byrow = TRUE)

# EVH classes to reclass
evh_class_mat <- matrix(c(
  101,105,3,
  106,110,8,
  111,125,18,
  126,150,38),
  nrow = 4, ncol = 3, byrow = TRUE)


# set EVT to display EVT_GP
activeCat(evt) <- 7

# get evt levels - to reclassify evt to evt_gp
evt_levels <- levels(evt)[[1]] %>%
  mutate(EVT_GP = as.numeric(EVT_GP))




# Loop through landfire zones, masking and reclassifying and saving data by zone ----

# First create output directory 
dir.create("./03_Outputs/05_Target_Rasters/v2020/pre_mask/")

for (i in lf_zone_nums){
  
  lf_zone<- lf_zones[lf_zones$ZONE_NUM == i,]
  
  # Make a directory for saving landfire data for the zone
  ifelse(i<10, 
         dir.create(paste0("./03_Outputs/05_Target_Rasters/v2020/pre_mask/z0",i)),
         dir.create(paste0("./03_Outputs/05_Target_Rasters/v2020/pre_mask/z",i)))
  
  # Save that directory path as a string
  ifelse(i<10, 
         out_dir<- paste0("./03_Outputs/05_Target_Rasters/v2020/pre_mask/z0",i),
         out_dir<- paste0("./03_Outputs/05_Target_Rasters/v2020/pre_mask/z",i))
  
  
  # Crop and mask layers to each landfire zone---
  
  # Crop and Reclassify EVC
  evc_zone<- terra::classify(terra::crop(evc, lf_zone, mask = TRUE),
                             evc_forest_codes_mat, right = NA)
  # Crop and Reclassify EVH
  evh_zone<- terra::classify(terra::crop(evh, lf_zone, mask = TRUE),
                             evh_class_mat, right = NA)
  
  # Crop EVT and Reclassify to EVT-GP
  evt_gp_zone<- terra::classify(terra::crop(evt, evc_zone, mask = TRUE),
                                evt_levels)
  
  # For remaining layers, crop and mask to zone
  aspect_zone<- terra::crop(aspect, evc_zone, mask = TRUE)
  #
  elevation_zone<- terra::crop(elevation, evc_zone, mask = TRUE)
  #
  slope_zone<- terra::crop(slope, evc_zone, mask = TRUE)
  
  # Calculate northing and easting
  
  northing_zone <- terra::app(aspect_zone, function(i) cos((pi/180)*i))
  easting_zone <- terra::app(aspect_zone, function(i) sin((pi/180)*i))
  
  
  # Save the final rasters for the zone ----
  writeRaster(evc_zone, paste0(out_dir, "/evc.tif"), datatype = "FLT4S")
  writeRaster(evh_zone, paste0(out_dir, "/evh.tif"), datatype = "FLT4S")
  writeRaster(evt_gp_zone, paste0(out_dir, "/evt_gp.tif"), datatype = "FLT4S")
  #
  writeRaster(aspect_zone, paste0(out_dir, "/aspect.tif"), datatype = "FLT4S")
  writeRaster(elevation_zone, paste0(out_dir, "/elevation.tif"), datatype = "FLT4S")
  writeRaster(slope_zone, paste0(out_dir, "/slope.tif"), datatype = "FLT4S")
  #
  writeRaster(northing_zone, paste0(out_dir, "/northing.tif"), datatype = "FLT4S")
  writeRaster(easting_zone, paste0(out_dir, "/easting.tif"), datatype = "FLT4S")
  
  
  # Clear garbage
  gc()
  
}

