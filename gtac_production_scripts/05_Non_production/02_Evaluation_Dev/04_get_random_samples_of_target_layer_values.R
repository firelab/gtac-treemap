# Evaluate target layer values against X table values
# Get random samples from target layers to compare against values in X table

# Written by Scott Zimmer
# Updated by Lila Leatherman

###################################################################################


library(tidyverse)
library(magrittr)
library(terra)

setwd("//166.2.126.25/Treemap")

home_dir <- "//166.2.126.25/Treemap"

year_in <- 2023
target_vars <- c("disturb_code",
                 "disturb_year",
                 "easting",
                 "elevation",
                 "evc",
                 "evh",
                 "northing",
                 "prcp",
                 "slope",
                 "srad",
                 "swe",
                 "tmax",
                 "tmin",
                 "vp",
                 "vpd")

#######################################################################

target_dir <- glue::glue("{home_dir}/03_Outputs/05_Target_Rasters/v2023/")
target_dir_mask <- glue::glue("{home_dir}/03_Outputs/05_Target_Rasters/v2023/one_mask/")

target_dir_eval <- glue::glue('{target_dir}/eval/')
target_dir_layer_comparison <- glue::glue('{target_dir}/eval/Value_Comparison_X_table_Target_Layers/target_layer_vectors/')

if(!exists(target_dir_layer_comparison)) {
  dir.create(target_dir_layer_comparison, recursive = TRUE)}

  
# Target Layer Values ----
# direct to all target rasters
dc_rasters<- list.files(target_dir_mask, pattern="disturb_code_LF.tif$", full.names=T, recursive = T)
dy_rasters<- list.files(target_dir_mask, pattern="disturb_year_LF.tif$", full.names=T, recursive = T)
easting_rasters<- list.files(target_dir_mask, pattern="easting.tif$", full.names=T, recursive = T)
elevation_rasters<- list.files(target_dir_mask, pattern="elevation.tif$", full.names=T, recursive = T)
evc_rasters<- list.files(target_dir_mask, pattern="evc.tif$", full.names=T, recursive = T)
evh_rasters<- list.files(target_dir_mask, pattern="evh.tif$", full.names=T, recursive = T)
northing_rasters<- list.files(target_dir_mask, pattern="northing.tif$", full.names=T, recursive = T)
prcp_rasters<- list.files(target_dir_mask, pattern="prcp.tif$", full.names=T, recursive = T)
slope_rasters<- list.files(target_dir_mask, pattern="slope.tif$", full.names=T, recursive = T)
srad_rasters<- list.files(target_dir_mask, pattern="srad.tif$", full.names=T, recursive = T)
swe_rasters<- list.files(target_dir_mask, pattern="swe.tif$", full.names=T, recursive = T)
tmax_rasters<- list.files(target_dir_mask, pattern="tmax.tif$", full.names=T, recursive = T)
tmin_rasters<- list.files(target_dir_mask, pattern="tmin.tif$", full.names=T, recursive = T)
vp_rasters<- list.files(target_dir_mask, pattern="vp.tif$", full.names=T, recursive = T)
vpd_rasters<- list.files(target_dir_mask, pattern="vpd.tif$", full.names=T, recursive = T)


# Load the landfire zones 
lf_zones<- vect("./01_Data/02_Landfire/LF_Zones/Landfire_zones/refreshGeoAreas_041210.shp")
lf_zone_nums<- sort(lf_zones$ZONE_NUM)



# Loop through all target layers, 
for(i in lf_zone_nums){
  #i = 1
  
  # Create directory for outputs
  if(!exists(paste0(target_dir_layer_comparison, "z",i))){
    dir.create(paste0(target_dir_layer_comparison, "z",i))
  }
  
  #Load raster
  r<- rast(dc_rasters[i])
  # Get non-NA values
  vals<- values(r, na.rm=T)
  # Generate a random sample of IDs to use for sampling from all target rasters
  random_ids<- sample((1:length(vals)), length(vals)/1000, replace=F)
  # Keep random sample of values
  vals_sub<- vals[random_ids]
  # Save it
  saveRDS(vals_sub, paste0(target_dir_layer_comparison, "z",i,"/dc.rds"))
  ########################
  #Load raster
  r<- rast(dy_rasters[i])
  # Get non-NA values
  vals<- values(r, na.rm=T)
  # Keep random sample of values
  vals_sub<- vals[random_ids]
  # Save it
  saveRDS(vals_sub, paste0(target_dir_layer_comparison, "z",i,"/dy.rds"))
  ########################
  #Load raster
  r<- rast(easting_rasters[i])
  # Get non-NA values
  vals<- values(r, na.rm=T)
  # Keep random sample of values
  vals_sub<- vals[random_ids]
  # Save it
  saveRDS(vals_sub, paste0(target_dir_layer_comparison, "z",i,"/easting.rds"))
  ########################
  #Load raster
  r<- rast(elevation_rasters[i])
  # Get non-NA values
  vals<- values(r, na.rm=T)
  # Keep random sample of values
  vals_sub<- vals[random_ids]
  # Save it
  saveRDS(vals_sub, paste0(target_dir_layer_comparison, "z",i,"/elevation.rds"))
  ########################
  #Load raster
  r<- rast(evc_rasters[i])
  # Get non-NA values
  vals<- values(r, na.rm=T)
  # Keep random sample of values
  vals_sub<- vals[random_ids]
  # Save it
  saveRDS(vals_sub, paste0(target_dir_layer_comparison, "z",i,"/evc.rds"))
  ########################
  #Load raster
  r<- rast(evh_rasters[i])
  # Get non-NA values
  vals<- values(r, na.rm=T)
  # Keep random sample of values
  vals_sub<- vals[random_ids]
  # Save it
  saveRDS(vals_sub, paste0(target_dir_layer_comparison, "z",i,"/evh.rds"))
  ########################
  #Load raster
  r<- rast(northing_rasters[i])
  # Get non-NA values
  vals<- values(r, na.rm=T)
  # Keep random sample of values
  vals_sub<- vals[random_ids]
  # Save it
  saveRDS(vals_sub, paste0(target_dir_layer_comparison, "z",i,"/northing.rds"))
  ########################
  #Load raster
  r<- rast(prcp_rasters[i])
  # Get non-NA values
  vals<- values(r, na.rm=T)
  # Keep random sample of values
  vals_sub<- vals[random_ids]
  # Save it
  saveRDS(vals_sub, paste0(target_dir_layer_comparison, "z",i,"/prcp.rds"))
  ########################
  #Load raster
  r<- rast(slope_rasters[i])
  # Get non-NA values
  vals<- values(r, na.rm=T)
  # Keep random sample of values
  vals_sub<- vals[random_ids]
  # Save it
  saveRDS(vals_sub, paste0(target_dir_layer_comparison, "z",i,"/slope.rds"))
  ########################
  #Load raster
  r<- rast(srad_rasters[i])
  # Get non-NA values
  vals<- values(r, na.rm=T)
  # Keep random sample of values
  vals_sub<- vals[random_ids]
  # Save it
  saveRDS(vals_sub, paste0(target_dir_layer_comparison, "z",i,"/srad.rds"))
  ########################
  #Load raster
  r<- rast(swe_rasters[i])
  # Get non-NA values
  vals<- values(r, na.rm=T)
  # Keep random sample of values
  vals_sub<- vals[random_ids]
  # Save it
  saveRDS(vals_sub, paste0(target_dir_layer_comparison, "z",i,"/swe.rds"))
  ########################
  #Load raster
  r<- rast(tmax_rasters[i])
  # Get non-NA values
  vals<- values(r, na.rm=T)
  # Keep random sample of values
  vals_sub<- vals[random_ids]
  # Save it
  saveRDS(vals_sub, paste0(target_dir_layer_comparison, "z",i,"/tmax.rds"))
  ########################
  #Load raster
  r<- rast(tmin_rasters[i])
  # Get non-NA values
  vals<- values(r, na.rm=T)
  # Keep random sample of values
  vals_sub<- vals[random_ids]
  # Save it
  saveRDS(vals_sub, paste0(target_dir_layer_comparison, "z",i,"/tmin.rds"))
  ########################
  #Load raster
  r<- rast(vp_rasters[i])
  # Get non-NA values
  vals<- values(r, na.rm=T)
  # Keep random sample of values
  vals_sub<- vals[random_ids]
  # Save it
  saveRDS(vals_sub, paste0(target_dir_layer_comparison, "z",i,"/vp.rds"))
  ########################
  #Load raster
  r<- rast(vpd_rasters[i])
  # Get non-NA values
  vals<- values(r, na.rm=T)
  # Keep random sample of values
  vals_sub<- vals[random_ids]
  # Save it
  saveRDS(vals_sub, paste0(target_dir_layer_comparison, "z",i,"/vpd.rds"))
  ########################
  gc()
  print(paste0("Finished getting values from zone ",lf_zone_nums[i]))
  
}
