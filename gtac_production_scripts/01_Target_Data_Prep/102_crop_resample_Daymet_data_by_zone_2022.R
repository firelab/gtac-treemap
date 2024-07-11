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


# Load all Daymet rasters ----
prcp<- rast("../01_Data/07_Daymet/daymet_north_america_normal/prcp_normal_1981to2010.tif")
srad<- rast("../01_Data/07_Daymet/daymet_north_america_normal/srad_normal_1981to2010.tif")
swe<- rast("../01_Data/07_Daymet/daymet_north_america_normal/swe_normal_1981to2010.tif")
tmax<- rast("../01_Data/07_Daymet/daymet_north_america_normal/tmax_normal_1981to2010.tif")
tmin<- rast("../01_Data/07_Daymet/daymet_north_america_normal/tmin_normal_1981to2010.tif")
vp<- rast("../01_Data/07_Daymet/daymet_north_america_normal/vp_normal_1981to2010.tif")
vpd<- rast("../01_Data/07_Daymet/daymet_north_america_normal/vpd_normal_1981to2010.tif")

# 

# Loop through landfire zones, masking, reprojecting, resampling, and saving data by zone ----

# First create output directory 
dir.create("./03_Outputs/05_Target_Rasters/v2022/pre_mask/")


for (i in lf_zone_nums){
  
  lf_zone<- lf_zones[lf_zones$ZONE_NUM == i,]
  
  # Make a directory for saving landfire data for the zone
  ifelse(i<10, 
         dir.create(paste0("./03_Outputs/05_Target_Rasters/v2022/pre_mask/z0",i)),
         dir.create(paste0("./03_Outputs/05_Target_Rasters/v2022/pre_mask/z",i)))
  
  # Save that directory path as a string
  ifelse(i<10, 
         out_dir<- paste0("./03_Outputs/05_Target_Rasters/v2022/pre_mask/z0",i),
         out_dir<- paste0("./03_Outputs/05_Target_Rasters/v2022/pre_mask/z",i))
  
  
  # Crop, mask, and resample layers to each landfire zone---
  
  # Read in the step 1 forest mask from the LandFire EVC layer for this zone----
  if (i<10) {
    evc<- rast(paste0("../03_Outputs/05_Target_Rasters/v2022/pre_mask/z0",i,"/evc.tif"))
  } else {
    evc<- rast(paste0("../03_Outputs/05_Target_Rasters/v2022/pre_mask/z",i,"/evc.tif"))
  }
  
  # Make a reprojected extent for cropping the climate data
  evc_extent_reproj<-  terra::project(ext(evc), 
                                      from = crs(evc), 
                                      to = crs(prcp))
  
  
  # First crop climate data by the reprojected extent ----
  
  prcp_crop<- terra::crop(prcp, evc_extent_reproj, snap="out")
  srad_crop<- terra::crop(srad, evc_extent_reproj, snap="out") 
  swe_crop<- terra::crop(swe, evc_extent_reproj, snap="out")
  tmax_crop<- terra::crop(tmax, evc_extent_reproj, snap="out")
  tmin_crop<- terra::crop(tmin, evc_extent_reproj, snap="out")
  vp_crop<- terra::crop(vp, evc_extent_reproj, snap="out")
  vpd_crop<- terra::crop(vpd, evc_extent_reproj, snap="out")
  
  
  # Then reproject, resample, and mask to the tree mask for the given landfire zone ----
  prcp_zone<- terra::crop(terra::project(x = prcp_crop, 
                                          y = evc,
                                          method = "cubic",
                                          align = TRUE,
                                          res = res(evc),
                                          threads = TRUE),
                           evc, mask = TRUE, touches = TRUE)
  #
  srad_zone<- terra::crop(terra::project(x = srad_crop, 
                                          y = evc,
                                          method = "cubic",
                                          align = TRUE,
                                          res = res(evc),
                                          threads = TRUE),
                           evc, mask = TRUE, touches = TRUE)
  #
  swe_zone<- terra::crop(terra::project(x = swe_crop, 
                                         y = evc,
                                         method = "cubic",
                                         align = TRUE,
                                         res = res(evc),
                                         threads = TRUE),
                          evc, mask = TRUE, touches = TRUE)
  #
  tmax_zone<- terra::crop(terra::project(x = tmax_crop, 
                                          y = evc,
                                          method = "cubic",
                                          align = TRUE,
                                          res = res(evc),
                                          threads = TRUE),
                           evc, mask = TRUE, touches = TRUE)
  #
  tmin_zone<- terra::crop(terra::project(x = tmin_crop, 
                                          y = evc,
                                          method = "cubic",
                                          align = TRUE,
                                          res = res(evc),
                                          threads = TRUE),
                           evc, mask = TRUE, touches = TRUE)
  #
  vp_zone<- terra::crop(terra::project(x = vp_crop, 
                                        y = evc,
                                        method = "cubic",
                                        align = TRUE,
                                        res = res(evc),
                                        threads = TRUE),
                         evc, mask = TRUE, touches = TRUE)
  #
  vpd_zone<- terra::crop(terra::project(x = vpd_crop, 
                                         y = evc,
                                         method = "cubic",
                                         align = TRUE,
                                         res = res(evc),
                                         threads = TRUE),
                          evc, mask = TRUE, touches = TRUE)
  
  
  # Save the final raster for each zone ----
  
  writeRaster(prcp_zone, paste0(out_dir, "/prcp_normal_1981to2020.tif"), datatype = "FLT4S")
  writeRaster(srad_zone, paste0(out_dir, "/srad_normal_1981to2020.tif"), datatype = "FLT4S")
  writeRaster(swe_zone, paste0(out_dir, "/swe_normal_1981to2020.tif"), datatype = "FLT4S")
  writeRaster(tmax_zone, paste0(out_dir, "/tmax_normal_1981to2020.tif"), datatype = "FLT4S")
  writeRaster(tmin_zone, paste0(out_dir, "/tmin_normal_1981to2020.tif"), datatype = "FLT4S")
  writeRaster(vp_zone, paste0(out_dir, "/vp_normal_1981to2020.tif"), datatype = "FLT4S")
  writeRaster(vpd_zone, paste0(out_dir, "/vpd_normal_1981to2020.tif"), datatype = "FLT4S")
  
  
  # Clear garbage
  gc()

}

