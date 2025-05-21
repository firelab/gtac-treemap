# Set up
setwd("//166.2.126.25/TreeMap/")

library(terra)

# Load the landfire zones and get zone numbers
lf_zones<- vect("./01_Data/02_Landfire/LF_zones/Landfire_zones/refreshGeoAreas_041210.shp")
lf_zone_nums<- sort(lf_zones$ZONE_NUM)


# Load all full daymet normal climate rasters
prcp<- rast("./01_Data/07_Daymet/daymet_north_america_normal/prcp_normal_1981to2010.tif")
srad<- rast("./01_Data/07_Daymet/daymet_north_america_normal/srad_normal_1981to2010.tif")
swe<- rast("./01_Data/07_Daymet/daymet_north_america_normal/swe_normal_1981to2010.tif")
tmax<- rast("./01_Data/07_Daymet/daymet_north_america_normal/tmax_normal_1981to2010.tif")
tmin<- rast("./01_Data/07_Daymet/daymet_north_america_normal/tmin_normal_1981to2010.tif")
vp<- rast("./01_Data/07_Daymet/daymet_north_america_normal/vp_normal_1981to2010.tif")
vpd<- rast("./01_Data/07_Daymet/daymet_north_america_normal/vpd_normal_1981to2010.tif")


# Create output directory
dir.create("./03_Outputs/05_Target_Rasters/v2023/premask")

# Loop through Landfire zones, creating a folder for resampled climate data 


for (i in lf_zone_nums){
  
  # Make a directory for saving climate data for the zone
  if(i<10){
    dir.create(paste0("./03_Outputs/05_Target_Rasters/v2023/pre_mask/z0",i))
  } else {
    dir.create(paste0("./03_Outputs/05_Target_Rasters/v2023/pre_mask/z",i))
  }

  
  # Read in the preliminary tree mask from the LandFire EVC layer for this zone----
  if (i<10) {
    evc<- rast(paste0("./03_Outputs/05_Target_Rasters/v2023/pre_mask/z0",i,"/evc.tif"))
  } else {
    evc<- rast(paste0("./03_Outputs/05_Target_Rasters/v2023/pre_mask/z",i,"/evc.tif"))
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
  prcp_final<- terra::crop(terra::project(x = prcp_crop, 
                            y = evc,
                            method = "cubic",
                            align = TRUE,
                            res = res(evc),
                            threads = TRUE),
                           evc, mask = TRUE, touches = TRUE)
  #
  srad_final<- terra::crop(terra::project(x = srad_crop, 
                                          y = evc,
                                          method = "cubic",
                                          align = TRUE,
                                          res = res(evc),
                                          threads = TRUE),
                           evc, mask = TRUE, touches = TRUE)
  #
  swe_final<- terra::crop(terra::project(x = swe_crop, 
                                          y = evc,
                                          method = "cubic",
                                          align = TRUE,
                                          res = res(evc),
                                          threads = TRUE),
                           evc, mask = TRUE, touches = TRUE)
  # correct negative SWE values
  swe_final[swe_final < 0]<- 0
  
  #
  tmax_final<- terra::crop(terra::project(x = tmax_crop, 
                                          y = evc,
                                          method = "cubic",
                                          align = TRUE,
                                          res = res(evc),
                                          threads = TRUE),
                           evc, mask = TRUE, touches = TRUE)
  #
  tmin_final<- terra::crop(terra::project(x = tmin_crop, 
                                          y = evc,
                                          method = "cubic",
                                          align = TRUE,
                                          res = res(evc),
                                          threads = TRUE),
                           evc, mask = TRUE, touches = TRUE)
  #
  vp_final<- terra::crop(terra::project(x = vp_crop, 
                                          y = evc,
                                          method = "cubic",
                                          align = TRUE,
                                          res = res(evc),
                                          threads = TRUE),
                           evc, mask = TRUE, touches = TRUE)
  #
  vpd_final<- terra::crop(terra::project(x = vpd_crop, 
                                          y = evc,
                                          method = "cubic",
                                          align = TRUE,
                                          res = res(evc),
                                          threads = TRUE),
                           evc, mask = TRUE, touches = TRUE)
  
  
  

  # Save the final raster for each zone ----
  if (i<10){
    writeRaster(prcp_final, paste0("./03_Outputs/05_Target_Rasters/v2023/pre_mask/z0",i,"/prcp_normal_1981to2020.tif"), datatype = "FLT4S")
  } else {
    writeRaster(prcp_final, paste0("./03_Outputs/05_Target_Rasters/v2023/pre_mask/z",i,"/prcp_normal_1981to2020.tif"), datatype = "FLT4S")
  }
  
  #
  
  if (i<10){
    writeRaster(srad_final, paste0("./03_Outputs/05_Target_Rasters/v2023/pre_mask/z0",i,"/srad_normal_1981to2020.tif"), datatype = "FLT4S")
  } else {
    writeRaster(srad_final, paste0("./03_Outputs/05_Target_Rasters/v2023/pre_mask/z",i,"/srad_normal_1981to2020.tif"), datatype = "FLT4S")
  }
  
  #
  
  if (i<10){
    writeRaster(swe_final, paste0("./03_Outputs/05_Target_Rasters/v2023/pre_mask/z0",i,"/swe_normal_1981to2020.tif"), datatype = "FLT4S")
  } else {
    writeRaster(swe_final, paste0("./03_Outputs/05_Target_Rasters/v2023/pre_mask/z",i,"/swe_normal_1981to2020.tif"), datatype = "FLT4S")
  }
  
  #
  
  if (i<10){
    writeRaster(tmax_final, paste0("./03_Outputs/05_Target_Rasters/v2023/pre_mask/z0",i,"/tmax_normal_1981to2020.tif"), datatype = "FLT4S")
  } else {
    writeRaster(tmax_final, paste0("./03_Outputs/05_Target_Rasters/v2023/pre_mask/z",i,"/tmax_normal_1981to2020.tif"), datatype = "FLT4S")
  }
  
  #
  
  if (i<10){
    writeRaster(tmin_final, paste0("./03_Outputs/05_Target_Rasters/v2023/pre_mask/z0",i,"/tmin_normal_1981to2020.tif"), datatype = "FLT4S")
  } else {
    writeRaster(tmin_final, paste0("./03_Outputs/05_Target_Rasters/v2023/pre_mask/z",i,"/tmin_normal_1981to2020.tif"), datatype = "FLT4S")
  }
  
  #
  
  if (i<10){
    writeRaster(vp_final, paste0("./03_Outputs/05_Target_Rasters/v2023/pre_mask/z0",i,"/vp_normal_1981to2020.tif"), datatype = "FLT4S")
  } else {
    writeRaster(vp_final, paste0("./03_Outputs/05_Target_Rasters/v2023/pre_mask/z",i,"/vp_normal_1981to2020.tif"), datatype = "FLT4S")
  }
  
  #
  
  if (i<10){
    writeRaster(vpd_final, paste0("./03_Outputs/05_Target_Rasters/v2023/pre_mask/z0",i,"/vpd_normal_1981to2020.tif"), datatype = "FLT4S")
  } else {
    writeRaster(vpd_final, paste0("./03_Outputs/05_Target_Rasters/v2023/pre_mask/z",i,"/vpd_normal_1981to2020.tif"), datatype = "FLT4S")
  }
  

}
