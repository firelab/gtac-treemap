# Set up
setwd(this.path::here())

library(terra)
library(glue)

# set file destination
#dir <- glue::glue("{data_dir}02_Landfire")

data_dir<-"//166.2.126.25/TreeMap/01_Data/"

# Load the landfire zones and get zone numbers
lf_zones<- vect(glue::glue("{dir}/LF_zones/Landfire_zones/refreshGeoAreas_041210.shp"))
lf_zone_nums<- sort(lf_zones$ZONE_NUM)


# Load all full daymet normal climate rasters
prcp<- rast(glue::glue("{data_dir}/07_Daymet/daymet_north_america_normal/prcp_normal_1981to2010.tif"))
srad<- rast(glue::glue("{data_dir}/07_Daymet/daymet_north_america_normal/srad_normal_1981to2010.tif"))
swe<- rast(glue::glue("{data_dir}/07_Daymet/daymet_north_america_normal/swe_normal_1981to2010.tif"))
tmax<- rast(glue::glue("{data_dir}/07_Daymet/daymet_north_america_normal/tmax_normal_1981to2010.tif"))
tmin<- rast(glue::glue("{data_dir}/07_Daymet/daymet_north_america_normal/tmin_normal_1981to2010.tif"))
vp<- rast(glue::glue("{data_dir}/07_Daymet/daymet_north_america_normal/vp_normal_1981to2010.tif"))
vpd<- rast(glue::glue("{data_dir}/07_Daymet/daymet_north_america_normal/vpd_normal_1981to2010.tif"))



# Loop through landfire zones, creating a folder for resampled climate data 

for (i in lf_zone_nums){
  
  # Make a data_directory for saving climate data for the zone
  if(i<10){
    data_dir.create(glue::glue("{data_dir}/08_Target_Data_By_Zone/z0{i}"))
  } else {
    data_dir.create(glue::glue("{data_dir}/08_Target_Data_By_Zone/z{i}"))
  }
  
  
  # Read in the tree mask from the LandFire EVC layer for this zone----
  if (i<10) {
    evc<- rast(glue::glue("{data_dir}/08_Target_Data_By_Zone/z0{i}/evc.tif"))
  } else {
    evc<- rast(glue::glue("{data_dir}/08_Target_Data_By_Zone/z{i}/evc.tif"))
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
    writeRaster(prcp_final, glue::glue("{data_dir}/08_Target_Data_By_Zone/z0{i}/prcp_normal_1981to2020.tif"), datatype = "FLT4S")
  } else {
    writeRaster(prcp_final, glue::glue("{data_dir}/08_Target_Data_By_Zone/z{i}/prcp_normal_1981to2020.tif"), datatype = "FLT4S")
  }
  
  #
  
  if (i<10){
    writeRaster(srad_final, glue::glue("{data_dir}/08_Target_Data_By_Zone/z0{i}/srad_normal_1981to2020.tif"), datatype = "FLT4S")
  } else {
    writeRaster(srad_final, glue::glue("{data_dir}/08_Target_Data_By_Zone/z{i}/srad_normal_1981to2020.tif"), datatype = "FLT4S")
  }
  
  
  #
  
  if (i<10){
    writeRaster(swe_final, glue::glue("{data_dir}/08_Target_Data_By_Zone/z0{i}/swe_normal_1981to2020.tif"), datatype = "FLT4S")
  } else {
    writeRaster(swe_final, glue::glue("{data_dir}/08_Target_Data_By_Zone/z{i}/swe_normal_1981to2020.tif"), datatype = "FLT4S")
  }
  
  
  #
  
  if (i<10){
    writeRaster(tmax_final, glue::glue("{data_dir}/08_Target_Data_By_Zone/z0{i}/tmax_normal_1981to2020.tif"), datatype = "FLT4S")
  } else {
    writeRaster(tmax_final, glue::glue("{data_dir}/08_Target_Data_By_Zone/z{i}/tmax_normal_1981to2020.tif"), datatype = "FLT4S")
  }
  
  
  #
  
  if (i<10){
    writeRaster(tmin_final, glue::glue("{data_dir}/08_Target_Data_By_Zone/z0{i}/tmin_normal_1981to2020.tif"), datatype = "FLT4S")
  } else {
    writeRaster(tmin_final, glue::glue("{data_dir}/08_Target_Data_By_Zone/z{i}/tmin_normal_1981to2020.tif"), datatype = "FLT4S")
  }
  
  
  #
  
  if (i<10){
    writeRaster(vp_final, glue::glue("{data_dir}/08_Target_Data_By_Zone/z0{i}/vp_normal_1981to2020.tif"), datatype = "FLT4S")
  } else {
    writeRaster(vp_final, glue::glue("{data_dir}/08_Target_Data_By_Zone/z{i}/vp_normal_1981to2020.tif"), datatype = "FLT4S")
  }
  
  
  #
  
  if (i<10){
    writeRaster(vpd_final, glue::glue("{data_dir}/08_Target_Data_By_Zone/z0{i}/vpd_normal_1981to2020.tif"), datatype = "FLT4S")
  } else {
    writeRaster(vpd_final, glue::glue("{data_dir}/08_Target_Data_By_Zone/z{i}/vpd_normal_1981to2020.tif"), datatype = "FLT4S")
  }
  
  
}
