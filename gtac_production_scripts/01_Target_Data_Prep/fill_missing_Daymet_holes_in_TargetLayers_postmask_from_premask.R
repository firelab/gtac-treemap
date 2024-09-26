library(terra)
setwd(this.path::here())

# Set up
home_dir  <<- "//166.2.126.25/TreeMap/"
setwd(home_dir)


# Load the landfire zones 
lf_zones<- vect("./01_Data/02_Landfire/LF_zones/Landfire_zones/refreshGeoAreas_041210.shp")
lf_zone_nums<- sort(lf_zones$ZONE_NUM)

# Read in DF of zones missing Daymet pixels
missing_daymet<- read.csv("./01_Data/07_Daymet/LF_zones_missing_daymet_pixels.csv")
missing_daymet<- missing_daymet[missing_daymet$missing_Daymet_pixels > 0,] # limit to those missing daymet pixels


# get lf zones missing
missing_lf_zones<- sort(missing_daymet$LF_zone)


# Load original Daymet target rasters
prcp<- rast("./01_Data/07_Daymet/daymet_north_america_normal/prcp_normal_1981to2010.tif")
srad<- rast("./01_Data/07_Daymet/daymet_north_america_normal/srad_normal_1981to2010.tif")
swe<- rast("./01_Data/07_Daymet/daymet_north_america_normal/swe_normal_1981to2010.tif")
tmax<- rast("./01_Data/07_Daymet/daymet_north_america_normal/tmax_normal_1981to2010.tif")
tmin<- rast("./01_Data/07_Daymet/daymet_north_america_normal/tmin_normal_1981to2010.tif")
vp<- rast("./01_Data/07_Daymet/daymet_north_america_normal/vp_normal_1981to2010.tif")
vpd<- rast("./01_Data/07_Daymet/daymet_north_america_normal/vpd_normal_1981to2010.tif")


# Manually fill missing values for Dry Tortugas area.
  # This was one missing pixel too far from others to fill by the focal function
    # This manually fills the needed pixel with values from the nearest pixel
values(prcp)[45100880]<- 1064.475342
values(srad)[17256450]<- 305.387207
values(swe)[45100880]<- 0
values(tmax)[45100880]<- 28.243658
values(tmin)[45100880]<- 22.773148
values(vp)[45100880]<- 2670.494629
values(vpd)[45100880]<- 720.197021



for (i in missing_lf_zones){
  print(paste0("Starting zone ", i))
  ifelse(i<10,
         i_zero<- paste0("0",i), 
         i_zero<-i)
  
  ### Get out_dir / in_dir
  out_dir<- paste0("./03_Outputs/05_Target_Rasters/v2022/post_mask/z",i_zero)
  in_dir<- paste0("./03_Outputs/05_Target_Rasters/v2022/post_mask/z",i_zero) 
  
  
  # Load final post-mask evc for the zone
  evc<- terra::rast(paste0(in_dir,"/evc.tif"))
  
  # Make a reprojected EVC extent for cropping the original Daymet rasters
  evc_extent<- terra::project(x=terra::ext(evc),
                              from=crs(evc),
                              to=crs(prcp))
  
  # Load the needed focal w setting for the zone. This is the minimum size needed to fill all pixels for the zone
  focal_w<- missing_daymet$focal_w[missing_daymet$LF_zone == i]
  
  # Crop and perform focal analysis on the original Daymet rasters
  prcp_focal<- terra::focal(crop(prcp, evc_extent, touches=T), w=focal_w, fun="mean", na.policy="only", na.rm=T)
  srad_focal<- terra::focal(crop(srad, evc_extent, touches=T), w=focal_w, fun="mean", na.policy="only", na.rm=T)
  swe_focal<- terra::focal(crop(swe, evc_extent, touches=T), w=focal_w, fun="mean", na.policy="only", na.rm=T)
  tmax_focal<- terra::focal(crop(tmax, evc_extent, touches=T), w=focal_w, fun="mean", na.policy="only", na.rm=T)
  tmin_focal<- terra::focal(crop(tmin, evc_extent, touches=T), w=focal_w, fun="mean", na.policy="only", na.rm=T)
  vp_focal<- terra::focal(crop(vp, evc_extent, touches=T), w=focal_w, fun="mean", na.policy="only", na.rm=T)
  vpd_focal<- terra::focal(crop(vpd, evc_extent, touches=T), w=focal_w, fun="mean", na.policy="only", na.rm=T)
  
  # Create the final post-mask Daymet layers for the zone. 
    # This requires resampling and reprojecting to EVC resolution/crs, aligning with EVC, then masking to match the EVC
  prcp_zone<- terra::crop(terra::project(x = prcp_focal, 
                                         y = evc,
                                         method = "cubic",
                                         align = TRUE,
                                         res = res(evc),
                                         threads = TRUE),
                          evc, mask = TRUE, touches = TRUE)
  #
  srad_zone<- terra::crop(terra::project(x = srad_focal, 
                                         y = evc,
                                         method = "cubic",
                                         align = TRUE,
                                         res = res(evc),
                                         threads = TRUE),
                          evc, mask = TRUE, touches = TRUE)
  #
  swe_zone<- terra::crop(terra::project(x = swe_focal, 
                                         y = evc,
                                         method = "cubic",
                                         align = TRUE,
                                         res = res(evc),
                                         threads = TRUE),
                          evc, mask = TRUE, touches = TRUE)
  #
  tmax_zone<- terra::crop(terra::project(x = tmax_focal, 
                                         y = evc,
                                         method = "cubic",
                                         align = TRUE,
                                         res = res(evc),
                                         threads = TRUE),
                          evc, mask = TRUE, touches = TRUE)
  #
  tmin_zone<- terra::crop(terra::project(x = tmin_focal, 
                                         y = evc,
                                         method = "cubic",
                                         align = TRUE,
                                         res = res(evc),
                                         threads = TRUE),
                          evc, mask = TRUE, touches = TRUE)
  #
  vp_zone<- terra::crop(terra::project(x = vp_focal, 
                                         y = evc,
                                         method = "cubic",
                                         align = TRUE,
                                         res = res(evc),
                                         threads = TRUE),
                          evc, mask = TRUE, touches = TRUE)
  #
  vpd_zone<- terra::crop(terra::project(x = vpd_focal, 
                                         y = evc,
                                         method = "cubic",
                                         align = TRUE,
                                         res = res(evc),
                                         threads = TRUE),
                          evc, mask = TRUE, touches = TRUE)
  

  # Check that the number of valid pixels between EVC and Daymet are the same
    # Only doing SRAD and PRCP, because Srad could be different but prcp and all others should be the same
  evc_valid_pixel_length<- length(values(evc, na.rm=T))
  prcp_valid_pixel_length<- length(values(prcp_zone, na.rm=T))
  srad_valid_pixel_length<- length(values(srad_zone, na.rm=T))
  
  
  if (evc_valid_pixel_length != srad_valid_pixel_length |
      evc_valid_pixel_length != prcp_valid_pixel_length) {
      print(paste0("!! EVC and Daymet valid pixel length not identical for zone ",i,"!!"))
  } else {
    print(paste0("EVC and Daymet valid pixel length are identical for zone ",i))
  }

  
  
  # Do not save out the rasters if pixel counts are not identical. Change the focal w setting and try again.
  
  if (evc_valid_pixel_length != srad_valid_pixel_length |
      evc_valid_pixel_length != prcp_valid_pixel_length) next
  

  # Save out the filled rasters if pixel counts are identical
  # Daymet
  writeRaster(prcp_zone, paste0(out_dir,"/prcp.tif"), datatype = "FLT4S", overwrite=T)
  writeRaster(srad_zone, paste0(out_dir,"/srad.tif"), datatype = "FLT4S", overwrite=T)
  writeRaster(swe_zone, paste0(out_dir,"/swe.tif"), datatype = "FLT4S", overwrite=T)
  writeRaster(tmax_zone, paste0(out_dir,"/tmax.tif"), datatype = "FLT4S", overwrite=T)
  writeRaster(tmin_zone, paste0(out_dir,"/tmin.tif"), datatype = "FLT4S", overwrite=T)
  writeRaster(vp_zone, paste0(out_dir,"/vp.tif"), datatype = "FLT4S", overwrite=T)
  writeRaster(vpd_zone, paste0(out_dir,"/vpd.tif"), datatype = "FLT4S", overwrite=T)
  
  gc()
}
