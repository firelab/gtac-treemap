library(terra)

# Set up
home_dir  <<- "//166.2.126.25/TreeMap/"
setwd(home_dir)



# Load the landfire zones 
lf_zones<- vect("./01_Data/02_Landfire/LF_zones/Landfire_zones/refreshGeoAreas_041210.shp")
lf_zone_nums<- sort(lf_zones$ZONE_NUM)

for (i in lf_zone_nums){
  ifelse(i<10,
         i_zero<- paste0("0",i), 
         i_zero<-i)
  
  in_dir<- paste0("./03_Outputs/05_Target_Rasters/v2022/post_mask/z",i_zero) 
  

  # Load final post-mask evc for the zone
  evc<- terra::rast(paste0(in_dir,"/evc.tif"))
  
  # Load prcp and srad
  prcp<- terra::rast(paste0(in_dir,"/prcp.tif"))
  srad<- terra::rast(paste0(in_dir,"/srad.tif"))
  
  
  # Check that the number of valid pixels between EVC and Daymet are the same
  # Only doing SRAD and PRCP, because Srad could be different but prcp and all others should be the same
  evc_valid_pixel_length<- length(values(evc, na.rm=T))
  prcp_valid_pixel_length<- length(values(prcp, na.rm=T))
  srad_valid_pixel_length<- length(values(srad, na.rm=T))
  
  
  if (evc_valid_pixel_length != srad_valid_pixel_length |
      evc_valid_pixel_length != prcp_valid_pixel_length) {
    print(paste0("!! EVC and Daymet valid pixel length not identical for zone ",i,"!!"))
  } else {
    print(paste0("EVC and Daymet valid pixel length are identical for zone ",i))
  }
  

  gc()
}
