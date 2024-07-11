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

# Set up EVT Groups needing reclassification ----

# These EVT_GPs are reclassified to NA
  # Developed, Agricultural, etc
evt_gps_na <- c(
  13,
  14,
  15,
  26,
  60,
  730
)

# Read in the necessary zone-specific reclassification
#zonal_evt_gp_reclass<- read.csv("")

zonal_evt_gp_reclass_test<- data.frame("zone" = rep(NA, 5),
                                       "original_evt_gp" = rep(NA,5),
                                       "new_evt_gp" = rep(NA, 5))

zonal_evt_gp_reclass_test$zone<- c(99,99,99,98,97)
zonal_evt_gp_reclass_test$original_evt_gp<- c(675, 678, 679, 675, 675)
zonal_evt_gp_reclass_test$new_evt_gp<- c(698, 602, 605, 698, 698)

# 
# Loop through each zone. Mask EVT-GP to set defined codes as NA. Then reclassify if needed ----

# First create output directory 
dir.create("./03_Outputs/05_Target_Rasters/v2020/post_mask/")

for (i in lf_zone_nums){
  
  lf_zone<- lf_zones[lf_zones$ZONE_NUM == i,]
  
  # Make a directory for saving landfire data for the zone
  ifelse(i<10, 
         dir.create(paste0("./03_Outputs/05_Target_Rasters/v2020/post_mask/z0",i)),
         dir.create(paste0("./03_Outputs/05_Target_Rasters/v2020/post_mask/z",i)))
  
  # Save that directory path as a string
  ifelse(i<10, 
         out_dir<- paste0("./03_Outputs/05_Target_Rasters/v2020/post_mask/z0",i),
         out_dir<- paste0("./03_Outputs/05_Target_Rasters/v2020/post_mask/z",i))
  
  # Get the in_dir for reading in pre-mask data
  ifelse(i<10, 
         in_dir<- paste0("./03_Outputs/05_Target_Rasters/v2020/pre_mask/z0",i),
         in_dir<- paste0("./03_Outputs/05_Target_Rasters/v2020/pre_mask/z",i))
  
  # EVT GP  ----
  
  # Read in EVT GP and reclassify necessary groups as NA
  evt_gp<- terra::classify(terra::rast(paste0(in_dir,"/evt_gp.tif")),
                           cbind(evt_gps_na, NA))
  
  
  # Then reclassify further groups if necessary in this zone
  
  # Identify if the zone has any EVT GPs to relcassify
  if(i %in%  unique(zonal_evt_gp_reclass_test$zone)){
    zonal_evt_gp_reclass_test_sub<- zonal_evt_gp_reclass_test[zonal_evt_gp_reclass_test$zone == i,]  # limit the reclassify df to that zone
    evt_gp<- terra::classify(evt_gp, zonal_evt_gp_reclass_test_sub[,c(2:3)]) # reclassify the remaining relevant codes for the zone
  }
  
  
  # Create the EVT GP Remap table to track how EVT GP gets remapped to 1,2,3,4,etc
  evt_gp_list <- unique(evt_gp)
  evg_remap_table <- data.frame(EVT_GP = evt_gp_list, 
                                EVT_GP_remap = seq(1:nrow(evt_gp_list)))
  
  # Make EVT_GP remap raster
  evt_gp <- terra::classify(evt_gp, evg_remap_table) 
  
  
  # Now mask all other LandFire and Daymet data by the final evt_gp layer
  
  # Landfire
  aspect<- terra::mask(rast(paste0(in_dir, "/aspect.tif")), evt_gp)
  easting<- terra::mask(rast(paste0(in_dir, "/easting.tif")), evt_gp)
  elevation<- terra::mask(rast(paste0(in_dir, "/elevation.tif")), evt_gp)
  evc<- terra::mask(rast(paste0(in_dir, "/evc.tif")), evt_gp)
  evh<- terra::mask(rast(paste0(in_dir, "/evh.tif")), evt_gp)
  northing<- terra::mask(rast(paste0(in_dir, "/northing.tif")), evt_gp)
  slope<- terra::mask(rast(paste0(in_dir, "/slope.tif")), evt_gp)
                            
  
  # Daymet
  prcp<- terra::mask(rast(paste0(in_dir, "/prcp_normal_1981to2020.tif")), evt_gp)
  srad<- terra::mask(rast(paste0(in_dir, "/srad_normal_1981to2020.tif")), evt_gp)
  swe<- terra::mask(rast(paste0(in_dir, "/swe_normal_1981to2020.tif")), evt_gp)
  tmax<- terra::mask(rast(paste0(in_dir, "/tmax_normal_1981to2020.tif")), evt_gp)
  tmin<- terra::mask(rast(paste0(in_dir, "/tmin_normal_1981to2020.tif")), evt_gp)
  vp<- terra::mask(rast(paste0(in_dir, "/vp_normal_1981to2020.tif")), evt_gp)
  vpd<- terra::mask(rast(paste0(in_dir, "/vpd_normal_1981to2020.tif")), evt_gp)
  
  
  # SAVE ----
  
  # Save the EVT GP Remap table for the zone 
  write.csv(evg_remap_table, paste0(out_dir,"/evt_gp_remap.csv"), row.names = F, quote=F)

  
  # Save all of the final rasters for the zone ----
  
  # Landfire
  writeRaster(aspect, paste0(out_dir, "/aspect.tif"), datatype = "FLT4S")
  writeRaster(easting, paste0(out_dir, "/easting.tif"), datatype = "FLT4S")
  writeRaster(elevation, paste0(out_dir, "/elevation.tif"), datatype = "FLT4S")
  writeRaster(evc, paste0(out_dir, "/evc.tif"), datatype = "FLT4S")
  writeRaster(evh, paste0(out_dir, "/evh.tif"), datatype = "FLT4S")
  writeRaster(evt_gp, paste0(out_dir, "/evt_gp.tif"), datatype = "FLT4S")
  writeRaster(northing, paste0(out_dir, "/northing.tif"), datatype = "FLT4S")
  writeRaster(slope, paste0(out_dir, "/slope.tif"), datatype = "FLT4S")
  
  # Daymet
  writeRaster(prcp, paste0(out_dir, "/prcp.tif"), datatype = "FLT4S")
  writeRaster(srad, paste0(out_dir, "/srad.tif"), datatype = "FLT4S")
  writeRaster(swe, paste0(out_dir, "/swe.tif"), datatype = "FLT4S")
  writeRaster(tmax, paste0(out_dir, "/tmax.tif"), datatype = "FLT4S")
  writeRaster(tmin, paste0(out_dir, "/tmin.tif"), datatype = "FLT4S")
  writeRaster(vp, paste0(out_dir, "/vp.tif"), datatype = "FLT4S")
  writeRaster(vpd, paste0(out_dir, "/vpd.tif"), datatype = "FLT4S")

  # Clear garbage
  gc()
  
}

