# Set up
setwd(this.path::here())

library(terra)
library(dplyr)

data_dir<-"//166.2.126.25/TreeMap/01_Data/"


# Load a landfire raster as a template to match
lf_raster<- rast(glue::glue("{data_dir}/02_Landfire/LF_220/EVC/LF2022_EVC_220_CONUS/LF2022_EVC_220_CONUS/Tif/LC22_EVC/220.tif"))

# Load the landfire zones 
lf_zones<- vect(glue::glue("{dir}/LF_zones/Landfire_zones/refreshGeoAreas_041210.shp"))

# Reproject the landfire zones to match the lf raster
lf_zones<- terra::project(lf_zones, lf_raster)
lf_zone_nums<- sort(lf_zones$ZONE_NUM)


# Load all LandFire rasters ----
evc<- rast(glue::glue("{data_dir}/02_Landfire/LF_220/EVC/LF2022_EVC_220_CONUS/LF2022_EVC_220_CONUS/Tif/LC22_EVC/220.tif"))
evh<- rast(glue::glue("{data_dir}/02_Landfire/LF_220/EVH/LF2022_EVH_220_CONUS/LF2022_EVH_220_CONUS/Tif/LC22_EVH/220.tif"))
evt<- rast(glue::glue("{data_dir}/02_Landfire/LF_220/EVT/LF2022_EVT_220_CONUS/LF2022_EVT_220_CONUS/Tif/LC22_EVT/220.tif"))
aspect<- rast("../landfire/topographic/LF2020_Asp_220_CONUS/LF2020_Asp_220_CONUS/Tif/LC20_Asp_220.tif")
elevation<- rast("../landfire/topographic/LF2020_Elev_220_CONUS/LF2020_Elev_220_CONUS/Tif/LC20_Elev_220.tif")
slope<- rast("../landfire/topographic/LF2020_SlpD_220_CONUS/LF2020_SlpD_220_CONUS/Tif/LC20_SlpD_220.tif")

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

# EVT_GPs that are reclassed to NA
# because there are only a few of these evt_gps
evt_gps_na <- c(
  13,
  14,
  15,
  26,
  60,
  730
)

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



# Loop through landfire zones, creating a folder for masked LF data ----

# First create output directory 
dir.create("../landfire_data_by_zone/")


for (i in lf_zone_nums){
  
  lf_zone<- lf_zones[lf_zones$ZONE_NUM == i,]
  
  # Make a directory for saving landfire data for the zone
  ifelse(i<10, 
         dir.create(paste0("../landfire_data_by_zone/z0",i)),
         dir.create(paste0("../landfire_data_by_zone/z",i)))
  
  
  # First crop and mask EVC to the landfire zones, then reclassify by the forest codes
  #
  evc_final<- terra::classify(terra::mask(terra::crop(evc, lf_zone),
                                          mask = lf_zone),
                              evc_forest_codes_mat, right = NA)
  
  # For all other layers, crop and mask landfire data to the zone, then mask to EVC (which is also the tree mask) ----
  aspect_final<- terra::mask(terra::mask(terra::crop(aspect, lf_zone),
                                         mask = lf_zone),
                             mask = evc_final)
  
  #
  elevation_final<- terra::mask(terra::mask(terra::crop(elevation, lf_zone),
                                            mask = lf_zone),
                                mask = evc_final)
  #
  slope_final<- terra::mask(terra::mask(terra::crop(slope, lf_zone),
                                        mask = lf_zone),
                            mask = evc_final)
  
  #
  evh_final<- terra::mask(terra::classify(terra::mask(terra::crop(evh, lf_zone),
                                                      mask = lf_zone),
                                          evh_class_mat, right = NA),
                          mask = evc_final)
  
  # EVT Processing ----
  
  # Mask EVT to zone, then to the tree mask (EVC)
  evt_masked<- terra::mask(terra::mask(terra::crop(evt, lf_zone),
                                       mask = lf_zone),
                           mask = evc_final)
  
  # Prepare EVT_GP layer and remap table 
  evt_gp <- terra::classify(terra::classify(evt_masked, evt_levels),
                            cbind(evt_gps_na, NA))
  
  
  # Create the EVT GP Remap table
  evt_gp_list <- unique(evt_gp)
  evg_remap_table <- data.frame(EVT_GP = evt_gp_list, 
                                EVT_GP_remap = seq(1:nrow(evt_gp_list)))
  
  # Make EVT_GP remap raster
  evt_final <- terra::classify(evt_gp, evg_remap_table) 
  
  
  # Save the final rasters for each zone ----
  if(i<10) {
    writeRaster(aspect_final, paste0("../landfire_data_by_zone/z0",i,"/aspect.tif"), datatype = "FLT4S")
  } else {writeRaster(aspect_final, paste0("../landfire_data_by_zone/z",i,"/aspect.tif"), datatype = "FLT4S")
  }
  
  if(i<10) {
    writeRaster(elevation_final, paste0("../landfire_data_by_zone/z0",i,"/elevation.tif"), datatype = "FLT4S")
  } else {writeRaster(elevation_final, paste0("../landfire_data_by_zone/z",i,"/elevation.tif"), datatype = "FLT4S")
  }
  
  if(i<10) {
    writeRaster(slope_final, paste0("../landfire_data_by_zone/z0",i,"/slope.tif"), datatype = "FLT4S")
  } else {writeRaster(slope_final, paste0("../landfire_data_by_zone/z",i,"/slope.tif"), datatype = "FLT4S")
  }
  
  if(i<10) {
    writeRaster(evc_final, paste0("../landfire_data_by_zone/z0",i,"/evc.tif"), datatype = "FLT4S")
  } else {writeRaster(evc_final, paste0("../landfire_data_by_zone/z",i,"/evc.tif"), datatype = "FLT4S")
  }
  
  if(i<10) {
    writeRaster(evh_final, paste0("../landfire_data_by_zone/z0",i,"/evh.tif"), datatype = "FLT4S")
  } else {writeRaster(evh_final, paste0("../landfire_data_by_zone/z",i,"/evh.tif"), datatype = "FLT4S")
  }
  
  if(i<10) {
    writeRaster(evt_final, paste0("../landfire_data_by_zone/z0",i,"/evt.tif"), datatype = "FLT4S")
  } else {writeRaster(evt_final, paste0("../landfire_data_by_zone/z",i,"/evt.tif"), datatype = "FLT4S")
  }
  
  
  # Save the EVT GP Remap table
  
  if(i<10) {
    write.csv(evg_remap_table, paste0("../landfire_data_by_zone/z0",i,"/evg_remap.csv"))
  } else {
    write.csv(evg_remap_table, paste0("../landfire_data_by_zone/z",i,"/evg_remap.csv"))
  }
  
  gc()
  
}

