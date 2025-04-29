# Set up
setwd("\\166.2126.25")

library(terra)

# Load a landfire raster as a template to match
lf_raster<- rast("./01_Data/02_Landfire/LF_220/Vegetation/EVC/LF2022_EVC_220_CONUS/Tif/LC22_EVC_220.tif")

# Load the landfire zones 
lf_zones<- vect("./01_Data/02_Landfire/LF_zones/Landfire_zones/refreshGeoAreas_041210.shp")

# Reproject the landfire zones to match the lf raster
lf_zones<- terra::project(lf_zones, lf_raster)
lf_zone_nums<- sort(lf_zones$ZONE_NUM)


# Load all LandFire rasters ----
evc<- rast("./01_Data/02_Landfire/LF_220/Vegetation/EVC/LF2022_EVC_220_CONUS/Tif/LC22_EVC_220.tif")
evt<- rast("./01_Data/02_Landfire/LF_220/Vegetation/EVT/LF2022_EVT_220_CONUS/Tif/LC22_EVT_220.tif")

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

for (i in lf_zone_nums){
  
  lf_zone<- lf_zones[lf_zones$ZONE_NUM == i,]
  
  
  # First crop and mask EVC to the landfire zones, then reclassify by the forest codes
  #
  evc_final<- terra::classify(terra::mask(terra::crop(evc, lf_zone),
                                          mask = lf_zone),
                              evc_forest_codes_mat, right = NA)
  
  # EVT Processing ----
  
  # Mask EVT to zone, then to the tree mask (EVC)
  evt_masked<- terra::mask(terra::mask(terra::crop(evt, lf_zone),
                                       mask = lf_zone),
                           mask = evc_final)
  
  activeCat(evt_masked) <- 1
 
  # Update the EVT categories so only valid categories are still present
  cats_update<- cats(evt_masked)[[1]][cats(evt_masked)[[1]]$EVT_NAME %in% freq(evt_masked)[,2],]
  set.cats(x=evt_masked, value=cats_update)
  
  # Save out
  if(i<10) {
    writeRaster(evt_masked, paste0("./03_Outputs/05_Target_Rasters/v2020/landfire_data_by_zone/z0",i,"/raw_evt_name.tif"), datatype = "FLT4S")
  } else {writeRaster(evt_masked, paste0("./03_Outputs/05_Target_Rasters/v2020/landfire_data_by_zone/z",i,"/raw_evt_name.tif"), datatype = "FLT4S")
  }
  

}

