# Script summary
# Written by Scott Zimmer (szimmer@usda.gov) and Lila Leatherman (lila.leatherman@usda.gov)

# Last Updated: 
# 4/13/2023

#####################################################################################
# Script inputs
#####################################################################################

# define year
year_input <- "2023"

# define project area
study_area <- "CONUS"

# landfire version - formatted as "LF_{###}"
LF_version <- 'LF_240' 


################################################################
# Load Library
################################################################

# Load TreeMap script library
#--------------------------------------------------#

# necessary to load this before calling any of the home_dir, fia_dir, or data_dir objects

# load library 
this_proj = this.path::this.proj()
lib_path = glue::glue("{this_proj}/gtac_production_scripts/00_Library/treeMapLib.R")
source(lib_path)

##############################################

# Load project inputs for target data
project_input_script = glue::glue("{this_proj}/gtac_production_scripts/01_Target_Data_Prep/02_Disturbance_data_prep/00a_project_inputs_for_targetdata.R")

zone_input_script = glue::glue("{this_proj}/gtac_production_scripts/01_Target_Data_Prep/02_Disturbance_data_prep/00b_zone_inputs_for_targetdata.R")

# run project input script now; save zone input script for later
source(project_input_script)


#####################################################


# Load a landfire raster as a template to match
lf_raster<- terra::rast(evc_path)

# Load the landfire zones 
lf_zones <- terra::vect(lf_zones_path)

# Reproject the landfire zones to match the lf raster
lf_zones<- terra::project(lf_zones, lf_raster)
lf_zone_nums<- sort(lf_zones$ZONE_NUM)


# Load all LandFire rasters ----
evc<- terra::rast(evc_path)
evh<- terra::rast(evh_path)
evt<- terra::rast(evt_path)
aspect<- terra::rast(asp_path)
elevation<- terra::rast(elev_path)
slope<- terra::rast(slopeD_path)

##########################################################

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
if(!file.exists(!target_dir)){
  dir.create(target_dir)
}


for (zone_input in lf_zone_nums){
  
  zone_input = 1
  
  # run zone setup script
  source(zone_input_script)
  
  lf_zone<- lf_zones[lf_zones$ZONE_NUM == i,]
  
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
  
  # For topo layers, crop and mask to zone
  aspect_zone<- terra::crop(aspect, evc_zone, mask = TRUE)
  elevation_zone<- terra::crop(elevation, evc_zone, mask = TRUE)
  slope_zone<- terra::crop(slope, evc_zone, mask = TRUE)
  
  # Calculate northing and easting
  northing_zone <- terra::app(aspect_zone, function(i) cos((pi/180)*i))
  easting_zone <- terra::app(aspect_zone, function(i) sin((pi/180)*i))
  
  # Correct an issue with northing and easting. Where aspect = -1, these layers should be 0. So mask aspect = -1 pixels to 0 in easting and northing
  northing_zone <- mask(northing_zone, aspect_zone, maskvalues=-1, updatevalue=0)
  easting_zone <- mask(easting_zone, aspect_zone, maskvalues=-1, updatevalue=0)
  
  
  # Save the final rasters for the zone ----
  writeRaster(evc_zone, glue::glue("{target_dir_z}/evc.tif"), datatype = "FLT4S")
  writeRaster(evh_zone, glue::glue("{target_dir_z}/evh.tif"), datatype = "FLT4S")
  writeRaster(evt_gp_zone, glue::glue("{target_dir_z}/evt_gp.tif"), datatype = "FLT4S")
  #
  writeRaster(aspect_zone, glue::glue("{target_dir_z}/aspect.tif"), datatype = "FLT4S")
  writeRaster(elevation_zone, glue::glue("{target_dir_z}/elevation.tif"), datatype = "FLT4S")
  writeRaster(slope_zone, glue::glue("{target_dir_z}/slope.tif"), datatype = "FLT4S")
  #
  writeRaster(northing_zone, glue::glue("{target_dir_z}/northing.tif"), datatype = "FLT4S")
  writeRaster(easting_zone, glue::glue("{target_dir_z}/easting.tif"), datatype = "FLT4S")
  
  
  # Clear garbage
  gc()

  gc()

}
