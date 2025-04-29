# Script summary
# For Landfire data to be used as target rasters to TreeMap: 
# - Loads Landfire data from local machine
#     - Layers include: EVC, EVH, EVT, slope, aspect, elevation
# - Runs over each Landfire zone in study area
#     - Crop EVC to zone and reclassify EVC to represent tree cover >10% - a preliminary forest mask
#     - Mask EVT_GP by EVC, classify desired EVT_GPs to NA, remap specific EVT_GPs in zones 
#     - Remap EVT_GP 
#     - Crop and mask remaining layers to zone using EVT-GP
#     - Reclassify EVT to EVT_GP, using the attribute table in the layer
#     - Calculate northing and easting from aspect
#     - Fix no-aspect issue with northing and easting
#     - Export .tifs for each layer (clipped to zone, masked using EVC) to "pre_mask" folder
# - Next step: 
#     - Confirm reference data prep
#     - Run "final masking" script, after confirming any EVT_GPs to reclass based on EVT_GPs present in the x-table

# Written by Scott Zimmer (szimmer@usda.gov) and Lila Leatherman (lila.leatherman@usda.gov)

# Last Updated: 
# 4/29/2025

#####################################################################################
# Script inputs
#####################################################################################

# define year
year_input <- "2023"

# define project area
study_area <- "CONUS"

# landfire version - formatted as "LF_{###}"
#LF_version <- 'LF_240' 


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
#project_input_script = glue::glue("{this_proj}/gtac_production_scripts/00_Inputs/02_Disturbance_data_prep/00a_project_inputs_for_targetdata.R")

#zone_input_script = glue::glue("{this_proj}/gtac_production_scripts/01_Target_Data_Prep/02_Disturbance_data_prep/00b_zone_inputs_for_targetdata.R")

# run project input script now; save zone input script for later
#source(project_input_script)

targetDataProjectInputs(year_input = year_input,
                        study_area = study_area)


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

# Settings for Landfire Rasters
activeCat(evc) <- 'Value'
activeCat(evh) <- 'Value'
activeCat(evt) <- 'EVT_GP'

##########################################################

# Set up variables to reclassify ----

# EVC codes that represent forest
# represented as range start, range end, desired end value
# Attribute table: https://landfire.gov/sites/default/files/DataDictionary/2024/LF24_EVCADD.pdf
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

# EVH classes to reclass
# data dictionary: https://landfire.gov/sites/default/files/DataDictionary/2024/LF24_EVHADD.pdf
evh_class_mat <- matrix(c(
  -9999,100,NA, # noData or non-tree land cover
  101,105,3, # 1-5m tree height: gets class = 3
  106,110,8, # 6-10m tree height: gets class = 8
  111,125,18, # 11-25m tree height: gets class = 18
  126,150,38, # 26-50m tree height: gets class = 38
  151,199, NA, # 51m - 99 m tree height: NA
  200,500, NA), 
  nrow = 7, ncol = 3, byrow = TRUE)

# get evt levels - to reclassify evt to evt_gp
evt_levels <- levels(evt)[[1]] %>%
  mutate(EVT_GP = as.numeric(EVT_GP))

# These EVT_GPs are reclassified to NA for all zones. These are Developed and Agricultural
evt_gps_na <- c(
  13,
  14,
  15,
  26,
  60,
  730
)

# Load table of zone-specific EVT GP reclasses
zonal_evt_gp_reclass <- read.csv(zonal_evt_gp_reclass_path)

# Loop through landfire zones, creating a folder for masked LF data ----

# # First create output directory 
# if(!file.exists(target_dir_mask)){
#   dir.create(target_dir_mask)
# }


for (zone_input in lf_zone_nums){
  
  #zone_input = 1
  
  print(glue::glue('working on target veg and topo data for zone {zone_input}'))
  
  # # run zone setup script
  # source(zone_input_script)
  
  targetDataZonalInputs(zone_input)
  
  lf_zone<- lf_zones[lf_zones$ZONE_NUM == zone_input,]
  
  #######################################################################
  # Crop and mask layers to each landfire zone---
  ########################################################################
  
  message("cropping, masking, and pre-processing layers")
  
  # EVC and EVT GP prep
  ########################################################################
  
  # Crop and Reclassify EVC to forested areas
  evc_zone<- terra::classify(terra::crop(evc, lf_zone, mask = TRUE),
                             evc_forest_codes_mat, right = NA)
  

  # Crop EVT, mask by EVC, Reclassify to EVT_GP, and Reclassify some gps to NA
  evt_gp_zone <- terra::classify(
    terra::classify(
      terra::crop(evt, evc_zone, mask = TRUE),
      evt_levels),
    cbind(evt_gps_na, NA))
  
  # Then reclassify further groups if necessary in this zone
  
  # Identify if the zone has any EVT GPs to reclassify, then reclassify
  if(zone_input %in%  unique(zonal_evt_gp_reclass$zone)){
    zonal_evt_gp_reclass_sub<- zonal_evt_gp_reclass[zonal_evt_gp_reclass$zone == zone_input,]  # limit the reclassify df to that zone
    evt_gp_zone<- terra::classify(evt_gp_zone, zonal_evt_gp_reclass_sub[,c(2:3)]) # reclassify the remaining relevant codes for the zone
  }
  
  
  # Create the EVT GP Remap table to track how EVT GP gets remapped to 1,2,3,4,etc
  evt_gp_list <- unique(evt_gp_zone)
  evt_gp_remap_table <- data.frame(EVT_GP = evt_gp_list, 
                                   EVT_GP_remap = seq(1:nrow(evt_gp_list)))
  
  # Make EVT_GP remap raster
  evt_gp_remap <- terra::classify(evt_gp_zone, evt_gp_remap_table) 
  
  # Save the EVT GP Remap table for the zone 
  write.csv(evt_gp_remap_table, glue::glue('{evt_gp_remap_table_path}/evt_gp_remap_table.csv'), row.names=F, quote=F)
  
  # Apply new mask to EVC
  evc_zone<- terra::classify(terra::crop(evc, evt_gp_remap, mask = TRUE),
                             evc_forest_codes_mat, right = NA)
  
  # Masking subsequent layers with EVT_GP mask
  ############################################################################
  
  # Crop and Reclassify EVH to classes
  evh_zone<- terra::classify(terra::crop(evh, evt_gp_remap, mask = TRUE),
                             evh_class_mat, right = NA)
  
  
  # Crop and mask EVT, set as EVT_NAME
  evt_name_zone<- terra::crop(evt, evt_gp_remap, mask = TRUE)
  activeCat(evt_name_zone) <- 'EVT_NAME'
  
  # Update the EVT categories so only valid categories are still present
  cats_update<- cats(evt_name_zone)[[1]][cats(evt_name_zone)[[1]]$EVT_NAME %in% freq(evt_name_zone)[,2],]
  set.cats(x=evt_name_zone, value=cats_update)
  
  # For topo layers, crop and mask to zone
  aspect_zone<- terra::crop(aspect, evt_gp_remap, mask = TRUE)
  elevation_zone<- terra::crop(elevation, evt_gp_remap, mask = TRUE)
  slope_zone<- terra::crop(slope, evt_gp_remap, mask = TRUE)
  
  # Calculate northing and easting
  northing_zone <- terra::app(aspect_zone, function(i) cos((pi/180)*i))
  easting_zone <- terra::app(aspect_zone, function(i) sin((pi/180)*i))
  
  # Correct an issue with northing and easting. Where aspect = -1, these layers should be 0. So mask aspect = -1 pixels to 0 in easting and northing
  northing_zone <- mask(northing_zone, aspect_zone, maskvalues=-1, updatevalue=0)
  easting_zone <- mask(easting_zone, aspect_zone, maskvalues=-1, updatevalue=0)
  
  
  # Save the intermediate rasters for the zone ----
  message("exporting layers")
  writeRaster(evc_zone, glue::glue("{target_dir_mask_z}/evc.tif"), datatype = "FLT4S",  overwrite = TRUE)
  writeRaster(evh_zone, glue::glue("{target_dir_mask_z}/evh.tif"), datatype = "FLT4S",  overwrite = TRUE)
  writeRaster(evt_gp_zone, glue::glue("{target_dir_mask_z}/evt_gp.tif"), datatype = "FLT4S",  overwrite = TRUE)
  writeRaster(evt_gp_remap, glue::glue("{target_dir_mask_z}/evt_gp_remap.tif"), datatype = "FLT4S",  overwrite = TRUE)
  writeRaster(evt_name_zone, glue::glue("{target_dir_mask_z}/evt_name.tif"), datatype = "FLT4S",  overwrite = TRUE)
  writeRaster(aspect_zone, glue::glue("{target_dir_mask_z}/aspect.tif"), datatype = "FLT4S",  overwrite = TRUE)
  writeRaster(elevation_zone, glue::glue("{target_dir_mask_z}/elevation.tif"), datatype = "FLT4S",  overwrite = TRUE)
  writeRaster(slope_zone, glue::glue("{target_dir_mask_z}/slope.tif"), datatype = "FLT4S",  overwrite = TRUE)
  writeRaster(northing_zone, glue::glue("{target_dir_mask_z}/northing.tif"), datatype = "FLT4S",  overwrite = TRUE)
  writeRaster(easting_zone, glue::glue("{target_dir_mask_z}/easting.tif"), datatype = "FLT4S",  overwrite = TRUE)
  
  
  # Clear garbage
  gc()


}

