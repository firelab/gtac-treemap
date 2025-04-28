### This script is used to set inputs for all steps in the target data prep process
## To ensure that all scripts refer to the same input and output products

# Written by Lila Leatherman (lila.leatherman@usda.gov)

# Last updated: 4/14/25

# TO DO:
# - add a switch for LF only disturbance vs LCMS+LF disturbance ? 

###########################################################################
# Set inputs
###########################################################################

# General inputs - specific to each project
#-----------------------------------------------#

year <- year_input

#project name
project_name <- glue::glue("{year}_Production")
# project_name <- "DistLayerPrep_GTAC_test" #for testing

# target data version
target_data_version <- glue::glue("v{year}")

# set year range
start_year <- 1999
end_year <- year

# # default crs for output products - for CONUS
# #options include: "lcms_crs", "lf200_crs", "lf220_crs", "lf230_crs", "tm16_crs"
default_crs_name <- "lf240_crs"

# set landfire version

# VEG
LFveg_yearDict <- list("2016" = 200,
                       "2020" = 220,
                       "2022" = 230,
                       "2023" = 240)

LF_year <- as.character(year)
LF_version <- LFveg_yearDict[[as.character(LF_year)]]

# set current modeling year (for years since disturbance)
model_year <- as.integer(end_year)

# Create abbreviations that will be used in making paths
year_short <- substr(year, 3, 4)

# Load TreeMap script library
#--------------------------------------------------#

# necessary to load this before calling any of the home_dir, fia_dir, or data_dir objects

# load library 
this_proj = this.path::this.proj()
lib_path = glue::glue("{this_proj}/gtac_production_scripts/00_Library/treeMapLib.R")
source(lib_path)


# Data Inputs - less likely to change
#---------------------------------------------------------#

#build year list
year_list <- seq(start_year, end_year, 1)

# data directory - where source data are located
data_dir <- glue::glue('{home_dir}/01_Data/')

# path to zone metadata
# relative to home_dir
zone_metadata_path <- glue::glue('{data_dir}02_Landfire/metadata/LF_zones_all_byStudyArea.csv')

# set path to landfire vector data
lf_zones_path_CONUS <- glue::glue('{data_dir}/02_Landfire/LF_zones/Landfire_zones/refreshGeoAreas_041210.shp')
lf_zones_path_AK <- glue::glue('{data_dir}/02_Landfire/LF_zones/Landfire_zones/alaska_mapzones.shp')
lf_zones_path_HI <- glue::glue('{data_dir}/02_Landfire/LF_zones/Landfire_zones/hawaii_mapzones.shp')

###################################################################################

# set path to landfire rasters 
lf_dist_dir <- glue::glue('{data_dir}02_Landfire/Annual_Disturbance/')
lf_veg_dir <- glue::glue("{data_dir}/02_Landfire/LF_{LF_version}/Vegetation/")
lf_topo_dir <- glue::glue("{data_dir}/02_Landfire/LF_220/Topo/")

# set dir to lcms raw probability rasters
lcms_dir <- glue::glue('{data_dir}05_LCMS/01_Threshold_Testing/01_Raw/02_Raw_Probabilities/')

###########################################################

# paths to specific landfire rasters

evc_path <- glue::glue("{lf_veg_dir}/EVC/LF{year}_EVC_{LF_version}_{study_area}/Tif/LC{year_short}_EVC_{LF_version}.tif")
evh_path <- glue::glue("{lf_veg_dir}/EVH/LF{year}_EVH_{LF_version}_{study_area}/Tif/LC{year_short}_EVH_{LF_version}.tif")
evt_path <- glue::glue("{lf_veg_dir}/EVT/LF{year}_EVT_{LF_version}_{study_area}/Tif/LC{year_short}_EVT_{LF_version}.tif")

elev_path <- glue::glue('{lf_topo_dir}/Elev/LF2020_Elev_220_CONUS/Tif/LC20_Elev_220.tif')
slopeP_path <- glue::glue('{lf_topo_dir}/SlpP/LF2020_SlpP_220_CONUS/Tif/LC20_SlpP_220.tif')
slopeD_path <- glue::glue('{lf_topo_dir}/SlpD/LF2020_SlpD_220_CONUS/Tif/LC20_SlpD_220.tif')
asp_path <- glue::glue('{lf_topo_dir}/Asp/LF2020_Asp_220_CONUS/Tif/LC20_Asp_220.tif')

###################################################################

# Read in the necessary zone-specific reclassifications
zonal_evt_gp_reclass_path <- glue::glue("{data_dir}/11_EVG/zonal_evt_gp_reclass_LF2020.csv") # currently this file is used for 2020,2022,and2023


# Load CRS
#----------------------------------------------------#

# load lcms projections
lcms_crs <- terra::crs(glue::glue("{data_dir}05_LCMS/00_Supporting/lcms_crs_albers.prj"))
# load treemap projections
tm16_crs <- terra::crs(glue::glue("{data_dir}01_TreeMap2016_RDA/04_CRS/TreeMap2016_crs.prj"))
lf200_crs <- terra::crs(glue::glue("{home_dir}/01_Data/02_Landfire/LF_200/CRS/LF_200_crs.prj"))
lf220_crs <- terra::crs(glue::glue("{home_dir}/01_Data/02_Landfire/LF_220/CRS/LF_220_crs.prj"))
lf230_crs <- terra::crs(glue::glue("{home_dir}/01_Data/02_Landfire/LF_230/CRS/LF_230_crs.prj"))
lf240_crs <- terra::crs(glue::glue("{home_dir}/01_Data/02_Landfire/LF_240/CRS/LF_240_crs.prj"))
ak_crs <- terra::crs(glue::glue("{home_dir}/01_Data/02_Landfire/LF_zones/Landfire_zones/alaska_mapzones.prj"))
hi_crs <- terra::crs(glue::glue("{home_dir}/01_Data/02_Landfire/LF_zones/Landfire_zones/hawaii_mapzones.prj"))
default_crs <- eval(parse(text = default_crs_name))


#########################################################################

#set paths to zones and projection, based on map area
if(study_area == "CONUS") {
  lf_zones_path = lf_zones_path_CONUS
  output_crs = default_crs
  file_pattern = "US"
} else if(study_area == "AK") {
  lf_zones_path = lf_zones_path_AK
  output_crs = ak_crs
  file_pattern = study_area
} else if(study_area == "HI") {
  lf_zones_path = lf_zones_path_HI
  output_crs = hi_crs
  file_pattern = study_area
} else {message("enter a valid field for 'study_area'. Options are: 'CONUS', 'HI', 'AK'")}



# Export data directories
#----------------------------------------------------#

# where version-specific inputs and outputs will live
project_dir <- glue::glue('{home_dir}/03_Outputs/07_Projects/{project_name}/')

# Directory where target data lives
target_dir_premask <- glue::glue("{home_dir}/03_Outputs/05_Target_Rasters/{target_data_version}/pre_mask/")

target_dir_postmask <- glue::glue("{home_dir}/03_Outputs/05_Target_Rasters/{target_data_version}/post_mask/")

# Directory where EVT_GP remap table will be located
evt_gp_remap_table_path <- target_dir_postmask

# Make RDS of input parameters used
#---------------------------------------------------------#

# Export to scripts folder for easy access
# over-writes by default
#save(list = ls(), file = glue::glue('{this.path::this.dir()}/params/{target_data_version}_target_data_inputs.RDS'))
