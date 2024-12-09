### This script is used to set inputs for all steps in the target data prep process
## To ensure that all scripts refer to the same input and output products

# Written by Lila Leatherman (lila.leatherman@usda.gov)

# Last updated: 7/2/24

# TO DO:
# - add a switch for LF only disturbance vs LCMS+LF disturbance ? 

###########################################################################
# Set inputs
###########################################################################

# General inputs - specific to each project
#-----------------------------------------------#

#year <- year_input
year <- 2016

#project name
project_name <- glue::glue("{year}_Production")
# project_name <- "DistLayerPrep_GTAC_test" #for testing

# project area - for 2020 version, leave blank for CONUS
# other options are "AK" and "HI"

# target data version
target_data_version <- glue::glue("v{year}_GTAC")

# set year range
start_year <- 1999
end_year <- year

# set current modeling year (for years since disturbance)
model_year <- as.integer(end_year)

#build year list
year_list <- seq(start_year, end_year, 1)

# SET a 'tmp' directory to hold temporary files
# tmp_dir <- "D:/tmp" # directory specified in "setup_dirs.R" script

# set landfire version

# TOPO
landfire_version_topo <- 220
landfire_year_topo <- 2020

# VEG
LFveg_yearDict <- list("2016" = 200, 
                       "2020" = 220, 
                       "2022" = 230)

landfire_year_veg <- year
landfire_version_veg <- LFveg_yearDict[[as.character(landfire_year_veg)]]

# path to zone metadata
# relative to home_dir
zone_metadata_path <- "/01_Data/02_Landfire/metadata/LF_zones_all_byArea.csv"

# Data Inputs - less likely to change
#---------------------------------------------------------#

# data directory - where source data are located
data_dir <- glue::glue('{home_dir}/01_Data/')

# set path to landfire rasters 
#landfire_dir <- glue::glue('{data_dir}02_Landfire/LF_{landfire_version}/')
landfire_veg_dir <- glue::glue('{data_dir}02_Landfire/LF_{landfire_version_veg}/Vegetation/')
landfire_topo_dir <- glue::glue('{data_dir}02_Landfire/LF_{landfire_version_topo}/Topo/')
landfire_disturbance_dir_1999_2014 <- glue::glue('{data_dir}02_Landfire/LF_USDIST/')
landfire_disturbance_dir_2015_2020 <- glue::glue('{data_dir}02_Landfire/LF_220/Disturbance/')
landfire_disturbance_dir_2021_2022 <- glue::glue('{data_dir}02_Landfire/LF_230/Disturbance/')

# set path to landfire vector data
lf_zones_path <- glue::glue('{data_dir}/02_Landfire/LF_zones/Landfire_zones/refreshGeoAreas_041210.shp')

# set dir to lcms raw probability rasters
lcms_dir <- glue::glue('{data_dir}05_LCMS/01_Threshold_Testing/01_Raw/02_Raw_Probabilities/')

# Paths to specific Landfire rasters - not disturbance
# evc_path <- glue::glue('{landfire_veg_dir}/EVC/LF{landfire_year_veg}_EVC_{landfire_version_veg}_CONUS/Tif/LC{substr(landfire_year_veg, 3,4)}_EVC_{landfire_version_veg}.tif')
# evh_path <- glue::glue('{landfire_veg_dir}/EVH/LF{landfire_year_veg}_EVH_{landfire_version_veg}_CONUS/Tif/LC{substr(landfire_year_veg, 3,4)}_EVH_{landfire_version_veg}.tif')
# evt_path <- glue::glue('{landfire_veg_dir}/EVT/LF{landfire_year_veg}_EVT_{landfire_version_veg}_CONUS/Tif/LC{substr(landfire_year_veg, 3,4)}_EVT_{landfire_version_veg}.tif')

#elev_path <- glue::glue('{landfire_topo_dir}/Elev/LF{landfire_year_topo}_Elev_{landfire_version_topo}_CONUS/Tif/LC{substr(landfire_year_topo, 3,4)}_Elev_{landfire_version_topo}.tif')
# slopeP_path <- glue::glue('{landfire_topo_dir}/SlpP/LF{landfire_year_topo}_SlpP_{landfire_version_topo}_CONUS/Tif/LC{substr(landfire_year_topo, 3,4)}_SlpP_{landfire_version_topo}.tif')
# slopeD_path <- glue::glue('{landfire_topo_dir}/SlpD/LF{landfire_year_topo}_SlpD_{landfire_version_topo}_CONUS/Tif/LC{substr(landfire_year_topo, 3,4)}_SlpD_{landfire_version_topo}.tif')
# asp_path <- glue::glue('{landfire_topo_dir}/Asp/LF{landfire_year_topo}_Asp_{landfire_version_topo}_CONUS/Tif/LC{substr(landfire_year_topo, 3,4)}_Asp_{landfire_version_topo}.tif')


# Load various crs
#--------------------------------#
# load lcms projections
lcms_crs <- terra::crs(glue::glue('{data_dir}05_LCMS/00_Supporting/lcms_crs_albers.prj'))

# load treemap projection
tm16_crs <- terra::crs(glue::glue("{data_dir}01_TreeMap2016_RDA/04_CRS/TreeMap2016_crs.prj"))

# lf200
lf200_crs <- terra::crs(glue::glue("{home_dir}/01_Data/02_Landfire/LF_200/CRS/LF_200_crs.prj"))

# lf220
lf220_crs <- terra::crs(glue::glue("{home_dir}/01_Data/02_Landfire/LF_220/CRS/LF_220_crs.prj"))

# lf230
lf230_crs <- terra::crs(glue::glue("{home_dir}/01_Data/02_Landfire/LF_230/CRS/LF_230_crs.prj"))

# determine which CRS will actually be used - 
#   - specifically, used to project zone for cropping
#   - used to define projection for all historic landfire disturbance

# lf_crs_version <- lf200_crs
# 
# # load output crs
# lf_output_crs <- lf_crs_version

# Export data directories
#----------------------------------------------------#

# where version-specific inputs and outputs will live
project_dir <- glue::glue('{home_dir}/03_Outputs/07_Projects/{project_name}/')

# Directory where target data lives
target_dir <- glue::glue("{home_dir}/03_Outputs/05_Target_Rasters/{target_data_version}/")

# Directory where EVT_GP remap table will be located
evt_gp_remap_table_path <- target_dir

# Make RDS of input parameters used
#---------------------------------------------------------#

# Export to scripts folder for easy access
# over-writes by default
save(list = ls(), file = glue::glue('{this.path::this.dir()}/params/{target_data_version}_target_data_inputs.RDS'))
