### This script is used to set inputs for all steps in the target data prep process
## To ensure that all scripts refer to the same input and output products

# Written by Lila Leatherman (lila.leatherman@usda.gov)

# Last updated: 12/9/24

# TO DO:
# - add a switch for LF only disturbance vs LCMS+LF disturbance ? 

###########################################################################
# Set inputs
###########################################################################

# General inputs - specific to each project
#-----------------------------------------------#

#year <- year_input
year <- 2020

#project name
project_name <- glue::glue("{year}_Production")
# project_name <- "DistLayerPrep_GTAC_test" #for testing

# target data version
target_data_version <- glue::glue("v{year}")

# set year range
start_year <- 1999
end_year <- year

# set current modeling year (for years since disturbance)
model_year <- as.integer(end_year)

# # default crs for output products - for CONUS
# #options include: "lcms_crs", "lf200_crs", "lf220_crs", "lf230_crs", "tm16_crs"
default_crs_name <- "lf230_crs"

# set landfire version

# # TOPO
# landfire_version_topo <- 220
# landfire_year_topo <- 2020
# 
# # VEG
# LFveg_yearDict <- list("2016" = 200, 
#                        "2020" = 220, 
#                        "2022" = 230,
#                        "2023" = 240)
# 
# landfire_year_veg <- year
# landfire_version_veg <- LFveg_yearDict[[as.character(landfire_year_veg)]]

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


# set path to landfire rasters 
landfire_disturbance_dir_1999_2014 <- glue::glue('{data_dir}02_Landfire/Historic_Disturbance/')
landfire_disturbance_dir_2015_2016 <- glue::glue('{data_dir}02_Landfire/LF_200/Disturbance/')
landfire_disturbance_dir_2017_2020 <- glue::glue('{data_dir}02_Landfire/LF_220/Disturbance/')
landfire_disturbance_dir_2021_2022 <- glue::glue('{data_dir}02_Landfire/LF_230/Disturbance/')

#landfire_veg_dir <- glue::glue('{data_dir}02_Landfire/LF_{landfire_version_veg}/Vegetation/')
#landfire_topo_dir <- glue::glue('{data_dir}02_Landfire/LF_{landfire_version_topo}/Topo/')


# set path to landfire vector data
lf_zones_path_CONUS <- glue::glue('{data_dir}/02_Landfire/LF_zones/Landfire_zones/refreshGeoAreas_041210.shp')
lf_zones_path_AK <- glue::glue('{data_dir}/02_Landfire/LF_zones/Landfire_zones/alaska_mapzones.shp')
lf_zones_path_HI <- glue::glue('{data_dir}/02_Landfire/LF_zones/Landfire_zones/hawaii_mapzones.shp')

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


# Load CRS
#----------------------------------------------------#

# load lcms projections
lcms_crs <- terra::crs(glue::glue("{data_dir}05_LCMS/00_Supporting/lcms_crs_albers.prj"))

# load treemap projection
tm16_crs <- terra::crs(glue::glue("{data_dir}01_TreeMap2016_RDA/04_CRS/TreeMap2016_crs.prj"))

# lf200
lf200_crs <- terra::crs(glue::glue("{home_dir}/01_Data/02_Landfire/LF_200/CRS/LF_200_crs.prj"))

# lf220
lf220_crs <- terra::crs(glue::glue("{home_dir}/01_Data/02_Landfire/LF_220/CRS/LF_220_crs.prj"))

# lf230
lf230_crs <- terra::crs(glue::glue("{home_dir}/01_Data/02_Landfire/LF_230/CRS/LF_230_crs.prj"))

#alaska
ak_crs <- terra::crs(glue::glue("{home_dir}/01_Data/02_Landfire/LF_zones/Landfire_zones/alaska_mapzones.prj"))

#hawaii
hi_crs <- terra::crs(glue::glue("{home_dir}/01_Data/02_Landfire/LF_zones/Landfire_zones/hawaii_mapzones.prj"))

# load output crs
default_crs <- eval(parse(text = default_crs_name))


# Export data directories
#----------------------------------------------------#

# where version-specific inputs and outputs will live
project_dir <- glue::glue('{home_dir}/03_Outputs/07_Projects/{project_name}/')

# Directory where target data lives
target_dir <- glue::glue("{home_dir}/03_Outputs/05_Target_Rasters/{target_data_version}/pre_mask/")

# Directory where EVT_GP remap table will be located
#evt_gp_remap_table_path <- target_dir

# Make RDS of input parameters used
#---------------------------------------------------------#

# Export to scripts folder for easy access
# over-writes by default
#save(list = ls(), file = glue::glue('{this.path::this.dir()}/params/{target_data_version}_target_data_inputs.RDS'))
