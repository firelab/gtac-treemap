### This script is used to set inputs for all steps in the target data prep process
## To ensure that all scripts refer to the same input and output products

# Written by Lila Leatherman (lila.leatherman@usda.gov)

# Last updated: 4/1/24

# TO DO:
# - address inconsistencies in EVT vs Topo Landfire Paths
# - add a switch for LCMS+LF disturbance vs LF only disturbance? 
# - export file showing input data and parameters: 
# - model year, years of data, versions of input data used, lcms thresholds

###########################################################################
# Set inputs
###########################################################################

# Inputs for testing
#-----------------------------------------------#

# supply path to a shapefile to use as a subset, or NA
#aoi_path <- "//166.2.126.25/TreeMap/01_Data/03_AOIs/UT_Uintas_rect_NAD1983.shp"
#aoi_name <- "UT_Uintas_rect_"
aoi_path <- NA
aoi_name <- NA

# Standard inputs
#-----------------------------------------------#

# Zone list
#zone_list <- c(16)

zone_num <- 16

#project name
project_name <- "2016_GTAC_Test"

# target data version
target_data_version <- "v2016_GTAC"

# reference data version
ref_data_version <- "v2016_GTAC"

# set year range
start_year <- 1999
end_year <- 2016

# set current modeling year (for years since disturbance)
model_year <- end_year

#build year list
year_list <- seq(start_year, end_year, 1)

# Input data directories
#-----------------------------------------#

# Initialize home dir
#-----------------------------------------------#
# Id where THIS script is located
this.path <- this.path::this.path()

# get path to input script
spl <- stringr::str_split(this.path, "/")[[1]]
setup_dirs.path <- paste( c(spl[c(1:(length(spl)-2))],
                              "00_Library/setup_dirs.R" ),
                            collapse = "/")

source(setup_dirs.path)


# data directory - where source data are located
data_dir <- glue::glue('{home_dir}/01_Data/')

# set landfire version 
landfire_version_veg <- 200
landfire_year_veg <- 2016
landfire_version_disturbance <- 220
landfire_year_disturbance <- 2020
landfire_version_topo <- 220
landfire_year_topo <- 2020

# Constructed Inputs (less likely to change)
#----------------------------------------------------------#

# set path to landfire rasters 
#landfire_dir <- glue::glue('{data_dir}02_Landfire/LF_{landfire_version}/')
landfire_veg_dir <- glue::glue('{data_dir}02_Landfire/LF_{landfire_version_veg}/Vegetation/')
landfire_topo_dir <- glue::glue('{data_dir}02_Landfire/LF_{landfire_version_topo}/Topo/')
landfire_disturbance_dir <- glue::glue('{data_dir}02_Landfire/LF_{landfire_version_disturbance}/Disturbance/')

# set path to landfire vector data
lf_zones_path <- glue::glue('{data_dir}/02_Landfire/LF_zones/Landfire_zones/refreshGeoAreas_041210.shp')

# set dir to lcms raw probability rasters
lcms_dir <- glue::glue('{data_dir}05_LCMS/01_Threshold_Testing/01_Raw/02_Raw_Probabilities/')

# set projection used for processing lcms rasters
lcms_proj <- glue::glue('{data_dir}05_LCMS/00_Supporting/lcms_crs_albers.prj')

# path to projection used for processing landfire rasters
landfire_proj <- glue::glue('{data_dir}02_Landfire/landfire_crs.prj')

# path to desired projection for end outputs (tm = TreeMap)
tm_proj <- glue::glue("{data_dir}01_TreeMap2016_RDA/04_CRS/TreeMap2016_crs.prj")

# Paths to specific Landfire rasters - not disturbance
evc_path <- glue::glue('{landfire_veg_dir}/EVC/LF{landfire_year_veg}_EVC_{landfire_version_veg}_CONUS/Tif/LC{substr(landfire_year_veg, 3,4)}_EVC_{landfire_version_veg}.tif')
evh_path <- glue::glue('{landfire_veg_dir}/EVH/LF{landfire_year_veg}_EVH_{landfire_version_veg}_CONUS/Tif/LC{substr(landfire_year_veg, 3,4)}_EVH_{landfire_version_veg}.tif')
evt_path <- glue::glue('{landfire_veg_dir}/EVT/LF{landfire_year_veg}_EVT_{landfire_version_veg}_CONUS/Tif/LC{substr(landfire_year_veg, 3,4)}_EVT_{landfire_version_veg}.tif')

elev_path <- glue::glue('{landfire_topo_dir}/LF{landfire_year_topo}_Elev_{landfire_version_topo}_CONUS/Tif/LC{substr(landfire_year_topo, 3,4)}_Elev_{landfire_version_topo}.tif')
slopeP_path <- glue::glue('{landfire_topo_dir}/LF{landfire_year_topo}_SlpP_{landfire_version_topo}_CONUS/Tif/LC{substr(landfire_year_topo, 3,4)}_SlpP_{landfire_version_topo}.tif')
slopeD_path <- glue::glue('{landfire_topo_dir}/LF{landfire_year_topo}_SlpD_{landfire_version_topo}_CONUS/Tif/LC{substr(landfire_year_topo, 3,4)}_SlpD_{landfire_version_topo}.tif')
asp_path <- glue::glue('{landfire_topo_dir}/LF{landfire_year_topo}_Asp_{landfire_version_topo}_CONUS/Tif/LC{substr(landfire_year_topo, 3,4)}_Asp_{landfire_version_topo}.tif')

# set dir for input biophys rasters
biophys_dir <- glue::glue('{data_dir}02_Landfire/BioPhys/')

# Export data directories
#----------------------------------------------------#

# where version-specific inputs and outputs will live
project_dir <- glue::glue('{home_dir}/03_Outputs/99_Projects/{project_name}/')

# Directory where target data lives
target_dir <- glue::glue("{home_dir}/03_Outputs/05_Target_Rasters/{target_data_version}/")

# Directory where EVT_GP remap table will be located
evt_gp_remap_table_path <- target_dir

# Prep zone
#-----------------------------------------#

#set zone identifiers
cur.zone <- glue::glue('z{zone_num}')
cur.zone.zero <- if(zone_num < 10) {
  glue::glue('z0{zone_num}') } else {
    cur.zone
  }

# Update dirs with zone
# -----------------------------------------#

# Set folder paths
target_dir_z = glue::glue('{target_dir}/{cur.zone.zero}/')

# update biophys path - biophys layers stored by zone
biophys_dir_z <- glue::glue('{biophys_dir}/{cur.zone.zero}/')


# set aoi_name field if it doesn't already exist via aoi subset
if(is.na(aoi_name)) {
  aoi_name <- ""
}

# Export data paths
#---------------------------------------------------------------#
# create output file names
landfire_fire_years_outpath <- glue::glue('{target_dir_z}/00_prelim_dist/{start_year}_{end_year}_{cur.zone.zero}_{aoi_name}LandfireDist_Fire_Years.tif')
landfire_fire_binary_outpath <- glue::glue('{target_dir_z}/00_prelim_dist/{start_year}_{end_year}_{cur.zone.zero}_{aoi_name}LandfireDist_Fire_Binary.tif')

landfire_ind_years_outpath <- glue::glue('{target_dir_z}/00_prelim_dist/{start_year}_{end_year}_{cur.zone.zero}_{aoi_name}LandfireDist_InsectDisease_Years.tif')
landfire_ind_binary_outpath <- glue::glue('{target_dir_z}/00_prelim_dist/{start_year}_{end_year}_{cur.zone.zero}_{aoi_name}LandfireDist_InsectDisease_Binary.tif')

lcms_slowloss_years_outpath <- glue::glue('{target_dir_z}/00_prelim_dist/{start_year}_{end_year}_{cur.zone.zero}_{aoi_name}LCMSDist_SlowLoss_Years.tif')
lcms_slowloss_binary_outpath <- glue::glue('{target_dir_z}/00_prelim_dist/{start_year}_{end_year}_{cur.zone.zero}_{aoi_name}LCMSDist_SlowLoss_Binary.tif')

lf_disturb_code_outpath <- glue::glue('{target_dir_z}/01_final/disturb_code_LF.tif')
lf_disturb_year_outpath <- glue::glue('{target_dir_z}/01_final/disturb_year_LF.tif')

lcms_disturb_code_outpath <- glue::glue('{target_dir_z}/01_final/disturb_code_LFLCMS.tif')
lcms_disturb_year_outpath <- glue::glue('{target_dir_z}/01_final/disturb_year_LFLCMS.tif')

# Input parameters for LCMS Disturbance
#-----------------------------------------------------------#

# Set variables
LCMS_NAvalue <- -32768

# set threshold for probability of slow loss from LCMS
slow_loss_thresh <- 14 # default value for LCMS processing: 14

# set probability thresholds for change
LCMS_change_thresholds <- c(29, # fast loss; default = 29
                       slow_loss_thresh, # slow loss; default = 14
                       20 # gain; default = 20
                       )

##########################################
# SETUP
######################

# Load TreeMap script library
#--------------------------------------------------#

# Set inputs - from input script
# Id where script is located
this.path <- this.path::this.path()

# get path to input script
spl <- stringr::str_split(this.path, "/")[[1]]
input_script.path <- paste( c(spl[c(1:(length(spl)-2))],
                              "00_Library/treemapLib.R" ),
                            collapse = "/")

source(input_script.path)


# Temp directories 
#----------------------------------#

# set tmp directory
tmp_dir <- "D:/tmp"

# check if tmp directory exists 
if (file.exists(tmp_dir)){
  
} else {
  # create a new sub directory inside the main path
  dir.create(tmp_dir, recursive = TRUE)
  
}

# create tmp dir folder for LCMS tiles
if (!file.exists(glue::glue('{tmp_dir}/lcms/'))) {
  dir.create(glue::glue('{tmp_dir}/lcms/'))
}

# create tmp dir folder for LF tiles
if (!file.exists(glue::glue('{tmp_dir}/lf/'))) {
  dir.create(glue::glue('{tmp_dir}/lf/'))
}


#empty temp dir
do.call(file.remove, list(list.files(tmp_dir, full.names = TRUE, recursive = TRUE)))

# set temp directory - helps save space with R terra
write(paste0("TMPDIR = ", tmp_dir), file=file.path(Sys.getenv('R_USER'), '.Renviron'))

# Packages and functions
#---------------------------------#

# packages required
list.of.packages <- c("terra", "tidyverse", "magrittr", "glue", "tictoc", "foreach", "doParallel", "this.path")

# #check for packages and install if needed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages)

# load all packages
vapply(list.of.packages, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)

# Other file directories
#---------------------------------#
if(!file.exists(target_dir)){
  dir.create(target_dir, recursive = TRUE) }

# Terra options
# --------------------------------#

#increase memory fraction available
terraOptions(memfrac = 0.8)

# Create all directories
# ----------------------------------#

# target dir
if (!file.exists(target_dir)) {
  dir.create(target_dir, recursive = TRUE)
}

if(!file.exists(target_dir_z)) {
  dir.create(target_dir_z, recursive = TRUE)
  }

# target dir
if (!file.exists(glue::glue('{target_dir_z}/00_prelim_dist'))) {
  dir.create(glue::glue('{target_dir_z}/00_prelim_dist'), recursive = TRUE)
}

# target dir
if (!file.exists(glue::glue('{target_dir_z}/01_final'))) {
  dir.create(glue::glue('{target_dir_z}/01_final'), recursive = TRUE)
}    

# Load crs objects
#-----------------------------------------------------#

# load lcms projections
lcms_crs <- terra::crs(lcms_proj)

#load landfire projection
landfire_crs <- terra::crs(landfire_proj)

# load treemap projection
tm_crs <- terra::crs(tm_proj)

# Remove unused objects
#------------------------------------------------#
rm(input_script.path, list.of.packages)

