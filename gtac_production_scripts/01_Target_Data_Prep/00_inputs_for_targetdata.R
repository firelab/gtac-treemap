### This script is used to set inputs for all steps in the target data prep process
## To ensure that all scripts refer to the same input and output products

# Written by Lila Leatherman (lila.leatherman@usda.gov)

# Last updated: 3/4/24

# TO DO:
# - address inconsistencies in EVT vs Topo Landfire Paths
# - add a switch for LCMS+LF disturbance vs LF only disturbance? 

###########################################################################
# Set inputs
###########################################################################

# Inputs for testing
#-----------------------------------------------#

# supply path to a shapefile to use as a subset, or NA
# aoi_path <- "//166.2.126.25/TreeMap/01_Data/03_AOIs/UT_Uintas_rect_NAD1983.shp"
# aoi_name <- "UT_Uintas_rect"
aoi_path <- NA

# setting to remove intermediate files from memory
# all intermediate files required to generate end product are written to disk
# Y: deletes intermediate files (better for iteration and development)
# N: retains intermediate files (better for computational efficiency)
remove_intermediate_files <- "N"

#option to calculate landfire fire files anew
#may not be necessary if this has already been run for the year and zone of interest
calculate_landfire_fire <- "Y"

# Standard inputs
#-----------------------------------------------#

# Zone list
zone_list <- c(16)

# Project name
project_name <- "2016_GTAC_Test"

# target data version
target_data_version <- "v2016_RMRS"

# reference data version
ref_data_version <- "v2016_RMRS"

# set year range
start_year <- 1999
end_year <- 2016

# set current modeling year (for years since disturbance)
model_year <- end_year

# Input data directories
#-----------------------------------------#

# home dir
home_dir <- "D:/LilaLeatherman/01_TreeMap/"
#home_dir <- "//166.2.126.25/TreeMap/"

# data directory - where source data are located
#data_dir <- glue::glue('{home_dir}/01_Data/')
data_dir <- "//166.2.126.25/TreeMap/01_Data/"

# set path to landfire rasters 
landfire_dir <- glue::glue('{data_dir}02_Landfire/LF_220/')
landfire_version <- 220
landfire_year <- 2020

# set path to landfire vector data
lf_zones_path <- glue::glue('{data_dir}/02_Landfire/LF_zones/Landfire_zones/refreshGeoAreas_041210.shp')

# set dir to lcms raw probability rasters
lcms_dir <- glue::glue('{data_dir}05_LCMS/01_Threshold_Testing/01_Raw/02_Raw_Probabilities/')

# set projection used for processing lcms rasters
lcms_proj <- glue::glue('{data_dir}05_LCMS/00_Supporting/lcms_crs_albers.prj')

# path to projection used for processing landfire rasters
landfire_proj <- glue::glue('{data_dir}02_Landfire/landfire_crs.prj')

# Paths to specific Landfire rasters - not disturbance
evc_path <- glue::glue('{landfire_dir}EVC/LF2016_EVC_200_CONUS/Tif/LC16_EVC_200.tif')
evh_path <- glue::glue('{landfire_dir}EVH/LF2016_EVH_200_CONUS/Tif/LC16_EVH_200.tif')
evt_path <- glue::glue('{landfire_dir}EVT/LF2016_EVT_200_CONUS/Tif/LC16_EVT_200.tif')

topo_dir <-glue::glue('{landfire_dir}/Topo/')
elev_path <- glue::glue('{topo_dir}LF2020_Elev_220_CONUS/Tif/LC20_Elev_220.tif')
slopeP_path <- glue::glue('{topo_dir}LF2020_SlpP_220_CONUS/Tif/LC20_SlpP_220.tif')
slopeD_path <- glue::glue('{topo_dir}LF2020_SlpD_220_CONUS/Tif/LC20_SlpD_220.tif')
asp_path <- glue::glue('{topo_dir}LF2020_Asp_220_CONUS/Tif/LC20_Asp_220.tif')

# set paths to input biophys rasters
biphys_path <- glue::glue('{data_dir}02_Landfire/BioPhys/')

# Export data directories
#----------------------------------------------------#

# where version-specific inputs and outputs will live
project_dir <- glue::glue('{home_dir}/03_Outputs/99_Projects/{project_name}/')

# Directory where target data lives
target_dir <- glue::glue("{home_dir}/01_Data/03_Outputs/05_Target_Rasters/{target_data_version}/")

# Directory where EVT_GP remap table will be located
evt_gp_remap_table_path <- target_dir

# Input parameters for LCMS Disturbance
#-----------------------------------------------------------#

# Set variables
LCMS_NAvalue<- -32768

# set threshold for probability of slow loss from LCMS
slow_loss_thresh <- 14 # default value for LCMS processing: 14


# set probability thresholds for change
LCMS_change_thresholds <- c(29, # fast loss; default = 29
                       slow_loss_thresh, # slow loss; default = 14
                       20 # gain; default = 20
                       )

##########################################

# set tmp directory
tmp_dir <- "D:/tmp"

# setting to remove intermediate files from memory
# all intermediate files required to generate end product are written to disk
# Y: deletes intermediate files (better for iteration and development)
# N: retains intermediate files (better for computational efficiency)
remove_intermediate_files <- "N"

#option to calculate landfire fire files anew
#may not be necessary if this has already been run for the year and zone of interest
calculate_landfire_fire <- "Y"

#####################
# SETUP
######################

# Temp directories 
#----------------------------------#

# check if tmp directory exists 
if (file.exists(tmp_dir)){
  
} else {
  # create a new sub directory inside the main path
  dir.create(tmp_dir, recursive = TRUE)
  
}

#empty temp dir
#do.call(file.remove, list(list.files(tmp_dir, full.names = TRUE)))

# set temp directory - helps save space with R terra
write(paste0("TMPDIR = ", tmp_dir), file=file.path(Sys.getenv('R_USER'), '.Renviron'))

#remove unused memory
gc()

# Packages and functions
#---------------------------------#

# packages required
list.of.packages <- c("terra", "tidyverse", "magrittr", "glue", "tictoc")

# #check for packages and install if needed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages)

# load all packages
vapply(list.of.packages, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)

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

# Remove unused objects
#------------------------------------------------#
rm(input_script.path, list.of.packages)
