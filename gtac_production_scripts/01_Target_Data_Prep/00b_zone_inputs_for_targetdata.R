### This script is used to set inputs for all steps in the target data prep process
## To ensure that all scripts refer to the same input and output products

# Written by Lila Leatherman (lila.leatherman@usda.gov)

# Last updated: 6/17/24

# TO DO:
# - address inconsistencies in EVT vs Topo Landfire Paths
# - add a switch for LCMS+LF disturbance vs LF only disturbance? 
# - export file showing input data and parameters: 
# - model year, years of data, versions of input data used, lcms thresholds

###########################################################################
# Set user inputs
###########################################################################

zone = 19

# path to an RDS file containing parameters, or NA - NA runs 00a_inputs_for_target_data.R
# path is relative to script location
target_prep_params_path <- "/params/v2016_GTAC_target_data_inputs.RDS"

# Inputs for testing
#-----------------------------------------------#
# supply path to a shapefile to use as a subset, or NA
#aoi_path <- "//166.2.126.25/TreeMap/01_Data/03_AOIs/UT_Uintas_rect_NAD1983.shp"
#aoi_name <- "UT_Uintas_rect_"
aoi_path <- NA
aoi_name <- NA

##########################################
# Run
##########################################

# Packages and functions
#---------------------------------#

# load library 
this_proj = this.path::this.proj()
lib_path = glue::glue('{this_proj}/gtac_production_scripts/00_Library/treeMapLib.R')
source(lib_path)

# Terra options
# --------------------------------#

#increase memory fraction available
terraOptions(memfrac = 0.8)

# Load pre-existing params, if available
#--------------------------------------------#

this_dir <- this.path::this.dir()

if(!is.na(target_prep_params_path)) {
  # load params
  
  params_script_path <- glue::glue('{this_dir}/{target_prep_params_path}')
  
  load(params_script_path)
  
  } else {
    
    inputs_for_target_data <- glue::glue('{this_dir}/{00a_project_inputs_for_target_data.R')
    
    source(inputs_for_target_data)
    
}


# Build constructed inputs (less likely to change)
#----------------------------------------------------------#

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

# Export data directories
#----------------------------------------------------#

# where version-specific inputs and outputs will live
project_dir <- glue::glue('{home_dir}/03_Outputs/99_Projects/{project_name}/')

# Directory where target data lives
target_dir <- glue::glue("{home_dir}/03_Outputs/05_Target_Rasters/{target_data_version}/")

# Directory where EVT_GP remap table will be located
evt_gp_remap_table_path <- target_dir

##################################################################
# CREATE ZONE-SPECIFIC VARIABLES AND PATHS
##################################################################

# Prep zone
#-----------------------------------------#

zone_num <- zone

#set zone identifiers
cur_zone <- glue::glue('z{zone_num}')
cur_zone_zero <- if(zone_num < 10) {
  glue::glue('z0{zone_num}') } else {
    cur_zone
  }

# Update dirs with zone
# -----------------------------------------#

# Set folder paths
target_dir_z = glue::glue('{target_dir}/{cur_zone_zero}/')

# update biophys path - biophys layers stored by zone
biophys_dir_z <- glue::glue('{biophys_dir}/{cur_zone_zero}/')


# set aoi_name field if it doesn't already exist via aoi subset
if(is.na(aoi_name)) {
  aoi_name <- ""
}

# Export data paths- disturbance
#---------------------------------------------------------------#
# create output file names
landfire_fire_years_outpath <- glue::glue('{target_dir_z}/{start_year}_{end_year}_{cur_zone_zero}_{aoi_name}LandfireDist_Fire_Years.tif')
landfire_fire_binary_outpath <- glue::glue('{target_dir_z}/{start_year}_{end_year}_{cur_zone_zero}_{aoi_name}LandfireDist_Fire_Binary.tif')

landfire_ind_years_outpath <- glue::glue('{target_dir_z}/{start_year}_{end_year}_{cur_zone_zero}_{aoi_name}LandfireDist_InsectDisease_Years.tif')
landfire_ind_binary_outpath <- glue::glue('{target_dir_z}/{start_year}_{end_year}_{cur_zone_zero}_{aoi_name}LandfireDist_InsectDisease_Binary.tif')

lcms_slowloss_years_outpath <- glue::glue('{target_dir_z}/{start_year}_{end_year}_{cur_zone_zero}_{aoi_name}LCMSDist_SlowLoss_Years.tif')
lcms_slowloss_binary_outpath <- glue::glue('{target_dir_z}/{start_year}_{end_year}_{cur_zone_zero}_{aoi_name}LCMSDist_SlowLoss_Binary.tif')

lf_disturb_code_outpath <- glue::glue('{target_dir_z}/disturb_code_LF.tif')
lf_disturb_year_outpath <- glue::glue('{target_dir_z}/disturb_year_LF.tif')

lcms_disturb_code_outpath <- glue::glue('{target_dir_z}/disturb_code_LFLCMS.tif')
lcms_disturb_year_outpath <- glue::glue('{target_dir_z}/disturb_year_LFLCMS.tif')

# Load crs objects
#-----------------------------------------------------#

# load lcms projections
lcms_crs <- terra::crs(lcms_proj)

#load landfire projection
landfire_crs <- terra::crs(landfire_proj)

# load treemap projection
tm_crs <- terra::crs(tm_proj)


# Temp directories 
#----------------------------------#

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

# set temp directory - allows you to inspect files in progress more easily
write(paste0("TMPDIR = ", tmp_dir), file=file.path(Sys.getenv('R_USER'), '.Renviron'))


# Create all directories
# ----------------------------------#

# target dir
if (!file.exists(target_dir)) {
  dir.create(target_dir, recursive = TRUE)
}

if(!file.exists(target_dir_z)) {
  dir.create(target_dir_z, recursive = TRUE)
  }

if(!file.exists(glue::glue('{target_dir_z}/params/'))) {
  dir.create(glue::glue('{target_dir_z}/params/'), recursive = TRUE)
}

# # target dir
# if (!file.exists(glue::glue('{target_dir_z}/00_prelim_dist'))) {
#   dir.create(glue::glue('{target_dir_z}/00_prelim_dist'), recursive = TRUE)
# }
# 
# # target dir
# if (!file.exists(glue::glue('{target_dir_z}/01_final'))) {
#   dir.create(glue::glue('{target_dir_z}/01_final'), recursive = TRUE)
# }    


# Remove unused objects
#------------------------------------------------#



# Make RDS of input parameters used
#---------------------------------------------------------#
save(list = ls(), file = glue::glue('{target_dir_z}/params/{cur_zone_zero}_env.RDS'))

