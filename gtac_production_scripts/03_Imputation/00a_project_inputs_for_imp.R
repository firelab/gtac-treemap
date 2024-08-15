### This script is used to set inputs for all steps in the imputation process
## To ensure that all scripts refer to the same input and output products

# Written by: Lila Leatherman (lila.leatherman@usda.gov)

# Last updated: 7/10/24

###########################################################################
# Set inputs
###########################################################################

year <- year_input

#project_name <- glue::glue("{year}_Production")
project_name <- glue::glue("{year}_Production") #for testing

# name for products - includes params here if desired
#e.g., #output_name <- "2016_GTAC_LCMSDist"
output_name <- glue::glue("{year}_Production") 

# target data version to use
target_data_version <- glue::glue("v{year}")

# reference data version to use
ref_data_version <- glue::glue("v{year}")

# disturbance type - options are "LF" or "LFLCMS"
dist_layer_type <- "LF"

# # output crs - desired crs for output products
# #options include: "lcms_crs", "lf200_crs", "lf220_crs", "lf230_crs", "tm16_crs"
output_crs_name <- "lf230_crs"

# list names of xvars in reference / x table
#xvars <- c("SLOPE", "ELEV", "PARI", "PPTI", "RELHUMI", "TMAXI", "TMINI", "VPDI", "disturb_code", "disturb_year", "canopy_cover", "canopy_height", "EVT_GP", "NORTHING", "EASTING", "POINT_X", "POINT_Y") # 2016 version
xvars <- c("slope", "elevation", "easting", "northing",
           "prcp", "srad", "swe", "tmax", "tmin", "vp", "vpd",
           "disturb_code", "disturb_year", "evc", "evh", "evt_gp_remap",
           "point_x", "point_y") #2020/2022 version
  
# list names of yvars used as response vars for modeling
yvars <- c("evc", "evh", "evt_gp_remap", "disturb_code") # 2020 version
  
# list names of target vars / target layers
#targetvars <- c("SLOPE", "ELEV", "PARI", "PPTI", "RELHUMI", "TMAXI", "TMINI", "VPDI", "disturb_code", "disturb_year", "canopy_cover", "canopy_height", "EVT_GP", "NORTHING", "EASTING", "POINT_X", "POINT_Y") # 2016 version
targetvars <- c("elevation", "easting", "northing",
                "prcp", "srad", "swe", "tmax", "tmin", "vp", "vpd",
                "disturb_code", "disturb_year", "evc", "evh", "evt_gp_remap",
                "point_x", "point_y") #2020/2022 version
  

# Load TreeMap script library
#--------------------------------------------------#

# necessary to load this before calling any of the home_dir, fia_dir, or data_dir objects

# load library 
this_proj = this.path::this.proj()
lib_path = glue::glue("{this_proj}/gtac_production_scripts/00_Library/treeMapLib.R")
source(lib_path)

# Data inputs - less likely to change
#----------------------------------------------------------------#

# Plot coordinates - relative to FIA_dir
coords_path <- glue::glue("{FIA_dir}/06_Coordinates/select_TREEMAP2022_2send/select_TREEMAP2022_2send.csv")

# Dir for X table - relative to home_dir
xtable_dir <- glue::glue("{home_dir}/03_Outputs/06_Reference_Data/{ref_data_version}/01_X_tables_by_zone/")

# Raster Attribute table for evaluation
# Currently only available for 2016!
rat_path <- glue::glue("{home_dir}01_Data/01_TreeMap2016_RDA/RDS-2021-0074_Data/Data/TreeMap2016.tif")

# Analysis settings
#----------------------------------------------------------------#

# digits to round to in evaluation
round_dig <- 4

# Allow for sufficient digits to differentiate plot cn numbers
options("scipen" = 100, "digits" = 8)

#increase memory fraction available for terra
terra::terraOptions(memfrac = 0.8)



# Build constructed inputs - less likely to change
#-----------------------------------------------------------------#
# data directory - where source data are located
data_dir <- glue::glue("{home_dir}/01_Data/")

# set path to landfire vector data
lf_zones_path <- glue::glue("{data_dir}/02_Landfire/LF_zones/Landfire_zones/refreshGeoAreas_041210.shp")

# Directory where target rasters live
target_dir <- glue::glue("{home_dir}03_Outputs/05_Target_Rasters/{target_data_version}/post_mask/")

# Directory where disturbance layers live 
dist_raster_dir <- target_dir 

# Directory where EVT_GP remap table is located
evt_gp_remap_table_dir <- target_dir


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

# load output crs
output_crs <- eval(parse(text = output_crs_name))

# Paths for exporting data
#--------------------------------------#

# set path to save output rasters
# this directory will be created if it does not already exist
raw_outputs_dir <- glue::glue("{home_dir}03_Outputs/07_Projects/{project_name}/01_Raw_model_outputs/")

#set path for assembled rasters
assembled_dir <- glue::glue("{home_dir}03_Outputs/07_Projects/{project_name}/02_Assembled_model_outputs/")

# Evaluation dir
eval_dir <- glue::glue("{home_dir}03_Outputs/07_Projects/{project_name}/03_Evaluation/")


# Make RDS of input parameters used
#---------------------------------------------------------#

# Export to scripts folder for easy access 
# over-writes by default
save(list = ls(), file = glue::glue("{this.path::this.dir()}/params/{project_name}_imputation_inputs.RDS"))

