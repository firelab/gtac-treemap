### This script is used to set inputs for all steps in the imputation process
## To ensure that all scripts refer to the same input and output products

# Written by: Lila Leatherman (lila.leatherman@usda.gov)

# Last updated: 6/17/24

# TO DO: what target data params can i pull in from target data RDS? 

###########################################################################
# Set user inputs
###########################################################################

# zone to run
zone = 19

# path to an RDS file containing parameters, or NA - NA runs 00a_project_inputs_for_imp.R
# path is relative to script location
imputation_params_path <- "/params/2016_GTAC_LCMSDist_imputation_inputs.RDS"

# model to use - supply path specific model to pull into imputation, or NA
# path should be relative to home directory
# if NA, uses default model name and path
#model_path <- '/03_Outputs/07_Projects/2016_GTAC_Test/01_Raw_model_outputs/z16/model/z16_2016_GTAC_Test_ntree250_yai_treelist_bin.RDS'
model_path <- NA

# Test application settings
#-----------------------------------------#

# # supply path to a shapefile to use as subset, or NA
# aoi_path <- "//166.2.126.25/TreeMap/01_Data/03_AOIs/UT_Uintas_rect_NAD1983.shp"
# aoi_name <- "UT_Uintas_rect"
aoi_path <- NA
aoi_name <- NA

##################################################################
# Run
##################################################################

# Load pre-existing params, if available
#--------------------------------------------#

this_dir <- this.path::this.dir()

if(!is.na(imputation_params_path)) {
  
  # load params
  params_script_path <- glue::glue('{this_dir}/{imputation_params_path}')
  load(params_script_path)
  
} else {
  
  inputs_for_imputation<- glue::glue('{this_dir}/{00a_project_inputs_for_imputation.R')
  source(inputs_imputation)
  
}

# Load TreeMap script library
#--------------------------------------------------#

# load library 
this_proj = this.path::this.proj()
lib_path = glue::glue('{this_proj}/gtac_production_scripts/00_Library/treeMapLib.R')
source(lib_path)


# Build constructed inputs - less likely to change
#-----------------------------------------------------------------#

# data directory - where source data are located
data_dir <- glue::glue('{home_dir}/01_Data/')

# Path to X table
xtable_path <- glue::glue("{home_dir}/{xtable_path}")

# Directory where target rasters live
target_dir <- glue::glue("{home_dir}03_Outputs/05_Target_Rasters/{target_data_version}/")

# Directory where disturbance layers live 
dist_raster_dir <- target_dir 
#dist_raster_dir <- glue::glue("{home_dir}03_Outputs/05_Target_Rasters/v2016_GTAC/")

# Directory where EVT_GP remap table is located
evt_gp_remap_table_dir <- target_dir
#evt_gp_remap_table_dir <- glue::glue('{home_dir}03_Outputs/05_Target_Rasters/v2016_GTAC/')

# Plot coordinates
coords_path <- glue::glue('{FIA_dir}/{coords_path}')

# Paths for exporting data
#--------------------------------------#

# set path to save output rasters
# this directory will be created if it does not already exist
output_dir <- glue::glue('{home_dir}/03_Outputs/07_Projects/{project_name}/01_Raw_model_outputs/')

#set path for assembled rasters
assembled_dir <- glue::glue('{home_dir}/03_Outputs/07_Projects/{project_name}/02_Assembled_model_outputs/')

# Evaluation dir
eval_dir <- glue::glue('{home_dir}/03_Outputs/07_Projects/{project_name}/03_Evaluation/')

# Params path
params_path <- glue::glue('{output_dir}/params/')

# CRS paths
#----------------------------------------------------#
# set projection used for processing lcms rasters
lcms_proj <- glue::glue('{data_dir}05_LCMS/00_Supporting/lcms_crs_albers.prj')

# path to projection used for processing landfire rasters
landfire_proj <- glue::glue('{data_dir}02_Landfire/landfire_crs.prj')

# path to desired projection for end outputs (tm = TreeMap)
tm16_proj <- glue::glue("{data_dir}01_TreeMap2016_RDA/04_CRS/TreeMap2016_crs.prj")

# load lcms projections
lcms_crs <- terra::crs(lcms_proj)

#load landfire projection
landfire_crs <- terra::crs(landfire_proj)

# load treemap projection
tm16_crs <- terra::crs(tm16_proj)

# load output crs
output_crs <- eval(parse(text = output_crs_name))

# Set zone_number
# ----------------------------------------------#

zone_num <- as.numeric(zone)

# Set zone name options
cur_zone <- glue::glue('z{zone_num}')
cur_zone_zero <- if(zone_num < 10) {
  glue::glue('z0{zone_num}') } else {
    cur_zone
}

if(is.na(aoi_name)) {
  aoi_name <- ""
}


# Update names and dirs with zone
# -----------------------------------------#

output_name <- glue::glue("{cur_zone_zero}_{output_name}")  


target_dir = glue::glue('{target_dir}/{cur_zone_zero}/')
output_dir = glue::glue('{output_dir}/{cur_zone_zero}/')
assembled_dir = glue::glue('{assembled_dir}/{cur_zone_zero}')
eval_dir <- glue::glue('{eval_dir}{cur_zone_zero}')

tile_dir <- glue::glue('{output_dir}raster/tiles/')
model_dir = glue::glue('{output_dir}/model/')

evt_gp_remap_table_path = glue::glue('{evt_gp_remap_table_dir}/{cur_zone_zero}/EVG_remap_table.csv')
params_dir = glue::glue('{output_dir}/params/')

if (!is.na(dist_raster_dir)) {
  dist_raster_dir = glue::glue('{dist_raster_dir}/{cur_zone_zero}/')
}

# Model inputs
#----------------------------------#

# build default model path
if(is.na(model_path)) {
  model_path1 <- glue::glue('{output_name}_yai_treelist_bin')
  
  # Path where model is located
  # This path will be used for export and import
  model_path <- glue::glue('{output_dir}/model/{model_path1}.RDS')
  
  rm(model_path1)
  
}

# Set up temp directory 
#----------------------------------#

# check if tmp directory exists ; create it if it doesn't
if (!file.exists(tmp_dir)){
  dir.create(tmp_dir) }

# set temp directory - helps save space with R terra
write(paste0("TMPDIR = ", tmp_dir), file=file.path(Sys.getenv('R_USER'), '.Renviron'))

#empty temp dir
#do.call(file.remove, list(list.files(tmp_dir, full.names = TRUE, recursive = TRUE)))

# create tmp dir folder for rows
if (!file.exists(glue::glue('{tmp_dir}/rows/'))) {
  dir.create(glue::glue('{tmp_dir}/rows/'))
}
                                                       
#remove unused memory
gc()

# Create all directories
# ----------------------------------#

# output dir
if (!file.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

#create output directory
if(!file.exists(glue('{output_dir}/xytables'))){
  dir.create(glue('{output_dir}/xytables'))
}

#create output directory
if(!file.exists(glue('{output_dir}/model'))){
  dir.create(glue('{output_dir}/model'))
}

#create output directory
if(!file.exists(glue('{output_dir}/model_eval'))){
  dir.create(glue('{output_dir}/model_eval'))
}

#create output directory
if(!file.exists(glue('{output_dir}/params'))){
  dir.create(glue('{output_dir}/params'))
}

# tile_dir
if(!file.exists(tile_dir)){
  dir.create(tile_dir, recursive = TRUE)
}

# assembled_dir
if(!file.exists(assembled_dir)){
  dir.create(assembled_dir, recursive = TRUE)
}

# create assembled dir if necessary
if(!file.exists(glue::glue('{assembled_dir}/01_Imputation/'))){
  dir.create(glue::glue('{assembled_dir}/01_Imputation/'), recursive = TRUE)
}

# create assembled dir if necessary
if(!file.exists(glue::glue('{assembled_dir}/02_Assembled_vars/'))){
  dir.create(glue::glue('{assembled_dir}/02_Assembled_vars/'), recursive = TRUE)
}

# Remove unused objects
#------------------------------------------------#



# Make RDS of input parameters used
#---------------------------------------------------------#
save(list = ls(), file = glue::glue('{params_dir}/{output_name}_env.RDS'))

