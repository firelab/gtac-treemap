### This script is used to set inputs for all steps in the imputation process
## To ensure that all scripts refer to the same input and output products

# TO DO: 
# - write out input parameters used for a given output name

# Last updated: 3/19/2024

###########################################################################
# Set inputs
###########################################################################

# Identifiers - for outputs 
#--------------------------------------------------#
# Project name
project_name <- "2016_GTAC_LCMSDist"

# Output imputation name
output_name <- "2016_GTAC_LCMSDist"

# name of output raster / raster to validate
# will get overwritten in 02_run_imputation 
#raster_name <- glue::glue('{output_name}_tilesz2000_nT36')

# Test application settings
#-----------------------------------------#

# # supply path to a shapefile to use as subset, or NA
# aoi_path <- "//166.2.126.25/TreeMap/01_Data/03_AOIs/UT_Uintas_rect_NAD1983.shp"
# aoi_name <- "UT_Uintas_rect"
aoi_path <- NA
aoi_name <- NA

# Standard inputs
#-----------------------------------------------#

# Zone list
zone_num <- 16

# target data version to use
target_data_version <- "v2016_RMRS"

# reference data version to use
ref_data_version <- "v2016_RMRS"

# model to use - supply specific model to pull into imputation, or NA
# if NA, uses default model name and path
model_path <- '//166.2.126.25/TreeMap/03_Outputs/99_Projects/2016_GTAC_Test/01_Raw_model_outputs/z16/model/z16_2016_GTAC_Test_ntree250_yai_treelist_bin.RDS'

#home_dir
home_dir <- "//166.2.126.25/TreeMap/"


# Constructed inputs - less likely to change
#-----------------------------------------------------------------#

# Path to X table
xtable_path <- glue::glue("{home_dir}03_Outputs/06_Reference_Data/{ref_data_version}/X_table_all_singlecondition.txt")

# Directory where target rasters live
target_dir <- glue::glue("{home_dir}03_Outputs/05_Target_Rasters/{target_data_version}/")

# Directory where disturbance layers live
# If disturbance layers live in the same dir, then NA
#dist_raster_dir <- NA
dist_raster_dir <- glue::glue("{home_dir}03_Outputs/05_Target_Rasters/v2016_GTAC/")

# disturbance type - options are "LF" or "LFLCMS".
# This param only used if !is.na(dist_raster_dir)
dist_layer_type <- "LFLCMS"

# Directory where EVT_GP remap table is located
#evt_gp_remap_table_dir <- target_dir
evt_gp_remap_table_dir <- glue::glue('{home_dir}03_Outputs/05_Target_Rasters/v2016_GTAC/')

# Plot coordinates
#points_path <- glue::glue('{home_dir}01_Data\04_FIA\06_Coordinates\select_TREEMAP2022_2send\select_TREEMAP2022_2send.xlsx')

# path to file with desired projection
prj_path <- glue::glue('{home_dir}01_Data/02_Landfire/landfire_crs.prj')

# Paths for exporting data
#--------------------------------------#

# set path to save output rasters
# this directory will be created if it does not already exist
# UPDATE THIS TO BE FLEX FOR FUTURE RUNS
output_dir <- glue::glue('{home_dir}/03_Outputs/99_Projects/{project_name}/01_Raw_model_outputs/')

#set path for assembled rasters
assembled_dir <- glue::glue('{home_dir}/03_Outputs/99_Projects/{project_name}/02_Assembled_model_outputs/')

# Evaluation dir
eval_dir <- glue::glue('{home_dir}/03_Outputs/99_Projects/{project_name}/03_Evaluation/')

# set tmp directory
tmp_dir <- "D:/tmp/"

# set zone_number
# ----------------------------------------------#

# Set zone name options
cur.zone <- glue::glue('z{zone_num}')
cur.zone.zero <- if(zone_num < 10) {
  glue::glue('z0{zone_num}') } else {
    cur.zone
  }

if(!is.na(aoi_name)) {
  output_name <- glue::glue('{output_name}_{aoi_name}')
}

if(is.na(aoi_name)) {
  aoi_name <- ""
}


# Update dirs with zone
# -----------------------------------------#
# Set folder paths
target_dir = glue::glue('{target_dir}/{cur.zone.zero}/')
dist_raster_dir = glue::glue('{dist_raster_dir}/{cur.zone.zero}/01_final')
output_dir = glue::glue('{output_dir}/{cur.zone.zero}/')
assembled_dir = glue::glue('{assembled_dir}/{cur.zone.zero}')
eval_dir <- glue::glue('{eval_dir}{cur.zone.zero}')

tile_dir <- glue::glue('{output_dir}raster/tiles/')
model_dir = glue::glue('{output_dir}/model/')

evt_gp_remap_table_path = glue::glue('{evt_gp_remap_table_dir}/{cur.zone.zero}/01_final/EVG_remap_table.csv')


# Model inputs
#----------------------------------#

# build default model path
if(is.na(model_path)) {
  model_path1 <- glue::glue('{output_name}_yai_treelist_bin')
  
  # Path where model is located
  # This path will be used for export and import
  model_path <- glue::glue('{output_dir}/model/{cur.zone.zero}_{model_path1}.RDS')
  
}


###########################################################################
# Set up libraries and directories
###########################################################################

# Packages and functions
#---------------------------------#

# install dev version of yaimpute from forked repo
#devtools::install_github("lleather/yaImpute")

# packages required
list.of.packages <- c("terra",   
                      "yaImpute",
                      "randomForest",
                      "this.path",
                      "tidyverse", "magrittr", "glue", "tictoc",
                      "caret", 
                      "doParallel")

# #check for packages and install if needed
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages) > 0) install.packages(new.packages)

# load all packages
vapply(list.of.packages, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)

# Load TreeMap script library
#--------------------------------------------------#

# Set inputs - from input script
# Id where script is located
this.path <- this.path::this.path()

# get path to library script
spl1 <- stringr::str_split(this.path, "/")[[1]]
lib.path <- paste( c(spl1[c(1:(length(spl1)-2))],
                              "00_Library/treemapLib.R" ),
                            collapse = "/")

source(lib.path)

# Set up temp directory 
#----------------------------------#

# check if tmp directory exists ; create it if it doesn't
if (!file.exists(tmp_dir)){
  dir.create(tmp_dir) }

# set temp directory - helps save space with R terra
write(paste0("TMPDIR = ", tmp_dir), file=file.path(Sys.getenv('R_USER'), '.Renviron'))

#empty temp dir
do.call(file.remove, list(list.files(tmp_dir, full.names = TRUE, recursive = TRUE)))
#rmdir(tmp_dir, recursive = FALSE)

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
if(!file.exists(glue::glue('{assembled_dir}/02_Derived_vars/'))){
  dir.create(glue::glue('{assembled_dir}/02_Derived_vars/'), recursive = TRUE)
}

# create eval dir if necessary 
if(!file.exists(glue::glue('{eval_dir}/01_OOB_Evaluation'))) {
  dir.create(glue::glue('{eval_dir}/01_OOB_Evaluation'), recursive = TRUE)
}

# create eval dir if necessary 
if(!file.exists(glue::glue('{eval_dir}/02_LF_Comparison'))) {
  dir.create(glue::glue('{eval_dir}/02_LF_Comparison'), recursive = TRUE)
}

# create eval dir if necessary 
if(!file.exists(glue::glue('{eval_dir}/03_Eval_Reports'))) {
  dir.create(glue::glue('{eval_dir}/03_Eval_Reports'), recursive = TRUE)
}



# Load other standard inputs
#---------------------------------------------#
prj <- terra::crs(prj_path)

# Remove unused objects
#------------------------------------------------#
rm(input_script.path, list.of.packages, evt_gp_remap_table_dir)

# Write out input parameters used
#----------------------------------------------------#

params_out <- c(
  glue::glue('project_name : {project_name}'),
  glue::glue('output_name  : {output_name}'), 
  glue::glue('cur.zone.zero : {cur.zone.zero}'),
  glue::glue('aoi_name : {aoi_name}'), 
  glue::glue('target_data_version : {target_data_version}'),
  glue::glue('ref_data_version : {ref_data_version}'), 
  glue::glue('dist_raster_dir : {dist_raster_dir}'),
  glue::glue('model_path : {model_path}'),
  glue::glue('xtable_path : {xtable_path}')
)

# params_out <- c(
#   project_name, 
#   output_name,
#   cur.zone.zero,
#   aoi_name,
#   target_data_version,
#   ref_data_version,
#   dist_raster_dir,
#   model_path,
#   xtable_path
#   )


write.table(as.data.frame(params_out), glue('{output_dir}/params/{cur.zone.zero}_{output_name}_params.txt'),
            row.names = FALSE)

