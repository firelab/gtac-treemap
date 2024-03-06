### This script is used to set inputs for all steps in the imputation process
## To ensure that all scripts refer to the same input and output products

# Last updated: 2/14/2024

###########################################################################
# Set inputs
###########################################################################

# Output imputation name
output_name <- "2016_Orig_Test_keepinbag_ntree250"

# Standard inputs
#-----------------------------------------------#

# Zone list
zone_num <- 16

# Project name
project_name <- "2016_GTAC_Test"

# target data version
target_data_version <- "v2016_RMRS"

# reference data version
ref_data_version <- "v2016_RMRS"

# home dir
home_dir <- "D:/LilaLeatherman/01_TreeMap/"
#home_dir <- "//166.2.126.25/TreeMap/"

# data directory - where source data are located
data_dir <- glue::glue('{home_dir}/01_Data/')

# where version-specific inputs and outputs will live
project_dir <- glue::glue('{home_dir}/03_Outputs/99_Projects/{project_name}/')

# Directory where target data lives
target_dir <- glue::glue("{data_dir}/01_TreeMap2016_RDA/01_Target/") # specific to first iteration
#target_dir <- glue::glue("{data_dir}/03_Outputs/05_Target_Rasters/{target_data_version}/")

# Directory where reference data lives
ref_dir <- glue::glue("{data_dir}/03_outputs/06_Reference_Data/{ref_data_varsion}/")

# Path to X table
xtable_path <- glue::glue("{data_dir}/01_TreeMap2016_RDA/01_Input/03_XTables/X_table_all_singlecondition.txt") #  specific to first version
#xtable_path <- glue::glue("{ref_dir}/03_XTables/X_table_all_singlecondition.txt")


# Directory where EVT_GP remap table is located
evt_gp_remap_table_path <- glue::glue("{project_dir}/02_Target_Rasters/") # specific to first version
#evt_gp_remap_table_path <- target_dir

# Plot coordinates- shapefile
#points_path <- "//166.2.126.25/TreeMap/01_Data/04_FIA/03_FullShp/FIA_US.shp"

# path to file with desired projection
prj_path <- glue::glue('{data_dir}/02_Landfire/landfire_crs.prj')


# Paths for exporting data
#--------------------------------------#

# set path to save output rasters
# this directory will be created if it does not already exist
# UPDATE THIS TO BE FLEX FOR FUTURE RUNS
output_dir <- glue::glue('{project_dir}/03_Raw_Model_Outputs/')

#set path for assembled rasters
# UPDATE THIS TO BE FLEX FOR FUTURE RUNS
assembled_dir <- glue::glue('{project_dir}/04_Assembled_Model_Outputs/')

# Evaluation dir
eval_dir <- glue::glue('{project_dir}/05_Evaluation/')

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

# Update dirs with zone
# -----------------------------------------#
# Set folder paths
target_dir = glue::glue('{target_dir}/{cur.zone.zero}/')
output_dir = glue::glue('{output_dir}/{cur.zone.zero}/')
assembled_dir = glue::glue('{assembled_dir}/{cur.zone.zero}')
eval_dir <- glue::glue('{eval_dir}{cur.zone.zero}')

tile_dir <- glue::glue('{output_dir}raster/tiles/')

evt_gp_remap_table_path = glue::glue('{evt_gp_remap_table_path}/{cur.zone.zero}/EVG_remap_table.csv')


# Model inputs
#----------------------------------#

# Path where model is located
model_path <- glue::glue('{output_dir}/model/{cur.zone.zero}_{output_name}_yai_treelist_bin.RDS')

###########################################################################
# Set up libraries and directories
###########################################################################

# Packages and functions
#---------------------------------#

# install dev version of yaimpute from forked repo
#devtools::install_github("lleather/yaImpute")

# packages required
list.of.packages <- c("raster",   
                      "yaImpute",
                      "randomForest",
                      "this.path",
                      "terra", "tidyverse", "magrittr", "glue", "tictoc",
                      "caret", 
                      "furrr", "progressr",
                      "doParallel")

#check for packages and install if needed
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages) > 0) install.packages(new.packages)

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
if(!file.exists(glue::glue('{eval_dir}/01_Map_Validation'))) {
  dir.create(glue::glue('{eval_dir}/01_Map_Validation'), recursive = TRUE)
}

# Load other standard inputs
#---------------------------------------------#
prj <- terra::crs(prj_path)

# Remove unused objects
#------------------------------------------------#
rm(input_script.path, list.of.packages)
