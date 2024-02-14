### This script is used to set inputs for all steps in the imputation process
## To ensure that all scripts refer to the same input and output products

# Last updated: 2/12/2024

###########################################################################
# Set inputs
###########################################################################

# Output imputation name
output_name <- "2016_Orig_Test_keepinbag"

# Standard inputs
#-----------------------------------------------#

# Zone list
zone_list <- c(16)
zone_num <- zone_list[1]

#home_dir
#home_dir <- "D:/LilaLeatherman/01_TreeMap/"
home_dir<- "//166.2.126.25/TreeMap/"

# Path to X table
xtable_path <- glue::glue("{home_dir}01_Data/01_TreeMap2016_RDA/01_Input/03_XTables/X_table_all_singlecondition.txt")

# Directory where target rasters live
target_dir <- glue::glue("{home_dir}01_Data/01_TreeMap2016_RDA/02_Target/")

# Directory where EVT_GP remap table is located
evt_gp_remap_table_path <- glue::glue("{home_dir}03_Outputs/05_Target_Rasters/02_Vegetation/")

# Plot coordinates- shapefile
#points_path <- "//166.2.126.25/TreeMap/01_Data/04_FIA/03_FullShp/FIA_US.shp"

# path to file with desired projection
prj_path <- glue::glue('{home_dir}01_Data/02_Landfire/landfire_crs.prj')


# Test application settings
#-----------------------------------------#

# # supply path to a shapefile to use as subset, or NA
aoi_path <- "//166.2.126.25/TreeMap/01_Data/03_AOIs/UT_Uintas_rect_NAD1983.shp"
aoi_name <- "UT_Uintas_rect"
#aoi_path <- NA

# Paths for exporting data
#--------------------------------------#

# set path to save output rasters
# this directory will be created if it does not already exist
# UPDATE THIS TO BE FLEX FOR FUTURE RUNS
output_dir <- glue::glue('{home_dir}03_Outputs/07_Raw_model_outputs/2016_Original_Test/')

#set path for assembled rasters
# UPDATE THIS TO BE FLEX FOR FUTURE RUNS
assembled_dir <- glue::glue('{home_dir}03_Outputs/08_Assembled_model_outputs/2016_Original_Test/')

# Evaluation dir
eval_dir <- glue::glue('{home_dir}/03_Outputs/09_Evaluation/02_TreeMap_Runs/2016_Original_Test/')

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
                      "caret", "furrr", "progressr")

#check for packages and install if needed
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

# Set up temp directory 
#----------------------------------#

# check if tmp directory exists ; create it if it doesn't
if (!file.exists(tmp_dir)){
  dir.create(tmp_dir) }

# set temp directory - helps save space with R terra
write(paste0("TMPDIR = ", tmp_dir), file=file.path(Sys.getenv('R_USER'), '.Renviron'))
#empty temp dir
do.call(file.remove, list(list.files(tmp_dir, full.names = TRUE)))
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