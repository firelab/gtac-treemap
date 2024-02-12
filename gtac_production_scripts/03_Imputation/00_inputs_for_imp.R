### This script is used to set inputs for all steps in the imputation process
## To ensure that all scripts refer to the same input and output products

###########################################################################
# Set inputs
###########################################################################

# Standard inputs
#-----------------------------------------------#

# Zone list
zone_list <- c(16)

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

# Test application settings
#-----------------------------------------#

# set dimensions of tile - value is the length of one side
max_px <- 1000

# first row to start test on 
test_row <- 1 # adjust this if using a test AOI or tiles vs whole zone

ntest_rows <- max_px

# set number of tiles to run
# if NA, defaults to all tiles in list
ntiles <- NA

# # supply path to a shapefile to use as subset, or NA
aoi_path <- "//166.2.126.25/TreeMap/01_Data/03_AOIs/UT_Uintas_rect_NAD1983.shp"
aoi_name <- "UT_Uintas_rect"
#aoi_path <- NA

# Paths for exporting data
#--------------------------------------#

# set path to save output rasters
# this directory will be created if it does not already exist
output_dir <- glue::glue('{home_dir}03_Outputs/07_Raw_model_outputs/2016_Original_Test/')

# Output imputation name
output_name <- "2016_Orig_Test_keepinbag"

# set tmp directory
tmp_dir <- "D:/tmp/"

# set zone_number
# ----------------------------------------------#
zone_num <- zone_list[1]

# Set zone name options
cur.zone <- glue::glue('z{zone_num}')
cur.zone.zero <- if(zone_num < 10) {
  glue::glue('z0{zone_num}') } else {
    cur.zone
  }

# Update output and target dir with zone
# -----------------------------------------#
# Set folder paths
target_dir = glue::glue('{target_dir}/{cur.zone.zero}/')
output_dir = glue::glue('{output_dir}/{cur.zone.zero}/')


tile_dir <- glue::glue('{output_dir}raster/tiles/')


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
                      "terra", "tidyverse", "magrittr", "glue", "tictoc",
                      "caret")

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
input_script.path <- paste( c(spl[c(1:length(spl)-2)],
                              "00_Library/00_inputs_for_imp.R" ),
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

if (!file.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
  
# create output directory
if(!file.exists(tile_dir)){
  dir.create(tile_dir, recursive = TRUE)
}
  

