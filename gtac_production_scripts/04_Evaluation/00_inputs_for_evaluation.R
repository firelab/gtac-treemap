# INPUTS FOR EVALUATION

# so I don't have to update the inputs for imputation script every time 
# / so imputation and evaluation can happen at different rates / times 

# Specific inputs
#----------------------------------------------#

# zone number
zone_num <- 16

# Project name
# project_name <- "2016_GTAC_Test"
project_name <- "2016_GTAC_LCMSDist"

# output name - name of raster and CM outputs
# output_name <- "2016_Orig_Test_keepinbag_ntree250"
output_name <- "2016_GTAC_LCMSDist"

# name of raster to validate
# raster_name <- glue::glue("2016_Orig_Test_keepinbag_ntree250_tilesz2000_nT36")
raster_name <- glue::glue("2016_GTAC_LCMSDist_tilesz2000_nT36")

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

# model to use - supply specific model to pull into imputation, or NA
# if NA, uses default model name and path

model_path <- '{home_dir}03_Outputs/07_Projects/2016_GTAC_Test/01_Raw_model_outputs/z16/model/z16_2016_GTAC_Test_ntree250_yai_treelist_bin.RDS'

#model_path <- NA

# Path to X table used to make model
#xtable_path <- glue::glue("{home_dir}03_Outputs/06_Reference_Data/v2016_RMRS/X_table_all_singlecondition.txt")
xtable_path <- glue::glue("{home_dir}03_Outputs/07_Projects/2016_GTAC_Test/01_Raw_model_outputs/z16/xytables/z16_2016_Orig_Test_keepinbag_ntree250_Xdf_bin.csv")

# Directory where disturbance layers live
# If disturbance layers live in the same dir, then NA
dist_raster_dir <- NA
#dist_raster_dir <- glue::glue("{home_dir}03_Outputs/05_Target_Rasters/v2016_GTAC/")

# disturbance type - options are "LF" or "LFLCMS".
# This param only used if !is.na(dist_raster_dir)
dist_layer_type <- "LF"
#dist_layer_type <- "LCMS"

# target data version to use
target_data_version <- "v2016_RMRS"

# reference data version to use
ref_data_version <- "v2016_RMRS"

# General inputs
#--------------------------------------------------#

# set number of digits to round to
round_dig <- 4

# set location of raster attribute table
rat_path <- glue::glue("{home_dir}01_Data/01_TreeMap2016_RDA/RDS-2021-0074_Data/Data/")

# set path to landfire vector data
lf_zones_path <- glue::glue("{home_dir}01_Data/02_Landfire/LF_zones/Landfire_zones/refreshGeoAreas_041210.shp")

# Directory where EVT_GP remap table is located
#evt_gp_remap_table_dir <- target_dir
evt_gp_remap_table_dir <- glue::glue('{home_dir}03_Outputs/05_Target_Rasters/v2016_GTAC/')

# path to evt_gp metadata
evt_path <- glue::glue("{home_dir}01_Data/02_Landfire/LF_200/EVT/LF2016_EVT_200_CONUS/CSV_Data/LF16_EVT_200.csv")

# path to coords
coords_path <- glue::glue("{home_dir}01_Data/04_FIA/06_Coordinates/select_TREEMAP2022_2send/select_TREEMAP2022_2send.csv")

# Directory where target rasters live
target_dir <- glue::glue("{home_dir}03_Outputs/05_Target_Rasters/{target_data_version}/")



# Paths for exporting data
#--------------------------------------#

# set path to save output rasters
# this directory will be created if it does not already exist
# output_dir <- glue::glue('{home_dir}/03_Outputs/07_Projects/{project_name}/01_Raw_model_outputs/')
output_dir <- "C:/Users/abhinavshrestha/OneDrive - USDA/Documents/02_TreeMap/temp_dir" # for testing

#set path for assembled rasters
assembled_dir <- glue::glue('{home_dir}/03_Outputs/07_Projects/{project_name}/02_Assembled_model_outputs/')

# Evaluation dir
eval_dir <- glue::glue('{home_dir}/03_Outputs/07_Projects/{project_name}/03_Evaluation/')

# crs raster data are in 
landfire_crs <- terra::crs(glue::glue('{home_dir}/01_Data/02_Landfire/landfire_crs.prj'))

# set tmp directory
tmp_dir <- "C:/Users/abhinavshrestha/OneDrive - USDA/Documents/02_TreeMap/temp_dir"

# Prep constructed paths
#----------------------------------------------#

# Set zone name options
cur_zone <- glue::glue("z{zone_num}")
cur_zone_zero <- if(zone_num < 10) {
  glue::glue("z0{zone_num}") } else {
    cur_zone
  }

#set path for assembled rasters
assembled_dir <- glue::glue("{home_dir}/03_Outputs/07_Projects/{project_name}/02_Assembled_model_outputs/{cur_zone_zero}/")

# Evaluation dir

eval_dir <- glue::glue("{home_dir}/03_Outputs/07_Projects/{project_name}/03_Evaluation/{cur_zone_zero}")

target_dir = glue::glue('{target_dir}/{cur_zone_zero}/')

if (!is.na(dist_raster_dir)) {
  dist_raster_dir = glue::glue('{dist_raster_dir}/{cur_zone_zero}/01_final')
}

evt_gp_remap_table_path = glue::glue('{evt_gp_remap_table_dir}/{cur_zone_zero}/01_final/EVG_remap_table.csv')


# Model inputs
#----------------------------------#

# build default model path
if(is.na(model_path)) {
  model_path1 <- glue::glue('{output_name}_yai_treelist_bin')
  
  # Path where model is located
  # This path will be used for export and import
  model_path <- glue::glue('{output_dir}/model/{cur_zone_zero}_{model_path1}.RDS')
  
}

# Other options
# --------------------------------#

# Allow for sufficient digits to differentiate plot cn numbers

options("scipen" = 100, "digits" = 8)


# Load library
#-------------------------------------------------------#

this.path <- this.path::this.path()

# get path to library script
spl1 <- stringr::str_split(this.path, "/")[[1]]
lib_path <- paste(c(spl1[c(1:(length(spl1) - 2))],
                    "00_Library/treemapLib.R"),
                  collapse = "/")

source(lib_path)

# # Create directories
# #----------------------------------------------#
# 
# # create eval dir if necessary 
# if(!file.exists(glue::glue('{eval_dir}/01_OOB_Evaluation/figs/'))) {
#   dir.create(glue::glue('{eval_dir}/01_OOB_Evaluation/figs/'), recursive = TRUE)
# }
# 
# # create eval dir if necessary 
# if(!file.exists(glue::glue('{eval_dir}/02_Target_Layer_Comparison'))) {
#   dir.create(glue::glue('{eval_dir}/02_Target_Layer_Comparison'), recursive = TRUE)
# }
# 
# # create eval dir if necessary 
# if(!file.exists(glue::glue('{eval_dir}/03_FIA_Comparison/figs/'))) {
#   dir.create(glue::glue('{eval_dir}/03_FIA_Comparison/figs/'), recursive = TRUE)
# }
# 
# # create eval dir if necessary 
# if(!file.exists(glue::glue('{eval_dir}/04_Eval_Reports'))) {
#   dir.create(glue::glue('{eval_dir}/04_Eval_Reports'), recursive = TRUE)
# }