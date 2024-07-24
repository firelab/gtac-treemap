# INPUTS FOR EVALUATION

# TO DO: option to pull in params from .RDS instead of running inputs script over again
# how to set path to RDS most easily? 

#############################################################

# calls inputs for imputation 

# set path to save output rasters
# this directory will be created if it does not already exist
# raw_outputs_dir <- glue::glue('{home_dir}/03_Outputs/07_Projects/{project_name}/01_Raw_model_outputs/')
#tmp_output_dir <- "C:/Users/abhinavshrestha/OneDrive - USDA/Documents/02_TreeMap/temp_dir" # for testing

# Load 
#-----------------------------------------------#
# Id where THIS script is located
this_proj <- this.path::this.proj()

# get path to imputation inputs RDS
#inputs_for_imputation <- glue::glue('{this_proj}/gtac_production_scripts/03_Imputation/00b_zonal_inputs_for_imp.R')
#source(inputs_for_imputation)

# General inputs
#--------------------------------------------------#

# set location of raster attribute table
rat_path <- glue::glue("{home_dir}01_Data/01_TreeMap2016_RDA/RDS-2021-0074_Data/Data/")

# set number of digits to round to
round_dig <- 4

# Path to X table used to make model
xtable_path <- glue::glue("{raw_outputs_dir}/xytables/{output_name}_Xdf_bin.csv")


# model to use - supply specific model to pull into imputation, or NA
# if NA, uses default model name and path

#model_path <- '{home_dir}03_Outputs/07_Projects/2016_GTAC_Test/01_Raw_model_outputs/z16/model/z16_2016_GTAC_Test_ntree250_yai_treelist_bin.RDS'

#model_path <- NA


# Directory where disturbance layers live
# If disturbance layers live in the same dir, then NA
#dist_raster_dir <- NA
#dist_raster_dir <- glue::glue("{home_dir}03_Outputs/05_Target_Rasters/v2016_GTAC/")

# disturbance type - options are "LF" or "LFLCMS".
# This param only used if !is.na(dist_raster_dir)
#dist_layer_type <- "LF"
#dist_layer_type <- "LCMS"

# target data version to use
#target_data_version <- "v2016_RMRS"

# reference data version to use
#ref_data_version <- "v2016_RMRS"

# Directory where EVT_GP remap table is located
#evt_gp_remap_table_dir <- target_dir
#evt_gp_remap_table_dir <- glue::glue('{home_dir}03_Outputs/05_Target_Rasters/v2016_GTAC/')

# path to evt_gp metadata
#evt_path <- glue::glue("{home_dir}01_Data/02_Landfire/LF_200/EVT/LF2016_EVT_200_CONUS/CSV_Data/LF16_EVT_200.csv")

# path to coords
#coords_path <- glue::glue("{home_dir}01_Data/04_FIA/06_Coordinates/select_TREEMAP2022_2send/select_TREEMAP2022_2send.csv")

# Directory where target rasters live
#target_dir <- glue::glue("{home_dir}03_Outputs/05_Target_Rasters/{target_data_version}/")


# Create directories
#----------------------------------------------#

# create eval dir if necessary
if(!file.exists(glue::glue('{eval_dir}/01_OOB_Evaluation/figs/'))) {
  dir.create(glue::glue('{eval_dir}/01_OOB_Evaluation/figs/'), recursive = TRUE)
}

# create eval dir if necessary
if(!file.exists(glue::glue('{eval_dir}/02_Target_Layer_Comparison'))) {
  dir.create(glue::glue('{eval_dir}/02_Target_Layer_Comparison'), recursive = TRUE)
}

# create eval dir if necessary
if(!file.exists(glue::glue('{eval_dir}/03_FIA_Comparison/figs/'))) {
  dir.create(glue::glue('{eval_dir}/03_FIA_Comparison/figs/'), recursive = TRUE)
}

# create eval dir if necessary
if(!file.exists(glue::glue('{eval_dir}/04_Eval_Reports'))) {
  dir.create(glue::glue('{eval_dir}/04_Eval_Reports'), recursive = TRUE)
}