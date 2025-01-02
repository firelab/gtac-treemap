### This script is used to set inputs for all steps in the imputation process
## To ensure that all scripts refer to the same input and output products

# Written by: Lila Leatherman (lila.leatherman@usda.gov)

# Last updated: 12/17/24

# TO DO: what target data params can i pull in from target data RDS? 

###########################################################################
# Set user inputs
###########################################################################

# zone to run
zone = zone_input

# path to an RDS file containing parameters, or NA - NA runs 00a_project_inputs_for_imp.R
# path is relative to script location
#imputation_params_path <- glue::glue("/params/{project_name}_imputation_inputs.RDS")

# model to use - supply path specific model to pull into imputation, or NA
# path should be relative to home directory
# if NA, uses default model name and path
#model_path <- '/03_Outputs/07_Projects/2016_GTAC_Test/01_Raw_model_outputs/z16/model/z16_2016_GTAC_Test_ntree250_yai_treelist_bin.RDS'
model_path <- NA

# Test application settings
#-----------------------------------------#

# # supply path to a shapefile to use as subset, or NA
#aoi_path <- "//166.2.126.25/TreeMap/01_Data/03_AOIs/UT_Uintas_rect_NAD1983.shp"
#aoi_name <- "UT_Uintas_rect"
aoi_path <- NA
aoi_name <- NA

##################################################################
# Run
##################################################################

# # Load pre-existing params, if available
# #--------------------------------------------#
# 
# this_dir <- this.path::this.dir()
# 
# if(!is.na(imputation_params_path)) {
#   
#   # load params
#   params_script_path <- glue::glue('{this_dir}/{imputation_params_path}')
#   load(params_script_path)
#   
# } else {
#   
#   inputs_for_imputation<- glue::glue('{this_dir}/00a_project_inputs_for_imputation.R')
#   source(inputs_imputation)
#   
# }


##################################################################
# CREATE ZONE-SPECIFIC VARIABLES AND PATHS
##################################################################

# Prep zone
#-----------------------------------------#

zone_num <- zone

# Set zone identifiers 
cur_zone <- glue::glue('z{zone_num}') 
cur_zone_zero <- if(zone_num < 10) {
  glue::glue('z0{zone_num}') } else {
    cur_zone
  }

# Load zone metadata
#----------------------------------------#

# load zone metadata
LF_zone_metadata <- read.csv(zone_metadata_path)

# identify which geographic area zone is in 
study_area <- LF_zone_metadata %>%
  dplyr::filter(ZONE_NUM == zone_num) %>%
  dplyr::select(STUDY_AREA) %>%
  toString()

# conditionally set path to zones and projection, based on map area
if(study_area == "CONUS") {
  zones_path = lf_zones_path_CONUS
  zone_output_crs = default_crs
  file_pattern = "US"
} else if(study_area == "AK") {
  zones_path = lf_zones_path_AK
  zone_output_crs = ak_crs
  file_pattern = study_area
} else if(study_area == "HI") {
  zones_path = lf_zones_path_HI
  zone_output_crs = hi_crs
  file_pattern = study_area
}


# Update names and dirs with zone
# -----------------------------------------#

output_name <- glue::glue("{cur_zone_zero}_{output_name}")  

xtable_path = glue::glue("{xtable_dir}/x_table_{zone_num}.csv")

target_dir = glue::glue("{target_dir}/{cur_zone_zero}")
raw_outputs_dir = glue::glue("{raw_outputs_dir}/{cur_zone_zero}/")
params_dir <- glue::glue("{raw_outputs_dir}/params/")
assembled_dir = glue::glue("{assembled_dir}/{cur_zone_zero}")
eval_dir <- glue::glue("{eval_dir}{cur_zone_zero}")

tile_dir <- glue::glue("{raw_outputs_dir}raster/tiles/")
model_dir = glue::glue("{raw_outputs_dir}/model/")

evt_gp_remap_table_path = glue::glue("{evt_gp_remap_table_dir}/{cur_zone_zero}/evt_gp_remap.csv")
params_dir = glue::glue("{raw_outputs_dir}/params/")


# Model inputs / outputs
#----------------------------------#

# build default model path
if(is.na(model_path)) {
  model_path1 <- glue::glue('{output_name}_yai_treelist_bin')
  
  # Path where model is located
  # This path will be used for export and import
  model_path <- glue::glue('{raw_outputs_dir}/model/{model_path1}.RDS')
  
  rm(model_path1)
  
}

# path to save x and y tables used in model
xtable_path_model <- glue::glue("{raw_outputs_dir}/xytables/{output_name}_Xdf_bin.csv")
ytable_path_model <- glue::glue("{raw_outputs_dir}/xytables/{output_name}_Ydf_bin.csv")


# Create all directories
# ----------------------------------#

# output dir
if (!file.exists(raw_outputs_dir)) {
  dir.create(raw_outputs_dir, recursive = TRUE)
}

#create output directory
if(!file.exists(glue('{raw_outputs_dir}/xytables/'))){
  dir.create(glue('{raw_outputs_dir}/xytables/'))
}

#create output directory
if(!file.exists(glue('{raw_outputs_dir}/model'))){
  dir.create(glue('{raw_outputs_dir}/model'))
}

#create output directory
if(!file.exists(glue('{raw_outputs_dir}/model_eval'))){
  dir.create(glue('{raw_outputs_dir}/model_eval'))
}

#create output directory
if(!file.exists(glue('{raw_outputs_dir}/params'))){
  dir.create(glue('{raw_outputs_dir}/params'))
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

# yai-model evaluation outputs
if(!file.exists(glue::glue('{eval_dir}/00_Model_Evaluation'))) {
  dir.create(glue::glue('{eval_dir}/00_Model_Evaluation'), recursive = TRUE)
}

# Target Layer COmparison Outputs
if(!file.exists(glue::glue('{eval_dir}/01_Target_Layer_Comparison'))) {
  dir.create(glue::glue('{eval_dir}/01_Target_Layer_Comparison'), recursive = TRUE)
}

# OOB outputs
if(!file.exists(glue::glue('{eval_dir}/02_OOB_Manual_Evaluation/figs/'))) {
  dir.create(glue::glue('{eval_dir}/02_OOB_Manual_Evaluation/figs/'), recursive = TRUE)
}

# CV outputs
if(!file.exists(glue::glue('{eval_dir}/03_Cross_Validation/figs/'))) {
  dir.create(glue::glue('{eval_dir}/03_Cross_Validation/figs/'), recursive = TRUE)
}

# Evaluation reports
if(!file.exists(glue::glue('{eval_dir}/04_Eval_Reports'))) {
  dir.create(glue::glue('{eval_dir}/04_Eval_Reports'), recursive = TRUE)
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


# Remove unused objects
#------------------------------------------------#
params_out <- data.frame(
  project_name,
  output_name,
  cur_zone_zero,
  aoi_name,
  target_data_version,
  ref_data_version,
  model_path,
  xtable_path,
  zone_output_crs
  ) %>%
    bind_rows() %>%
    t() %>%
    data.frame() %>%
    rownames_to_column() %>%
    rename(value = ".",
           param = "rowname") %>%
    select(param, value)

# write out params used as table
####################################
write.csv(params_out, 
          glue::glue('{params_dir}/{output_name}_paramsTable.csv'))

rm(params_out)

# Make RDS of input parameters used
#---------------------------------------------------------#
save(list = ls(), file = glue::glue('{params_dir}/{output_name}_env.RDS'))




