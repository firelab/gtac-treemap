### Generate attribute layers for evaluation and inspection
### Load RAT and build specific attribute layers of interest prior to final attribute layer generation

# This script does the following: 

# - Load preliminary imputation output rasters, with TMID
# - Load RAT as a lookup table
# - For specific attributes of interest, assemble attribute layers as raster layers 
# - Export attribute rasters

##########################################################
# SET INPUTS
##########################################################

# Specific inputs
#-----------------------------------------------------#
#attributes_export <- c("TPA_DEAD", "TPA_LIVE", "TPA_DEAD_LIVE_RATIO")
attributes_export <- c("TPA_DEAD", "disturb_year")


# Set inputs manually - if running standalone
#-----------------------------------------------------#

# cur_zone_zero_standalone <- "z08"
# year_standalone <- 2022
# project_name_standalone <- glue::glue("{year_standalone}_Production_newXtable")
standalone <- "N"


#####################################################################
# Load data
####################################################################

# Set inputs manually - if running standalone
#-----------------------------------------------------#

if(standalone == 'Y') {
  
  # assign main variables
  cur_zone_zero <- cur_zone_zero_standalone
  year <- year_standalone
  
  this_proj <- this.path::this.proj()
  this_dir <- this.path::this.dir()
  
  ## load treemap library
  lib_path = glue::glue('{this_proj}/gtac_production_scripts/00_Library/treeMapLib.R')
  source(lib_path)
  
  #load settings for zone
  zone_settings <- glue::glue("{home_dir}/03_Outputs/07_Projects/{project_name}/01_Raw_model_outputs/{cur_zone_zero}/params/{cur_zone_zero}_{project_name_standalone}_env.RDS")
  
  load(zone_settings)
  
  # load library again in case functions have been updated since initial creation of RDS
  source(lib_path)
}


# - Make export folder for attribute vars - 
dir.create(glue::glue("{assembled_dir}/03_Attribute_vars"))

###########################################################
# LOAD DATA
###########################################################

message("Loading data for attribute layer assembly")

# Imputed raster
#-------------------------------------------#
# name of raster to validate
raster_name <- glue::glue("{output_name}_Imputation")

#load raw imputation output raster
ras <- terra::rast(glue::glue("{assembled_dir}/01_Imputation/{raster_name}.tif"))

# trim off NA values
ras <- terra::trim(ras)

# check projection - an inspection
#crs(ras, describe = TRUE)

#set name of input column
names(ras) <- c("value")

# inspect
ras
plot(ras,
     main = glue::glue("Raw imputed ids: Zone {zone_num}"))

# Load and prep Raster Attribute Table
#-----------------------------------------------------------------#

rat_path <- "//166.2.126.25/TreeMap/03_Outputs/06_Reference_Data/v2020/03_Raster_attributes/TM2020RAT_tmid.csv"

# handle RAT input cases - if it's a TIF (tm2016 we are using a tif input)

# if(str_detect(rat_path, ".tif")) {
#   
#   # load rat
#   rat <- terra::rast(rat_path)
#   rat <- data.frame(cats(rat))
#   
#   # 2016 - specific 
#   if(str_detect(rat_path, "2016")) {
#     
#     rat %<>%
#       # rename shortened field names
#       dplyr::rename("SDIPCT_RMRS" = SDIPCT_RMR,
#                     "CARBON_DOWN_DEAD" = CARBON_DWN) %>%
#       select(-Value)
#   }
#   
  
  
#}

# handle RAT input cases - if it's a CSV

if(str_detect(rat_path, ".csv")) {
  
  # load rat
  rat <- read.csv(rat_path)
  
  rat %<>%
    select(-X) %>%
    dplyr::rename("CN" = PLT_CN) 
  
}


# identify eval_vars_cont that are not from RMRS - we handle NAs differently
eval_vars_cont_RMRS <- stringr::str_subset(names(rat), "RMRS")
eval_vars_cont_nonRMRS <- stringr::str_subset(names(rat %>% dplyr::select(where(is.numeric))), "RMRS", negate = TRUE)

# prep rat table
rat %<>%
  # convert CN to numeric
  dplyr::mutate(CN = as.numeric(CN)) %>%
  # round and fix NA values for RMRS and non RMRS vars
  dplyr::mutate(across(any_of(eval_vars_cont_nonRMRS), ~ round(.x, digits = round_dig))) %>%
  dplyr::mutate(across(any_of(eval_vars_cont_nonRMRS), ~ ifelse(.x == -99.000, 0, .x))) %>%
  dplyr::mutate(across(any_of(eval_vars_cont_RMRS), ~ dplyr::na_if(.x, -99))) %>%
  # calculate TPA_DEAD_LIVE_RATIO
  dplyr::mutate(TPA_DEAD_LIVE_RATIO = TPA_DEAD/TPA_LIVE) #%>%
  # remove columns
  #dplyr::select(-tm_id)


# Load and prep X table
#-----------------------------------------------------------------#

# X table may be used for assembling additional model vars that weren't included in target layer comparison
xtable <- read.csv(xtable_path_model)
xtable <- read.csv("//166.2.126.25/TreeMap/03_Outputs/06_Reference_Data/v2020/01_X_tables_by_zone/x_table_1.csv")

########################################################################
# Assemble from RAT
########################################################################

#assembleExport() function

attributes_export %>%
  map(\(x) assembleExport(x, 
                      raster = ras,
                      lookup = rat,
                      id_field = "TM_ID",
                      export_path = glue::glue('{assembled_dir}/03_Attribute_vars/{output_name}'),
                      plot = TRUE
                      
  ))

########################################################################
# Assemble from X table
########################################################################

#assembleExport() function

attributes_export %>%
  map(\(x) assembleExport(x, 
                          raster = ras,
                          lookup = xtable,
                          id_field = "TM_ID",
                          export_path = glue::glue('{assembled_dir}/03_Attribute_vars/{output_name}'),
                          plot = TRUE
                          
  ))



