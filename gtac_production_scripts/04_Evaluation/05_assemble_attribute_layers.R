### Generate attribute layers, for single zone, for evaluation and inspection
### Load RAT and build specific attribute layers of interest prior to final attribute layer generation

# This script does the following: 

# - Load preliminary imputation output rasters, with TM_ID
# - Load RAT as a lookup table
# - For specific attributes of interest, assemble attribute layers as raster layers 
# - Export attribute rasters

##########################################################
# SET INPUTS
##########################################################

# Specific inputs
#-----------------------------------------------------#
#attributes_export <- c("TPA_DEAD", "TPA_LIVE", "TPA_DEAD_LIVE_RATIO")
attributes_export <- c("TPA_DEAD")


# Set inputs manually - if running standalone
#-----------------------------------------------------#

standalone <- "N"
# cur_zone_zero_standalone <- "z08"
# year_standalone <- 2022
# project_name_standalone <- glue::glue("{year_standalone}_Production_newXtable")



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

load_RAT(rat_path, 
         CN_column = "PLT_CN", 
         ID_column = "TM_ID")


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




