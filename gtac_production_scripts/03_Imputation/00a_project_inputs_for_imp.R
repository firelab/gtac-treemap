### This script is used to set inputs for all steps in the imputation process
## To ensure that all scripts refer to the same input and output products

# Written by: Lila Leatherman (lila.leatherman@usda.gov)

# Last updated: 6/17/24

###########################################################################
# Set inputs
###########################################################################

# Project name - name for overarching folders
project_name <- "2016_GTAC_Test" 

# name for products - includes params here if desired
#e.g., #output_name <- "2016_GTAC_LCMSDist"
output_name <- "2016_GTAC_Test"  

# target data version to use
target_data_version <- "v2016_GTAC"

# reference data version to use
ref_data_version <- "v2016_RMRS"

# disturbance type - options are "LF" or "LFLCMS"
dist_layer_type <- "LF"

# # output crs - desired crs for output products
# #options include: "lcms_crs", "landfire_crs", "tm16_crs"
output_crs_name <- "tm16_crs"

#set tmp directory
tmp_dir <- "D:/tmp/"

####################################

# Plot coordinates - relative to FIA_dir
coords_path <- '/06_Coordinates/select_TREEMAP2022_2send/select_TREEMAP2022_2send.csv'

# Path to X table - relative to home_dir
xtable_path <- glue::glue("/03_Outputs/06_Reference_Data/{ref_data_version}/X_table_all_singlecondition.txt")

# Make RDS of input parameters used
#---------------------------------------------------------#

# Export to scripts folder for easy access 
# over-writes by default
save(list = ls(), file = glue::glue('{this.path::this.dir()}/params/{project_name}_imputation_inputs.RDS'))

