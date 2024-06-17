### This script is used to set inputs for all steps in the imputation process
## To ensure that all scripts refer to the same input and output products

# Written by: Lila Leatherman (lila.leatherman@usda.gov)

# Last updated: 6/17/24

###########################################################################
# Set inputs
###########################################################################

# Project name - name for overarching folders
project_name <- "2016_GTAC_Test" 

# Output name
#e.g., #output_name <- "2016_GTAC_LCMSDist"
output_name <- "2016_GTAC_Test" # name for products - includes params here if desired 

# target data version to use
target_data_version <- "v2016_RMRS"

# reference data version to use
ref_data_version <- "v2016_RMRS"

# disturbance type - options are "LF" or "LFLCMS".
# This param only used if !is.na(dist_raster_dir)
dist_layer_type <- "LFLCMS"

# # output crs - desired crs for output products
# #options include: "lcms_crs", "landfire_crs", "tm16_crs"
output_crs_name <- "tm16_crs"

#set tmp directory
tmp_dir <- "D:/tmp/"

####################################

# Make RDS of input parameters used
#---------------------------------------------------------#

# Export to scripts folder for easy access 
# over-writes by default
save(list = ls(), file = glue::glue('{this.path::this.dir()}/params/{project_name}_imputation_inputs.RDS'))

