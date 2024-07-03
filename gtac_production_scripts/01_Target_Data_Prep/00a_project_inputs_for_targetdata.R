### This script is used to set inputs for all steps in the target data prep process
## To ensure that all scripts refer to the same input and output products

# Written by Lila Leatherman (lila.leatherman@usda.gov)

# Last updated: 7/2/24

# TO DO:
# - add a switch for LF only disturbance vs LCMS+LF disturbance ? 

###########################################################################
# Set inputs
###########################################################################

# General inputs - specific to each project
#-----------------------------------------------#

#project name
project_name <- "2020_GTAC_Test"

# target data version
target_data_version <- "v2020_GTAC"

# set year range
start_year <- 1999
end_year <- 2020

# set current modeling year (for years since disturbance)
model_year <- end_year

#build year list
year_list <- seq(start_year, end_year, 1)

# set tmp directory
tmp_dir <- "D:/tmp"

# set landfire version 
landfire_version_veg <- 220
landfire_year_veg <- 2020
landfire_version_topo <- 220
landfire_year_topo <- 2020

# set landfire crs to use
lf_crs_version <- "lf220_crs" # options include: "lf200_crs", "lf220_crs", "lf230_crs"

# Make RDS of input parameters used
#---------------------------------------------------------#

# Export to scripts folder for easy access 
# over-writes by default
save(list = ls(), file = glue::glue('{this.path::this.dir()}/params/{target_data_version}_target_data_inputs.RDS'))

