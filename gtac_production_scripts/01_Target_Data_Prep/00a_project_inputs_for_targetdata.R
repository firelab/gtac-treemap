### This script is used to set inputs for all steps in the target data prep process
## To ensure that all scripts refer to the same input and output products

# Written by Lila Leatherman (lila.leatherman@usda.gov)

# Last updated: 6/17/24

# TO DO:
# - add a switch for LCMS+LF disturbance vs LF only disturbance? 

###########################################################################
# Set inputs
###########################################################################

# General inputs - specific to each project
#-----------------------------------------------#

#project name
project_name <- "2016_GTAC_Test"

# target data version
target_data_version <- "v2016_GTAC"

# set year range
start_year <- 1999
end_year <- 2016

# set current modeling year (for years since disturbance)
model_year <- end_year

#build year list
year_list <- seq(start_year, end_year, 1)

# set tmp directory
tmp_dir <- "D:/tmp"

# set landfire version 
landfire_version_veg <- 200
landfire_year_veg <- 2016
landfire_version_topo <- 220
landfire_year_topo <- 2020

# Make RDS of input parameters used
#---------------------------------------------------------#

# Export to scripts folder for easy access 
# over-writes by default
save(list = ls(), file = glue::glue('{this.path::this.dir()}/params/{target_data_version}_target_data_inputs.RDS'))

