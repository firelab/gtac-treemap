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

year <- year_input

#project name
project_name <- glue::glue("{year}_Production")
# project_name <- "DistLayerPrep_GTAC_test" #for testing


# target data version
target_data_version <- glue::glue("v{year}_GTAC")

# set year range
start_year <- 1999
end_year <- year

# set current modeling year (for years since disturbance)
model_year <- as.integer(end_year)

#build year list
year_list <- seq(start_year, end_year, 1)

# SET a 'tmp' directory to hold temporary files
# tmp_dir <- "D:/tmp" # directory specified in "setup_dirs.R" script

# set landfire version

# TOPO
landfire_version_topo <- 220
landfire_year_topo <- 2020

# VEG
LFveg_yearDict <- list("2020" = 220, 
                        "2022" = 230)

landfire_year_veg <- year
landfire_version_veg <- LFveg_yearDict[[as.character(landfire_year_veg)]]

# Make RDS of input parameters used
#---------------------------------------------------------#

# Export to scripts folder for easy access
# over-writes by default
save(list = ls(), file = glue::glue('{this.path::this.dir()}/params/{target_data_version}_target_data_inputs.RDS'))
