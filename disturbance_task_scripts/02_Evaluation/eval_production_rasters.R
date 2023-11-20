# Evaluate GTAC Rasters vs Karin Riley rasters

# Idea: that this script can be re-purposed in multiple contexts 
# E.g., to compare: 
# - GTAC 2016 Disturbance vs Karin Riley 2016 Disturbance
# - GTAC LCMS 2016 Disturbance vs Karin Riley 2016 Disturbance
# - GTAC Landfire Target Rasters vs. Karin Riley Target Rasters

# - MAKE THIS INTO A FUNCTION ? 

# Written by: Lila Leatherman (lila.leatherman@usda.gov)
# Redcastle Resources and USFS Geospatial Technology and Applications Center (GTAC)
# Last updated: 11/17/2023

###############################################
# USER INPUTS

# set home dir
home_dir <- "//166.2.126.25/TreeMap/"

# set path to desired projection
proj_path <- "//166.2.126.25/TreeMap/01_Data/02_Landfire/landfire_crs.prj"

# set tmp directory
tmp_dir <- "D:/tmp/"

# set path to "reference" raster
ref_raster <- ""

# set path to "predicted" raster
pred_raster <- ""

# set path to points used for extraction, if desired
# else, NA
points <- ""

#####################
# SETUP
######################

# Packages and functions
#---------------------------------#

# packages required
list.of.packages <- c("terra", "tidyverse", "magrittr", "glue", "tictoc", "caret")

#check for packages and install if needed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages)

# load all packages
vapply(list.of.packages, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)

# make 'notin' function
`%notin%` <- Negate('%in%')

# Temp directories 
#----------------------------------#

# check if tmp directory exists 
if (file.exists(tmp_dir)){
  
} else {
  # create a new sub directory inside the main path
  dir.create(tmp_dir)
  
}

# set temp directory - helps save space with R terra
write(paste0("TMPDIR = ", tmp_dir), file=file.path(Sys.getenv('R_USER'), '.Renviron'))
#empty temp dir
do.call(file.remove, list(list.files(tmp_dir, full.names = TRUE)))
#remove unused memory
gc()

# Terra options
# --------------------------------#

#increase memory fraction available
terraOptions(memfrac = 0.8)

###################################################
# LOAD DATA
###################################################

#load landfire projection
crs <- crs(proj_path)

# load reference raster

# load predicted raster

# ensure rasters are in the same projection

###################################################
# Evaluation - confusion matrix
##################################################

# extract values to points

# confusion matrix

# process confusion matrix outputs into table 

# export confusion matrix results 
