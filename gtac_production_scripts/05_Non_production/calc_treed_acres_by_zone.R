# Calculate treed acres for each landfire zone

# load libraries
library(tidyverse)
library(terra)
library(magrittr)

# set inputs
#-------------------------------#

# home dir
# Initialize home dir
#-----------------------------------------------#
# Id where THIS script is located
this.path <- this.path::this.path()

# get path to input script
spl <- stringr::str_split(this.path, "/")[[1]]
setup_dirs.path <- paste( c(spl[c(1:(length(spl)-2))],
                              "00_Library/setup_dirs.R" ),
                            collapse = "/")

source(setup_dirs.path)

# data directory - where source data are located
data_dir <- glue::glue('{home_dir}/01_Data/')


landfire_version_veg <- 200
landfire_year_veg <- 2016

# export path
area_export_path <- glue::glue("{home_dir}/01_Data/02_Landfire/metadata/forest_area_by_zone_{landfire_year_veg}.csv")

# set path to landfire rasters 
landfire_veg_dir <- glue::glue('{data_dir}02_Landfire/LF_{landfire_version_veg}/')

# set path to landfire vector data
lf_zones_path <- glue::glue('{data_dir}/02_Landfire/LF_zones/Landfire_zones/refreshGeoAreas_041210.shp')

# path to projection used for processing landfire rasters
landfire_proj <- glue::glue('{data_dir}02_Landfire/landfire_crs.prj')

# Paths to specific Landfire rasters - not disturbance
evc_path <- glue::glue('{landfire_veg_dir}/EVC/LF{landfire_year_veg}_EVC_{landfire_version_veg}_CONUS/Tif/LC{substr(landfire_year_veg, 3,4)}_EVC_{landfire_version_veg}.tif')
evh_path <- glue::glue('{landfire_veg_dir}/EVH/LF{landfire_year_veg}_EVH_{landfire_version_veg}_CONUS/Tif/LC{substr(landfire_year_veg, 3,4)}_EVH_{landfire_version_veg}.tif')
evt_path <- glue::glue('{landfire_veg_dir}/EVT/LF{landfire_year_veg}_EVT_{landfire_version_veg}_CONUS/Tif/LC{substr(landfire_year_veg, 3,4)}_EVT_{landfire_version_veg}.tif')

# Load data
#-------------------------------------#

# load evc / evt data
evc <- terra::rast(evc_path)
evt <- terra::rast(evt_path)

# set EVT to display EVT_Gp - many layers
activeCat(evt) <- 7 # evt_gp

# get evt levels - to reclassify evt to evt_gp
evt_levels <- levels(evt)[[1]] %>%
  mutate(EVT_GP = as.numeric(EVT_GP))

# load landfire zones
lf_zones <- terra::vect(lf_zones_path)

# reproject to crs of evc 
lf_zones %<>% terra::project(crs(evc)) %>%
  terra::sort("ZONE_NUM") # arrange by zone number

# Set up variables to reclassify 
# --------------------------------#

# EVC codes that represent forest
# represented as range start, range end, desired end value
evc_forest_codes_mat <- matrix(c(
  -9999,100,NA,
  110,119,15, 
  120,129,25, 
  130,139,35, 
  140,149,45, 
  150,159,55,
  160,169,65,
  170,179,75, 
  180,189,85, 
  190,199,95,
  200,399,NA), 
  nrow = 11, ncol = 3, byrow = TRUE)

evc_forest_codes_bin_mat <- matrix(c(
  -9999,100,0,
  110,199,1, 
  200,399,0), 
  nrow = 3, ncol = 3, byrow = TRUE)

# EVT_GPs that are reclassed to NA
# because there are only a few of these evt_gps
evt_gps_na <- c(
  13,
  14,
  15,
  26,
  60,
  730
)

# make data frame to append info to
forest_area <- data.frame(ZONE_NUM = lf_zones$ZONE_NUM, 
                          pct_forest = rep(as.numeric(NA), length(lf_zones$ZONE_NUM)),
                          acres_forest = rep(as.numeric(NA), length(lf_zones$ZONE_NUM)))

# loop over zones
#for(i in seq_along(lf_zones)) {
for(i in(59:66)) {
  
  # for testing
  #i = 1
  
  #subset to single zone
  zone <- lf_zones[i]
  
  # crop to zone
  forest_mask <- terra::crop(evc, zone, mask = TRUE) %>%
    # reclass evc to forest codes
    terra::classify(evc_forest_codes_bin_mat, right = NA) 
  
  # calc forest area
  freq <- freq(forest_mask)
  
  px_forest <- freq$count[2]
  total_px <- sum(freq$count)
  pct_forest <- (px_forest / total_px) *100
  acres_forest <- px_forest * 0.2223945 # convert to acres
  
  # append to output dataframe
  forest_area$pct_forest[i] = pct_forest
  forest_area$acres_forest[i] = acres_forest

  print(glue::glue("done with zone {i}"))
  gc()
  
  }

# export data frame to Landfire data folder

write.csv(forest_area, area_export_path, row.names = FALSE)
