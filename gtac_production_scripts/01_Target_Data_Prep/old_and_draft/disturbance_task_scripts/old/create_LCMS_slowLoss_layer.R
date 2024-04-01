# load necessary packages
library(terra)
library(tidyverse)

# set home dir
home_dir <- ("//166.2.126.25/TreeMap/")

# set desired end crs (currently: matches tree map output data)
crs <- crs("epsg:4269")

# load LF zone data
LF_zones <- vect(paste0(home_dir, "01_Data/02_Landfire/LF_zones/Landfire_zones/refreshGeoAreas_041210.shp"))

# inspect
#LF_zones$ZONE_NAME

# select single LF zone
zone <- subset(LF_zones, LF_zones$ZONE_NAME == "Utah High Plateaus")

# create empty raster
r <- rast(crs = crs, ext(zone))

# get path to change rasters - LCMS
lcms_dir <- ("//166.2.126.227/lcms/Projects/11_LCMS_NationalProduct/06_DataDelivery/Conterminous_United_States/v2021-7/Change/Annual/")

# iterate through change rasters by year
year <- 2020

#load lcms change raster
lcms <- terra::rast(paste0(lcms_dir, "LCMS_CONUS_v2021-7_Change_", year, ".tif"))

# get crop zone into same projection -- easier than reproj
r <- project(r, crs(lcms)) 

#crop lcms change raster
lcms <- crop(lcms, r)

#load tree mask - use from tree map
tree_mask <- rast("//166.2.126.25/TreeMap/01_Data/01_TreeMap2016_RDA/RDS-2021-0074_Data/Data/TreeMap2016.tif")

# get crop zone into same projection -- easier than reproj
r <- project(r, crs(tree_mask)) 

#crop tree mask
tree_mask <- crop(tree_mask, r)

#get tree mask and lcms data into the same projection
lcms <- project(lcms, crs(tree_mask))

#inspect
freq(lcms)

#set values to reclassify -- keep only Slow Loss
# classes are ordered from 1-n in order: Stable, Slow Loss, Fast Loss, Gain, NP Area Mask
no.class.val <- c(1,3,4,5,NA)

# set no data values based on above inputs
lcms <- terra::classify(lcms, cbind(no.class.val,NA))

#inspect
freq(lcms)

  # where the value of slow loss = paste0(year,2) to match 
  # each year: update all px so that most recent slow loss is recorded 


# save for LF zone
