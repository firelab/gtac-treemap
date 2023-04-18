# v1: first try

# load necessary packages
library(terra)
library(tidyverse)

# Start the clock!
ptm <- proc.time()

############################
# USER INPUTS
############################

# select single landfire zone 
zone_name <- "LF_z16_UT_High_Plateaus" 

#select year range (LCMS available for 1985-2021)
start_year <- 2010
end_year <- 2021

# set tmp directory
tmp_dir <- "E:/tmp"

# set home dir
home_dir <- ("//166.2.126.25/TreeMap/")

# get path to change rasters - LCMS
lcms_dir <- ("//166.2.126.227/lcms/Projects/11_LCMS_NationalProduct/06_DataDelivery/Conterminous_United_States/v2021-7/Change/Annual/")

# path to 2016 treemap data
treemap_path <- "//166.2.126.25/TreeMap/01_Data/01_TreeMap2016_RDA/RDS-2021-0074_Data/Data/TreeMap2016.tif"

#####################
# SETUP
######################

# set desired end crs 
crs <- crs("epsg:5070") # tree map output data is in  NAD83 Albers

# set temp directory - helps save space with R terra
write(paste0("TMPDIR = ", tmp_dir), file=file.path(Sys.getenv('R_USER'), '.Renviron'))
#empty temp dir
do.call(file.remove, list(list.files(tmp_dir, full.names = TRUE)))
#remove unused memory
gc()

#####################
# LOAD DATA
######################

#load any lcms change raster - to get spatial specs; doesn't load values into memory yet
lcms <- terra::rast(paste0(lcms_dir, "LCMS_CONUS_v2021-7_Change_2020.tif"))

#####################
###### PREP AOI
#####################

# load LF zone data
LF_zones <- vect(paste0(home_dir, "01_Data/02_Landfire/LF_zones/Landfire_zones/refreshGeoAreas_041210.shp"))

# inspect
#LF_zones$ZONE_NAME

# select single LF zone
zone <- subset(LF_zones, LF_zones$ZONE_NUM == 16) #z16 = Utah High Plateaus

# load aoi subset - utah uintas only
aoi <- vect(paste0(home_dir, "01_Data/03_AOIs/UT_Uintas_rect_NAD1983.shp"))

# reassign
zone <- aoi

# project
zone <- project(zone, crs)


#####################
####### PREP TREE MASK
#####################

# load tree mask - pre-existing tree map data
tree_mask <- terra::rast(treemap_path)

# get crop zone into same projection
zone <- project(zone, crs(tree_mask)) 

#crop tree mask
tree_mask <- crop(tree_mask, zone)

#reclassify tree map input to binary tree mask
#reclassify so that areas with a CN go to 1
m <- c(0, 140393888010690, 1)
m <- matrix(m, ncol = 3, byrow= TRUE)
tree_mask <- terra::classify(tree_mask, m)

# get tree mask into desired projection
tree_mask <- project(tree_mask, crs, method = "near")

#inspect
plot(tree_mask)
summary(tree_mask)
freq(tree_mask)


#####################
####### PREP DESTINATION RASTER
#####################

# create empty raster to append data into
r <- rast(crs = crs, ext(tree_mask), res = res(tree_mask))
r <- setValues(r, 0)
r <- mask(r, tree_mask)
names(r) <- c("slowLoss")

#inspect
freq(r)

#create year range
year_list <- seq(start_year, end_year, 1)

#####################
###### ITERATE OVER YEARS
#####################

for(i in 1:length(year_list)){

  # iterate through change rasters by year
  year <- year_list[i]
  #year <- 2020
  
  print(paste0("working on ", year))
  
  #load annual lcms change raster
  lcms <- terra::rast(paste0(lcms_dir, "LCMS_CONUS_v2021-7_Change_", year, ".tif"))
  
  # get crop zone into same projection -- easier than reproj for lcms
  zone <- project(zone, crs(lcms)) 
  
  #crop lcms change raster
  lcms <- crop(lcms, zone)
  
  #get lcms layer and crop zone into the same projection
  lcms <- project(lcms, crs)
  zone <- project(zone, crs)
  
  #inspect
  #freq(lcms)
  
  #set values to reclassify -- keep only Slow Loss
  # classes are ordered from 1-n in order: Stable, Slow Loss, Fast Loss, Gain, NP Area Mask
  no.class.val <- c(1,3,4,5,NA)
  
  # set no data values based on above inputs
  lcms <- terra::classify(lcms, cbind(no.class.val,NA))
  
  #reclass all to 1
  lcms <- terra::subst(lcms, 2, 1)
  
  #inspect
  #freq(lcms)
  
  # mask with tree mask
  lcms_mask <- mask(lcms, tree_mask)
  
  #update nas to 0
  lcms_mask <- subst(lcms_mask, NA, 0)
  
  #inspect
  #freq(lcms_mask)
  
  # #inspect - make sure mask worked
  # freq(lcms)
  # plot(tree_mask)
  # plot(tree_mask, col = "green")
  # plot(lcms, col = "firebrick", add = TRUE)
  # inspect <- tree_mask + lcms
  # freq(inspect)
  
    # where the value of slow loss = 1, paste0(year,1) to match 
    # each year: update all px so that most recent slow loss is recorded 
  
  r <- r + lcms_mask
  r <- subst(r, 1, year)
  

 }



#update 0s to NA
r <- subst(r, 0, NA)

#inspect
freq(r)
plot(r)

# export
#exportname <- paste0()
writeRaster(r, paste0(home_dir, "03_Outputs/01_LCMS_Slow_Loss/01_Rasters/", start_year, "_", end_year, "_", zone_name ,"_LCMS_SlowLoss",   ".tif"),
            overwrite = TRUE)

# Stop the clock
proc.time() - ptm
