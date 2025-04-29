# create LF disturbance layer from historic LF disturbances

# v1: first try

# load necessary packages
library(terra)
library(tidyverse)

# Start the clock!
ptm <- proc.time()

############################
# USER INPUTS
############################

# set desired end crs 
crs <- crs("epsg:4269") # tree map output data

# give zone name for export
zone_name <- "UT_Uintas" 

#select year range (Landfire available for 1999-2021)
start_year <- 2016
end_year <- 2020

#####################
# SETUP
######################

#set temp dir 

# set home dir
home_dir <- ("//166.2.126.25/TreeMap/")

# get path to change rasters - landfre
lf_dir <- ("//166.2.126.25/TreeMap/01_Data/02_Landfire/LF_220/Disturbance/")

#load any lf change raster - to get spatial specs; doesn't load values into memory yet
lf <- terra::rast(paste0(lf_dir, "LF2019_Dist_220_CONUS/Tif/LC19_Dist_220.tif"))


#####################
###### PREP AOI
#####################

# load LF zone data
LF_zones <- vect(paste0(home_dir, "01_Data/02_Landfire/LF_zones/Landfire_zones/refreshGeoAreas_041210.shp"))

# inspect
LF_zones$ZONE_NAME

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
tree_mask <- rast("//166.2.126.25/TreeMap/01_Data/01_TreeMap2016_RDA/RDS-2021-0074_Data/Data/TreeMap2016.tif")

# get crop zone into same projection
zone <- project(zone, crs(tree_mask)) 

#crop tree mask
tree_mask <- crop(tree_mask, zone)

# get tree mask into desired projection
tree_mask <- project(tree_mask, crs)

#reclassify tree map input to binary tree mask
#reclassify so that areas with a CN go to 1
m <- c(0, 140393888010690, 1)
m <- matrix(m, ncol = 3, byrow= TRUE)
tree_mask <- terra::classify(tree_mask, m)

#inspect
#summary(tree_mask)
#freq(tree_mask)


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
  
  path_base_2_2016 <- "_Dist_200_CONUS"
  path_base_2_2020 <- "_Dist_220_CONUS"
    
    ##
    ##### Set appropriate path to lf change raster
    ##
    
    if(year_name < 2015){
      
      #create path
      lf_path <- paste0(lf_dir,"USDIST", year_name, "Tif\us_dist", year_name, ".tif")
      
    } else if(year_name >= 2015 & year_name <= 2016) {
      
      #create path
      lf_path <- paste0(lf_dir,"LF", year_name, "_Dist_200_CONUS\Tif\LC", substr(year_name, 3, 4), "_Dist_200.tif")
      
    } else if(year_name > 2016) {
      
      #create path
      lf_path <- paste0(lf_dir,"LF", year_name, "_Dist_220_CONUS\Tif\LC", substr(year_name, 3, 4), "_Dist_220.tif")
      
    }
  
  #load annual lf change raster
  lf <- terra::rast(lf_path)
  
  # get crop zone into same projection -- easier than reproj for lcms
  zone <- project(zone, crs(lf)) 
  
  #crop lf change raster
  lf <- crop(lf, zone)
  
  #get lf layer and crop zone into the same projection
  lf <- project(lf, crs)
  zone <- project(zone, crs)
  
  #inspect
  #freq(lf)
  
  #set values to reclassify -- keep only Slow Loss
  # classes are ordered from 1-n in order: Stable, Slow Loss, Fast Loss, Gain, NP Area Mask
  no.class.val <- c(1,3,4,5,NA)
  
  # set no data values based on above inputs
  lf <- terra::classify(lcms, cbind(no.class.val,NA))
  
  #reclass all to 1
  lf <- terra::subst(lcms, 2, 1)
  
  #inspect
  #freq(lcms)
  
  # mask with tree mask
  lf_mask <- mask(lf, tree_mask)
  
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

#inspect
freq(r)

#update 0s to NA
r <- subst(r, 0, NA)

# export
#exportname <- paste0()
writeRaster(r, paste0(home_dir, "04_Outputs/01_LCMS_Slow_Loss/", start_year, "_", end_year, "_", zone_name ,"_LCMS_SlowLoss",   ".tif"),
            overwrite = TRUE)

# Stop the clock
proc.time() - ptm
