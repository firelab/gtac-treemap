# v2: 
# - get slow loss, fast loss, and stable from LCMS into one layer
# - outputs are in lcms projection

# load necessary packages
library(terra)
library(tidyverse)

# Start the clock!
ptm <- proc.time()

############################
# USER INPUTS
############################

# give name for outputs 
zone_name <- "LF_z15_Mogollon_Rim" 
#zone_name <- "UT_Uintas_subset"

# select landfire zone
zone_num <- 15

#select year range (LCMS available for 1985-2021)
start_year <- 2010
end_year <- 2016

# set tmp directory
tmp_dir <- "E:/tmp"

# set home dir
home_dir <- ("//166.2.126.25/TreeMap/")

# get path to change rasters - LCMS
lcms_dir <- ("//166.2.126.227/lcms/Projects/11_LCMS_NationalProduct/06_DataDelivery/Conterminous_United_States/v2021-7/Change/Annual/")

# path to 2016 treemap data
treemap_path <- "//166.2.126.25/TreeMap/01_Data/01_TreeMap2016_RDA/RDS-2021-0074_Data/Data/TreeMap2016.tif"

# aoi path - if different from landfire zone
# supply path, or NA
#aoi_path <- paste0(home_dir, "01_Data/03_AOIs/UT_Uintas_rect_NAD1983.shp")
aoi_path <- NA

# determine whether to produce eval dataset with other LCMS values
# takes Y or N
eval <- "N"

#####################
# SETUP
######################

# set desired end crs 
#crs <- crs("epsg:5070") # tree map output data is in  NAD83 Albers

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

# get desired crs from LCMS
crs <- crs(lcms)

#####################
###### PREP AOI
#####################

# load LF zone data
LF_zones <- vect(paste0(home_dir, "01_Data/02_Landfire/LF_zones/Landfire_zones/refreshGeoAreas_041210.shp"))

# inspect
#LF_zones$ZONE_NAME

# select single LF zone
zone <- subset(LF_zones, LF_zones$ZONE_NUM == zone_num) #z16 = Utah High Plateaus

if (!is.na(aoi_path)) {
  # load aoi subset - utah uintas only
  aoi <- vect(aoi_path)
  
  # reassign
  zone <- aoi
} else{}


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

# #inspect
# tree_mask
# crs(tree_mask, describe = TRUE)
# plot(tree_mask)
# summary(tree_mask)
# freq(tree_mask)
# crs(lcms, describe = TRUE)

# clear memory
gc()

#####################
####### PREP DESTINATION RASTER
#####################

# create empty raster to append data into
r <- rast(crs = crs, ext(tree_mask), res = res(tree_mask))
r <- setValues(r, 0)
r <- mask(r, tree_mask)
#names(r) <- c("slowLoss")

# create new empty raster to use for eval
r_eval <- r

# #inspect
# freq(r)

#create year range
year_list <- seq(start_year, end_year, 1)

#####################
###### ITERATE OVER YEARS
#####################

for(i in 1:length(year_list)){

  #for testing
  #i = 1
  
  # iterate through change rasters by year
  year <- year_list[i]
  
  print(paste0("working on ", year))
  
  #load annual lcms change raster
  lcms <- terra::rast(paste0(lcms_dir, "LCMS_CONUS_v2021-7_Change_", year, ".tif"))
  
  # get crop zone into same projection -- easier than reproj for lcms
  zone <- project(zone, crs(lcms)) 
  
  #crop lcms change raster
  print("cropping")
  lcms <- crop(lcms, zone)
  lcms <- mask(lcms, zone)
  
  #get lcms layer and crop zone into the same projection
  #lcms <- project(lcms, crs)
  #zone <- project(zone, crs)
  
  #inspect
  #freq(lcms)
  
  #set values to reclassify 
  # classes are ordered from 1-n in order: Stable, Slow Loss, Fast Loss, Gain, NP Area Mask
  no.class.val.slowloss <- c(1,3,4,5,NA) # keep only slow loss
  no.class.val.eval <- c(4,5,NA) # keep: stable, slow loss, fast loss
  
  if (eval =="Y") {
    
    ####### FIRST set up eval data set
    # includes slow loss, fast loss, and stable -- one layer for each year 
    lcms.eval <- terra::classify(lcms, cbind(no.class.val.eval,NA))
    
    # mask with tree mask
    lcms.eval_mask <- mask(lcms.eval, tree_mask)
    
    # update NAs to 0
    lcms.eval_mask <- subst(lcms.eval_mask, NA, 0)
    
    #update name of layer
    names(lcms.eval_mask) <- year
    
    # for eval: add all layers, organize by year
    r_eval <- c(r_eval, lcms.eval_mask)
    
    
    ####### SECOND, use eval data set to derive slow loss layer
    # save computational space
    lcms.slowloss <- terra::classify(lcms.eval, cbind(no.class.val.slowloss,NA))
    
    
  } else if (eval == "N") {
    
    # set no data values based on above inputs
    print("classifying slow loss")
    lcms.slowloss <- terra::classify(lcms, cbind(no.class.val.slowloss,NA))
    
  }
  
  ## Slow loss layer processing
  
  #reclass all to 1 for slow loss
  print("reclassifying to binary")
  lcms.slowloss <- terra::subst(lcms.slowloss, 2, 1)
   
  # mask with tree mask
  print("applying tree mask")
  lcms.slowloss_mask <- mask(lcms.slowloss, tree_mask)
  
  #update nas to 0
  print("updating nas to 0")
  lcms.slowloss_mask <- subst(lcms.slowloss_mask, NA, 0)
  
  # #inspect
  # freq(lcms.slowloss_mask)
  # 
  # # #inspect - make sure mask worked
  # freq(lcms)
  # plot(tree_mask, col = "green")
  # plot(subst(lcms.slowloss_mask, 0, NA), add = TRUE)
  # inspect <- tree_mask + (subst(lcms.slowloss_mask, NA, 0)*10)
  # freq(inspect)
  
  # where the value of slow loss = 1, change value to value of year 
  # each year: update all px so that most recent slow loss is recorded 
  print("adding to previous years")
  r <- r + lcms.slowloss_mask
  r <- subst(r, 1, year)
  
  # remove files to save space
  rm(lcms.eval, lcms.slowloss,
     lcms.eval_mask, lcms.slowloss_mask)
  gc()

 }

#inspect
r
r_eval

if (eval == "Y") {
  
  # remove empty first raster
  r_eval <- r_eval[[2:length(year_list)]]
  
  # add layer names to raster 
  #names(r_eval) <- year_list
  
  #update 0s to NA
  r_eval <- subst(r_eval, 0, NA)
  
  # #inspect
  # r_eval
  # freq(r_eval)
  # plot(r_eval)
  
} else if (eval == "N") {
  
  names(r) <- c("slowloss")
  
  #update 0s to NA
  r <- subst(r, 0, NA)
  
  # #inspect
  # r
  # freq(r)
  # plot(r)
}


# export
#exportname <- paste0()
writeRaster(r, paste0(home_dir, "03_Outputs/01_LCMS_Slow_Loss/01_Rasters/01_SlowLoss/", start_year, "_", end_year, "_", zone_name ,"_LCMS_SlowLoss",   ".tif"),
            overwrite = TRUE)


writeRaster(r_eval, paste0(home_dir, "03_Outputs/01_LCMS_Slow_Loss/01_Rasters/02_Eval/", start_year, "_", end_year, "_", zone_name ,"_LCMS_SlowFastStableEval",   ".tif"),
            overwrite = TRUE)

# Stop the clock
proc.time() - ptm
