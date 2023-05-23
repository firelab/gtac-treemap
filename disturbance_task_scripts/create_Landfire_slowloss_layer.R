# 

# load necessary packages
library(terra)
library(tidyverse)
library(foreach)
library(doParallel)



############################
# USER INPUTS
############################

# # give name for outputs 
# zone_name <- "LF_z16_UT_High_Plateaus" 
# #zone_name <- "LF_z21_MiddleRockyMountains"
# #zone_name <- "UT_Uintas_subset"
# 
# # select landfire zone
# zone_num <- 16

#list landfire zones of interest
zone_list <- c(#15,
               #16,
               #19,
               #21,
               #28
                17,
                18)

#select year range (LCMS available for 1985-2021, Landfire available for 1999-2020)
start_year <- 2010
end_year <- 2016

# set tmp directory
tmp_dir <- "E:/tmp/"

# set home dir
home_dir <- ("//166.2.126.25/TreeMap/")

# get path to change rasters - LCMS
lcms_dir <- ("//166.2.126.227/lcms/Projects/11_LCMS_NationalProduct/06_DataDelivery/Conterminous_United_States/v2021-7/Change/Annual/")

# get path to change rasters - Landfire
landfire_dir <- ("//166.2.126.25/TreeMap/01_Data/02_Landfire/LF_220/Disturbance/")

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
#terraOptions(tempdir = tmp_dir)
#empty temp dir
do.call(file.remove, list(list.files(tmp_dir, full.names = TRUE)))
#remove unused memory
gc()

#increase memory fraction available
terraOptions(memfrac = 0.8)

terraOptions(progress = 1)

#make %notin% function
`%notin%` <- Negate('%in%')

#####################
# LOAD DATA
######################

#load any lcms change raster - to get spatial specs; doesn't load values into memory yet
lcms <- terra::rast(paste0(lcms_dir, "LCMS_CONUS_v2021-7_Change_2020.tif"))

# get desired crs from LCMS
crs <- crs(lcms)

# load LF zone data
LF_zones <- vect(paste0(home_dir, "01_Data/02_Landfire/LF_zones/Landfire_zones/refreshGeoAreas_041210.shp"))

# inspect
#LF_zones$ZONE_NAME

################### 
# LOOP OVER LANDFIRE ZONES
##########################


for (z in 1:length(zone_list)) {
  
  #for testing
  #z <- 1
  
  zone_num <- zone_list[z]
  
  # Start the clock!
  ptm <- proc.time()
  
  # status update
  print(paste0("working on zone ", zone_num))
  
  #####################
  ###### PREP AOI
  #####################
  
  # select single LF zone
  zone <- subset(LF_zones, LF_zones$ZONE_NUM == zone_num) #z16 = Utah High Plateaus
  
  # get name of zone
  zone_name <- paste0("LFz", zone_num, "_", gsub(" ", "", zone$ZONE_NAME))
  
  if (!is.na(aoi_path)) {
    # load aoi subset - utah uintas only
    aoi <- vect(aoi_path)
    
    # reassign
    zone <- aoi
  } else{}
  
  
  # project
  zone <- project(zone, crs)
  
  #####################
  ####### PREP LCMS
  #####################
  
  lcms_crop <- terra::crop(lcms, zone)
  
  
  # #####################
  # ####### PREP TREE MASK
  # #####################
  # 
  # # load tree mask - pre-existing tree map data
  # tree_mask <- terra::rast(treemap_path)
  # 
  # # get crop zone into same projection
  # zone <- terra::project(zone, crs(tree_mask))
  # 
  # #crop tree mask
  # tree_mask <- terra::crop(tree_mask, zone)
  # 
  # #reclassify tree map input to binary tree mask
  # #reclassify so that areas with a CN go to 1
  # m <- c(0, 140393888010690, 1)
  # m <- matrix(m, ncol = 3, byrow= TRUE)
  # tree_mask <- terra::classify(tree_mask, m)
  # 
  # 
  # # get tree mask into desired projection
  # tree_mask <- terra::project(tree_mask, crs, method = "near", threads = TRUE)
   
  # # #inspect
  # # tree_mask
  # # crs(tree_mask, describe = TRUE)
  # # plot(tree_mask)
  # # summary(tree_mask)
  # # freq(tree_mask)
  # # crs(lcms, describe = TRUE)
  
  
  #####################
  ####### PREP DESTINATION RASTER
  #####################
  
  # create empty raster to append data into
  #r <- rast(crs = crs, ext(tree_mask), res = res(tree_mask))
  r <- rast(crs = crs, ext(lcms_crop), res = res(lcms_crop))
  r <- setValues(r, 0)
  #r <- mask(r, tree_mask)
  #names(r) <- c("slowLoss")
  
  # create new empty raster to use for eval
  #r_eval <- r
  
  # #inspect
  # freq(r)
  
  #create year range
  year_list <- seq(start_year, end_year, 1)
  
  #####################
  ###### ITERATE OVER YEARS
  #####################
  
  gc()
  
  for(i in 1:length(year_list)){
  
    #for testing
    #i = 1
    
    # iterate through change rasters by year
    year <- year_list[i]
    
    print(paste0("working on ", year))
    
    #load annual lcms change raster
    #lcms <- terra::rast(paste0(lcms_dir, "LCMS_CONUS_v2021-7_Change_", year, ".tif"))
    
    #load annual landfire change raster
    if(year < 2015){
      
      # create file name
      lf <- terra::rast(paste0(landfire_dir, "US_DIST", year, "/Tif/us_dist", year, ".tif"))
      
    } else if(year == 2015 | year == 2016) {
      
      lf <- terra::rast(paste0(landfire_dir, "LF", year, "_Dist_200_CONUS/Tif/LC", substr(year, 3, 4), "_Dist_200.tif"))
      
    } else if(year > 2016) {
      
      lf <- terra::rast(paste0(landfire_dir, "LF", year, "_Dist_220_CONUS/Tif/LC", substr(year, 3, 4), "_Dist_220.tif"))
    }
    
    # get crop zone into same projection -- easier than reproj for lf
    zone <- terra::project(zone, terra::crs(lf)) 
    
    #crop lf change raster
    print("cropping")
    #names(cats(lf)[[1]])
    
    
    if ("VALUE" %in% names(cats(lf)[[1]])){
      activeCat(lf) <- "VALUE"
    } else if  ("VALUE" %notin% names(cats(lf)[[1]]) & "VALUE1" %in% names(cats(lf)[[1]])){
      activeCat(lf) <- "VALUE1"
    }
      
      
    lf <- terra::crop(lf, zone, mask = TRUE, datatype = "INT2U")
    #lf_mask <- terra::mask(lf, zone, datatype = "INT2U")
    
    print("projecting")
    #get lf layer and crop zone into the same projection
    lf <- project(lf, crs, method = "near", threads = TRUE)
    zone <- project(zone, crs)
    
    #inspect
    #freq(lcms)
    
    #####set values to reclassify 
    # for lcms: classes are ordered from 1-n in order: Stable, Slow Loss, Fast Loss, Gain, NP Area Mask
    # no.class.val.slowloss <- c(1,3,4,5,NA) # keep only slow loss
    # no.class.val.eval <- c(4,5,NA) # keep: stable, slow loss, fast loss
    
    # for landfire: classes of change are denoted by middle digit
    #first digit = source; third digit = severity (1-3 low to high)
    #0: wildland fire
    #1: development; fire
    #2: clearcut; fire
    #3: chemical; fire; harvest
    #4: thinning (441-443; 741-743); insects (541-543; 841-843; 1041-1043)
    #5: mastication; disease (551-553; 851-853; 1051-1053)
    #6: 
    #7: herbicide; wildfire
    #8: biological (581-583; 881-883; 1081-1083)
    #9: prescribed fire
    #10:
    #11: unknown
    
    #list of changes to keep:
    keep <- sort(c(541,542,543, 841,842,843,1041,1042,1043, #insects
              551,552,553,851,852,853,1051,1052,1053, # disease
              581,582,583,881,882,883,1081,1082,1083)) # "biological"
    
    nums <- c(-9999, seq(0, 1133, 1))
    no.class.val.slowloss <- nums[nums %notin% keep]
    
    if (eval =="Y") {
      
      ### NOT USED WITH LANDFIRE 
      
      # ####### FIRST set up eval data set
      # # includes slow loss, fast loss, and stable -- one layer for each year 
      # lcms.eval <- terra::classify(lcms, cbind(no.class.val.eval,NA))
      # 
      # # mask with tree mask
      # lcms.eval_mask <- mask(lcms.eval, tree_mask)
      # 
      # # update NAs to 0
      # lcms.eval_mask <- subst(lcms.eval_mask, NA, 0)
      # 
      # #update name of layer
      # names(lcms.eval_mask) <- year
      # 
      # # for eval: add all layers, organize by year
      # r_eval <- c(r_eval, lcms.eval_mask)
      # 
      # 
      # ####### SECOND, use eval data set to derive slow loss layer
      # # save computational space
      # lcms.slowloss <- terra::classify(lcms.eval, cbind(no.class.val.slowloss,NA))
      
      
    } else if (eval == "N") {
      
      # set no data values based on above inputs
      print("classifying slow loss")
      lf.slowloss <- terra::classify(lf, cbind(no.class.val.slowloss,NA))
      
    }
    
    ## Slow loss layer processing
    
    #reclass all to 1 for slow loss
    print("reclassifying to binary")
    lf.slowloss <- terra::classify(lf.slowloss, cbind(keep, 1))
     
    # mask with tree mask
    print("applying tree mask")
    #lf.slowloss_mask <- mask(lf.slowloss, tree_mask)
    
    #update nas to 0
    print("updating nas to 0")
    lf.slowloss <- subst(lf.slowloss, NA, 0)
    
    # #inspect
    # freq(lf.slowloss_mask)
    # 
    # # #inspect - make sure mask worked
    # freq(lf)
    # plot(tree_mask, col = "green")
    # plot(subst(lf.slowloss_mask, 0, NA), add = TRUE)
    # inspect <- tree_mask + (subst(lf.slowloss_mask, NA, 0)*10)
    # freq(inspect)
    
    # where the value of slow loss = 1, change value to value of year 
    # each year: update all px so that most recent slow loss is recorded 
    print("adding to previous years")
    r <- r + lf.slowloss
    r <- subst(r, 1, year)
    
    # remove files to save space
    rm(lf.eval, lf.slowloss,
       lf.eval_mask, lf.slowloss_mask)
    gc()
  
   }
  
  #inspect
  r
  plot(r)
  #r_eval
  
  if (eval == "Y") {
    
    # remove empty first raster
    r_eval <- r_eval[[2:length(year_list)]]
    
    # add layer names to raster 
    #names(r_eval) <- year_list
    
    #update 0s to NA
    r_eval <- subst(r_eval, 0, NA)
    
  } else if (eval == "N") {
    
    names(r) <- c("lf.slowloss")
    
    #update 0s to NA
    r <- subst(r, 0, NA)
  
  }
  
  #inspect
  r
  freq(r)
  plot(r)
  
  # export
  #exportname <- paste0()
  writeRaster(r, paste0(home_dir, "03_Outputs/02_Landfire_Slow_Loss/01_Rasters/01_SlowLoss/", start_year, "_", end_year, "_", zone_name ,"_Landfire_SlowLoss",   ".tif"),
              overwrite = TRUE)
  
  
  # Stop the clock
  proc.time() - ptm
  
  rm(r)
  gc()
  
  }
