# Disturbance Layer for TreeMap includes: 
# Slow loss from Landscape Change Monitoring System (LCMS)
# Fast loss (i.e., Fire) from Landfire

# Output raster values indicate most recent year and disturbance type
# But, fire always takes precedence over slow loss
# Eg, "20161" = most recent year of disturbance and type is 2016, fire
#     "20162" = most recent year of disturbance and type is 2016, slow loss

# Output format: 
# Year since disturbance
# Type of disturbance

# Runs for one Landfire zone at a time, for the year range specified
# LCMS is available: 1985-2022
# Landfire is available: 1999-2020

# Necessary inputs: 

# LCMS slow loss layers ( ADD URL / download path)
# Landfire zones shapefiles
# Landfire disturbance layers (ADD URL / download path)
# previous treemap input - used only as tree mask 



############################
# USER INPUTS
############################

#list landfire zones of interest
zone_list <- c(#15,
  16#,
  #19,
  #21,
  #28
  #17,
  #18
)

#select year range (LCMS available for 1985-2021)
start_year <- 2015
end_year <- 2016

# set desired end crs  - if different from treemap / LF default
#crs <- crs("epsg:5070") # tree map output data is in  NAD83 Albers

# set home dir
home_dir <- ("//166.2.126.25/TreeMap/")

# get path to change rasters - LCMS
lcms_dir <- ("//166.2.126.227/lcms/Projects/11_LCMS_NationalProduct/06_DataDelivery/Conterminous_United_States/v2021-7/Change/Annual/")



# get path to change rasters - Landfire
#landfire_dir <- ("//166.2.126.25/TreeMap/01_Data/02_Landfire/LF_200/Disturbance/") # 2016 version
landfire_dir <- ("//166.2.126.25/TreeMap/01_Data/02_Landfire/LF_220/") # 2020 version

# path to 2016 treemap data
treemap_path <- "//166.2.126.25/TreeMap/01_Data/01_TreeMap2016_RDA/RDS-2021-0074_Data/Data/TreeMap2016.tif"

# aoi path - if different from landfire zone
# supply path, or NA
aoi_path <- paste0(home_dir, "01_Data/03_AOIs/UT_Uintas_rect_NAD1983.shp")
aoi_name <- "UT_Uintas_rect"
#aoi_path <- NA

# set tmp directory
tmp_dir <- "D:/tmp/"

#####################
# SETUP
######################

# check for required packages, install if needed and then load
list.of.packages <- c("terra", "tidyverse", "purrr", "furrr", "tictoc")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages) > 0) install.packages(new.packages)

# load all packages
vapply(list.of.packages, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)


# check if tmp directory exists 
if (file.exists(tmp_dir)){
  
} else {
  
  # create a new sub directory inside
  # the main path
  dir.create(tmp_dir)
  
}

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
#lcms_raw <- terra::rast(paste0(lcms_dir, "LCMS_CONUS_v2021-7_Change_2020.tif"))

# # load any lf change raster - to get spatial specs
# year_raw <- 1999
# 
# # get evt layer from landfire - for spatial specs and forest cover
# lf_evt <- terra::rast(paste0(landfire_dir, "EVT/LF2020_EVT_220_CONUS/LF2020_EVT_220_CONUS/Tif/LC20_EVT_220.tif"))
# activeCat(lf_evt) <- "EVT_GP"
# 
# # get desired crs from LCMS
# crs <- crs(lf_evt)

# load LF zone data
LF_zones <- vect(paste0(home_dir, "01_Data/02_Landfire/LF_zones/Landfire_zones/refreshGeoAreas_041210.shp"))


#####################
###### LOOP OVER Landfire zones
#####################


#for (z in 1:length(zone_list)) {
  
  #for testing
  z <- 1
  
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
  
  # inspect
  #LF_zones$ZONE_NAME
  
  if (!is.na(aoi_path)) {
    # load aoi subset - utah uintas only
    aoi <- vect(aoi_path)
    
    # reassign
    zone <- aoi
    zone_name <- aoi_name
    print("using input shapefile as AOI")
  } else{
    print("using landfire zone as AOI")
  }
  
  
  #####################
  ####### PREP DESTINATION RASTER
  #####################
  
  # # project
  # zone <- project(zone, crs)
  # 
  # lf_evt_crop <- terra::crop(lf_evt, zone, mask = TRUE)
  # #activeCat(lf_evt_crop) <- "EVT_GP"
  # #activeCat(lf_evt_crop) <- "EVT_GP_N"
  # 
  # # create empty raster to append data into
  # r <- rast(crs = crs, ext(lf_evt_crop), res = res(lf_evt_crop))
  # r <- setValues(r, 0)
  # 
  # # #inspect
  # # freq(r)
  # ncell(r)
  
  #create year range
  year_list <- seq(start_year, end_year, 1)
  
  #clear memory
  gc()
  
  ###################
  ###### PREPARE AND APPLY FOREST MASK
  
  # reclass evt layer to forest mask
  
  # set up reclass matrix
  
  #lf_forest <- terra::rcl(lf_evt_crop)
  
  
  # mask r - so we're only getting forested px
  
  # convert input raster to list of pts - to table
  
  # LCMS - CONVERT RAW PROBABILITY TO SLOW LOSS
  #############################################

  # load LCMS raw probability rasters - as VRT
  
  # crop to zone 
  
  # for each year: get most probable class
  
  #template function from LCMS workflow 
# def getMostProbableClass(raw_lcms_product_yr,product):
#     # Pull the index of the most probable value
#     # Since 0 is not used for LCMS outputs, add 1
#     max_prob_class = raw_lcms_product_yr.arrayArgmax().arrayGet(0).add(1).byte().rename([product])
#     max_prob_class = ee.Image(max_prob_class)
# 
#     null_code = lcms_viz_dict[f'{product}_class_values'][-1]
#     max_prob_class = max_prob_class.unmask(null_code)
#     return max_prob_class.copyProperties(raw_lcms_product_yr,['system:time_start'])

  # return stack of change class, by years
  
  # from stack of years,   
  # get most recent year slow loss
  # where change class = 2, what is the maximum band index? 
  
  # have some kind of code to make sure we can convert this back to year
  
  
  ###### BRING IN LANDFIRE DATA YEARS STACK AS VRT
  
  # extract vrt values to points
  
  ###### data table manipulation - 
  
  # convert to long table w year, source, change type
  
  # filter out change types we aren't including
  
  # group by pt to get most recent 
  
  # output column with desired change value
  
  ###### convert table back to raster
  
  # export
  
  
  
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
    lcms <- crop(lcms, zone, mask = TRUE)
    
    #get lcms layer and crop zone into the same projection
    #lcms <- project(lcms, crs)
    #zone <- project(zone, crs)
    
    #inspect
    #freq(lcms)
    
    #set values to reclassify 
    # classes are ordered from 1-n in order: Stable, Slow Loss, Fast Loss, Gain, NP Area Mask
    no.class.val.slowloss <- c(1,3,4,5,NA) # keep only slow loss
    no.class.val.eval <- c(4,5,NA) # keep: stable, slow loss, fast loss
    
    # set no data values based on above inputs
    print("classifying slow loss")
    lcms.slowloss <- terra::classify(lcms, cbind(no.class.val.slowloss,NA))
      
    
    ## Slow loss layer processing
    
    #reclass all to 1 for slow loss
    print("reclassifying to binary")
    lcms.slowloss <- terra::subst(lcms.slowloss, 2, 1)
    
    
    #update nas to 0
    print("updating nas to 0")
    lcms.slowloss <- subst(lcms.slowloss, NA, 0)
    
    
    # where the value of slow loss = 1, change value to value of year 
    # each year: update all px so that most recent slow loss is recorded 
    print("adding to previous years")
    r <- r + lcms.slowloss
    r <- subst(r, 1, year)
    
    # remove files to save space
    rm(lcms.slowloss)
    gc()
    
  }
  
  #inspect
  r
  
 
  names(r) <- c("slowloss")
  
  #update 0s to NA
  r <- subst(r, 0, NA)
  
  # #inspect
  # r
  # freq(r)
  # plot(r)

  
  #project
  r <- project(r, crs)
  
  #####################################################
  ###### ADD FIRE FROM LANDFIRE - FIRE TAKES PRECEDENCE
  #####################################################
  
  
  
  #####################
  ####### PREP TREE MASK
  #####################
  
  print("processing tree mask")
  gc()
  
  # load tree mask - pre-existing tree map data
  tree_mask <- terra::rast(treemap_path)
  
  # get crop zone into same projection
  zone <- project(zone, crs(tree_mask)) 
  
  #crop tree mask
  print("cropping tree mask")
  activeCat(tree_mask) <- 1 # ensure active cat is CN
  tree_mask <- terra::crop(tree_mask, zone)
  
  #reclassify tree map input to binary tree mask
  #reclassify so that areas with a CN go to 1
  m <- c(0, 140393888010690, 1)
  m <- matrix(m, ncol = 3, byrow= TRUE)
  tree_mask <- terra::classify(tree_mask, m)
  
  gc()
  
  # get tree mask into desired projection
  print("projecting tree mask")
  tree_mask <- terra::project(tree_mask, crs, method = "near")
  
  # #inspect
  # tree_mask
  # crs(tree_mask, describe = TRUE)
  # plot(tree_mask)
  # summary(tree_mask)
  # freq(tree_mask)
  # crs(lcms, describe = TRUE)
  
  # clear memory
  gc()
  
  # apply tree mask
  print("applying tree mask")
  r <- terra::mask(r, tree_mask)
  
  
  # export
  #exportname <- paste0()
  writeRaster(r, paste0(home_dir, "03_Outputs/01_LCMS_Slow_Loss/01_Rasters/01_SlowLoss/", start_year, "_", end_year, "_", zone_name ,"_LCMS_SlowLoss",   ".tif"),
              overwrite = TRUE)
  
  
  # writeRaster(r_eval, paste0(home_dir, "03_Outputs/01_LCMS_Slow_Loss/01_Rasters/02_Eval/", start_year, "_", end_year, "_", zone_name ,"_LCMS_SlowFastStableEval",   ".tif"),
  #             overwrite = TRUE)
  
  # Stop the clock
  print(proc.time() - ptm)
  
  #rm(r)
  gc()
  
}
