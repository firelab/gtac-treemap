# Create disturbance layer inputs - based on Landfire only

# Written By Lila Leatherman (lila.Leatherman@usda.gov)
# Based on script "rmrs_production_scripts/00_USDA_TreeMap_2014/reclass_Landfire_disturbance_rasters_for_tree_list.py" by Karin Riley (karin.riley@usda.gov)

# Last Updated: 4/24/25

# Final Output Rasters: 
# - disturbance code (0/1/2 for none/Fire/Other)
# - disturbance year - years since disturbance

# Intermediate Output Rasters: 
# - landfire fire years 
# - landfire fire binary
# - landfire insect and disease years
# - landfire insect and disease binary


#################################################################
# Set Inputs
#################################################################

# define year
year_input <- 2023

# define project area
study_area <- "CONUS"

# landfire version - formatted as "lf_{###}"
lf_version <- 'lf_240' 

# which zone to start on?
lf_zone_num_start <- 31

# Initialize directories
this_dir <- this.path::this.dir()
project_inputScript <- glue::glue("{this_dir}/00a_project_inputs_for_targetdata.R")
zone_inputScript <- glue::glue("{this_dir}/00b_zone_inputs_for_targetdata.R")

#######################################################
# Parallelization settings - within zone
#######################################################

# breakup factor - how many tiles to break the area into? as a factor of area px 
# 1 = 1 tile, 5 = many tiles
break.up <- 5

# set number of cores used for parallelization
ncores <- 4


###################################################
# LOAD DATA
###################################################

source(project_inputScript <- glue::glue("{this_dir}/00a_project_inputs_for_targetdata.R"))

# Load the landfire zones 
lf_zones <- terra::vect(lf_zones_path)

# Reproject the landfire zones to match the desired CRS
lf_zones<- terra::project(lf_zones, output_crs)
lf_zone_nums<- sort(lf_zones$ZONE_NUM)
z = which(lf_zone_num_start==lf_zone_nums)[1] # get index of starting zone
lf_zone_nums <- lf_zone_nums[z:length(lf_zone_nums)] # list zones to run

# Load Landfire disturbance data
#-----------------------------------------------------#

message("Identifying landfire disturbance data to load")

lf_files <- list.files(lf_dist_dir, full.names = TRUE, recursive = TRUE, pattern = ".tif$")

# filter files to only files we're interested in 
lf_files %<>% 
  #str_subset(pattern = "HDst", negate = TRUE)  %>% # remove historic disturbance
  str_subset(pattern = file_pattern) %>% # select only files from the map area of choice
  cbind(str_extract(., "[1-2][0,9][0-9][0-9]")) %>% # bind with year
  as.data.frame() %>% # convert the list to a data.frame
  dplyr::rename("year" = "V2",
                "path" = ".") %>%
  dplyr::mutate(year = as.numeric(year)) %>%
  arrange(year) %>% # sort by year
  filter(year %in% year_list) # filter to years of interest

# #inspect
# lf_files$path
# terra::rast(lf_files[25,1])

#message("Loading landfire data as VRT")
lf_dist <- terra::vrt(lf_files$path, filename = glue::glue("{tmp_dir}/lf_dist.vrt"), options = "-separate", overwrite = TRUE)

#inspect
#lf_dist[[20:25]]

##### Get change codes to reclassify
##############################################################

# field info in metadata: https://apps.fs.usda.gov/fsgisx01/rest/services/RDW_Landfire/US_Disturbance_v200/ImageServer/info/metadata
# for landfire: classes of change are denoted by middle digit
# first digit = source; third digit = severity (1-3 low to high)
# key to middle digit:
#0: wildland fire
#1: development; fire
#2: clearcut; fire
#3: chemical; fire; harvest
#4: thinning (441-443; 741-743); insects (541-543; 841-843; 1041-1043)
#5: mastication; disease (551-553; 851-853; 1051-1053)
#6: exotics: (561-563; 1061-1063; )
#7: herbicide; wildfire
#8: biological (581-583; 881-883; 1081-1083)
#9: prescribed fire
#10:
#11: unknown

# list codes that correspond to disturbance of interest
# ranges taken from karin riley's reclass script "reclass_Landfire_disturbance_rasters_for_tree_list.py"
# watch out for code 16 - water 
fire_codes <- c(seq(10,15,1), seq(17,234,1), seq(470,504,1), seq(770,804, 1), seq(970,1002,1)) 
ind_codes <- c(seq(540,564,1), seq(840,854,1), seq(861, 863, 1), seq(1040,1062,1))


# Create matrices for reclassifying
#---------------------------------------------#

# list all possible codes 
nums <- c(-9999, seq(0, 1133, 1))

# list numbers
rcl_fire <- nums
rcl_ind <- nums

# replace fire codes with 1, all others with na
rcl_fire[rcl_fire %in% fire_codes] <- 1 # fire code: 1
rcl_fire[rcl_fire !=1] <- NA

# replace ind codes with 2, all others with na
rcl_ind[rcl_ind %in% ind_codes] <- 2 # not-fire code: 2
rcl_ind[rcl_ind != 2] <- NA

#####################################################
# Prep Landfire Disturbance - by zone
####################################################

for(zone_input in lf_zone_nums){
  
  # for testing
  #zone_input = 29
  
  message(glue::glue("Creating disturbance layers for zone {zone_input}"))
  source(zone_inputScript)
  tic()
  
  # Subset to zone
  #----------------------------------------------------#
  
  lf_zone <- lf_zones[lf_zones$ZONE_NUM == zone_input,]
  
  
  # Make tiles
  #----------------------------------------------------#
  
  message("making tiles")
  
  # how big is the zone? 
  # maybe: if zone > size, then tile
  #expanse <- terra::expanse(zone, unit = "km")
  
  # convert zone shp to raster
  bbox <- terra::rast(lf_zone, crs = terra::crs(lf_zone), resolution = terra::res(lf_dist))
  lf_zone_r <- terra::rasterize(lf_zone, bbox)
  
  # break up zone into multiple sections to speed up processing
  h <- base::ceiling(ncol(lf_zone_r)/break.up)
  v <- base::ceiling(nrow(lf_zone_r)/break.up)
  
  # aggregate - template for making tiles to divvy up zone
  agg <- terra::aggregate(lf_zone_r, fact = c(h,v))
  agg[] <- 1:ncell(agg)
  
  # inspect tiles
  plot(agg)
  plot(lf_zone, add = TRUE, main = glue::glue('tiles for zone {zone_num}'))
  
  # subset the raster and create temporary files
  # tiles with only NA values are omitted
  # the function returns file names for the temporary files
  tiles <- lf_zone_r %>%
    terra::makeTiles(agg, paste0(tempfile(), '_.tif'), na.rm = TRUE)
  
  message(glue::glue('n tiles = {length(tiles)}; tile size = {h}px x {v}px'))
  
  rm(agg, bbox)
  gc()
  
  # Loop over tiles
  #---------------------------------------------------------------#
  message("Looping over tiles: Getting most recent year of change from Landfire stack")
  
  # set up dopar
  cl <- makeCluster(ncores, outfile = glue::glue("{tmp_dir}/cl_report.txt"))
  registerDoParallel(cl)
  #registerDoSEQ() # option to register sequentially - for testing
  
  # load packages to each cluster
  clusterCall(cl, function(){
    library(tidyverse);
    library(glue);
    library(terra)
  })
  
  # foreach loop dopar over tiles
  f <- foreach(i = 1:length(tiles),
               .packages= c("tidyverse", "terra", "doParallel", "foreach", "glue"),
               .export=c("lf_files", "lf_zone", "tmp_dir")
  ) %dopar% {
    
    # for testing
    #i = 1
    message(glue::glue("working on tile {i}"))
    
    fn <- tiles[i]
    
    # read raster tile into memory
    tile <- terra::rast(fn) %>%
      terra::trim()
    
    # read landfire data as vrt
    lf_dist <- terra::vrt(lf_files$path, filename = glue::glue("{tmp_dir}/lf_dist.vrt"), options = "-separate", overwrite = TRUE)
    
    # Crop landfire disturbance layers to tile
    #---------------------------------------------#
    tile_r <- terra::crop(lf_dist, tile)
    rm(lf_dist)
    
    # Prep Landfire fire layers
    # --------------------------------------------#
    # get year of most recent fire
    lf_fire_years_tile <- 
      tile_r %>%
      terra::classify(cbind(nums, rcl_fire)) %>% # reclass fire codes to binary indicator for each year
      terra::app(which.max.hightie) %>% # get most recent year
      terra::classify(cbind(c(seq(1:length(lf_files$year))), lf_files$year)) # reclassify index values to years
    
    # Export
    #---------------------------------------#
    writeRaster(lf_fire_years_tile, 
                filename = glue::glue('{tmp_dir}/lf/fire_years_tile{i}.tif'),
                datatype = "INT2U",
                overwrite = TRUE)
    
    
    # remove unused files
    rm(lf_fire_years_tile)
    gc()
    
    # Prep Landfire insect and disease
    # -----------------------------------------#
    
    # get year of most recent insect and disease
    lf_ind_years_tile <- 
      tile_r %>%
      terra::classify(cbind(nums, rcl_ind)) %>% # reclass disturbance codes to binary for each year
      terra::app(which.max.hightie) %>% # get most recent year of disturbance
      terra::classify(cbind(c(seq(1:length(lf_files$year))), lf_files$year)) # reclassify index values to years
    
    # Export tiles
    #---------------------------------------#
    
    writeRaster(lf_ind_years_tile, 
                filename = paste0(tmp_dir, "/lf/ind_years_tile", i, ".tif"),
                datatype = "INT2U",
                overwrite = TRUE)
    
    # remove unused files
    rm(lf_ind_years_tile)
    #rm(tile, tile_r)
    gc()
    
    #stopCluster(cl)
    
  } # end loop over tiles
  
  
  #############################################
  # MERGE TILES TO ZONE 
  #############################################
  
  message("merging tiles to zone")
  
  # list fire tiles
  fire_tiles <- list.files(path = glue::glue('{tmp_dir}/lf/'), 
                           pattern = "fire",
                           full.names = TRUE)
  
  # list ind tiles
  ind_tiles <- list.files(path = glue::glue('{tmp_dir}/lf/'), 
                          pattern = "ind",
                          full.names = TRUE)
  
  # Read in tiles as vrt and mask to zone
  lf_fire_years <- terra::vrt(fire_tiles,  glue('{tmp_dir}/lf_fire.vrt'), overwrite = TRUE) %>%
    terra::mask(lf_zone)
  lf_ind_years <- terra::vrt(ind_tiles, glue('{tmp_dir}/lf_ind.vrt'), overwrite = TRUE) %>%
    terra::mask(lf_zone)
  
  # Reclass to binary 
  #----------------------------------------_---#
  
  # reclassify to binary indicator of fire over all years
  fire_code = 1
  lf_fire_binary <-
    lf_fire_years %>%
    terra::classify(cbind(year_list, fire_code)) %>%
    terra::mask(lf_zone)
  
  # reclassify to binary indicator of insect and disease (ind) over all years
  ind_code = 2
  lf_ind_binary <-
    lf_ind_years %>%
    terra::classify(cbind(year_list, ind_code)) %>%
    terra::mask(lf_zone)
  
  # # Export intermediate files
  # #-------------------------------------------------#
  # message("Exporting lf fire years and binary raster...")
  # # write these files out
  # writeRaster(lf_fire_years, lf_fire_years_outpath,
  #             datatype = "INT2U",
  #             overwrite = TRUE)
  # writeRaster(lf_fire_binary, lf_fire_binary_outpath,
  #             datatype = "INT1U",
  #             overwrite = TRUE)
  # 
  # message("Exporting lf insect-disease years and binary raster....")
  # # write these files out
  # writeRaster(lf_ind_years, lf_ind_years_outpath,
  #             datatype = "INT2U",
  #             overwrite = TRUE)
  # writeRaster(lf_ind_binary, lf_ind_binary_outpath,
  #             datatype = "INT1U",
  #             overwrite = TRUE)
  
  #rm(lf_ind_binary, lf_ind_years, lf_fire_binary, lf_fire_years)
  
  gc()
  
  #####################################################
  # MERGE DISTURBANCE TYPE TO FINAL DISTURBANCE LAYERS
  ######################################################
  
  message("creating final disturbance layers")
  
  # for existing disturbance layer: 
  # fire code: 1
  # slow loss code: 2
  
  dist_year <- terra::merge(lf_fire_years, lf_ind_years) %>% # merge fire and slow loss 
    terra::app(function(x) model_year - x ) %>% # calculate years since disturbance
    terra::classify(cbind(NA,99)) %>%  # set no data values 
    terra::project(output_crs) # make sure it's in the correct crs
  
  gc()
  
  dist_code <- terra::merge(lf_fire_binary, lf_ind_binary) %>% # merge fire and slow loss
    terra::classify(cbind(NA, 0)) %>% # set no data values 
    terra::project(output_crs)  # make sure it's in the correct crs
  
  gc()
  
  # #inspect
  # plot(lf_fire_years)
  # plot(dist_year)
  # plot(lf_fire_binary)
  # plot(dist_code)

  
  # Export
  # -------------------------------------------------#
  
  
  message("exporting disturbance year and disturbance type")
  
  #export
  writeRaster(dist_year, lf_disturb_year_outpath,
              datatype = "INT1U",
              overwrite = TRUE)
  
  writeRaster(dist_code, lf_disturb_code_outpath,
              datatype = "INT1U",
              overwrite = TRUE)
  
  # Remove unused and files to start prep for next zone
  rm(lf_ind_binary, lf_fire_binary, lf_ind_years, lf_fire_years)
  rm(dist_year, dist_code)
  
  gc()
  
  # remove intermediate files from folder
  # file.remove(lf_fire_years_outpath, lf_fire_binary_outpath,
  #             lf_ind_years_outpath, lf_ind_binary_outpath)
  
  message(glue::glue('done with zone {zone_input}'))
  toc()

  
  }

