# Create input disturbance layer - GTAC LCMS 2016

# Written by: Lila Leatherman (lila.leatherman@usda.gov)
# Redcastle Resources and USFS Geospatial Technology and Applications Center (GTAC)

# Last updated: 3/4/24

# Input rasters: 
# - Annual raw probability of slowloss, fast loss, and gain from LCMS
# - Annual Landfire disturbance 

# Output rasters, with fire taking priority: 
# - years since disturbance
# - type of disturbance 


###############################
# SET Inputs
###############################

# Set inputs - from input script
this.path <- this.path::this.path() # Id where THIS script is located

# get path to input script
spl <- stringr::str_split(this.path, "/")[[1]]
input_script.path <- paste( c(spl[c(1:(length(spl)-1))],
                              "00_inputs_for_targetdata.R" ),
                            collapse = "/")

source(input_script.path)


# #list landfire zones of interest
# zone_list <- c(16)
# 
# # set year range
# start_year <- 1999
# end_year <- 2016
# 
# # set current modeling year (for years since disturbance)
# model_year <- end_year
# 
# # set threshold for probability of slow loss from LCMS
# slow_loss_thresh <- 14 # default value for LCMS processing: 14
# 
# # set home dir
# home_dir <- "D:/LilaLeatherman/01_TreeMap/"
# #home_dir <- "//166.2.126.25/TreeMap/"
# 
# # data directory - where source data are located. these won't be changed
# #data_dir <- glue::glue('{home_dir}/01_Data/')
# data_dir <- "//166.2.126.25/TreeMap/01_Data/"
# 
# # where version-specific inputs and outputs will live
# project_dir <- glue::glue('{home_dir}/03_Outputs/99_Projects/2016_GTAC_Test/')
# 
# # set path to save output rasters
# # this directory will be created if it does not already exist
# target_dir <- glue::glue('{project_dir}/01_Target_Data/')
# 
# # set dir to lcms raw probability rasters
# lcms_dir <- glue::glue('{data_dir}05_LCMS/01_Threshold_Testing/01_Raw/02_Raw_Probabilities/')
# 
# # set path to landfire rasters 
# landfire_dir <- glue::glue('{data_dir}02_Landfire/LF_220/Disturbance/')
# 
# # set projection used for processing lcms rasters
# lcms_proj <- glue::glue('{data_dir}05_LCMS/00_Supporting/lcms_crs_albers.prj')
# 
# # path to projection used for processing landfire rasters
# landfire_proj <- glue::glue('{data_dir}02_Landfire/landfire_crs.prj')
# 
# # supply path, or NA
# # aoi_path <- "//166.2.126.25/TreeMap/01_Data/03_AOIs/UT_Uintas_rect_NAD1983.shp"
# # aoi_name <- "UT_Uintas_rect"
# aoi_path <- NA
# 
# # set tmp directory
# tmp_dir <- "D:/tmp/"
# 
# # setting to remove intermediate files from memory
# # all intermediate files required to generate end product are written to disk
# # Y: deletes intermediate files (better for iteration and development)
# # N: retains intermediate files (better for computational efficiency)
# remove_intermediate_files <- "N"
# 
# ##############################################
# # SETUP
# #############################################
# 
# # Packages and functions
# #---------------------------------#
# 
# # packages required
# list.of.packages <- c("terra", "tidyverse", "magrittr", "glue", "tictoc")
# 
# #check for packages and install if needed
# #new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages) > 0) install.packages(new.packages)
# 
# # load all packages
# vapply(list.of.packages, library, logical(1L),
#        character.only = TRUE, logical.return = TRUE)
# 
# # make 'notin' function
# `%notin%` <- Negate('%in%')
# 
# # Set up temp directory 
# #----------------------------------#
# 
# # check if tmp directory exists 
# if (file.exists(tmp_dir)){
#   
# } else {
#   # create a new sub directory inside the main path
#   dir.create(tmp_dir)
# }
# 
# # set temp directory - helps save space with R terra
# write(paste0("TMPDIR = ", tmp_dir), file=file.path(Sys.getenv('R_USER'), '.Renviron'))
# #empty temp dir
# do.call(file.remove, list(list.files(tmp_dir, full.names = TRUE)))
# #remove unused memory
# gc()
# 
# # Set up other directories
# # ----------------------------------#
# if(!file.exists(target_dir)) {
#   dir.create(target_dir, recursive = TRUE)}
# 
# # Terra options
# # --------------------------------#
# 
# #increase memory fraction available
# terraOptions(memfrac = 0.8)

###################################################
# LOAD DATA
###################################################

# load lcms projections
lcms_crs <- crs(lcms_proj)

#load landfire projection
landfire_crs <- crs(landfire_proj)

# load LF zone data
LF_zones <- vect(lf_zones_path)

#build year list
year_list <- seq(start_year, end_year, 1)

###################################################
# LOOP OVER ZONES
##################################################

tic()
for (z in zone_list) {
  
  #for testing
  z = 16
  
  zone_num <- z
  
  # status update
  print(glue("working on zone {zone_num}"))
  
  # Prep zone
  #-----------------------------------------#
  
  # select single LF zone
  zone <- subset(LF_zones, LF_zones$ZONE_NUM == zone_num) 
  
  #project
  zone %<>%
    terra::project(lcms_crs)
  
  # get name of zone
  zone_name <- glue('LFz{zone_num}_{gsub(" ", "", zone$ZONE_NAME)}')
  
  #set zone identifiers
  cur.zone <- glue::glue('z{zone_num}')
  cur.zone.zero <- if(zone_num < 10) {
    glue::glue('z0{zone_num}') } else {
      cur.zone
    }
  
  # Update dirs with zone
  # -----------------------------------------#
  # Set folder paths
  target_dir_z = glue::glue('{target_dir}/{cur.zone.zero}/')
  if(!file.exists(target_dir_z)) {dir.create(target_dir_z)}
  
  
  # Optional subset
  #---------------------------------------#
  
  if (!is.na(aoi_path)) {
    # load aoi subset - utah uintas only
    aoi <- vect(aoi_path) %>%
      project(lcms_crs)
    
    # reassign
    zone <- aoi
    zone_name <- aoi_name
    print("using input shapefile as AOI")
  } else{
    print("using landfire zone as AOI")
  }
  
  
  #####################################################
  # PREP LCMS SLOW LOSS
  ####################################################
  
  # # Set variables
  # NAvalue <- -32768
  # 
  # # set probability thresholds for change
  # change_thresholds <- c(29, # fast loss; default = 29
  #                        slow_loss_thresh, # slow loss; default = 14
  #                        20 # gain; default = 20
  #                        )
  
  
  # Convert raw probability layers into change layers
  #---------------------------------------------------------------#
  
  # create empty raster to append things into
  lcms_slowloss <- terra::rast()
  
  # bookkeeping
  print("preparing LCMS slow loss")
  
  # Loop over years
  for(i in 1:length(year_list)){
  
    # for testing
    #i = 1
  
    year <- year_list[i]
  
    # bookkeeping
    print(glue("working on {year}"))
  
    # list raw probability tile layers for a given year
    year_files <- list.files(lcms_dir, pattern = paste0(year, '.+.tif$'), full.names = TRUE)
  
    # load in all tiles for single year as a vrt
    raw_prob <- vrt(year_files, glue::glue('{tmp_dir}/raw_prob_{year}.vrt'), overwrite = TRUE)
  
    # add layer names for clarity
    names(raw_prob) <- c( "FastLoss_Raw_Prob", "SlowLoss_Raw_Prob", "Gain_Raw_Prob")
  
    # prepare raw probability input
    raw_prob_process <-
    raw_prob %>%
      crop(zone, mask = TRUE) %>% # crop to zone
      terra::classify(cbind(LCMS_NAvalue, NA)) # update NA values
    
    # # inspect
    #   rbind(freq(raw_prob_process[[1]], value = NA),
    #         freq(raw_prob_process[[1]])      )
    #   
    #   plot(raw_prob_process)
      
    # prepare non-processing area mask
    NPArea_mask <-
      raw_prob_process %>%
      min()
  
    # reclassify classes: values below threshold go to NA
    raw_prob_process[[1]] <- terra::classify(raw_prob_process[[1]], cbind(seq(1, LCMS_change_thresholds[1], 1), NA))
    raw_prob_process[[2]] <- terra::classify(raw_prob_process[[2]], cbind(seq(1, LCMS_change_thresholds[2], 1), NA))
    raw_prob_process[[3]] <- terra::classify(raw_prob_process[[3]], cbind(seq(1, LCMS_change_thresholds[3], 1), NA))
  
    maxProbClass <-
      raw_prob_process %>%
      which.max() %>% # get index of highest value that exceeds threshold
      terra::classify(cbind(NA, 4)) %>% # fill in stable
      mask(NPArea_mask) # mask with NP area mask
  
    # #inspect
    # maxProbClass
    # rbind(freq(maxProbClass),
    #            freq(maxProbClass, value = NA))
    # plot(maxProbClass)
  
    # bind with rasters
    lcms_slowloss <- c(lcms_slowloss, maxProbClass)
    
    # write out single year? and then read back in as VRT
  
    #remove unused files
    rm(raw_prob, NPArea_mask, raw_prob_process, maxProbClass)
    
    # clear memory
    gc()
    
    }
  
  
  # Get most recent year of slow loss
  #-----------------------------------------------#
  
  #rename layers for clarity
  names(lcms_slowloss) <- year_list
  
  #inspect
  lcms_slowloss
  
  # get most recent year of slow loss
  lcms_slowloss_years <-
    lcms_slowloss %>%
    terra::classify(cbind(c(1,3,4), NA)) %>% # reclass to only slow loss - value = 2
    which.max() %>% # identify year with maximum
    terra::classify(cbind(c(seq(1:length(year_list))), year_list)) # reclassify index values to years
  
  # convert to binary indicator of slow loss
  # slow loss value for input to TreeMap disturbance layer = 2
  lcms_slowloss_binary <-
    lcms_slowloss_years %>%
    terra::classify(cbind(year_list, 2))
  
  # inspect
  # lcms_slowloss
  # freq(lcms_slowloss)
  # plot(lcms_slowloss_years)
  # plot(lcms_slowloss_binary)
  
  # Export
  #---------------------------------------#
  
  

  
  # # write these files out
  # writeRaster(lcms_slowloss_years, glue::glue('{target_dir_prelim}/disturb_year_LFLCMS.tif'),
  #             overwrite = TRUE)
  # writeRaster(lcms_slowloss_binary, glue::glue('{target_dir_prelim}/disturb_code_LFLCMS.tif'),
  #             overwrite = TRUE)
  
  # clear memory
  gc()
  
  # # option to remove intermediate files
  # if(remove_intermediate_files == "Y") {
  #   rm(lcms_slowloss, lcms_slowloss_years, lcms_slowloss_binary)
  # } else {}
  
  ###############################################
  # PREP LANDFIRE FIRE DATA
  ################################################
  
  ##### Change codes to reclassify
  
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
  
  # list codes that correspond to fire
  # ranges taken from karin riley's reclass script "reclass_Landfire_disturbance_rasters_for_tree_list.py"
  fire_codes <- c(seq(10,15,1), seq(17,234,1), seq(470,504,1), seq(770,804, 1), seq(970,1002,1))
  
  # list codes to reclassify
  nums <- c(-9999, seq(0, 1133, 1))
  no.class.val.fire <- nums[nums %notin% fire_codes]
  
  # reproject zone for landfire
  zone %<>% 
    project(landfire_crs)
  
  # bookkeeping
  print("preparing landfire fire")
  
  # list landfire files
  landfire_files <- list.files(glue::glue('{landfire_dir}/Disturbance'), pattern = '.tif$', full.names = TRUE, recursive = TRUE)
  
  # filter files to only files we're interested in
  landfire_files %<>%
    str_subset(pattern = "test", negate = TRUE) %>% # remove files in test folder
    str_subset(pattern = "mostRecent", negate = TRUE) %>% # remove files named "mostRecent"
    str_subset(pattern = "Reclass", negate = TRUE)  %>% # remove files with "Reclass"
    cbind(str_extract(., "[1-2][0,9][0-9][0-9]")) %>% # bind with year
    as.data.frame() %>%
    dplyr::rename("year" = "V2",
                  "path" = ".") %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    arrange(year) %>% # sort by year
    filter(year %in% year_list) # filter to years of interest
  
  #inspect
  #landfire_files
  
  # load all landfire files 
  landfire_dist <- vrt(landfire_files$path, glue('{tmp_dir}/landfire_dist.vrt'), options = '-separate', overwrite = TRUE)
  #landfire_dist <- terra::rast(landfire_files$path)
  
  # add names to layers for clarity
  names(landfire_dist) <- year_list
  
  # #inspect
  # landfire_dist
  
  # get year of most recent fire
  landfire_fire_years <-
    landfire_dist %>%
    terra::crop(zone, mask = TRUE) %>% # crop to zone
    terra::classify(cbind(no.class.val.fire, NA)) %>% # reclass to include only fire
    terra::classify(cbind(seq(1,1133,1), 1)) %>% # reclass fire to binary indicator for each year
    which.max() %>% # get most recent year
    terra::classify(cbind(c(seq(1:length(year_list))), year_list)) # reclassify index values to years
  
  
  # reclassify to binary indicator of fire over all years
  # fire code = 1
  landfire_fire_binary <-
    landfire_fire_years %>%
    terra::classify(cbind(year_list, 1))
  
  # #inspect
  # landfire_fire_years
  # plot(landfire_fire_years)
  # landfire_fire_binary
  # plot(landfire_fire_binary)
  
  
  # Export
  #---------------------------------------#
  
  # # create export directory
  # output_dir_landfire <- glue('{output_dir}01_Input/02_Landfire_Fire')
  # dir.create(output_dir_landfire)
  # 
  # # create output file names
  # landfire_fire_years_outpath <- glue('{output_dir_landfire}/{start_year}_{end_year}_{zone_name}_Landfire_Fire_Years.tif')
  # landfire_fire_binary_outpath <- glue('{output_dir_landfire}/{start_year}_{end_year}_{zone_name}_Landfire_Fire_Binary.tif')
  # 
  # # write these files out
  # writeRaster(landfire_fire_years, landfire_fire_years_outpath, 
  #             overwrite = TRUE)
  # writeRaster(landfire_fire_binary, landfire_fire_binary_outpath, 
  #             overwrite = TRUE)
  # 
  # # remove unused files
  # rm(landfire_dist, landfire_files)
  # gc()
  # 
  # # option to remove intermediate files
  # if(remove_intermediate_files == "Y") {
  #   rm(landfire_fire_years, landfire_fire_binary)
  # } else {}
  
  #################################################
  # MERGE LCMS and LANDIRE LAYERS
  #################################################
  
  # bookkeeping
  print("combining LCMS slow loss and Landfire fire")
  
  # #paths to rasters
  # dist_paths <- c(
  #   #"//166.2.126.25/TreeMap/03_Outputs/05_Target_Rasters/01_Disturbance/02_Landfire_Fire/2010_2016_LFz16_UtahHighPlateaus_Landfire_Fire_Years.tif",
  #            "//166.2.126.25/TreeMap/03_Outputs/01_LCMS_Slow_Loss/01_Rasters/01_SlowLoss/2010_2016_LFz16_UtahHighPlateaus_LCMS_SlowLoss_Years.tif"
  #            #,
  #            #"//166.2.126.25/TreeMap/03_Outputs/05_Target_Rasters/01_Disturbance/02_Landfire_Fire/2010_2016_LFz16_UtahHighPlateaus_Landfire_Fire_Binary.tif",
  #            #"//166.2.126.25/TreeMap/03_Outputs/01_LCMS_Slow_Loss/01_Rasters/01_SlowLoss/2010_2016_LFz16_UtahHighPlateaus_LCMS_SlowLoss_Binary.tif"
  #            )
  # 
  # fnames <- c("Landfire_fire_years",
  #             "LCMS_slowloss_years",
  #             "Landfire_fire_binary",
  #             "LCMS_slowloss_binary")
  # 
  # # load as vrt
  # dist_input <- terra::vrt(dist_paths, glue('{tmp_dir}/dist_input.vrt'), options = '-separate', overwrite = TRUE)
  # 
  # #inspect
  # dist_input
  # 
  # #apply names for clarity
  # names(dist_input) <-fnames
  
  # # load input rasters back in - save memory
  # landfire_fire_years <- terra::rast(landfire_fire_years_outpath)
  # lcms_slowloss_years <- terra::rast(lcms_slowloss_years_outpath)
  # landfire_fire_binary <- terra::rast(landfire_fire_binary_outpath)
  # lcms_slowloss_binary <- terra::rast(lcms_slowloss_binary_outpath)
  
  # ensure rasters are in the same projection
  lcms_slowloss_binary %<>% terra::project(landfire_crs)
  lcms_slowloss_years %<>% terra::project(landfire_crs)
  landfire_fire_binary %<>% terra::project(landfire_crs)
  landfire_fire_binary %<>% terra::project(landfire_crs)
  
    gc()
  
  # for existing disturbance layer: 
  # fire code: 1
  # slow loss code: 2
  
  
  dist_year <- terra::merge(landfire_fire_years, lcms_slowloss_years) %>% # merge fire and slow loss 
    terra::app(function(x) model_year - x ) %>% # calculate years since disturbance
    terra::classify(cbind(NA, 99)) %>% # set no data values
    terra::mask(zone) # mask
  
  dist_type <- terra::merge(landfire_fire_binary, lcms_slowloss_binary) %>% # merge fire and slow loss
    terra::classify(cbind(NA, 0)) %>% # set no data values
    terra::mask(zone) # mask
  
  
  # #inspect
  # plot(landfire_fire_years)
  # plot(lcms_slowloss_years)
  # plot(dist_year)
  # plot(landfire_fire_binary)
  # plot(lcms_slowloss_binary)
  # plot(dist_type)
  
  # Export
  # -------------------------------------------------#
  
  #export
  writeRaster(dist_year, glue::glue('{target_dir_z}/disturb_year_LFLCMS.tif'),
              overwrite = TRUE)
  writeRaster(dist_type, glue::glue('{target_dir_z}/disturb_code_LFLCMS.tif'),
              overwrite = TRUE)
  
  toc()
  
  # #remove products
  # # option to remove intermediate files
  # if(remove_intermediate_files == "Y") {
  #   rm(dist_year, dist_type)
  # } else {}
  
  #clear unused memory
  gc()
  
  }
