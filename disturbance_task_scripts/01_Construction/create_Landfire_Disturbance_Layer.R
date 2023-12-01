# Create input disturbance layer - "GTAC Landfire 2016"

# Most recent year of insect and disease from Landfire
# Most recent year of fire from Landfire 

# Output rasters: 
# - years since disturbance
# - type of disturbance 

# Load in rasters as VRTs and write out end files

###############################
# SET USER INPUTS
###############################

#list landfire zones of interest
zone_list <- c(16)

# set year range
start_year <- 1999
end_year <- 2016

# set current modeling year (for years since disturbance)
cur_year <- end_year

# set home dir
home_dir <- "//166.2.126.25/TreeMap/"

# set path to save output rasters
# this directory will be created if it does not already exist
output_dir <- '//166.2.126.25/TreeMap/03_Outputs/05_Target_Rasters/01_Disturbance/'

# set path to lcms raw probability rasters
lcms_path <- '//166.2.126.25/TreeMap/01_Data/05_LCMS/01_Threshold_Testing/01_Raw/02_Raw_Probabilities/'

# set path to landfire rasters 
landfire_path <- '//166.2.126.25/TreeMap/01_Data/02_Landfire/LF_220/Disturbance/'

# set projection used for processing lcms rasters
lcms_proj <- "//166.2.126.25/TreeMap/01_Data/05_LCMS/00_Supporting/lcms_crs_albers.prj"

# set projection used for processing landfire rasters
landfire_proj <- "//166.2.126.25/TreeMap/01_Data/02_Landfire/landfire_crs.prj"

# supply path, or NA
#aoi_path <- "//166.2.126.25/TreeMap/01_Data/03_AOIs/UT_Uintas_rect_NAD1983.shp"
#aoi_name <- "UT_Uintas_rect"
aoi_path <- NA

# set tmp directory
tmp_dir <- "D:/tmp"

# setting to remove intermediate files from memory
# all intermediate files required to generate end product are written to disk
# Y: deletes intermediate files (better for iteration and development)
# N: retains intermediate files (better for computational efficiency)
remove_intermediate_files <- "Y"

#option to calculate landfire fire files anew
#may not be necessary if this has already been run for the year and zone of interest
calculate_landfire_fire <- "Y"

#####################
# SETUP
######################

# Temp directories 
#----------------------------------#

# check if tmp directory exists 
if (file.exists(tmp_dir)){
  
} else {
  # create a new sub directory inside the main path
  dir.create(tmp_dir, recursive = TRUE)
  
}

#empty temp dir
#do.call(file.remove, list(list.files(tmp_dir, full.names = TRUE)))

# set temp directory - helps save space with R terra
write(paste0("TMPDIR = ", tmp_dir), file=file.path(Sys.getenv('R_USER'), '.Renviron'))

#remove unused memory
gc()

# Packages and functions
#---------------------------------#

# packages required
list.of.packages <- c("terra", "tidyverse", "magrittr", "glue", "tictoc")

# #check for packages and install if needed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages)

# load all packages
vapply(list.of.packages, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)

# make 'notin' function
`%notin%` <- Negate('%in%')


# Other file directories
#---------------------------------#
dir.create(output_dir, recursive = TRUE)

# Terra options
# --------------------------------#

#increase memory fraction available
#terraOptions(memfrac = 0.8)

###################################################
# LOAD DATA
###################################################

# load lcms projections
#lcms_crs <- crs(lcms_proj)

#load landfire projection
landfire_crs <- crs(landfire_proj)

# load LF zone data
LF_zones <- vect(glue('{home_dir}01_Data/02_Landfire/LF_zones/Landfire_zones/refreshGeoAreas_041210.shp'))

#project
LF_zones %<>%
  project(landfire_crs)

#build year list
year_list <- seq(start_year, end_year, 1)

###################################################
# LOOP OVER ZONES
##################################################

tic()
#for (z in 1:length(zone_list)) {
  
  #for testing
  z <- 1
  
  zone_num <- zone_list[z]
  
  # status update
  print(glue("working on zone {zone_num}"))
  
  # Prep zone
  #-----------------------------------------#
  
  # select single LF zone
  zone <- subset(LF_zones, LF_zones$ZONE_NUM == zone_num) #z16 = Utah High Plateaus
  
  # get name of zone
  zone_name <- glue('LFz{zone_num}_{gsub(" ", "", zone$ZONE_NAME)}')
  
  # inspect
  #LF_zones$ZONE_NAME
  
  if (!is.na(aoi_path)) {
    # load aoi subset - utah uintas only
    aoi <- vect(aoi_path) %>%
      project(landfire_crs)
    
    # reassign
    zone <- aoi
    zone_name <- aoi_name
    print("using input shapefile as AOI")
  } else{
    print("using landfire zone as AOI")
  }
  
  
  ###############################################
  # PREPARE LANDFIRE DATA
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
  
  # list codes that correspond to disturbance of interest
  # ranges taken from karin riley's reclass script "reclass_Landfire_disturbance_rasters_for_tree_list.py"
  # watch out for code 16 - water 
  fire_codes <- c(seq(10,15,1), seq(17,234,1), seq(470,504,1), seq(770,804, 1), seq(970,1002,1)) 
  ind_codes <- c(seq(540,564,1), seq(840,854,1), seq(861, 863), seq(1040,1062,1))
  
  # list codes to reclassify
  nums <- c(-9999, seq(0, 1133, 1))
  no.class.val.fire <- nums[nums %notin% fire_codes]
  no.class.val.ind <- nums[nums %notin% ind_codes]
  
  # bookkeeping
  print("loading landfire data")
  
  # list landfire files 
  landfire_files <- list.files(landfire_path, pattern = '.tif$', full.names = TRUE, recursive = TRUE)
  
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
  
  # load all landfire files as vrt
  landfire_dist <- vrt(landfire_files$path, glue('{tmp_dir}/landfire_dist.vrt'), options = '-separate', overwrite = TRUE)
  
  # add names to layers for clarity
  names(landfire_dist) <- year_list
  
  # #inspect
  landfire_dist
  
  # create export directory
  output_dir_landfire <- glue('{output_dir}01_Input/02_Landfire_Fire')
  dir.create(output_dir_landfire)
  
  # create output file names
  landfire_fire_years_outpath <- glue('{output_dir_landfire}/{start_year}_{end_year}_{zone_name}_Landfire_Fire_Years.tif')
  landfire_fire_binary_outpath <- glue('{output_dir_landfire}/{start_year}_{end_year}_{zone_name}_Landfire_Fire_Binary.tif')
  
  if(calculate_landfire_fire == "N"){
    print("skipping landfire fire calculation!")
  } else if(calculate_landfire_fire == "Y") {
    print("preparing landfire fire layers!")
    
    # Prep Landfire fire layers
    # --------------------------------------------#
    
    # get year of most recent fire
    landfire_fire_years <- 
      landfire_dist %>%
      terra::crop(zone) %>% # crop to zone
      terra::classify(cbind(no.class.val.fire, NA)) %>% # reclass to include only fire
      terra::classify(cbind(seq(1,1133,1), 1)) %>% # reclass fire to binary indicator for each year - fire dist code = 1
      which.max() %>% # get most recent year
      terra::classify(cbind(c(seq(1:length(year_list))), year_list)) # reclassify index values to years
    
    #look for fire
    landfire_fire_check <- 
      landfire_dist %>%
      terra::crop(zone) %>%
      freq() %>%
      arrange(value)
    
    # reclassify to binary indicator of fire over all years
    # fire code = 1
    landfire_fire_binary <- 
      landfire_fire_years %>%
      terra::classify(cbind(year_list, 1))
    
    #inspect
    # landfire_fire_years
    # plot(landfire_fire_years)
    # landfire_fire_binary
    # plot(landfire_fire_binary)
    
    # Export
    #---------------------------------------#
    
    # create export directory
    output_dir_landfire <- glue('{output_dir}01_Input/02_Landfire_Fire')
    dir.create(output_dir_landfire)
    
    # create output file names
    landfire_fire_years_outpath <- glue('{output_dir_landfire}/{start_year}_{end_year}_{zone_name}_Landfire_Fire_Years.tif')
    landfire_fire_binary_outpath <- glue('{output_dir_landfire}/{start_year}_{end_year}_{zone_name}_Landfire_Fire_Binary.tif')
    
    # write these files out
    writeRaster(landfire_fire_years, landfire_fire_years_outpath, 
                overwrite = TRUE)
    writeRaster(landfire_fire_binary, landfire_fire_binary_outpath, 
                overwrite = TRUE)
    
    # remove unused files
    gc()
    
    # option to remove intermediate files
    if(remove_intermediate_files == "Y") {
      rm(landfire_fire_years, landfire_fire_binary)
    } else {}
  } else {
    print("check your calculate_landfire_fire perameter")
  }
  
  # Prep Landfire insect and disease
  # -----------------------------------------#
  
  print("preparing landfire insect and disease")
  
  # get year of most recent insect and disease
  landfire_ind_years <- 
    landfire_dist %>%
    terra::crop(zone, mask = TRUE) %>% # crop to zone
    terra::classify(cbind(no.class.val.ind, NA)) %>% # reclass to include only insect and disease
    terra::classify(cbind(seq(1,1133,1), 2)) %>% # reclass to binary indicator for each year - i&d dist code = 2
    which.max() %>% # get most recent year
    terra::classify(cbind(c(seq(1:length(year_list))), year_list)) # reclassify index values to years
  
  
  # reclassify to binary indicator of fire over all years
  #inset and disease code = 2
  landfire_ind_binary <- 
    landfire_ind_years %>%
    terra::classify(cbind(year_list, 2))
  
  # #inspect
  # landfire_ind_years
  # plot(landfire_ind_years)
  # landfire_ind_binary
  # plot(landfire_ind_binary)
  
  # Export
  #---------------------------------------#
  
  # create export directory
  output_dir_landfire <- glue('{output_dir}01_Input/03_Landfire_InsectDisease')
  dir.create(output_dir_landfire)
  
  # create output file names
  landfire_ind_years_outpath <- glue('{output_dir_landfire}/{start_year}_{end_year}_{zone_name}_Landfire_InD_Years.tif')
  landfire_ind_binary_outpath <- glue('{output_dir_landfire}/{start_year}_{end_year}_{zone_name}_Landfire_InD_Binary.tif')
  
  # write these files out
  writeRaster(landfire_ind_years, landfire_ind_years_outpath, 
              overwrite = TRUE)
  writeRaster(landfire_ind_binary, landfire_ind_binary_outpath, 
              overwrite = TRUE)
  
  # remove unused files
  rm(landfire_dist, landfire_files)
  gc()
  
  
  # option to remove intermediate files
  if(remove_intermediate_files == "Y") {
    rm(landfire_ind_years, landfire_ind_binary)
  } else {}
  
  #################################################
  # MERGE Fire and Insect and Disease
  #################################################
  
  # bookkeeping
  print("combining Landfire fire and Landfire insect and disease")
  
  # load input rasters back in - save memory
  landfire_fire_years <- terra::rast(landfire_fire_years_outpath)
  landfire_ind_years <- terra::rast(landfire_ind_years_outpath)
  landfire_fire_binary <- terra::rast(landfire_fire_binary_outpath)
  landfire_ind_binary <- terra::rast(landfire_ind_binary_outpath)
  
  # for existing disturbance layer: 
  # fire code: 1
  # slow loss code: 2
  
  
  dist_year <- terra::merge(landfire_fire_years, landfire_ind_years) %>% # merge fire and slow loss 
    terra::app(function(x) cur_year - x ) %>% # calculate years since disturbance
    terra::classify(cbind(NA,-99)) # set no data values
  
  dist_type <- terra::merge(landfire_fire_binary, landfire_ind_binary) %>% # merge fire and slow loss
    terra::classify(cbind(NA, 0)) # set no data values
  
  
  # #inspect
  # plot(landfire_fire_years)
  # plot(dist_year)
  # plot(landfire_fire_binary)
  # plot(dist_type)
  
  # Export
  # -------------------------------------------------#
  
  # set projection?
  
  print("exporting disturbance year and disturbance type!")
  
  # create export directory
  output_dir_dist <- glue('{output_dir}02_Final/Landfire_Disturbance/')
  dir.create(output_dir_dist)
  
  # create output file names
  dist_year_outpath <- glue('{output_dir_dist}/{start_year}_{end_year}_{zone_name}_DisturbanceYear.tif')
  dist_type_outpath <- glue('{output_dir_dist}/{start_year}_{end_year}_{zone_name}_DisturbanceType.tif')
  
  #export
  writeRaster(dist_year, dist_year_outpath,
              overwrite = TRUE)
  writeRaster(dist_type, dist_type_outpath,
              overwrite = TRUE)
  
  toc()
  
  #remove products
  # option to remove intermediate files
  if(remove_intermediate_files == "Y") {
    rm(dist_year, dist_type)
  } else {}
  
  #clear unused memory
  gc()
}
