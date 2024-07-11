# Create disturbance layer inputs - based on Landfire only

# Written By Lila Leatherman (lila.Leatherman@usda.gov)
# Based on script "rmrs_production_scripts/00_USDA_TreeMap_2014/reclass_Landfire_disturbance_rasters_for_tree_list.py" by Karin Riley (karin.riley@usda.gov)

# Last Updated: 7/2/24


# Output rasters: 
# - landfire fire years 
# - landfire fire binary
# - landfire insect and disease years
# - landfire insect and disease binary


##############################
# SET Inputs
###############################

# breakup factor - how many tiles to break the area into? as a factor of area px 
# 1 = 1 tile, 5 = many tiles
break.up <- 5

# set number of cores used for parallelization
ncores <- 5

# get path to inputs script
this_dir <- this.path::this.dir()
inputs_script <- glue::glue('{this_dir}/00b_zone_inputs_for_targetdata.R')

# source(inputs_script) # un-comment to run independently from the control script


###################################################
# LOAD DATA
###################################################

# Load zone
#----------------------------------------#

# load LF zone data
LF_zones <- terra::vect(lf_zones_path)

# select single LF zone
zone <- subset(LF_zones, LF_zones$ZONE_NUM == zone_num) 

#project
zone <- terra::project(zone, lf_output_crs)

# get name of zone
zone_name <- glue::glue('LFz{zone_num}_{gsub(" ", "", zone$ZONE_NAME)}')


# Optional subset
#---------------------------------------#

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

# Final zone prep
#-------------------------------------------#
# set aoi_name field if it doesn't already exist via aoi subset
if(is.na(aoi_name)) {
  aoi_name <- ""
}

# Load Landfire disturbance data
#-----------------------------------------------------#

message("Loading all landfire data")
# list landfire files 
landfire_files_1999_2014 <- list.files(landfire_disturbance_dir_1999_2014, full.names = TRUE, recursive = TRUE, pattern = ".tif$")
landfire_files_2015_2020 <- list.files(landfire_disturbance_dir_2015_2020, full.names = TRUE, recursive = TRUE, pattern = ".tif$")
landfire_files_2021_2022 <- list.files(landfire_disturbance_dir_2021_2022, full.names = TRUE, recursive = TRUE, pattern = ".tif$")

# join all
landfire_files = c(landfire_files_1999_2014,
                   landfire_files_2015_2020,
                   landfire_files_2021_2022)

# filter files to only files we're interested in 
landfire_files %<>% 
  str_subset(pattern = "HDst", negate = TRUE)  %>% # remove historic disturbance
  cbind(str_extract(., "[1-2][0,9][0-9][0-9]")) %>% # bind with year
  as.data.frame() %>% # convert the list to a data.frame
  dplyr::rename("year" = "V2",
                "path" = ".") %>%
  dplyr::mutate(year = as.numeric(year)) %>%
  arrange(year) %>% # sort by year
  filter(year %in% year_list) # filter to years of interest

#inspect
#landfire_files

message("Loading landfire data as VRT")
# load all landfire files as vrt
landfire_dist <- vrt(landfire_files$path, glue::glue('{tmp_dir}/landfire_dist.vrt'), options = '-separate', overwrite = TRUE)

# get crs
#landfire_crs <- terra::crs(landfire_dist)
identical(landfire_crs, lf_output_crs)

# crop to zone
landfire_dist %<>%
  terra::crop(zone, mask = TRUE) 

# rename layers as years
names(landfire_dist) <- year_list

gc()

# #inspect
#landfire_dist

##### Change codes to reclassify
#--------------------------------------------------#
message("Reclassifying Landfire codes...")

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

# list codes to reclassify
nums <- c(-9999, seq(0, 1133, 1))


# Create matrices for reclassifying
#---------------------------------------------#

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
# PREP Landfire Disturbance
####################################################

# Crop landfire data
#------------------------------------#
# # Prep landfire data
# landfire_dist <- landfire_dist %>%
#   terra::crop(zone, mask = TRUE)
# 
# gc()

# Make tiles
#----------------------------------------------------#

# how big is the zone? 
# maybe: if zone > size, then tile
expanse <- terra::expanse(zone, unit = "km")

# break up raster into multiple sections to speed up processing
h <- base::ceiling(ncol(landfire_dist[[1]])/break.up)
v <- base::ceiling(nrow(landfire_dist[[1]])/break.up)

# aggregate - template for making tiles to divvy up zone
agg <- terra::aggregate(landfire_dist[[1]], fact = c(h,v))
agg[] <- 1:ncell(agg)

# inspect zones
plot(agg, alpha = 0.5)
plot(zone, add = TRUE)

# subset the raster and create temporary files
# tiles with only NA values are omitted
# the function returns file names for the temporary files
tiles <- landfire_dist %>%
  terra::makeTiles(agg, paste0(tempfile(), '_.tif'), na.rm = TRUE)

gc()

# Loop over tiles
#---------------------------------------------------------------#
message("Converting raw probability layers in change layers")

# set up dopar
cl <- makeCluster(ncores, outfile = glue::glue("{tmp_dir}/cl_report.txt"))
registerDoParallel(cl)
#registerDoSEQ() # option to register sequentially - for testing

# load packages to each cluster
clusterCall(cl, function(){
  library(tidyverse);
  library(magrittr);
  #library(glue);
  library(terra)
})

tic()

# foreach loop dopar over tiles
f <- foreach(i = 1:length(tiles),
             .packages= c("tidyverse", "terra", "doParallel", "foreach")
) %dopar% {
  
  # for testing
  #i = 1
  
  fn <- tiles[i]
  
  # read raster tile into memory
  tile_r <- terra::rast(fn) %>%
    terra::trim()
  
  # Prep Landfire fire layers
  # --------------------------------------------#
  
  # get year of most recent fire
  landfire_fire_years_tile <- 
    tile_r %>%
    terra::classify(cbind(nums, rcl_fire))  # reclass fire codes to binary indicator for each year 
  landfire_fire_years_tile <- 
    terra::app(landfire_fire_years_tile, which.max.hightie) %>% # get most recent year
    terra::classify(cbind(c(seq(1:length(year_list))), year_list)) # reclassify index values to years
  
  gc()
  
  # Export

  #---------------------------------------#
  # write these files out
  #writeRaster(landfire_fire_years, landfire_fire_years_outpath,
  #            overwrite = TRUE)
  #writeRaster(landfire_fire_binary, landfire_fire_binary_outpath,
  #            overwrite = TRUE)

  
  # remove unused files
  gc()
  rm(landfire_fire_years_tile)
  
  # Prep Landfire insect and disease
  # -----------------------------------------#
  
  # get year of most recent insect and disease
  landfire_ind_years_tile <- 
    tile_r %>%
    terra::classify(cbind(nums, rcl_ind))
  
  landfire_ind_years_tile <- 
    terra::app(landfire_ind_years_tile, which.max.hightie) %>% # get most recent year
    terra::classify(cbind(c(seq(1:length(year_list))), year_list)) # reclassify index values to years
  
  gc()
  
  

  # Export
  #---------------------------------------#
  
  writeRaster(landfire_ind_years_tile, 
              filename = paste0(tmp_dir, "/lf/ind_years_tile", i, ".tif"),
              datatype = "INT2U",
              overwrite = TRUE)
  
  # remove unused files
  rm(landfire_ind_years_tile)
  gc()
  
} # end loop over tiles

#############################################
# MERGE TILES
#############################################

# list fire tiles
fire_tiles <- list.files(path = glue::glue('{tmp_dir}/lf/'), 
                         pattern = "fire",
                         full.names = TRUE)

# list ind tiles
ind_tiles <- list.files(path = glue::glue('{tmp_dir}/lf/'), 
                        pattern = "ind",
                        full.names = TRUE)

# Read in tiles as vrt
lf_fire_years <- terra::vrt(fire_tiles,  glue('{tmp_dir}/lf_fire.vrt'), overwrite = TRUE)
lf_ind_years <- terra::vrt(ind_tiles, glue('{tmp_dir}/lf_ind.vrt'), overwrite = TRUE)

# Reclass to binary 
#----------------------------------------_---#

# reclassify to binary indicator of fire over all years
# fire code = 1
lf_fire_binary <-
  lf_fire_years %>%
  terra::classify(cbind(year_list, 1))

# reclassify to binary indicator of ind over all years
#insect and disease code = 2
lf_ind_binary <-
  lf_ind_years %>%
  terra::classify(cbind(year_list, 2))

# Export
#-------------------------------------------------#
message("Exporting LF fire years and binary raster...")
# write these files out
writeRaster(lf_fire_years, landfire_fire_years_outpath,
            datatype = "INT2U",
            overwrite = TRUE)
writeRaster(lf_fire_binary, landfire_fire_binary_outpath,
            datatype = "INT1U",
            overwrite = TRUE)

message("Exporting LF insect-disease years and binary raster....")
# write these files out
writeRaster(lf_ind_years, landfire_ind_years_outpath,
            datatype = "INT2U",
            overwrite = TRUE)
writeRaster(lf_ind_binary, landfire_ind_binary_outpath,
            datatype = "INT1U",
            overwrite = TRUE)