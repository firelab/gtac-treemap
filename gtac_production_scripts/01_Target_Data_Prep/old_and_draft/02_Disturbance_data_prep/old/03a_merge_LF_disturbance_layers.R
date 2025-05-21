# Merge landfire disturbance layers into disturbance year and disturbance code

# Written By Lila Leatherman (lila.Leatherman@usda.gov)
# Based on script "reclass_Landfire_disturbance_rasters_for_tree_list.py" by Karin Riley (karin.riley@usda.gov)

# Last Updated: 4/14/24

# Output rasters: 
# - years since most recent disturbance
# - type of disturbance 
# Based on Landfire disturbance only 

###############################
# SET Inputs
###############################

# Set inputs - from input script
this_dir <- this.path::this.dir() # Id where THIS script is located

# get path to input script
input_script_path <- glue::glue('{this_dir}/00b_zone_inputs_for_targetdata.R')

# source(input_script_path) # un-comment to run independently from the control script

###################################################
# LOAD DATA
###################################################

# load LF zone data
LF_zones <- vect(zones_path)

# Prep zone
#-----------------------------------------#

# select single LF zone
zone <- subset(LF_zones, LF_zones$ZONE_NUM == zone_num)

#project
zone %<>%
  terra::project(zone_output_crs)

# get name of zone
zone_name <- glue('LFz{zone_num}_{gsub(" ", "", zone$ZONE_NAME)}')

# Optional subset
#---------------------------------------#

if (!is.na(aoi_path)) {
  # load aoi subset - utah uintas only
  aoi <- vect(aoi_path) %>%
    project(zone_output_crs)
  
  # reassign
  zone <- aoi
  zone_name <- aoi_name
  message("using input shapefile as AOI")
} else{
  message("using landfire zone as AOI")
}

# set aoi_name field if it doesn't already exist via aoi subset
if(is.na(aoi_name)) {
  aoi_name <- ""
}

# Load input rasters
#-------------------------------------------#

# bookkeeping
message("combining Landfire fire and Landfire insect and disease")

# load input rasters back in - save memory
LF_fire_years <- terra::rast(LF_fire_years_outpath)
LF_ind_years <- terra::rast(LF_ind_years_outpath)
LF_fire_binary <- terra::rast(LF_fire_binary_outpath)
LF_ind_binary <- terra::rast(LF_ind_binary_outpath)


#####################################################
# MERGE
######################################################

# for existing disturbance layer: 
# fire code: 1
# slow loss code: 2

dist_year <- terra::merge(LF_fire_years, LF_ind_years) %>% # merge fire and slow loss 
  terra::app(function(x) model_year - x ) %>% # calculate years since disturbance
  terra::classify(cbind(NA,99)) %>%  # set no data values 
  terra::project(zone_output_crs) # make sure it's in the correct crs

dist_code <- terra::merge(LF_fire_binary, LF_ind_binary) %>% # merge fire and slow loss
  terra::classify(cbind(NA, 0)) %>% # set no data values 
  terra::project(zone_output_crs)  # make sure it's in the correct crs

#inspect
plot(LF_fire_years)
plot(dist_year)
plot(LF_fire_binary)
plot(dist_code)

# Export
# -------------------------------------------------#


message("exporting disturbance year and disturbance type!")

#export
writeRaster(dist_year, lf_disturb_year_outpath,
            datatype = "INT1U",
            overwrite = TRUE)

writeRaster(dist_code, lf_disturb_code_outpath,
            datatype = "INT1U",
            overwrite = TRUE)

# Remove unused files to start prep for next zone
rm(dist_year, dist_code)
rm(LF_ind_binary, LF_fire_binary, LF_ind_years, LF_fire_years)

gc()

# remove preliminary files
file.remove(LF_fire_years_outpath, LF_fire_binary_outpath,
            LF_ind_years_outpath, LF_ind_binary_outpath)

