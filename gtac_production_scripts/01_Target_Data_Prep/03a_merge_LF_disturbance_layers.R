# Merge landfire disturbance layers into disturbance year and disturbance code

# Written By Lila Leatherman (lila.Leatherman@usda.gov)
# Based on script "reclass_Landfire_disturbance_rasters_for_tree_list.py" by Karin Riley (karin.riley@usda.gov)

# Last Updated: 3/11/24

# Output rasters: 
# - years since most recent disturbance
# - type of disturbance 
# Based on Landfire disturbance only 

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

###################################################
# LOAD DATA
###################################################

# load lcms projections
lcms_crs <- crs(lcms_proj)

#load landfire projection
landfire_crs <- crs(landfire_proj)

# bookkeeping
print("combining Landfire fire and Landfire insect and disease")

# load input rasters back in - save memory
landfire_fire_years <- terra::rast(landfire_fire_years_outpath)
landfire_ind_years <- terra::rast(landfire_ind_years_outpath)
landfire_fire_binary <- terra::rast(landfire_fire_binary_outpath)
landfire_ind_binary <- terra::rast(landfire_ind_binary_outpath)

#####################################################
# MERGE
######################################################

# for existing disturbance layer: 
# fire code: 1
# slow loss code: 2

dist_year <- terra::merge(landfire_fire_years, landfire_ind_years) %>% # merge fire and slow loss 
  terra::app(function(x) model_year - x ) %>% # calculate years since disturbance
  terra::classify(cbind(NA,99)) # set no data values

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

#export
writeRaster(dist_year, glue::glue('{target_dir_z}/01_final/{cur.zone.zero}_{aoi_name}disturb_year_LF.tif'),
            overwrite = TRUE)
writeRaster(dist_type, glue::glue('{target_dir_z}/01_final/{cur.zone.zero}_{aoi_name}disturb_code_LF.tif'),
            overwrite = TRUE)

