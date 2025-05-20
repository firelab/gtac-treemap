# For each zone:
# Compare all target layers to make sure they have the same number of pixels, extents match, etc 

#################################################################
# Set Inputs
#################################################################

# define year
year_input <- 2023

# define project area
study_area <- "CONUS"

# which zone to start on?
lf_zone_num_start <- 7

################################################################
# Load Library
################################################################

# Load TreeMap script library
#--------------------------------------------------#

# necessary to load this before calling any of the home_dir, fia_dir, or data_dir objects

# load library 
this_proj = this.path::this.proj()
lib_path = glue::glue("{this_proj}/gtac_production_scripts/00_Library/treeMapLib.R")
source(lib_path)


###################################################
# LOAD DATA
###################################################

#source(project_inputScript <- glue::glue("{this_dir}/00a_project_inputs_for_targetdata.R"))
targetDataProjectInputs(year_input = year_input,
                        study_area = study_area)


# Load the landfire zones 
lf_zones <- terra::vect(lf_zones_path)

# Reproject the landfire zones to match the desired CRS
lf_zones<- terra::project(lf_zones, output_crs)
lf_zone_nums<- sort(lf_zones$ZONE_NUM)
z = which(lf_zone_num_start==lf_zone_nums)[1] # get index of starting zone
lf_zone_nums <- lf_zone_nums[z:length(lf_zone_nums)] # list zones to run

# create data frame to hold outputs
layer_info_all <- data.frame()

# create directory where that data frame will be saved
dir.create(glue::glue("{target_dir}/eval/layer_pixel_counts/"))

for(zone_input in lf_zone_nums){
 
  #zone_input = 1 # for testing
  zone_num = zone_input
  

  message(glue::glue("Checking target layer pixel numbers for zone {zone_input}"))
  targetDataZonalInputs(zone_input)
  
  # list all tifs in target folder
  layer_fns <- list.files(target_dir_mask_z, pattern = ".tif$", full.names = TRUE)
  
  # we don't care about evt_name
  layer_fns <- layer_fns[(str_detect(layer_fns,"evt_name", negate = TRUE))]
  
  # create data frame
  layer_info = data.frame(layer_fns)
  layer_info$name = NA
  layer_info$numpx = NA
  layer_info$zone = zone_input
  
  # get layer names from fnames using regex / gsub
    for (i in seq_along(layer_info$layer_fns)) {
      layer_info$name[i] = gsub(".tif", "", str_split(layer_info$layer_fns, "/")[[i]][length(str_split(layer_info$layer_fns,"/")[[i]])])
  }
  
  
  # load all .tifs in target folder
  target_stack <- terra::vrt(layer_fns, filename = glue::glue("{tmp_dir}/target_layers.vrt"), options = "-separate", overwrite = TRUE)
  names(target_stack) = layer_info$name
  
  message("counting pixel numbers in each layer")
  
  # loop over each layer
  for (i in seq_along(names(target_stack))){
    
    # count px in each tif and append to data fra,e
    r <- target_stack[[i]]
    layer_info$numpx[[i]] <- length(values(r, na.rm=T))
    
  }
  
  gc()
  
  # bind with data frame
  layer_info_all <- rbind(layer_info_all, layer_info)
  
  # write out to file
  write.csv(layer_info_all, glue::glue('{target_dir}/eval/layer_pixel_counts/px_counts.csv'), row.names = FALSE)
  
  # look at data frame; report out if any layers don't have the same # of px   
  
  if(n_distinct(layer_info$numpx) == 1) {
    print(glue::glue("zone {zone_input} - all target layers have the same number of px"))
  } else if(n_distinct(layer_info$numpx > 1)){
    print(layer_info)
    print(glue::glue("zone {zone_input} - target layers have different numbers of pixels, see table"))
    

  }
  
  
}
 


