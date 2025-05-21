# Crop, resample, and mask for DayMet climate data

# Written by Scott Zimmer (scott.zimmer@usda.gov)
# Updated by Lila Leatherman (lila.Leatherman@usda.gov)

# Required inputs: 
# - Raw climate layers
# - Reclassed final EVT_GP layers for each zone

# Last Updated: 5/6/25



#################################################################
# Set Inputs
#################################################################
# define year
year_input <- 2023

# define project area
study_area <- "CONUS"

# which zone to start on?
lf_zone_num_start <- 1


###################################################
# LOAD DATA
###################################################

# Load TreeMap script library
#--------------------------------------------------#

# load library 
this_proj = this.path::this.proj()
lib_path = glue::glue("{this_proj}/gtac_production_scripts/00_Library/treeMapLib.R")
source(lib_path)

targetDataProjectInputs(year_input, 
                        study_area)


# Load the landfire zones 
lf_zones <- terra::vect(lf_zones_path)

# Reproject the landfire zones to match the desired CRS
lf_zones<- terra::project(lf_zones, output_crs)
lf_zone_nums<- sort(lf_zones$ZONE_NUM)
z = which(lf_zone_num_start==lf_zone_nums)[1] # get index of starting zone
lf_zone_nums <- lf_zone_nums[z:length(lf_zone_nums)] # list zones to run

# Identify path to Daymet rasters
daymet_dir <- glue::glue('{home_dir}/01_Data/07_Daymet/daymet_north_america_normal/')

# list climate vars
clim_vars <- c("prcp", "srad", "swe", "tmax", "tmin", "vp", "vpd")

# Loop through landfire zones, masking, reprojecting, resampling, and saving data by zone ----


for (i in lf_zone_nums){
  
  #i = 1 # for testing
  zone_input <- i
  
  print(glue::glue("working on zone {zone_input}"))
  
  # set up target data env for zone
  targetDataZonalInputs(zone_input)
  
  lf_zone<- lf_zones[lf_zones$ZONE_NUM == zone_input,]
  
  # load desired mask for cropping and masking
  zmask <- terra::rast(glue::glue('{target_dir_mask_z}/evt_gp.tif'))
  
  # set focal width for potential resampling in final step
  # setting focal width here keeps it the same for all clim variables in a zone
  focal_w = 3
  
  for(var in clim_vars) {
    
    # for testing
    #var = clim_vars[1]
    
    print(glue::glue("working on {var}"))
    
    # load climate var
    clim <- terra::rast(glue::glue("{daymet_dir}/{var}_normal_1981to2010.tif"))
    
    # fill any holes in Dry Tortugas with custom function
    clim <- fillDayMetDryTortugas(clim, var)
  
    # Make a reprojected extent for cropping the climate data
    zmask_extent_reproj<-  terra::project(ext(zmask), 
                                          from = crs(zmask), 
                                          to = crs(clim))  
      
    # First crop climate data by the reprojected extent ----
    clim_crop <- terra::crop(clim, zmask_extent_reproj, snap="out")
    
    # Then reproject, resample, and mask to the tree mask for the given landfire zone ----
    clim_zone <- terra::crop(terra::project(x = clim_crop, 
                                            y = zmask,
                                            method = "cubic",
                                            align = TRUE,
                                            res = res(zmask),
                                            threads = TRUE),
                             zmask, mask = TRUE, touches = TRUE)
  
    
    
    #Check valid pixel length
    #------------------------------------------------------------------------------------#
    
    # Check that the number of valid pixels between Mask and Daymet are the same
  
    zmask_px <- length(values(zmask, na.rm = T))
    clim_px <- length(values(clim_zone, na.rm = T))
    
    print(glue::glue("zmask pixels: {zmask_px}, clim pixels: {clim_px} "))
    
    
    # Either Export layer, or apply focal zone to make the reprojection align 
    if(zmask_px == clim_px) {
      
      print(glue::glue("Mask layer and DayMet have the same number of pixels! Exporting {var} for zone {i}"))
      
      #If zmask pixels = clim px, then export
      writeRaster(clim_zone,
                  glue::glue('{target_dir_mask_z}/{var}.tif'),
                  datatype = "FLT4S", overwrite = TRUE)
      
    } else {
    
      # Start 
      repeat {
      
        # Fill holes if necessary - Try focal widths
        #--------------------------------------------------------------------------------------#
        
        print(glue::glue("zmask and {var} have different pixel numbers; filling DayMet holes using focal width = {focal_w}"))
        
        # Initial crop and perform focal analysis on the original Daymet rasters
        clim_focal<- terra::focal(terra::crop(clim, zmask_extent_reproj, touches=T), w=focal_w, fun="mean", na.policy="only", na.rm=T)
        
        # Final mask
        clim_zone<- terra::crop(terra::project(x = clim_focal, 
                                               y = zmask,
                                               method = "cubic",
                                               align = TRUE,
                                               res = res(zmask),
                                               threads = TRUE),
                                zmask, mask = TRUE, touches = TRUE)
        
        # Test for number of pixels again
        zmask_px <- length(values(zmask, na.rm = T))
        clim_px <- length(values(clim_zone, na.rm = T))
        zmask_px == clim_px
        #print(glue::glue("zmask pixels: {zmask_px}, clim pixels: {clim_px} "))
        
        
        # Either export the final output, or try again
        #----------------------------------------------------------------#
        if(zmask_px == clim_px) {
          
          print(glue::glue("zmask and DayMet have the same number of pixels! Exporting {var} for zone {i}"))
         
          #If zmask pixels = clim px, then export
          writeRaster(clim_zone,
                        glue::glue('{target_dir_mask_z}/{var}.tif'),
                        datatype = "FLT4S", overwrite = TRUE) 
          
          break
        } else {
          # increase focal width for the next attempt, but it was successful last time, keep focal_w the same for vars in the same zone
          focal_w = focal_w+2 
        }
        }
  
  # Clear garbage
  gc()

  
  
}
  }
}
