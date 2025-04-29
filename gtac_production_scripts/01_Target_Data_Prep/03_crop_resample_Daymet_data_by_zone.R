# Crop, resample, and initial mask for DayMet climate data

# Written by Scott Zimmer (scott.zimmer@usda.gov)
# Updated by Lila Leatherman (lila.Leatherman@usda.gov)


# Last Updated: 4/28/25

# Final Output Rasters: 


# Intermediate Output Rasters: 



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


# Load a landfire raster as a template to match
lf_rast <- terra::rast(evc_path)


# Identify path to Daymet rasters
daymet_dir <- glue::glue('{home_dir}/01_Data/07_Daymet/daymet_north_america_normal/')

# list climate vars
clim_vars <- c("prcp", "srad", "swe", "tmax", "tmin", "vp", "vpd")

# prcp<- terra::rast(glue::glue("{daymet_dir}/prcp_normal_1981to2010.tif"))
# srad<- terra::rast(glue::glue("{daymet_dir}/srad_normal_1981to2010.tif"))
# swe<- terra::rast(glue::glue("{daymet_dir}/swe_normal_1981to2010.tif"))
# tmax<- terra::rast(glue::glue("{daymet_dir}/tmax_normal_1981to2010.tif"))
# tmin<- terra::rast(glue::glue("{daymet_dir}/tmin_normal_1981to2010.tif"))
# vp<- terra::rast(glue::glue("{daymet_dir}/vp_normal_1981to2010.tif"))
# vpd<- terra::rast(glue::glue("{daymet_dir}/vpd_normal_1981to2010.tif"))

# 

# Loop through landfire zones, masking, reprojecting, resampling, and saving data by zone ----

# First create output directory 
#dir.create("./03_Outputs/05_Target_Rasters/v{year}/pre_mask/")


for (i in lf_zone_nums){
  
  #i = 1 # for testing
  zone_input <- i
  
  print(glue::glue("working on zone {zone_input}"))
  
  # set up target data env for zone
  targetDataZonalInputs(zone_input)
  
  lf_zone<- lf_zones[lf_zones$ZONE_NUM == zone_input,]
  
  # # Make a directory for saving landfire data for the zone
  # ifelse(i<10, 
  #        dir.create(paste0("./03_Outputs/05_Target_Rasters/v2023/pre_mask/z0",i)),
  #        dir.create(paste0("./03_Outputs/05_Target_Rasters/v2023/pre_mask/z",i)))
  # 
  # # Save that directory path as a string
  # ifelse(i<10, 
  #        out_dir<- paste0("./03_Outputs/05_Target_Rasters/v2023/pre_mask/z0",i),
  #        out_dir<- paste0("./03_Outputs/05_Target_Rasters/v2023/pre_mask/z",i))
  
  
  # Crop, mask, and resample layers to each landfire zone---
  
  # Read in the step 1 forest mask from the LandFire EVC layer for this zone----
  # if (i<10) {
  #   evc<- rast(paste0("../03_Outputs/05_Target_Rasters/v2023/pre_mask/z0",i,"/evc.tif"))
  # } else {
  #   evc<- rast(paste0("../03_Outputs/05_Target_Rasters/v2023/pre_mask/z",i,"/evc.tif"))
  # }
  
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
    
    # prcp_crop<- terra::crop(prcp, evc_extent_reproj, snap="out")
    # srad_crop<- terra::crop(srad, evc_extent_reproj, snap="out") 
    # swe_crop<- terra::crop(swe, evc_extent_reproj, snap="out")
    # tmax_crop<- terra::crop(tmax, evc_extent_reproj, snap="out")
    # tmin_crop<- terra::crop(tmin, evc_extent_reproj, snap="out")
    # vp_crop<- terra::crop(vp, evc_extent_reproj, snap="out")
    # vpd_crop<- terra::crop(vpd, evc_extent_reproj, snap="out")
    
    
    # Then reproject, resample, and mask to the tree mask for the given landfire zone ----
    clim_zone <- terra::crop(terra::project(x = clim_crop, 
                                            y = zmask,
                                            method = "cubic",
                                            align = TRUE,
                                            res = res(zmask),
                                            threads = TRUE),
                             zmask, mask = TRUE, touches = TRUE)
    
    # prcp_zone<- terra::crop(terra::project(x = prcp_crop, 
    #                                         y = evc,
    #                                         method = "cubic",
    #                                         align = TRUE,
    #                                         res = res(evc),
    #                                         threads = TRUE),
    #                          evc, mask = TRUE, touches = TRUE)
    # #
    # srad_zone<- terra::crop(terra::project(x = srad_crop, 
    #                                         y = evc,
    #                                         method = "cubic",
    #                                         align = TRUE,
    #                                         res = res(evc),
    #                                         threads = TRUE),
    #                          evc, mask = TRUE, touches = TRUE)
    # #
    # swe_zone<- terra::crop(terra::project(x = swe_crop, 
    #                                        y = evc,
    #                                        method = "cubic",
    #                                        align = TRUE,
    #                                        res = res(evc),
    #                                        threads = TRUE),
    #                         evc, mask = TRUE, touches = TRUE)
    # # correct negative SWE values
    # swe_zone[swe_zone < 0]<- 0
    # 
    # #
    # tmax_zone<- terra::crop(terra::project(x = tmax_crop, 
    #                                         y = evc,
    #                                         method = "cubic",
    #                                         align = TRUE,
    #                                         res = res(evc),
    #                                         threads = TRUE),
    #                          evc, mask = TRUE, touches = TRUE)
    # #
    # tmin_zone<- terra::crop(terra::project(x = tmin_crop, 
    #                                         y = evc,
    #                                         method = "cubic",
    #                                         align = TRUE,
    #                                         res = res(evc),
    #                                         threads = TRUE),
    #                          evc, mask = TRUE, touches = TRUE)
    # #
    # vp_zone<- terra::crop(terra::project(x = vp_crop, 
    #                                       y = evc,
    #                                       method = "cubic",
    #                                       align = TRUE,
    #                                       res = res(evc),
    #                                       threads = TRUE),
    #                        evc, mask = TRUE, touches = TRUE)
    # #
    # vpd_zone<- terra::crop(terra::project(x = vpd_crop, 
    #                                        y = evc,
    #                                        method = "cubic",
    #                                        align = TRUE,
    #                                        res = res(evc),
    #                                        threads = TRUE),
    #                         evc, mask = TRUE, touches = TRUE)
    
    
    # Save the final raster for each zone ----
    
    # writeRaster(clim_zone, 
    #             glue::glue('{target_dir_mask_z}/{var}.tif'), 
    #             datatype = "FLT4S", overwrite = TRUE)
    
    # writeRaster(prcp_zone, paste0(out_dir, "/prcp_normal_1981to2020.tif"), datatype = "FLT4S", overwrite = TRUE)
    # writeRaster(srad_zone, paste0(out_dir, "/srad_normal_1981to2020.tif"), datatype = "FLT4S", overwrite = TRUE)
    # writeRaster(swe_zone, paste0(out_dir, "/swe_normal_1981to2020.tif"), datatype = "FLT4S", overwrite = TRUE)
    # writeRaster(tmax_zone, paste0(out_dir, "/tmax_normal_1981to2020.tif"), datatype = "FLT4S", overwrite = TRUE)
    # writeRaster(tmin_zone, paste0(out_dir, "/tmin_normal_1981to2020.tif"), datatype = "FLT4S", overwrite = TRUE)
    # writeRaster(vp_zone, paste0(out_dir, "/vp_normal_1981to2020.tif"), datatype = "FLT4S", overwrite = TRUE)
    # writeRaster(vpd_zone, paste0(out_dir, "/vpd_normal_1981to2020.tif"), datatype = "FLT4S", overwrite = TRUE)
  
    # Load final post-mask evc for the zone
    #evc<- terra::rast(glue::glue('{target_dir_mask_z}/evc.tif'))
    
    # Load prcp and srad
    #prcp<- terra::rast(paste0(in_dir,"/prcp.tif"))
    #srad<- terra::rast(paste0(in_dir,"/srad.tif"))
    
    
    #Check valid pixel length
    #------------------------------------------------------------------------------------#
    
    # Check that the number of valid pixels between EVC and Daymet are the same
    # Only doing SRAD and PRCP, because Srad could be different but prcp and all others should be the same
    #evc_valid_pixel_length<- length(values(evc, na.rm=T))
    #prcp_valid_pixel_length<- length(values(prcp, na.rm=T))
    #srad_valid_pixel_length<- length(values(srad, na.rm=T))
  
    zmask_px <- length(values(zmask, na.rm = T))
    clim_px <- length(values(clim_zone, na.rm = T))
    
    print(glue::glue("zmask pixels: {zmask_px}, clim pixels: {clim_px} "))
    
    # if (#evc_valid_pixel_length != srad_valid_pixel_length |
    #     #evc_valid_pixel_length != prcp_valid_pixel_length
    #     evc_valid_pixel_length != clim_valid_pixel_length) {
    #   print(paste0("!! Mask layer and Daymet valid pixel length not identical for zone ",i,"!!"))
    # } else {
    #   print(paste0("Mask layer and Daymet valid pixel length are identical for zone ",i))
    # }
    
    
    
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
