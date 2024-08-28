# FUNCTION TO REMOVE THE PROJECTION OF AN INPUT TIF

# USER INPUTS
#############################################

tif_path_in <- "//166.2.126.25/TreeMap/03_Outputs/07_Projects/2016_ImputationPrep/01_Raw_model_outputs/z16/raster/tiles/z16_2016_GTAC_ImputationPrep_tile1.tif" 

tif_path_out <- "D:/tmp/tif_strip_test.tif"

# End user inputs
##################################################

library(terra)

# define function
stripProjection <- function(tif) {
  
  crs(tif) <- NULL
  
  return(tif)
  
}

# load raster
tif_in <- terra::rast(tif_path_in)

# apply function
tif_out <- stripProjection(tif_in)

#export raster
terra::writeRaster(tif_out, 
                   tif_path_out)

