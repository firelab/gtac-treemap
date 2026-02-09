library(terra)
library(dplyr)
library(foreign)
library(tidyverse)
library(future.apply)
library(parallel)

# Write function for running catalyze in parallel on tiles
process_tile <- function(tile_path, output_folder) {
  library(terra)
  
  # Load the specific tile
  r <- rast(tile_path)
  
  # Re-assign levels of the raster
  levels(r)<- final_cats
  
  # Run the catalyze function to assemble multiband tif of attribute layers
  cat_r <- catalyze(r)
  #
  # Save the result
  out_path <- file.path(output_folder, basename(tile_path))
  writeRaster(cat_r, out_path, overwrite = TRUE)
  
  return(out_path)
}

# Setup
setGDALconfig("GDAL_PAM_ENABLED", "NO") # set this to avoid making unnecessary .tif.aux.xml files
'%!in%' <- function(x,y)!('%in%'(x,y))
setwd(this.path::here())

# Create directories for storing outputs
dir.create("../tiles")
dir.create("../processed_tiles")
dir.create("../attributes")
output_folder<- "../processed_tiles"

# Set number of cores to use
n_cores<- detectCores()-10



# Direct to imputation mosaic
imputation<- rast("../imputation_mosaic.tif")
imputation_rat<- read.dbf("../imputation_mosaic.tif.vat.dbf")
  
# Get the categories from the imputation  
all_cats<- cats(imputation)[[1]]

# Define the layers you want to make rasters of, along with the TM_ID
final_cats<- all_cats[,c("TM_ID", "ALSTK", "BALIVE")]

# Make tiles to run in parallel
tile_files <- makeTiles(imputation, y = c((nrow(imputation)/(n_cores)*2), (ncol(imputation)/(n_cores)*2)), # this generates approximately 10 tiles per core
                            filename = file.path("../tiles", "tile_.tif"),
                            na.rm=T)
    
    
#  Run the attribute layer processing in parallel
t1<-Sys.time()
cl <- makeCluster(n_cores)
clusterExport(cl, varlist = c("output_folder", "final_cats")) # Send variables to workers
processed_paths <- clusterApplyLB(cl, tile_files, process_tile, output_folder = output_folder)
    
    
stopCluster(cl)
print(difftime(Sys.time(),t1))
    
    
# Merge the results back together into one raster with the correct names and save
final_raster <- vrt(list.files(output_folder, pattern = ".tif$", full.names = TRUE), set_names=T)
    
# write the final raster mosaic of attributes
writeRaster(final_raster, paste0("../attributes/attribute_layers.tif"), 
                gdal=c("COMPRESS=LZW"),
                overwrite=T)
    
