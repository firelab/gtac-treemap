# Written by Scott Zimmer (scott.zimmer@usda.gov)
# Updated by Lila Leatherman (lila.leatherman@usda.gov)

#Load libraries
library(terra)
library(dplyr)
library(gdalUtilities)


# will need to be updated to account for zone numbers for CONUS vs AK and HI

# Set up
home_dir  <<- "//166.2.126.25/TreeMap/"
setwd(home_dir)

year_input <- 2023

# Make directory for vrts
dir.create(glue::glue("{home_dir}/03_Outputs/05_Target_Rasters/v{year_input}/national_vrts/"))
# Make directory for vrts translated to tifs
dir.create(glue::glue("{home_dir}/03_Outputs/05_Target_Rasters/v{year_input}/national_tifs/"))


# Get files
layer_types<- list.files(glue::glue("./03_Outputs/05_Target_Rasters/v{year_input}/one_mask/z01/"), pattern=".tif$")

for (i in seq_along(layer_types)){
  
  #i = 1 # for testing
  
  # Get files of the given layer type
  files<- list.files(glue::glue("./03_Outputs/05_Target_Rasters/v{year_input}/one_mask"), recursive = T, pattern=layer_types[i], full.names = T)   
  
  # Make vrt
  vrt_fname = glue::glue('./03_Outputs/05_Target_Rasters/v{year_input}/national_vrts/{gsub(".tif","",layer_types[i])}.vrt')
  vrt<- terra::vrt(files, 
            filename = vrt_fname,
            overwrite = TRUE) 
  
  # Use gdal translate to convert vrt to tif - overwrites by default
  gdalUtilities::gdal_translate(vrt_fname, 
                                glue::glue('./03_Outputs/05_Target_Rasters/v{year_input}/national_tifs/{gsub(".vrt", ".tif", layer_types[i])}'))

  }
  
  
 
  
  
    