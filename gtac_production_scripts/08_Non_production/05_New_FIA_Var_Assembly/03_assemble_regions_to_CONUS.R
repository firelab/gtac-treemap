library(terra)
#library(foreign)
library(glue)

home_dir<- "//afssxgtacnas311/ForestMAP/80_Workspace/40_TreeMap_Volume"

year <- 2022
project_name <- glue::glue("{year}_TM_Volume_Calcs")

attributes_list <- c("VOLCFSND_L_PX", 
                     "VOLCFSND_D_PX")

#set path to attribute_rasters
attributes_dir <- glue::glue("{home_dir}/Attribute_assembly/01_Zones/")

# set path for mosaicked CONUS outputs
mosaic_dir <- glue::glue("{home_dir}/Attribute_assembly/02_CONUS")


# Do the work
#---------------------------------------------------------------------------#

for(att in attributes_list) {
  att = attributes_list[2]
  
  print(glue::glue("Working on {att}"))
  
  #Direct to all zonal attribute layers
  att_rasters<- list.files(attributes_dir, pattern = glue::glue('{att}.tif$'), full.names = T, recursive = T)
  

  vrt_fname = glue::glue("{mosaic_dir}/{year}_{att}_CONUS.vrt")
  
  # Make a VRT and assemble a complete, mosaicked tif
  vrt <- terra::vrt(att_rasters, 
             filename = vrt_fname,
             overwrite = T)
  
  
  # Write out as tif
  writeRaster(vrt, 
              filename = gsub(".vrt", ".tif", vrt_fname),
              overwrite = T)
  
  # # Use gdal translate to convert vrt to tif - overwrites by default
  # gdalUtilities::gdal_translate(vrt_fname, 
  #                               gsub(vrt_fname, ".vrt", ".tif") )
}
