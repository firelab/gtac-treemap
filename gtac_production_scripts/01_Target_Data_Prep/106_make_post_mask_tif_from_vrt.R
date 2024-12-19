#Load libraries
library(terra)
library(gdalUtilities)

# Set up
home_dir  <<- "//166.2.126.25/TreeMap/"
setwd(home_dir)


# 2020 ----

# Make directory for vrts translated to tifs
dir.create(paste0("./03_Outputs/05_Target_Rasters/v2020/post_mask_vrt_to_tif/"))

# Get files
layers<- list.files("./03_Outputs/05_Target_Rasters/v2020/post_mask_vrt/", pattern=".vrt$", full.names = T)
layer_names<- list.files("./03_Outputs/05_Target_Rasters/v2020/post_mask_vrt/", pattern=".vrt$", full.names = F)

for (i in seq_along(layers)){
  # Use gdal translate to convert vrt to tif
  gdalUtilities::gdal_translate(layers[i], paste0("./03_Outputs/05_Target_Rasters/v2020/post_mask_vrt_to_tif/", gsub(".vrt", ".tif", layer_names[i])))
}



# 2022 ----

# Make directory for vrts translated to tifs
dir.create(paste0("./03_Outputs/05_Target_Rasters/v2022/post_mask_vrt_to_tif/"))

# Get files
layers<- list.files("./03_Outputs/05_Target_Rasters/v2022/post_mask_vrt/", pattern=".vrt$", full.names = T)
layer_names<- list.files("./03_Outputs/05_Target_Rasters/v2022/post_mask_vrt/", pattern=".vrt$", full.names = F)

for (i in seq_along(layers)){
  # Use gdal translate to convert vrt to tif
  gdalUtilities::gdal_translate(layers[i], paste0("./03_Outputs/05_Target_Rasters/v2022/post_mask_vrt_to_tif/", gsub(".vrt", ".tif", layer_names[i])))
}


