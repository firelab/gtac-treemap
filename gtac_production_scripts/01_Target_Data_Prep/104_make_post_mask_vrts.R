#Load libraries
library(terra)
library(dplyr)

# Set up
home_dir  <<- "//166.2.126.25/TreeMap/"
setwd(home_dir)

# 2020 ----

# Make directory for vrts
dir.create(paste0("./03_Outputs/05_Target_Rasters/v2020/post_mask_vrt"))

# Get files
layer_types<- list.files("./03_Outputs/05_Target_Rasters/v2020/post_mask/z01/", pattern=".tif$")

for (i in seq_along(layer_types)){
  files<- list.files("./03_Outputs/05_Target_Rasters/v2020/post_mask", recursive = T, pattern=layer_types[i], full.names = T)   # Get files of the given layer type
  vrt<- vrt(files, filename = paste0("./03_Outputs/05_Target_Rasters/v2020/post_mask_vrt/", gsub(".tif","",layer_types[i]), ".vrt")) # Make vrt
  #
}



# 2022 ----

# Make directory for vrts
dir.create(paste0("./03_Outputs/05_Target_Rasters/v2022/post_mask_vrt"))

# Get files
layer_types<- list.files("./03_Outputs/05_Target_Rasters/v2022/post_mask/z01/", pattern=".tif$")

for (i in seq_along(layer_types)){
  files<- list.files("./03_Outputs/05_Target_Rasters/v2022/post_mask", recursive = T, pattern=layer_types[i], full.names = T)   # Get files of the given layer type
  vrt<- vrt(files, filename = paste0("./03_Outputs/05_Target_Rasters/v2022/post_mask_vrt/", gsub(".tif","",layer_types[i]), ".vrt")) # Make vrt
  #
}


