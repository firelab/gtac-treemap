#Load libraries
library(terra)

# Set up

home_dir  <<- "//166.2.126.25/TreeMap/"
setwd(home_dir)


# Load a landfire raster as a template to match
lf_raster<- rast("./01_Data/02_Landfire/LF_220/Vegetation/EVC/LF2022_EVC_220_CONUS/Tif/LC22_EVC_220.tif")

# Load the landfire zones 
lf_zones<- vect("./01_Data/02_Landfire/LF_zones/Landfire_zones/refreshGeoAreas_041210.shp")

# Reproject the landfire zones to match the lf raster
lf_zones<- terra::project(lf_zones, lf_raster)
lf_zone_nums<- sort(lf_zones$ZONE_NUM)



# 2020 ----

for (i in lf_zone_nums){
  
  lf_zone<- lf_zones[lf_zones$ZONE_NUM == i,]
  
  # Save that directory path as a string
  ifelse(i<10, 
       out_dir<- paste0("./03_Outputs/05_Target_Rasters/v2020/post_mask/z0",i),
       out_dir<- paste0("./03_Outputs/05_Target_Rasters/v2020/post_mask/z",i))
  
  # Get the in_dir for reading in pre-mask data
  ifelse(i<10, 
        in_dir<- paste0("./03_Outputs/05_Target_Rasters/v2020/post_mask/z0",i),
       in_dir<- paste0("./03_Outputs/05_Target_Rasters/v2020/post_mask/z",i))
  
  
  # Read in layers
  aspect<- rast(paste0(in_dir, "/aspect.tif"))
  easting<- rast(paste0(in_dir, "/easting.tif"))
  northing<- rast(paste0(in_dir, "/northing.tif"))
  
  # Correct the issue. Where aspect = -1, easting and northing should be 0
  easting<- mask(easting, aspect, maskvalues=-1, updatevalue=0)
  northing<- mask(northing, aspect, maskvalues=-1, updatevalue=0)
  
  # First need to rename the original incorrect easting/northing
  file.rename(paste0(in_dir, "/easting.tif"), paste0(in_dir, "/easting_original.tif"))
  file.rename(paste0(in_dir, "/northing.tif"), paste0(in_dir, "/northing_original.tif"))
  
  # Then save corrected easting/northing
  writeRaster(easting, paste0(out_dir, "/easting.tif"), datatype = "FLT4S")
  writeRaster(northing, paste0(out_dir, "/northing.tif"), datatype = "FLT4S")
  
  # Then delete original, incorrect
  file.remove(paste0(in_dir, "/easting_original.tif"))
  file.remove(paste0(in_dir, "/northing_original.tif"))
  
}



# 2022 ----

for (i in lf_zone_nums){
  
  lf_zone<- lf_zones[lf_zones$ZONE_NUM == i,]
  
  # Save that directory path as a string
  ifelse(i<10, 
         out_dir<- paste0("./03_Outputs/05_Target_Rasters/v2022/post_mask/z0",i),
         out_dir<- paste0("./03_Outputs/05_Target_Rasters/v2022/post_mask/z",i))
  
  # Get the in_dir for reading in pre-mask data
  ifelse(i<10, 
         in_dir<- paste0("./03_Outputs/05_Target_Rasters/v2022/post_mask/z0",i),
         in_dir<- paste0("./03_Outputs/05_Target_Rasters/v2022/post_mask/z",i))
  
  
  # Read in layers
  aspect<- rast(paste0(in_dir, "/aspect.tif"))
  easting<- rast(paste0(in_dir, "/easting.tif"))
  northing<- rast(paste0(in_dir, "/northing.tif"))
  
  # Correct the issue. Where aspect = -1, easting and northing should be 0
  easting<- mask(easting, aspect, maskvalues=-1, updatevalue=0)
  northing<- mask(northing, aspect, maskvalues=-1, updatevalue=0)
  
  # First need to rename the original incorrect easting/northing
  file.rename(paste0(in_dir, "/easting.tif"), paste0(in_dir, "/easting_original.tif"))
  file.rename(paste0(in_dir, "/northing.tif"), paste0(in_dir, "/northing_original.tif"))
  
  # Then save corrected easting/northing
  writeRaster(easting, paste0(out_dir, "/easting.tif"), datatype = "FLT4S")
  writeRaster(northing, paste0(out_dir, "/northing.tif"), datatype = "FLT4S")
  
  # Then delete original, incorrect
  file.remove(paste0(in_dir, "/easting_original.tif"))
  file.remove(paste0(in_dir, "/northing_original.tif"))
  
}

