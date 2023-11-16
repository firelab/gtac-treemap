# vrt testing

#### EXAMPLE

library(terra)
library(tidyverse)
library(magrittr)

y <- rast(nrow=10, ncol=10, res=1, vals = 1:100)
writeRaster(y,"y.tif", overwrite=TRUE)
writeRaster(y, "y1.tif", overwrite = TRUE)
writeRaster(y, "y2.tif", overwrite = TRUE)
r.list <- c("y.tif","y1.tif","y2.tif")

#If all rasters have the same extent and resolution, as in your example, you can just use rast()

rast(r.list)


#But you can now also do

vrt(r.list, "test.vrt", options="-separate", overwrite=TRUE)


# set path to rasters
lcms_path <- '//166.2.126.25/TreeMap/01_Data/05_LCMS/01_Threshold_Testing/01_Raw/02_Raw_Probabilities/'

year <- 1999

list.files(lcms_path, pattern = "*.tif$")
list.files(lcms_path, pattern = "1999.+.tif$")
list.files(lcms_path, pattern = paste0(year, '.+.tif$'))

year_files <- list.files(lcms_path, pattern = paste0(year, '.+.tif$'), full.names = TRUE)

# subset for testing
#year_files <- year_files[1:10]
year_files


#make vrt
vrt_test <- vrt(year_files, "test.vrt", options = 'separate', overwrite = TRUE)

#inspect
vrt_test

vrt_test[1]
vrt_test[[1]]

# rename layers
names(vrt_test) <- c( "SlowLoss_Raw_Prob", "FastLoss_Raw_Prob", "Gain_Raw_Prob")
vrt_test


# summarize
#summary(vrt_test) # hangs

# get aoi for cropping
# set home dir
home_dir <- ("//166.2.126.25/TreeMap/")
aoi_path <- paste0(home_dir, "01_Data/03_AOIs/UT_Uintas_rect_NAD1983.shp")

zone <- terra::vect(aoi_path)

# crop
zone <- project(zone, crs(vrt_test))
vrt_crop <- terra::crop(vrt_test, zone)

#inspect
vrt_crop

NAvalue <- -32768

# update NA values
vrt_crop_noNA <- terra::classify(vrt_crop, cbind(NAvalue, NA))

#inspect
vrt_crop_noNA

# how to get maximum probability class - simple
maxClass <- which.max(vrt_crop_noNA)
maxClass

# how to get maximum probability class that also exceeds threshold
change_thresholds <- c(14, # slow loss
                       29, # fast loss
                       20 # gain
                       )


# Find the most confident class
maxConf <- max(vrt_crop_noNA)
maxConf

# reclassify classes to match threshold
# there's probably a better way to do this
vrt_reclass <- vrt_crop_noNA
vrt_reclass[[1]] <- terra::classify(vrt_reclass[[1]], cbind(seq(1, change_thresholds[1], 1), NA))
vrt_reclass[[2]] <- terra::classify(vrt_reclass[[2]], cbind(seq(1, change_thresholds[2], 1), NA))
vrt_reclass[[3]] <- terra::classify(vrt_reclass[[3]], cbind(seq(1, change_thresholds[3], 1), NA))

# get maximum value that also exceeds threshold
maxClass2 <- which.max(vrt_reclass)

# inpsect
plot(maxClass2)
freq(maxClass2)

# fill stable back in
maxClass2 <- terra::classify(maxClass2, cbind(NA, 4))
plot(maxClass2)
freq(maxClass2)


# burn in non-processing where the input was null
non_processing <- 
  vrt_crop %>%
  min()

maxClass2 %<>%
  mask(non_processing, maskvalue = NAvalue)

freq(maxClass2)


# do this for every year, make a new layer stack of yearly change

# reclass to only slow loss

# which.max

#### i can do a similar thing for landfire -- 
# but i don't have to do the threshold part, i can just load all years as a VRT 
# reclass to extract fire
# and do the which max
# to get most recent year fire


# then i need to combine 


