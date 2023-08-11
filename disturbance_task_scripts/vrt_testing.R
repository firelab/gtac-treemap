# vrt testing

#### EXAMPLE

library(terra)
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
names(vrt_test) <- c("FastLoss_Raw_Prob", "SlowLoss_Raw_Prob", "Gain_Raw_Prob")
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

vrt_crop

# test extract 