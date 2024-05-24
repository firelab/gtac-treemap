# Setup
setwd(this.path::here())

library(daymetr)  
library(terra)

#
# Read in rasters
north_america_ras<- rast("./daymet_north_america_normal/prcp_normal_1981to2010.tif")
hawaii_ras<- rast("./daymet_hawaii_normal/prcp_normal_1981to2010.tif")

##

srad_from_gee<- rast("./daymet_srad_normal_from_GEE/srad_normal_1981to2010.tif")


# First crop srad by a reprojected extent ----
north_america_extent_reproj<- terra::project(ext(north_america_ras), 
                                             from = crs(north_america_ras),
                                             to = crs(srad_from_gee))

hawaii_extent_reproj<- terra::project(ext(hawaii_ras), 
                                      from = crs(hawaii_ras),
                                      to = crs(srad_from_gee))


srad_from_gee_na<- crop(srad_from_gee, north_america_extent_reproj)
srad_from_gee_hawaii<- crop(srad_from_gee, hawaii_extent_reproj)



# Reproject SRAD from Google Earth Engine to match other rasters
srad_from_gee_na<- terra::project(x=srad_from_gee_na,y=north_america_ras, 
                                  method="near",res=res(north_america_ras), 
                                  align=T)

srad_from_gee_hawaii<- terra::project(x=srad_from_gee_hawaii,y=hawaii_ras, 
                                      method="near",res=res(north_america_ras), 
                                      align=T)

# Crop and mask
srad_north_america<- terra::mask(terra::crop(srad_from_gee_na, north_america_ras), north_america_ras)
srad_hawaii<- terra::mask(terra::crop(srad_from_gee_hawaii, hawaii_ras), hawaii_ras)


# Save
writeRaster(srad_north_america, "./daymet_north_america_normal/srad_normal_1981to2010.tif")
writeRaster(srad_hawaii, "./daymet_hawaii_normal/srad_normal_1981to2010.tif")

