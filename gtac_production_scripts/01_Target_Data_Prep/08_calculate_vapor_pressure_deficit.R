# Set up
setwd(this.path::here())

library(terra)

# Hawaii ----

# Load annual rasters  --
tmax_files<- list.files("./daymet_hawaii_annual/tmax/", full.names = T)
tmin_files<- list.files("./daymet_hawaii_annual/tmin/", full.names = T)


for (i in seq_along(tmax_files)){
  
  # Load the min and max raster and convert to kelvin
  max<- rast(tmax_files[i]) + 273.15
  min<- rast(tmin_files[i]) + 273.15
  
  # Calculate annual vpd from tmin and tmax 
  # Citaton: David B. Lobell et al. ,Greater Sensitivity to Drought Accompanies
  # Maize Yield Increase in the U.S. Midwest.Science344,516-519(2014).DOI:10.1126/science.1251423
  
  assign(paste0("vpd_",i), (0.6107 * exp(17.269 * max / (237.3 + max))) - (0.6107 * exp(17.269 * min / (237.3 + min))) )
  
}


# Stack the annual vpd rasters
stack<- rast(mget(paste0("vpd_",seq_along(tmax_files))))

# Calculate pixel means
stack_mean<- mean(stack, na.rm=T)

# Then save it with the correct name
writeRaster(stack_mean, paste0("./daymet_hawaii_normal/vpd_normal_1981to2010.tif"))



# North America ----

# Clear environment
rm(list=ls())

# Load annual rasters  --
tmax_files<- list.files("./daymet_north_america_annual/tmax/", full.names = T)
tmin_files<- list.files("./daymet_north_america_annual/tmin/", full.names = T)


for (i in seq_along(tmax_files)){
  
  # Load the min and max raster and convert to kelvin
  max<- rast(tmax_files[i]) + 273.15
  min<- rast(tmin_files[i]) + 273.15
  
  # Calculate annual vpd from tmin and tmax 
  # Citaton: David B. Lobell et al. ,Greater Sensitivity to Drought Accompanies
  # Maize Yield Increase in the U.S. Midwest.Science344,516-519(2014).DOI:10.1126/science.1251423
  
  assign(paste0("vpd_",i), (0.6107 * exp(17.269 * max / (237.3 + max))) - (0.6107 * exp(17.269 * min / (237.3 + min))) )
  
}


# Stack the annual vpd rasters
stack<- rast(mget(paste0("vpd_",seq_along(tmax_files))))

# Calculate pixel means
stack_mean<- mean(stack, na.rm=T)

# Then save it with the correct name
writeRaster(stack_mean, paste0("./daymet_north_america_normal/vpd_normal_1981to2010.tif"))

