# Set up
setwd(this.path::here())

# home dir

# Id where script is located
this.path <- this.path::this.path()

# get path to input script
spl <- stringr::str_split(this.path, "/")[[1]]
setup_dirs.path <- paste( c(spl[c(1:(length(spl)-2))],
                              "00_Library/setup_dirs.R" ),
                            collapse = "/")

source(setup_dirs.path)

# data directory - where source data are located
data_dir <- glue::glue('{home_dir}/01_Data/')

library(terra)

# Hawaii ----

# Load annual rasters  --
tmax_files<- list.files(paste0(data_dir,"07_Daymet/daymet_hawaii_annual/tmax/"), full.names = T)
tmin_files<- list.files(paste0(data_dir,"07_Daymet/daymet_hawaii_annual/tmin/"), full.names = T)


for (i in seq_along(tmax_files)){
  
  # Load the min and max raster for a year
  max<- rast(tmax_files[i])
  min<- rast(tmin_files[i])
  
  # Calculate annual vpd from tmin and tmax 
  # Citaton: David B. Lobell et al. ,Greater Sensitivity to Drought Accompanies
  # Maize Yield Increase in the U.S. Midwest.Science344,516-519(2014).DOI:10.1126/science.1251423
  
  assign(paste0("vpd_",i), (0.6107 * exp(17.269 * max / (237.3 + max))) - (0.6107 * exp(17.269 * min / (237.3 + min))) )
  
}


# Stack the annual vpd rasters
stack<- rast(mget(paste0("vpd_",seq_along(tmax_files))))

# Calculate pixel means
stack_mean<- mean(stack, na.rm=T)

# Convert from kPa to Pa
stack_mean<- 1000*stack_mean

# Then save it with the correct name
writeRaster(stack_mean, paste0(data_dir,"07_Daymet/daymet_hawaii_normal/vpd_normal_1981to2010.tif"))



# North America ----

# Clear environment
rm(list=ls())

# Load annual rasters  --
tmax_files<- list.files(paste0(data_dir,"07_Daymet/daymet_north_america_annual/tmax/"), full.names = T)
tmin_files<- list.files(paste0(data_dir,"07_Daymet/daymet_north_america_annual/tmin/"), full.names = T)


for (i in seq_along(tmax_files)){
  
  # Load the min and max raster for a year
  max<- rast(tmax_files[i])
  min<- rast(tmin_files[i])
  
  # Calculate annual vpd from tmin and tmax 
  # Citaton: David B. Lobell et al. ,Greater Sensitivity to Drought Accompanies
  # Maize Yield Increase in the U.S. Midwest.Science344,516-519(2014).DOI:10.1126/science.1251423
  
  assign(paste0("vpd_",i), (0.6107 * exp(17.269 * max / (237.3 + max))) - (0.6107 * exp(17.269 * min / (237.3 + min))) )
  
}


# Stack the annual vpd rasters
stack<- rast(mget(paste0("vpd_",seq_along(tmax_files))))

# Calculate pixel means
stack_mean<- mean(stack, na.rm=T)

# Convert from kPa to Pa
stack_mean<- 1000*stack_mean

# Then save it with the correct name
writeRaster(stack_mean, paste0(data_dir,"07_Daymet/daymet_north_america_normal/vpd_normal_1981to2010.tif"))

