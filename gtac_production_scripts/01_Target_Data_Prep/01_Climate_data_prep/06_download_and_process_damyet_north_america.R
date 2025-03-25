# Setup
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

library(daymetr)  
library(terra)


#

# Define the climate parameters to download
params_to_download<- c("tmin","tmax","vp","prcp","swe")

# Create base directories
dir.create(paste0(data_dir,"07_Daymet/daymet_north_america_annual/"))
dir.create(paste0(data_dir,"07_Daymet/daymet_north_america_normal/"))


# Loop through the parameters, downloading a .nc and converting to .tif, then calculating mean of all .tifs ----

for (i in params_to_download){
  
  # Create directory to populate with annual climate rasters
  dir.create(paste0(data_dir,"07_Daymet/daymet_north_america_annual/",i))
  
  # Download all years for a single parameter
  daymetr::download_daymet_ncss(location = c(72, -180, 18, -66),
                                start = 1981,
                                end = 2010,
                                frequency = "annual",
                                param = i,
                                mosaic = "na",
                                path = paste0(data_dir,"07_Daymet/daymet_north_america_annual/",i),
                                silent = F)
  
  # Convert the .nc files to .tif
  daymetr::nc2tif(paste0(data_dir,"07_Daymet/daymet_north_america_annual/",i))
  
  # Delete all unnecessary files
  file.remove(list.files(paste0(data_dir,"07_Daymet/daymet_north_america_annual/",i), 
                         pattern = c(".nc$|.json$"), full.names = T))
  
  #------------------#
  
  # Create the 30-year normal (mean) of the climate parameter -----
  
  # Read raster files path
  tile.list <- list.files(path = paste0(data_dir,"07_Daymet/daymet_north_america_annual/",i), 
                          pattern = "*.tif$", recursive = TRUE, full.names = TRUE)
  
  # Then calculate mean
  mean<- terra::mean(rast(tile.list), na.rm=T)
  
  # And save out
  writeRaster(mean, paste0(data_dir,"07_Daymet/daymet_north_america_normal/",i,"_normal_1981to2010.tif"))
  
  
}
