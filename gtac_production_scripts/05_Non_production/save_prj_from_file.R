# get projection from Landfire rasters and write out as a .prj object 
# so that it can be referenced independent from the raster files themselves
library(terra)
library(tidyverse)

# Set inputs
#-----------------------------------------------#
# Landfire versions to process

LF_versions <- c("LF_200", "LF_220", "LF_230")

# Initialize home dir
#-----------------------------------------------#
# Id where THIS script is located
this_proj <- this.path::this.proj()

# get path to input script
setup_dirs_path <-  glue::glue("{this_proj}/gtac_production_scripts/00_Library/setup_dirs.R" )
                            
source(setup_dirs_path)


#-------------------------------------------------------------------#

#make dictionary of landfire version years
LF_version_dict <- data.frame(version = c("LF_200", "LF_220", "LF_230"),
                           year_full = c(2016, 2020, 2022)) %>%
  mutate(version_num = as.numeric(gsub("LF_", "", version)),
         year_short = as.numeric(substr(year_full, 3,4)))

# load and save CRS from different landfire versions
for (i in seq_along(LF_versions)) {
  
  #i = 1
  
  # get dictionary values for version
  version_i <- LF_versions[i]
  version_i_dict <- LF_version_dict %>%
    filter(version == version_i)
  
  version_num <- version_i_dict %>% select(version_num) %>% as.numeric()
  version_year <- version_i_dict %>% select(year_full) %>% as.numeric()
  version_year_short <- version_i_dict %>% select(year_short) %>% as.numeric()
  
  # load in raster
  lf_ras <- terra::rast(glue::glue("{home_dir}/01_Data/02_Landfire/{version_i}/Vegetation/EVT/LF{version_year}_EVT_{version_num}_CONUS/Tif/LC{version_year_short}_EVT_{version_num}.tif"))
  
  # get crs from raster
  lf_crs <- terra::crs(lf_ras)
  
  # make new dir for CRS if it does not exist
  # and save CRS
  if(!exists(glue::glue("{home_dir}/01_Data/02_Landfire/{version_i}/CRS"))){
    dir.create(glue::glue("{home_dir}/01_Data/02_Landfire/{version_i}/CRS"))
    write(lf_crs, file = glue::glue("{home_dir}/01_Data/02_Landfire/{version_i}/CRS/{version_i}_crs.prj"))
  }
  
}


#-------------------------------------------------------------------------#

# Load and compare CRS

lf1999 <- terra::rast("//166.2.126.25/TreeMap//01_Data/02_Landfire/LF_USDIST/US_DIST1999/Tif/us_dist1999.tif")
lf1999_crs <- terra::crs(lf1999)

# path to existing crs
lf_crs <- terra::crs(glue::glue("{home_dir}/01_Data/02_Landfire/landfire_crs.prj"))

# treemap raster to use as reference
tm16 <- terra::rast(glue::glue("{home_dir}/01_Data/01_TreeMap2016_RDA/RDS-2021-0074_Data/Data/TreeMap2016.tif"))

# get crs
tm16_crs <- terra::crs(tm16)

# Landfire raster(s) to use as reference- 2016 target
# list
target_rasters <- list.files(glue::glue("{home_dir}/03_Outputs/05_Target_Rasters/v2016_RMRS/z16/"), pattern = ".tif$", full.names = TRUE)

# load
target <- terra::rast(target_rasters[[1]])

# get crs
target_crs <- terra::crs(target)

# lf200
lf200_crs <- terra::crs(glue::glue("{home_dir}/01_Data/02_Landfire/LF_200/CRS/LF_200_crs.prj"))

# lf220
lf220_crs <- terra::crs(glue::glue("{home_dir}/01_Data/02_Landfire/LF_220/CRS/LF_220_crs.prj"))

# lf230
lf230_crs <- terra::crs(glue::glue("{home_dir}/01_Data/02_Landfire/LF_230/CRS/LF_230_crs.prj"))


# inspect
#----------------------------------#

terra::crs(lf1999_crs, describe = TRUE)
terra::crs(lf_crs, describe = TRUE)
terra::crs(tm16_crs, describe = TRUE)
terra::crs(target_crs, describe = TRUE)
terra::crs(lf200_crs, describe = TRUE)
terra::crs(lf220_crs, describe = TRUE)
terra::crs(lf230_crs, describe = TRUE)

identical(lf1999_crs, lf_crs)
identical(lf_crs, target_crs)
identical(lf_crs, tm16_crs)
identical(lf_crs, tm16_crs)
identical(lf_crs, lf200_crs)
identical(lf_crs, lf220_crs)
identical(lf_crs,lf230_crs)


identical(lf1999_crs, lf200_crs)
identical(lf200_crs, lf220_crs)
identical(lf220_crs, lf230_crs)


