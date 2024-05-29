# get desired projection from Landfire rasters and write out
library(terra)

# Initialize home dir
#-----------------------------------------------#
# Id where THIS script is located
this.path <- this.path::this.path()

# get path to input script
spl <- stringr::str_split(this.path, "/")[[1]]
setup_dirs.path <- paste( c(spl[c(1:(length(spl)-2))],
                              "00_Library/setup_dirs.R" ),
                            collapse = "/")

source(setup_dirs.path)

# path to existing crs
crs <- terra::crs(glue::glue("{home_dir}/01_Data/02_Landfire/landfire_crs.prj"))

# treemap raster to use as reference
tm <- terra::rast(glue::glue("{home_dir}/01_Data/01_TreeMap2016_RDA/RDS-2021-0074_Data/Data/TreeMap2016.tif"))

# get crs
tm_crs <- terra::crs(tm)

# Landfire raster(s) to use as reference- 2016 target
# list
target_rasters <- list.files(glue::glue("{home_dir}/03_Outputs/05_Target_Rasters/v2016_RMRS/z16/", pattern = ".tif", full.names = TRUE))

# load
target <- terra::rast(target_rasters[[1]])

# get crs
target_crs <- terra::crs(target)

# lf16
lf16 <- terra::rast(glue::glue("{home_dir}/01_Data/02_Landfire/LF_200/EVC/LF2016_EVC_200_CONUS/Tif/LC16_EVC_200.tif"))
lf16_crs <- terra::crs(lf16)

# lf20
lf20<- terra::rast(glue::glue("{home_dir}/01_Data/02_Landfire/LF_220/EVT/LF2020_EVT_220_CONUS/Tif/LC20_EVT_220.tif"))

lf20_crs <- terra::crs(lf20)

# inspect
#----------------------------------#

crs(crs, describe = TRUE)
crs(tm_crs, describe = TRUE)
crs(target_crs, describe = TRUE)
crs(lf16_crs, describe = TRUE)
crs(lf20_crs, describe = TRUE)

identical(crs, target_crs)
identical(crs, tm_crs)
identical(target_crs, tm_crs)
identical(crs, lf16_crs)
identical(crs, lf20_crs)


# Path to write out crs to 
crs_out_path <- glue::glue("{home_dir}/01_Data/01_TreeMap2016_RDA/04_CRS/TreeMap2016_crs.prj")

# write out CRS
write(tm_crs, file = crs_out_path)
