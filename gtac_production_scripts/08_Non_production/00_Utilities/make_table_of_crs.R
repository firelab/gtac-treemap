# Make a data frame of Landfire CRSs

# Load library
#----------------------------------------------------#

# Load CRS
#----------------------------------------------------#

# load lcms projections
lcms_crs <- terra::crs(glue::glue("{data_dir}05_LCMS/00_Supporting/lcms_crs_albers.prj"))
# load treemap projections
tm16_crs <- terra::crs(glue::glue("{data_dir}01_TreeMap2016_RDA/04_CRS/TreeMap2016_crs.prj"))
lf200_crs <- terra::crs(glue::glue("{home_dir}/01_Data/02_Landfire/LF_200/CRS/LF_200_crs.prj"))
lf220_crs <- terra::crs(glue::glue("{home_dir}/01_Data/02_Landfire/LF_220/CRS/LF_220_crs.prj"))
lf230_crs <- terra::crs(glue::glue("{home_dir}/01_Data/02_Landfire/LF_230/CRS/LF_230_crs.prj"))
lf240_crs <- terra::crs(glue::glue("{home_dir}/01_Data/02_Landfire/LF_240/CRS/LF_240_crs.prj"))
ak_crs <- terra::crs(glue::glue("{home_dir}/01_Data/02_Landfire/LF_zones/Landfire_zones/alaska_mapzones.prj"))
hi_crs <- terra::crs(glue::glue("{home_dir}/01_Data/02_Landfire/LF_zones/Landfire_zones/hawaii_mapzones.prj"))