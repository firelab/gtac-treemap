# TreeMap Imputation
# Lila Leatherman, lila.leatherman@usda.gov

# PART 3: 
# - Load tiles from output
# - assemble into one raster per zone

# TO DO: 
# - make a decision about whether to delete intermediate tiles or no

# Last updated: 7/19/2024

##################################################
# Set inputs
###################################################

# Set inputs - from input script
#--------------------------------------------#

# this_dir <- this.path::this.dir()
# 
# inputs_for_imputation<- glue::glue('{this_dir}/00b_zonal_inputs_for_imp.R')
# source(inputs_for_imputation)

#------------------------------------------------#
# # input raster tile base name
tile_name <- output_name

# desired name for output raster
rout_name <- glue::glue("{output_name}_Imputation")


#####################################################################
# Load supporting data
#####################################################################

message("loading data for raster assembly")

# Load target rasters - as reference data
# --------------------------------------------------------------------#

# list raster files
flist_tif <- list.files(path = target_dir, pattern = "*.tif$", recursive = TRUE, full.names = TRUE)

# load a single raster as reference data = 5 = index of layer
rs2 <- load_and_name_rasters(flist_tif, 5)


# FOR TESTING: Conditionally crop to aoi
#---------------------------------------------------#
if (!is.na(aoi_path)) {
  
  print("using input shapefile as AOI")
  
  # get input crs
  lf_crs <- crs(rs2)
  
  # crop and mask
  aoi <- terra::vect(aoi_path) %>% terra::project(lf_crs)
  rs2 <- terra::crop(rs2, aoi, mask = TRUE)
  
  gc()
  
} else {print("using extent of input raster stack as AOI")} 


#######################################################################
# Run
#######################################################################

message("assembling imputed tiles")

# list tiles from path
tile_list <- list.files(path = tile_dir, pattern = "*.tif$", recursive = TRUE, full.names = TRUE)

# filter to tiles of interest - match tile name
tile_list <- tile_list[str_detect(tile_list, tile_name)]

# load tiles as .vrt
vrt <- terra::vrt(tile_list, filename = glue::glue('{tmp_dir}/t_assemble.tif'),
                  overwrite = TRUE)
# inspect
plot(vrt,
     main = glue::glue("Zone {zone_num}"))


# export as single raster per zone
writeRaster(vrt, 
            glue('{assembled_dir}/01_Imputation/{rout_name}.tif'),
            overwrite = TRUE)


# clear unused memory
gc()

# do we want to delete the raster tiles from the NAS after this is done? 
