# TreeMap Imputation
# Lila Leatherman, lila.leatherman@usda.gov

# PART 3: 
# - Load tiles from output
# - assemble into one raster per zone

# TO DO: 


# Last updated: 6/18/2024

##################################################
# Set inputs
###################################################

# Set inputs - from input script
#--------------------------------------------#

this_dir <- this.path::this.dir()

inputs_for_imputation<- glue::glue('{this_dir}/00b_zonal_inputs_for_imp.R')
source(inputs_for_imputation)

#------------------------------------------------#
# input raster tile base name
tile_name <- glue::glue('{output_name}')

# desired name for output raster
rout_name <- tile_name


#####################################################################
# Load supporting data
#####################################################################

# Load target rasters - as reference data
# --------------------------------------------------------------------#

# list raster files
flist_tif <- list.files(path = target_dir, pattern = "*.tif$", recursive = TRUE, full.names = TRUE)

# load a single raster as reference data = 2 = index of layer
rs2 <- load_target_rasters(flist_tif, 2)



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

# list tiles from path
tile.list <- list.files(path = tile_dir, pattern = "*.tif$", recursive = TRUE, full.names = TRUE)

# filter to tiles of interest - match tile name
tile.list <- tile.list[str_detect(tile.list, tile_name)]

# load tiles as .vrt
vrt <- terra::vrt(tile.list, filename = glue::glue('{tmp_dir}/t_assemble.tif'),
                  overwrite = TRUE)
# inspect
plot(vrt)


# export as single raster per zone
writeRaster(vrt, 
            glue('{assembled_dir}/01_Imputation/{rout_name}.tif'),
            overwrite = TRUE)


# clear unused memory
gc()

# do we want to remove the raster tiles after this is done? 
