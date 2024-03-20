# TreeMap Imputation
# Lila Leatherman, lila.leatherman@usda.gov

# PART 3: 
# - Load tiles from output
# - assemble into one raster per zone

# TO DO: 


# Last updated: 2/14/2024

##################################################
# Set inputs
###################################################

# Standard Inputs
#--------------------------------------#

# Set inputs - from input script
# Id where script is located
this.path <- this.path::this.path()

# get path to input script
spl <- stringr::str_split(this.path, "/")[[1]]
input_script.path <- paste( c(spl[c(1:length(spl)-1)],
                              "00_inputs_for_imp.R" ),
                            collapse = "/")

source(input_script.path)

#------------------------------------------------#
# input raster tile base name
tile_name <- glue::glue('{output_name}_tilesz2000_nT36')
#tile_name <- glue::glue('{output_name}_tilesz2000_nT4')

# desired name for output raster
rout_name <- tile_name


#####################################################################
# Load supporting data
#####################################################################

# Load target rasters - as reference data
# --------------------------------------------------------------------#

# list raster files
flist.tif <- list.files(path = target_dir, pattern = "*.tif$", recursive = TRUE, full.names = TRUE)

# load raster files as terra raster
# - only load first raster, for reference and metadata
rs2 <- terra::rast(flist.tif[2])

# get raster layer names
layer_names <- flist.tif %>%
  str_extract(., "z[0-9][0-9]/([^.])*") %>%
  str_replace("z[0-9][0-9]/", "")

#add names to raster list
names(rs2) <- layer_names[2]

# get crs
lf.crs <- crs(rs2)

# FOR TESTING: Conditionally crop to aoi
#---------------------------------------------------#
if (!is.na(aoi_path)) {
  
  print("using input shapefile as AOI")
  
  # crop and mask
  aoi <- terra::vect(aoi_path) %>% terra::project(lf.crs)
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
