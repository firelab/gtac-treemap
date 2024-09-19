# TreeMap Imputation Assembly
# Written by Lila Leatherman, lila.leatherman@usda.gov

# This script accomplishes the following: 
# - Load tiles from output
# - Assemble into one raster per zone


# Last updated: 9/17/2024

##################################################
# Set inputs
###################################################

#------------------------------------------------#
# # input raster tile base name
tile_name <- output_name

# desired name for output raster
rout_name <- glue::glue("{output_name}_Imputation")


#####################################################################
# Load supporting data
#####################################################################

message("loading data for raster assembly")

# Load target rasters - for reference and comparison
# ---------------------------------------------------------- #

# list raster files
flist_tif <- list.files(path = target_dir, pattern = "*.tif$", recursive = TRUE, full.names = TRUE)

# filter to target rasters of interest
flist_tif <- filter_disturbance_rasters(flist_tif, dist_layer_type) # custom function

# # remove aspect if it's present 
# flist_tif %<>%
#   str_subset("aspect", negate = TRUE)

# load rasters using custom function
rs2 <- load_and_name_rasters(flist_tif)

# # Prep binary disturbance code layer
# # ---------------------------------------------------------- #
# # Reclass disturbance to binary
# rs2$disturb_code_bin <- terra::classify(rs2$disturb_code, cbind(2, 1))
# names(rs2$disturb_code_bin) <- "disturb_code_bin"
# varnames(rs2$disturb_code_bin) <- "disturb_code_bin"
# 
# # remove original disturb code - model runs with binary
# rs2$disturb_code <- NULL
# 
# # subset layers to target vars
# rs2 <- subset(rs2, targetvars)

gc()

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

# extend to match dimension / number of NAs in target raster
vrt <- terra::extend(vrt, rs2)

# inspect
plot(vrt,
     main = glue::glue("Zone {zone_num}"))

# check that imputed output has the same number of data pixels as the target layers
#-------------------------------------------------------------------#
# only checking one layer in the target data stack because at this point, they are ostensibly the same
px_imputed <- global(vrt, fun = "notNA")[[1]]
px_target <- global(rs2[[1]], fun = "notNA")[[1]]

na_imputed <- global(vrt, fun = "isNA")[[1]]
na_target <- global(rs2[[1]], fun = "isNA")[[1]]

if (px_imputed == px_target) {
  message("Imputed and target layers have the same number of valid pixels-- you are good to go!")
} else {
  stop(glue::glue("Imputed and target layers have different numbers of valid pixels:
                  Imputed: {px_imputed}
                  Target: {px_target}"))
}

if (na_imputed == na_target) {
  message("Imputed and target layers have the same number of NA pixels-- you are good to go!")
} else {
  stop(glue::glue("Imputed and target layers have different numbers of NA pixels:
                  Imputed: {na_imputed}
                  Target: {na_target}"))
}


# export as single raster per zone
writeRaster(vrt, 
            glue('{assembled_dir}/01_Imputation/{rout_name}.tif'),
            overwrite = TRUE)


# clear unused memory
gc()

rm(tile_list, vrt, px_imputed, px_target, na_imputed, na_target, rs2)
