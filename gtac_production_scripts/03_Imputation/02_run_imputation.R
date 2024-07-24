# TreeMap Imputation
# Original script written by Isaac Grenfell, RMRS (igrenfell@gmail.com) 
# original script: "rmrs_production_scripts/2016_updated_production_scripts/yai-treemap commented.R"

# Updated script written by Lila Leatherman (Lila.Leatherman@usda.gov)

# Last updated: 7/19/24

# PART 2: 
# - Run imputation over input area / target rasters
# - Save outputs as raster tiles


# TO DO: 
# - add progress bar to dopar - parabar https://parabar.mihaiconstantin.com/ 
# implementation for foreach: https://github.com/mihaiconstantin/parabar/issues/53


###########################################################################
# Set inputs
###########################################################################

# Set inputs - from input script
# Uncomment this if running a single zone at a time, outside of a loop for all zones
#--------------------------------------------#

# this_dir <- this.path::this.dir()
# 
# inputs_for_imputation<- glue::glue('{this_dir}/00b_zonal_inputs_for_imp.R')
# source(inputs_for_imputation)

############################################################

# Other options
# --------------------------------#

# Allow for sufficient digits to differentiate plot cn numbers
# plot CNs aren't present in the imputation portion, however

#options("scipen"=100, "digits"=8)

# Tiling settings
# ---------------------------------------#
# set dimensions of tile - value is the length of one side
tile_size <- 2000

# # select tiles to run
# # if NA, defaults to all tiles in list
which_tiles <- NA

# Parallelization settings
#--------------------------------------#
# Number of cores
ncores <- 35

####################################################################
# Load data
####################################################################

message("loading data")

# Load imputation model
# ---------------------------------------------------------- #

#load model
yai <- readr::read_rds(model_path)

# get names of variables included in model
model_vars <- names(yai$xRefs)
model_vars %<>% str_subset("POINT_", negate = TRUE) %>% sort() # remove point x and point y as these are calculated in the imputation model step

# Load target rasters
# --------------------------------------------------------------------#

# list raster files
flist_tif <- list.files(path = target_dir, pattern = "*.tif$", recursive = TRUE, full.names = TRUE)

# filter to target rasters of interest
flist_tif <- filter_disturbance_rasters(flist_tif, dist_layer_type) # custom function

# remove aspect if it's present - could make this conditional based on whether or not northing and easting are present in file list
flist_tif %<>%
  str_subset("ASPECT", negate = TRUE)

# remove prelim disturbance layers, if necessary
flist_tif %<>%
  str_subset("Dist_",negate = TRUE)

# load rasters using custom function
rs2 <- load_target_rasters(flist_tif)

# check if we have all the same layers as are included in the model
if( !identical(model_vars, sort(names(rs2)))) {
  message("ERROR: Target layers provided don't match variables in input model")
}

# FOR TESTING: Conditionally crop to aoi
#---------------------------------------------------#
if (!is.na(aoi_path)) {
  
  print("using input shapefile as AOI")
  
  # get desired crs
  lf_crs <- terra::crs(rs2)
  
  # crop and mask
  aoi <- terra::vect(aoi_path) %>% terra::project(lf_crs)
  rs2 <- terra::crop(rs2, aoi, mask = TRUE)
  
  gc()
  
} else { print("using extent of input raster stack as AOI") }

##################################################################
#Apply Imputation Function
###################################################################

message("setting up tiles for imputation")

# Set up run
# ---------------------------------------------------------- #

# first row to start test on
#test_row <- 1 # adjust this if using a test AOI or tiles vs whole zone

#ntest_rows <- tile_size

# set up rows to run over
row1 <- 1
row2 <- tile_size

# Set up tiles for imputation
# ----------------------------------------------------------

# aggregate - template for making tiles
# get values so i can see which tiles overlap the zone
agg <- terra::aggregate(rs2[[1]], fact = tile_size,
                        fun = "median", na.rm = TRUE)

# subset the raster and create temporary files
# tiles with only NA values are omitted
# the function returns file names for the temporary files
tiles <- rs2 %>%
  terra::makeTiles(agg, paste0(tempfile(), "_.tif"), na.rm = TRUE)

# garbage collector
gc()


# Inspect tiles
#--------------------------------------------#
# prepare tiles for plotting
p <- terra::init(agg, "cell") %>%
  terra::mask(agg) %>%
  terra::as.polygons(values  = TRUE)

names(p) <- "cell"
p$cell <- seq(1:nrow(p))

# plot tiles
plot(rs2[[2]], legend = FALSE)
plot(p, "cell", alpha = 0.25, add=TRUE, main = glue::glue("Tiles for imputation over zone {zone_num}"))

# Select tiles to run
#--------------------------------------------#

n_tiles <- length(tiles)

# if which_tiles is NA, default to all tiles
if(is.na(which_tiles[1])) {
  which_tiles <- 1:length(tiles)
}


# set up parallel processing
#---------------------------------------------#
message("setting up parallel processing")

# set up dopar
cl <- makeCluster(ncores)
registerDoParallel(cl)

# load packages to each cluster
clusterCall(cl, function() {
  library(tidyverse);
  library(yaImpute);
  library(randomForest);
  library(glue)})

#########################################
# APPLY Imputation OVER TILES
#------------------------------------------#

rm(rs2) # remove input raster to save some space
gc()

# run imputation 
message(glue::glue("Total tiles: {n_tiles}. Running over tiles {min(which_tiles)}
                 to {max(which_tiles)}!"))

for(j in which_tiles) {
  
  # start timer
  #tic()
  
  # for test 
  #j <- 3
  
  # select tile to run
  fn <- tiles[j]
  message(glue::glue("working on tile {j} of {n_tiles}!"))
  
  # read raster tile into memory
  ras <- terra::rast(fn)
  
  # get metadata from raster tile
  nrow_r <- nrow(ras)
  ncol_r <- ncol(ras)
  ext_r <- terra::ext(ras)
  crs_r <- terra::crs(ras)
  
  # Prep raster inputs
  #--------------------------------------------#

  # # Calculate northing and easting from aspect
  # ras$NORTHING <- terra::app(ras$ASPECT, function(i) cos((pi / 180) * i))
  # names(ras$NORTHING) <- "NORTHING"
  # ras$EASTING <- terra::app(ras$ASPECT, function(i) sin((pi / 180) * i))
  # names(ras$EASTING) <- "EASTING"
  # # remove aspect
  #ras$ASPECT <- NULL
  
  gc()
  
  # Reclass disturbance to binary
  ras$disturb_code <- terra::classify(ras$disturb_code, cbind(2, 1))
  
  gc()
  
  # add XY coords to raster
  ras$POINT_X <- terra::init(ras, "x")
  names(ras$POINT_X) <- "POINT_X"
  ras$POINT_Y <- terra::init(ras, "y")
  names(ras$POINT_Y) <- "POINT_Y"
  
  gc()
  
  # convert raster to matrix
  mat <- as.matrix(ras)
  
  
  # Do work on tile
  #--------------------------------#

  # PARALLEL WITH DOPAR
  #----------------------------#

  f <- foreach(i = 1:nrow_r,
    .packages = c("tidyverse", "yaImpute", "glue"),
    .export = c("tmp_dir" )
  ) %dopar% {

    # # for testing
    #i = 37
    # print(glue::glue("working on row {i}/{nrow_r}"))

    # get extracted values from each field for each row of input raster
    d <- data.frame(
                    mat[(ncol_r * (i - 1) + 1):(ncol_r * i), ])
    
    # impute on row of data - input is named row of data + yai
    row <- impute_row(dat = d,
                      yai = yai, test = FALSE)
    
    # label for row - to keep rows in order
    i_out <- if (i < 10) { glue::glue("000{i}")
    } else if (i < 100) { glue::glue("00{i}")
    } else if (i < 1000) { glue::glue("0{i}")
    } else if (i < 10000) {glue::glue("{i}")
    }
    
    saveRDS(row,
      glue::glue("{tmp_dir}/rows/row{i_out}.RDS")) 
   
   } # end do par
             
  # read rows back in 
  rlist <- list.files(glue::glue("{tmp_dir}/rows/"),
                      "row[0-9]*.RDS", full.names = TRUE)
  
  # potential for error catch - if length(rlist) > nrow_r
  
  # bind rows together
  mout <- do.call(rbind,
                  lapply(rlist, readRDS))

  # Turn rows into a raster tile
  #-----------------------------------------#
  if (nrow(mout) < nrow_r) {
    # fill any missing rows in tile, when compared to input raster tile
    tile_out <- fill_matrix_to_raster(mout, ncol_r, nrow_r, row1)
  } else {
    # convert to raster
    tile_out <- mout %>% terra::rast()
  }

  #post-process tile - with metadata from input raster
  terra::ext(tile_out) <- ext_r
  terra::crs(tile_out) <- crs_r

  # trim NAs from tile, now that we have appropriate extent and CRS
  tile_out <- terra::trim(tile_out)

  # inspect
  plot(tile_out,
       main = glue::glue('Zone {zone_num} ; tile {j} of {n_tiles}'))

  # export
  terra::writeRaster(tile_out,
              glue::glue('{raw_outputs_dir}/raster/tiles/{output_name}_tile{j}.tif'),
              datatype = "INT4U",
              overwrite = TRUE)

  #rm(tile_out)
  gc() # end of work on tile

  # delete rows from tmp dir - fresh start for next tile
  do.call(unlink, list(rlist))

  message(glue::glue("done with tile {j} of {n_tiles}!"))
  #toc() # report time elapsed

}

stopCluster(cl)

message(glue::glue("Done with zone {zone_num}!"))

rm(f, cl, p, agg, model_vars, yai, ras, rs2, tile_out, mout, mat, ext_r, ncol_r, nrow_r, i, j)
