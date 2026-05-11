# TreeMap Imputation
# Original script written by Isaac Grenfell, RMRS (igrenfell@gmail.com) 
# original script: "rmrs_production_scripts/2016_updated_production_scripts/yai-treemap commented.R"

# Updated script written by Lila Leatherman (Lila.Leatherman@usda.gov) and Scott Zimmer

# Last updated: 3/16/26

# This script accomplishes the following tasks: 
# - Run imputation over provided input area and target rasters
# - Save outputs as raster tiles


###########################################################################
# Set inputs
###########################################################################

# Parallelization settings
#----------------------------------------#
# Number of cores
ncores <- 35

# Tiling settings
# ---------------------------------------#
# set dimensions of tile - value is the length of one side of a tile, in pixels
tile_size <- 2000

# Select tiles to run
# if NA, defaults to all tiles in list
#----------------------------------------#
which_tiles <- NA

####################################################################
# Load data
####################################################################

message("  Loading data...")

# Load imputation model
# ---------------------------------------------------------- #

#load model
yai <- readr::read_rds(model_path)
message(glue::glue("Loaded model from {model_path}"))

# get names of variables included in model
model_vars <- names(yai$xRefs)
model_vars %<>% str_subset("point_", negate = TRUE) %>% sort() # remove point_x and point_y as these are calculated tile by tile in the imputation step

# Load target rasters
# ---------------------------------------------------------- #

message("  Loading target rasters...")

# list raster files
flist_tif <- list.files(path = target_dir, pattern = "*.tif$", recursive = TRUE, full.names = TRUE)

# filter to target rasters of interest
flist_tif <- filter_disturbance_rasters(flist_tif, dist_layer_type) # custom function

# remove aspect if it's present 
flist_tif %<>%
  str_subset("aspect", negate = TRUE)

# load rasters using custom function
rs2 <- load_and_name_rasters(flist_tif)

# Prep binary disturbance code layer
# ---------------------------------------------------------- #
# Reclass disturbance to binary
rs2$disturb_code_bin <- terra::classify(rs2$disturb_code, cbind(2, 1))
names(rs2$disturb_code_bin) <- "disturb_code_bin"
varnames(rs2$disturb_code_bin) <- "disturb_code_bin"

# remove original disturb code - model runs with binary
rs2$disturb_code <- NULL

# subset layers to vars present in model
rs2 <- subset(rs2, model_vars)

# Target layer checks
#----------------------------------------------------------------- #
# CHECK if all target layers have the same # of NA / non-NA pixels

# get count of non-NA px for all layers 
px_count <- data.frame(global(rs2, fun = "notNA"))
na_count <- data.frame(global(rs2, fun = "isNA"))

if (!identical(min(px_count[,1]), max(px_count[,1]))) {
  stop("ERROR: Target layers have different numbers of valid pixels. Check px_count object for details.")
} else {
  message("Target layers all have identical numbers of valid pixels.")
}

if (!identical(min(na_count[,1]), max(na_count[,1]))) {
  stop("ERROR: Target layers have different numbers of NA pixels. Check na_count object for details.")
} else {
  message("Target layers all have identical numbers of NA pixels.")
}

# check if we have all the same layers as are included in the model
if (!identical(model_vars, sort(names(rs2)))) {
  stop("ERROR: Target layer names don't match variables in input model")
} else {
  message("Target layer names match variables in input model - you're good to go!")
}

gc()

# FOR TESTING: Conditionally crop to aoi
# ---------------------------------------------------------- #
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
#Set up for applying imputation
###################################################################

message("  Setting up tiles for imputation...")

# Set inputs for run
# ---------------------------------------------------------- #

# first row to start test on
#test_row <- 1 # adjust this if using a test AOI or tiles vs whole zone

#ntest_rows <- tile_size

# set up rows to run over
row1 <- 1
row2 <- tile_size

# Set up tiles for imputation
# ---------------------------------------------------------- #

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
# ---------------------------------------------------------- #
# prepare tiles for plotting
p <- terra::init(agg, "cell") %>%
  terra::mask(agg) %>%
  terra::as.polygons(values  = TRUE)

names(p) <- "cell"
p$cell <- seq(1:nrow(p))

# # plot tiles
# plot(rs2[[2]], legend = FALSE)
# plot(p, "cell", alpha = 0.25, add=TRUE, main = glue::glue("Tiles for imputation over zone {zone_num}"))

# Select tiles to run
# ---------------------------------------------------------- #

n_tiles <- length(tiles)

# if which_tiles is NA, default to all tiles
if(is.na(which_tiles[1])) {
  which_tiles <- 1:length(tiles)
}


# Set up parallel processing
# ---------------------------------------------------------- #
message("  Setting up parallel processing...")

# set up dopar
cl <- parallel::makeCluster(ncores)
#doParallel::registerDoParallel(cl)
doSNOW::registerDoSNOW(cl)


# load packages to each cluster
parallel::clusterCall(cl, function() {
  library(yaImpute);
  library(randomForest)
})


# Export yai and impute_row function to each node. Doing this in the initial setup saves a ton of time
parallel::clusterExport(cl, varlist = c("yai", "impute_row_optimize"))



##################################################################################
# Apply imputation over tiles
##################################################################################

rm(rs2) # remove input raster to save some space
gc()

# run imputation 
message(glue::glue("Total tiles: {n_tiles}. Running over tiles {min(which_tiles)}
                 to {max(which_tiles)}!"))

for(j in which_tiles) {
  
  #j <- 1 # for test
  
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
  
  gc()
  
  # add XY coords to raster
  ras$point_x <- terra::init(ras, "x")
  names(ras$point_x) <- "point_x"
  ras$point_y <- terra::init(ras, "y")
  names(ras$point_y) <- "point_y"
  
  gc()
  
  # Convert raster values to data frame
  mat_df <- as.data.frame(terra::values(ras, mat = TRUE))
  
  # Identify rows that have valid pixels
  is_valid <- !is.na(mat_df[[1]]) 
  valid_indices <- which(is_valid)
  
  # Divide the valid data into chunks - up to 10 per core
  n_chunks <- 10 * ncores
  group_id <- cut(seq_along(valid_indices), breaks = n_chunks, labels = FALSE)
  
  # Split by chunk ID
  chunk_list <- split(mat_df[valid_indices, ], group_id)
  
  
  # Do work on tile - parallel with foreach and %dopar%
  # ---------------------------------------------------------- #
  
  # Parallelize over the row list and collect row imputations into list
  mout_list <- foreach(d = chunk_list, 
                       .packages = c("yaImpute"),
                       .noexport = c("yai"),
                       .options.snow = list(preschedule = FALSE)) %dopar% {
                         impute_row_optimize(dat = d, yai = yai)
                       }

  # Bind the nested list, with NA's filled in where they should be at the invalid indices
  final_vector <- rep(NA_integer_, nrow(mat_df))
  final_vector[valid_indices] <- unlist(mout_list, use.names = FALSE)
  
  
  # Arrange vector data to matrix with correct dimensions
  mout <- matrix(final_vector, 
                 nrow = nrow_r, # raster input nrow
                 ncol = ncol_r, # raster input ncol
                 byrow = TRUE)
  
  
  # Turn imputed values from matrix into a raster tile
  # ---------------------------------------------------------- #
  if (nrow(mout) < nrow_r) {
    # fill any missing rows in tile, when compared to input raster tile
    tile_out <- fill_matrix_to_raster(mout, ncol_r, nrow_r, row1)
  } else {
    # convert to raster
    tile_out <- mout %>% terra::rast()
  }
  
  # Post-process raster tile - assign metadata from input raster
  terra::ext(tile_out) <- ext_r
  terra::crs(tile_out) <- crs_r
  
  # Trim NAs from tile, now that we have appropriate extent and CRS
  tile_out <- terra::trim(tile_out)
  
  
  # # inspect
  # plot(tile_out,
  #      main = glue::glue('Zone {zone_num} ; tile {j} of {n_tiles}'))

  # export
  terra::writeRaster(tile_out,
              glue::glue('{raw_outputs_dir}/raster/tiles/{output_name}_tile{j}.tif'),
              datatype = "INT4U",
              overwrite = TRUE)

  # Remove objects
  rm(mout_list)
  rm(mout)
  rm(tile_out)
  #
  gc() # end of work on this tile
  
  message(glue::glue("    done with tile {j} of {n_tiles}!"))

}

stopCluster(cl)

message(glue::glue("Done with zone {zone_num}!"))

rm(f, cl, p, agg, yai, ras, tile_out, mout, mout_list, mat_df, ext_r, ncol_r, nrow_r, j)
