# TreeMap Imputation
# Updated script written by Lila Leatherman (Lila.Leatherman@usda.gov)
# Original script written by Isaac Grenfell, RMRS (igrenfell@gmail.com) 

# Last updated: 3/19/2024

# PART 2: 
# - Run imputation over input area / target rasters
# - Save outputs as raster tiles


# TO DO: 
# - add progress bar to dopar - parabar https://parabar.mihaiconstantin.com/


###########################################################################
# Set inputs
###########################################################################

# Number of cores
# --------------------------#
ncores <- 27

# Tiling settings
# ---------------------------------------#
# set dimensions of tile - value is the length of one side
tile_size <- 2000

# # select tiles to run
# # if NA, defaults to all tiles in list
which_tiles <- NA

# Standard inputs
#---------------------------------------------#

# Set inputs - from input script
this.path <- this.path::this.path() # Id where THIS script is located

# get path to input script
spl <- stringr::str_split(this.path, "/")[[1]]
input_script.path <- paste(c(spl[c(1:(length(spl) - 1))],
                             "00_inputs_for_imp.R"),
                            collapse = "/")

source(input_script.path)

# write out params used, as set in input script
write.table(as.data.frame(params_out),
            glue::glue('{output_dir}/params/{cur_zone_zero}_{output_name}_params.txt'),
            row.names = FALSE)

############################################################

# Terra options
# --------------------------------#

#increase memory fraction available
terraOptions(memfrac = 0.8)

# Other options
# --------------------------------#

# Allow for sufficient digits to differentiate plot cn numbers
# plot CNs aren't present in the imputation portion, however

#options("scipen"=100, "digits"=8)

# Parallelization settings
#--------------------------------------#

# set up dopar
cl <- makeCluster(ncores)
registerDoParallel(cl)

# load packages to each cluster
clusterCall(cl, function() {
                            library(tidyverse);
                            library(yaImpute);
                            library(randomForest);
                            library(glue)})

####################################################################
# Load data
####################################################################

# Load imputation model
# ---------------------------------------------------------- #

#load model
yai_treelist_bin <- readr::read_rds(model_path)


# Load target rasters
# --------------------------------------------------------------------#

# list target raster files
flist.tif <- list.files(path = target_dir, pattern = "*.tif$", 
                        recursive = TRUE, full.names = TRUE)

# load raster files as terra raster
rs2 <- terra::rast(flist.tif)

# get raster layer names
layer_names <- flist.tif %>%
  str_extract(., "z[0-9][0-9]/([^.])*") %>%
  str_replace("z[0-9][0-9]/", "")

#add names to raster list
names(rs2) <- layer_names

# Conditionally load disturbance rasters from another dir
#----------------------------------------------------------#

# conditional
if (!is.na(dist_raster_dir)) {
  
  print("loading disturbance rasters to test")

 # list files
  dist_files <- list.files(dist_raster_dir, full.names = TRUE, recursive = TRUE)
  
  # filter files if necessary
  dist_files %<>% 
    str_subset(pattern = glue::glue('{dist_layer_type}.tif$') ) %>%
    str_subset(pattern = glue::glue('{cur_zone_zero}_disturb'))
  
  # load files in
  dist <- terra::vrt(dist_files, options = "-separate", 
                     filename = glue::glue('{tmp_dir}/dist.tif'),
                     overwrite = TRUE)
  
  # ensure layer names match
  names(dist) <- c("disturb_code", "disturb_year")
  
  # reclass disturbance code
  dist$disturb_code <- terra::classify(dist$disturb_code, cbind(2, 1))
  
  # make sure they're in the same projection
  if (!identical(crs(dist), crs(rs2))) {
    dist %<>% terra::project(crs(rs2))
  }
  
  # make sure layers align
  dist %<>% terra::crop(rs2) %>%
    terra::extend(terra::ext(rs2)) %>%
    terra::crop(rs2) %>%
    terra::resample(rs2, method = "near") # check this / maybe make align earlier in processing? off by a fraction of a degree
  
  
  gc()
  
  # combine replace disturbance layers in target data
  rs2$disturb_code <- dist$disturb_code
  rs2$disturb_year <- dist$disturb_year
  
  rm(dist)
  
  gc()
  
}

# FOR TESTING: Conditionally crop to aoi
#---------------------------------------------------#
if (!is.na(aoi_path)) {
  
  print("using input shapefile as AOI")
  
  # crop and mask
  aoi <- terra::vect(aoi_path) %>% terra::project(lf.crs)
  rs2 <- terra::crop(rs2, aoi, mask = TRUE)
  
  gc()
  
} else { print("using extent of input raster stack as AOI") }

##################################################################
#Apply Imputation Function
###################################################################

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
# get values so i can see which tiles are retained vs NA
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
# prepare tiles for visualization
p <- terra::init(agg, "cell") %>%
  terra::mask(agg) %>%
  terra::as.polygons(values  = TRUE)

names(p) <- "cell"
p$cell <- seq(1:nrow(p))

# plot tiles
plot(rs2[[2]], legend = FALSE)
plot(p, "cell", alpha = 0.25, add=TRUE)

# Select tiles to run
#--------------------------------------------#

# if which_tiles is NA, default to all tiles
if(is.na(which_tiles[1])) {
  which_tiles <- 1:length(tiles)
}


#########################################
# APPLY Imputation OVER TILES
#------------------------------------------#

print(glue::glue("running over tiles {min(which_tiles)}
                 to {max(which_tiles)}!"))

for(j in which_tiles) {
  
  # start timer
  tic()
  
  # for test 
  #j <- 2
  
  # select tile to run
  fn <- tiles[j]
  print(glue::glue("working on tile {j}!"))
  
  # read raster tile into memory
  ras <- terra::rast(fn)
  
  # get metadata from raster tile
  nrow_r <- nrow(ras)
  ncol_r <- ncol(ras)
  ext_r <- terra::ext(ras)
  crs_r <- terra::crs(ras)
  
  # Prep raster inputs
  #--------------------------------------------#
  
  # Calculate northing and easting from aspect
  ras$NORTHING <- terra::app(ras$ASPECT, function(i) cos((pi / 180) * i))
  names(ras$NORTHING) <- "NORTHING"
  ras$EASTING <- terra::app(ras$ASPECT, function(i) sin((pi / 180) * i))
  names(ras$EASTING) <- "EASTING"
  
  gc()
  
  # Reclass disturbance to binary
  ras$disturb_code <- terra::classify(ras$disturb_code, cbind(2, 1))
  
  gc()
  
  # add XY coords to raster
  ras$POINT_X <- terra::init(ras, "x")
  names(ras$POINT_X) <- "POINT_X"
  ras$POINT_Y <- terra::init(ras, "y")
  names(ras$POINT_Y) <- "POINT_Y"
  
  # remove aspect
  ras$ASPECT <- NULL
  
  gc()
  
  # convert raster to matrix
  mat <- as.matrix(ras)
  
  
  # Do work on tile
  #--------------------------------#

  # PARALLEL WITH DOPAR
  #----------------------------#

  f <- foreach(i = 1:nrow_r,
    .packages = c("tidyverse", "yaImpute", "glue")
    #.export = c("mat", "impute.row", "yai_treelist_bin")
  ) %dopar% {

    # # for testing
    #i = 37
    # print(glue::glue("working on row {i}/{nrow_r}"))

    # get extracted values from each field for each row of input raster
    d <- data.frame(
                    mat[(ncol_r * (i - 1) + 1):(ncol_r * i), ])
    
    # impute on row of data - input is named row of data + yai
    row <- impute.row(dat = d,
                      yai = yai_treelist_bin, test = FALSE)
    
    # label for row - to keep rows in order
    i_out <- if (i < 10) { glue::glue("000{i}")
    } else if (i < 100) { glue::glue("00{i}")
    } else if (i < 1000) { glue::glue("0{i}")
    } else if (i < 10000) {glue::glue("{i}")
    }
    
    saveRDS(row,
      paste0(tmp_dir, "/rows/row", i_out, ".RDS") # glue doesn't work in parallel
    )
   
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
  plot(tile_out)

  # export
  terra::writeRaster(tile_out,
              # glue doesn't work for file names within future loop
              glue::glue('{output_dir}raster/tiles/{output_name}_tilesz{tile_size}_nT{length(tiles)}_tile{j}.tif'),
              #paste0(output_dir,'raster/tiles/',output_name,'_tilesz',tile_size,'_nT',length(tiles),'_tile',j,'.tif'),
              datatype = "INT4U",
              overwrite = TRUE)

  #rm(tile_out)
  gc() # end of work on tile

  # delete rows from tmp dir - fresh start for next tile
  do.call(unlink, list(rlist))

  print(glue::glue("done with tile {j} !"))
  toc() # report time elapsed

}

stopCluster(cl)

print(glue::glue("Done with zone {zone_num}!"))
