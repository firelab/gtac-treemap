# TreeMap Imputation
# Updated script written by Lila Leatherman (Lila.Leatherman@usda.gov)
# Original script written by Isaac Grenfell, RMRS (igrenfell@gmail.com) 

# Last updated: 2/27/2024

# PART 2: 
# - Run imputation over input area / target rasters
# - Save outputs as raster tiles


# TO DO: 
# - check for tiles available and then run whatever tiles aren't run


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
#which_tiles <- NA
which_tiles <- c(8)

# Test application settings
#-----------------------------------------#

# # supply path to a shapefile to use as subset, or NA
# aoi_path <- "//166.2.126.25/TreeMap/01_Data/03_AOIs/UT_Uintas_rect_NAD1983.shp"
# aoi_name <- "UT_Uintas_rect"
aoi_path <- NA


# Standard inputs
#---------------------------------------------#

# Set inputs - from input script
this.path <- this.path::this.path() # Id where THIS script is located

# get path to input script
spl <- stringr::str_split(this.path, "/")[[1]]
input_script.path <- paste( c(spl[c(1:(length(spl)-1))],
                              "00_inputs_for_imp.R" ),
                            collapse = "/")

source(input_script.path)

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

# options for future
options(future.rng.onMisuse = "ignore") # ignore errors with random number generators
options(future.globals.onReference = "error") # give an error if a pointer can't access something in a future loop
options(future.globals.maxSize = 750 * 1024 ^ 2 ) # maximum size for an object that can be exported to a future loop


# set up future session
future::plan("multisession", workers=ncores)

# # set up dopar
# cl <- makeCluster(ncores)
# registerDoParallel(cl)
# 
# # load packages to each cluster
# clusterCall(cl, function(){ 
#   library(tidyverse);
#   library(yaImpute);
#   library(randomForest)})

####################################################################
# Load data
####################################################################

# Load imputation model 
# ---------------------------------------------------------- #

#load model
yai.treelist.bin <- readr::read_rds(model_path)


# Load target rasters
# --------------------------------------------------------------------#

# list raster files
flist.tif <- list.files(path = target_dir, pattern = "*.tif$", recursive = TRUE, full.names = TRUE)

# load raster files as terra raster
rs2 <- terra::rast(flist.tif)

# get raster layer names
layer_names <- flist.tif %>%
  str_extract(., "z[0-9][0-9]/([^.])*") %>%
  str_replace("z[0-9][0-9]/", "")

#add names to raster list
names(rs2) <- layer_names

# get crs
lf.crs <- crs(rs2)

# FOR TESTING: Conditionally crop to aoi
if (!is.na(aoi_path)) {
  
  print("using input shapefile as AOI")
  
  # crop and mask
  aoi <- terra::vect(aoi_path) %>% terra::project(lf.crs)
  rs2 <- terra::crop(rs2, aoi, mask = TRUE)
  
  # update output name
  output_name = glue('{output_name}_{aoi_name}')
  
  gc()
  
} else {print("using extent of input raster stack as AOI")} 

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
agg <- terra::aggregate(rs2[[1]], fact = tile_size, fun = 'median', na.rm = TRUE)

# subset the raster and create temporary files
# tiles with only NA values are omitted
# the function returns file names for the temporary files
tiles <- rs2 %>%
  terra::makeTiles(agg, paste0(tempfile(), '_.tif'), na.rm = TRUE)

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
plot(rs2[[1]], legend = FALSE)
plot(p, 'cell', alpha = 0.25, add=TRUE)

# Select tiles to run
#--------------------------------------------#

# if which_tiles is NA, default to all tiles
if(is.na(which_tiles[1])) {
  which_tiles <- 1:length(tiles)
}


#########################################
# APPLY Imputation OVER TILES
#------------------------------------------#

print(glue::glue("running over tiles {min(which_tiles)} to {max(which_tiles)}!"))

for(j in which_tiles) {
  
  # start timer
  tic()
  
  # for test 
  j <- 8
  
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
  ras$NORTHING <- terra::app(ras$ASPECT, function(i) cos((pi/180)*i))
  names(ras$NORTHING) <- "NORTHING"
  ras$EASTING <- terra::app(ras$ASPECT, function(i) sin((pi/180)*i))
  names(ras$EASTING) <- "EASTING"
  
  gc()
  
  # Reclass disturbance to binary 
  ras$disturb_code <- terra::classify(ras$disturb_code, cbind(2, 1))
  
  gc()
  
  # add XY coords to raster
  ras$POINT_X <- terra::init(ras, 'x')
  names(ras$POINT_X) <- "POINT_X"
  ras$POINT_Y <- terra::init(ras, 'y')
  names(ras$POINT_Y) <- "POINT_Y"
  
  # remove aspect
  ras$ASPECT <- NULL
  
  gc()
  
  # convert raster to matrix
  mat <- as.matrix(ras)
  
  # # convert matrix to list of data frames - one data frame for each row
  # rows_in <- NULL
  # 
  # # MOVE DATA FRAME CONVERSION TO WITHIN FUTURE LOOP
  # for(r in row1:nrow_r) {
  #   
  #   d <- list(data.frame(
  #     mat[(ncol_r*(r-1)+1):(ncol_r*r),])) # get extracted values from each field for each row of input raster
  #   
  #   if(is.null(rows_in)){
  #     rows_in <- d
  #   } else {
  #     rows_in <- c(rows_in, d)
  #   }}
  
  # Do work on tile
  #--------------------------------#

  # PARALLEL WITH FURRR
  #---------------------------#
  
  #wrapper function to track progress
  progressr::with_progress({
    p <- progressr::progressor(steps = nrow_r)

    # work over tile
    #rows_out <-
      1:nrow_r %>%
      future_imap(function(fn, i, ...) {

        # report progress
        p()
  
  # # PARALLEL WITH DOPAR
  # #----------------------------#
  #       
  # foreach(i = 1:nrow_r, 
  #         .packages = "tidyverse","yaImpute", "glue") %do% {
      
        # # for testing
        #i = 37
        # print(glue::glue("working on row {i}/{nrow_r}"))
        
        # get extracted values from each field for each row of input raster
        d <- data.frame(
          mat[(ncol_r*(i-1)+1):(ncol_r*i),]) 
        
        # impute on row of data - input is named row of data + yai
        row <- impute.row(dat = d,
                          yai = yai.treelist.bin, test = FALSE)
        
        # label for row - to keep rows in order
        i_out <- if(i < 10) {glue::glue('000{i}') } else 
          if(i < 100) {glue::glue('00{i}')} else 
            if(i < 1000) {glue::glue('0{i}')} else 
              if(i < 10000) {glue('{i}')}
        
        saveRDS(row, 
                #file = glue::glue('{tmp_dir}/rows/row{i}.RDS') # glue doesn't work with variables in future loops
                paste0(tmp_dir, "/rows/row", i_out, ".RDS")
                )
   
   #}# end do par
             
      }) # end future over rows
    }) # end wrapper function to track progress
  
  # read rows back in 
  rlist <- list.files(glue::glue('{tmp_dir}/rows/'), "row[0-9]*.RDS", full.names = TRUE )
  
  # potential for error catch - if length(rlist) > nrow_r
  
  # bind rows together
  mout <- do.call(rbind, 
          lapply(rlist, readRDS))
  
  # delete rows from tmp dir - fresh start for next tile 
  #do.call(file.remove, rlist)
  
  
  # Turn rows into a raster tile 
  #-----------------------------------------#
  if(nrow(mout) < nrow_r) {
    
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
  
  # export
  terra::writeRaster(tile_out, 
              # glue doesn't work for file names within future loop
              glue::glue('{output_dir}raster/tiles/{output_name}_tilesz{tile_size}_nT{length(tiles)}_tile{j}.tif'),
              #paste0(output_dir,'raster/tiles/',output_name,'_tilesz',tile_size,'_nT',length(tiles),'_tile',j,'.tif'),
              overwrite = TRUE)
  
  #rm(tile_out)
  gc() # end of work on tile 
  
  print(glue::glue("done with tile {j} !"))
  toc() # report time elapsed
  
}

#stopCluster(myCluster)

print(glue::glue("Done with zone {zone_num}!"))

