# TreeMap Imputation
# Updated script written by Lila Leatherman (Lila.Leatherman@usda.gov)
# Original script written by Isaac Grenfell, RMRS (igrenfell@gmail.com) 


# PART 2: 
# - Run imputation
# - Save outputs as a raster


# TO DO: 
# - use progressr for progress bar in furrr
# - check disturb code layers
# - move imputation function to library
# - remove in-situ calc of northing and easting (pull from input rasters)


# Last updated: 1/23/2024



###########################################################################
# Set inputs
###########################################################################

# Test application settings
#-----------------------------------------#

# first row to start test on 
test_row <- 1 # adjust this if using a test AOI or tiles vs whole zone

ntest_rows <- 500 

# # supply path, or NA
aoi_path <- "//166.2.126.25/TreeMap/01_Data/03_AOIs/UT_Uintas_rect_NAD1983.shp"
aoi_name <- "UT_Uintas_rect"
#aoi_path <- NA

# Standard inputs
#---------------------------------------------#

library(glue)

# Zone list
zone_list <- c(16)

#home_dir
#home_dir <- "D:/LilaLeatherman/01_TreeMap/"
home_dir<- "//166.2.126.25/TreeMap/"

# Directory where target rasters live
target_dir <- glue("{home_dir}01_Data/01_TreeMap2016_RDA/02_Target/")

# Paths for exporting data
#--------------------------------------#

# set path to save output rasters
# this directory will be created if it does not already exist
output_dir <- glue('{home_dir}03_Outputs/07_Raw_model_outputs/2016_Original_Test/')

# Output imputation name
output_name <- "2016_Orig_TestLL"

# set tmp directory
tmp_dir <- "D:/tmp/"

# set zone_num
####################
zone_num <- zone_list[1]

# Set zone name options
cur.zone <- glue('z{zone_num}')
cur.zone.zero <- if(zone_num < 10) {
  glue('z0{zone_num}') } else {
    cur.zone
  }
##################

# Update output and target dir with zone
# -----------------------------------------#
# Set folder paths
target_dir = glue('{target_dir}/{cur.zone.zero}/')
output_dir = glue('{output_dir}/{cur.zone.zero}/')


# Model inputs
#----------------------------------#

# Path where model is located
model_path <- glue('{output_dir}/model/{cur.zone.zero}_{output_name}_yai_treelist_bin.RDS')


# Parallelization settings
#--------------------------------------#

# set number of cores that should be used
ncores <- 30
#ncores <- 3

# set up for parallel processing
#ncores <- availableCores(omit = 1)

# set percentage of available cores that should be used
#nCorefraction <- 0.75

# options for future 
options(future.rng.onMisuse = "ignore")
options(future.globals.onReference = "error")

###########################################################################
# Set up libraries and directories
###########################################################################

# Set up temp directory 
#----------------------------------#

# check if tmp directory exists 
if (file.exists(tmp_dir)){
  
} else {
  # create a new sub directory inside the main path
  dir.create(tmp_dir)
}

# set temp directory - helps save space with R terra
write(paste0("TMPDIR = ", tmp_dir), file=file.path(Sys.getenv('R_USER'), '.Renviron'))
#empty temp dir
do.call(file.remove, list(list.files(tmp_dir, full.names = TRUE)))

#detect and delete folders with pattern "Rtmp"
# folders <- dir(tmp_dir, pattern = "Rtmp", full.names = TRUE)
# unlink(folders, recursive = TRUE, force = TRUE, expand = TRUE)


#remove unused memory
gc()


# Packages and functions
#---------------------------------#

# packages required
list.of.packages <- c("raster", "yaImpute", "randomForest", 
                      "terra", "tidyverse", "magrittr", "glue", "tictoc",
                      "furrr")

#check for packages and install if needed
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages) > 0) install.packages(new.packages)

# load all packages
vapply(list.of.packages, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)

# Set up other directories
# ----------------------------------#

if (!file.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Terra options
# --------------------------------#

#increase memory fraction available
#terraOptions(memfrac = 0.8)

# Other options
# --------------------------------#

# Allow for sufficient digits to differentiate plot cn numbers
# plot CNs aren't present in the imputation portion, however

#options("scipen"=100, "digits"=8)


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

# FOR TESTING: crop to aoi
if (!is.na(aoi_path)) {
  
  print("using input shapefile as AOI")
  
  # process w terra package
  aoi <- terra::vect(aoi_path) %>% terra::project(lf.crs)
  rs2 <- terra::crop(rs2, aoi, mask = TRUE)
  
  # update output name
  output_name = glue('{output_name}_{aoi_name}')
    
} else {print("using extent of input raster stack as AOI")} 


##############################################################################
# Build imputation function
##############################################################################

impute.row <- function(currow, yai, ras, test)  { 

  # #for testing
  # currow <- 400
  # yai <- yai.treelist.bin
  # ras <- rs2
  # test <- TRUE
  
  # handle missing test param
  if(missing(test)) {
    test <- FALSE
  }
  
  # handle a wrapped raster as input
  if(is(ras) == "PackedSpatRaster") {
    ras <- terra::unwrap(ras)}
  
  # Get data from yai
  id.table <- as.numeric(row.names(yai$xRefs))
  maxrow <- max(id.table)
  
  #### Get dimensions of input raster stack
  nrows.out <- dim(ras)[1]
  ncols.out <- dim(ras)[2]
  
  # get cell numbers and raster values for current row
  rsvals <- terra::cellFromRowCol(ras, row = currow, col = 1:ncols.out)
  rsmat <- ras[rsvals]
  extract.currow <- data.frame(rsmat)

  #### Get coordinates from current row of raster
  xycoords <- terra::xyFromCell(ras, rsvals)
  xycoords <- data.frame(xycoords)

  #### Get dimensions of current row
  colseq <- 1:length(extract.currow[,1])
  valid.cols <- colseq[as.logical(1-is.na(extract.currow[,1]))]
  ncols.df <- dim(extract.currow)[2]

  #### Get coords of current row
  extract.currow$POINT_X <- xycoords$x
  extract.currow$POINT_Y <-xycoords$y

  # Remove NAs
  extract.currow <- na.exclude(extract.currow)

  # convert to data frame
  X.df.temp <- data.frame(extract.currow)

  #### Convert aspect to northing and easting
  X.df.temp <-
    X.df.temp %>%
    dplyr::mutate(radians = (pi/180)*ASPECT,           
                  NORTHING = cos(radians),
                  EASTING = sin(radians)) %>%
    dplyr::select(-radians)
    
  #### Identify EVGs in zone that don't appear in X.df   
  #evg.orig <- 1:n.evgs # INPUT OUTSIDE FUNCTION
  evg.orig <- levels(yai$xRefs$EVT_GP)
  evg.val.temp <- X.df.temp$EVT_GP  
  n.evgs.orig <- length(sort(unique(evg.orig)))  
  evg.orig.seq <- 1:n.evgs.orig  
    
  nonappearing.evgs <- evg.orig[-sort(unique(as.numeric(as.character(evg.val.temp))))]  
  n.dummy.rows <- length(nonappearing.evgs)  
    
  # Create dummy rows for non-appearing EVGs
  if(n.dummy.rows > 0)
    {    
    dummy.rows <- X.df.temp[1:n.dummy.rows,]    
    tempchar <- as.character(X.df.temp$EVT_GP)    
    X.df.temp$EVT_GP <- tempchar    
    dummy.rows$EVT_GP <- as.character(nonappearing.evgs)    
    X.df.temp <- rbind(X.df.temp, dummy.rows)    
    }
    
    # Set factor levels - make sure they match input factor levels in reference data used in model
    X.df.temp <- 
      X.df.temp %>%
      dplyr::mutate(EVT_GP = factor(EVT_GP, levels = levels(yai$xRefs$EVT_GP)),
             disturb_code = factor(disturb_code, levels = levels(yai$xRefs$disturb_code))) %>%
      # put columns in order expected
      dplyr::select(names(yai$xRefs))
    
    # Set up template for output of imputation
    nrows.orig <- dim(extract.currow)[1] # number of values to impute - without dummy rows for non-appearing evgs
    nrow.temp <- dim(X.df.temp)[1] # number of values to impute - with dummy rows for non-appearing evs
    nc.orig <-length(rsvals) # number of values in row, including NAs
    
    # Default output from imputation - all NAs 
    impute.out <- rep(NA,nc.orig) 
    
    # Option for TESTING  - skip imputation
    if(test == TRUE){
      # test output - simple extract the same size ans format as impute.row
      test.out.tmp <- as.numeric(unlist(extract.currow[2]))
      test.out <- impute.out
      test.out[valid.cols] <- test.out.tmp
      
      return(test.out)
      }
   
    else{
      # RUN IMPUTATION
      if(nrow.temp > 0) { # if there are any non-NA pixels in the row we're imputing

        # Format row names for X.df.temp - cannot overlap with input X.df
        colseq.out <- 1:dim(X.df.temp)[1]
        rownames.all <- colseq.out+maxrow
        rownames(X.df.temp) <- paste0("T- ", rownames.all)
  
        ### Perform imputation
        # take object from formed random forests model and use X.df.temp dataframe to make predictions
        temp.newtargs <- yaImpute::newtargets(yai, newdata = X.df.temp)
  
        #### Get outputs of interest
        #out.trgrows <- temp.newtargs$trgRows # row names for target observations
        #temp.xall <- temp.newtargs$xall # x-variables (predictors) for all observations
        out.neiIds <- temp.newtargs$neiIdsTrgs # a matrix of reference identifications that correspond to neiDstTrgs (distances between target and ref).
  
        #### Format outputs into imputation results
        yrows <- as.numeric(out.neiIds[,1]) # get list of plotIds; rowname = rowname from X.df.temp - corresponds to cell
        #id.out <- id.table[yrows] # subset id table to only the ids that appear in the output
        impute.out[valid.cols] <- yrows[1:nrows.orig] # for each valid column: match it with the output row from imputation
  
        # garbage collection
        gc()
        
      }
      
      return(impute.out) 
    } 
    }

##################################################################
#Apply Imputation Function
###################################################################

# Set up test 
# ---------------------------------------------------------- #

testrow1 <- test_row
testrow2 <- test_row + ntest_rows

# #test on one row - without parallelizing
# tic()
# out_test <- impute.row(testrow1, yai.treelist.bin, rs2)
# toc()

# # Run imputation - parallelize
# # ----------------------------------------------------

##############################################################
# Run imputation - furrr on rows
##############################################################
# # set up parallel processing
# #future::plan(sequential)
# future::plan(multisession, workers = ncores)
# 
# max_px <- 500
# 
# # make a teeny raster to test
# agg <- terra::aggregate(rs2, fact = max_px)
# 
# # subset the raster and create temporary files
# # the function returns file names for the temporary files
# tiles <- rs2 %>%
#   terra::makeTiles(agg, paste0(tempfile(), '_.tif'))
# 
# tile <- terra::rast(tiles[3])
# 
# # wrap() raster so it can be used in parallel - CUMBERSOME with large raster!!!
# #rs2_wrap <- terra::wrap(rs2)
# rs2_wrap <- terra::wrap(tile)
# 
# # garbage collection
# gc()
# 
# #run
# tic()
# 
# test <- testrow1:testrow2 %>%
#   future_map(impute.row, yai = yai.treelist.bin, ras = rs2_wrap, test = TRUE,
#              .progress = TRUE) #%>%
#   # set_names(c(testrow1:testrow2)) %>%
#   # dplyr::bind_rows() %>%
#   # as.matrix()
# 
# # bind rows of matrix together
# mout <- do.call(rbind, test)
# 
# # conditionally add extra rows-- to match input raster extent if working on a subset
# # add this to library?
# if(nrow(mout) < nrow(tile)) {
#   # make rows with NAs to make full raster of test 
#   ncols.out <- ncol(tile)
#   nrows.out <- nrow(tile)
#   d <- rep(NA, ncols.out)
#   blank_rows_top <- do.call("rbind", replicate(testrow1-1, d, simplify = FALSE))
#   blank_rows_bottom <- do.call("rbind", replicate(nrows.out-(testrow2), d, simplify = FALSE))
#   
#   # will the output raster, with blank rows, be the same size as the input raster?
#   identical(as.numeric(nrows.out), as.numeric(nrow(blank_rows_top) + nrow(mout) + nrow(blank_rows_bottom)))
#   
#   #bind test rows with NAs to make full raster
#   test_process <- terra::rast(rbind(blank_rows_top,
#                                 mout,
#                                 blank_rows_bottom))
#   
# 
# } else {
#   # convert to raster
#   test_process <-test2 %>%
#     terra::rast() 
# }
# 
# # set geospatial info
# ext(test_process) <- terra::ext(tile)
# crs(test_process) <- terra::crs(tile)
# 
# # trim
# test_process <- terra::trim(test_process)
#   
# # inspect
# test_process
# plot(test_process)
# 
# toc()

##############################################################
# Run imputation - furrr on tiles 
# ----------------------------------------------------------

# set max number of px on a side of a tile
max_px <- 500

# aggregate - template for making tiles
agg <- terra::aggregate(rs2, fact = max_px)

# subset the raster and create temporary files
# the function returns file names for the temporary files
tiles <- rs2 %>%
  terra::makeTiles(agg, paste0(tempfile(), '_.tif'), na.rm = TRUE)

# garbage collector
gc()

# set up nested future sessions
future::plan(
  list(
    future::tweak(
      future::multisession,
      workers = ncores/10),
    future::tweak(
      future::multisession,
      workers = ncores/2)
  )
)


#fn <- tiles[32]
#tiles_test <- tiles[1:5]


#########################################
# APPLY OVER TILES

tic()

r_out <- tiles %>%
  
  # Apply over tiles
  future_imap(function(fn, i, ...) {
    
    # read raster tile into memory
    ras <- terra::rast(fn)
    
    # get metadata from raster tile
    nrow_r <- nrow(ras)
    ncol_r <- ncol(ras)
    ext_r <- terra::ext(ras)
    crs_r <- terra::crs(ras)
    
    # wrap for inclusion in parallel
    ras <- terra::wrap(ras)
    
    # Do work on tile
    #--------------------------------#
    rows <-
      #testrow1:testrow2 %>%
      1:max_px %>%
      future_map(impute.row, # function
                 yai = yai.treelist.bin, ras = ras, test = FALSE, # function params
                 .progress = TRUE) # future_map params
    
    #bind rows together
    mout <- do.call(rbind, rows)

    #unwrap input raster - to get at metadata
    #ras <- terra::unwrap(ras)
    
    # turn into a raster tile 
    if(nrow(mout) < nrow_r) {
      
      # make rows with NAs to make full raster of test 
      ncols.out <- ncol_r
      nrows.out <- nrow_r
      d <- rep(NA, ncols.out)
      blank_rows_top <- do.call("rbind", replicate(testrow1-1, d, simplify = FALSE))
      blank_rows_bottom <- do.call("rbind", replicate(nrows.out-(testrow2), d, simplify = FALSE))
      
      # will the output raster, with blank rows, be the same size as the input raster?
      #identical(as.numeric(nrows.out), as.numeric(nrow(blank_rows_top) + nrow(mout) + nrow(blank_rows_bottom)))
      
      #bind test rows with NAs to make full raster
      tile_out <- terra::rast(rbind(blank_rows_top,
                                        mout,
                                        blank_rows_bottom))
      
      #post-process tile
      ext(tile_out) <- ext_r
      crs(tile_out) <- crs_r
      
      # wrap - for handling in parallel
      tile_out %<>% terra::wrap()
      
      return(tile_out)
      
      } else {
        
        # convert to raster
        tile_out <- mout %>% terra::rast() 
        
        #post-process tile
        ext(tile_out) <- ext_r
        crs(tile_out) <- crs_r
        
        # wrap - for handling in parallel
        tile_out %<>% terra::wrap()
        
        return(tile_out)
      }
      
      # end of work on tile 
    
    },
    .progress = TRUE) 

toc()

# convert tiles to a single raster
mout_ras <- r_out %>%
  terra::unwrap() %>% # unwrap packed rasters
  map(unwrap) %>%
  terra::sprc() %>% # convert to raster package
  terra::mosaic() # mosaic tiles

mout_ras
plot(mout_ras)
      
    

#------------------------------------
# Post-process test outputs
#------------------------------------
# 
# # make rows with NAs to make full raster of test 
# ncols.out <- ncol(rs2)
# nrows.out <- nrow(rs2)
# d <- rep(NA, ncols.out)
# blank_rows_top <- do.call("rbind", replicate(testrow1-1, d, simplify = FALSE))
# blank_rows_bottom <- do.call("rbind", replicate(nrows.out-(testrow2), d, simplify = FALSE))
# 
# # will the output raster, with blank rows, be the same size as the input raster?
# identical(as.numeric(nrows.out), as.numeric(nrow(blank_rows_top) + nrow(mout) + nrow(blank_rows_bottom)))
# 
# #bind test rows with NAs to make full raster
# mout_ras <- terra::rast(rbind(blank_rows_top,
#                                  mout,
#                                  blank_rows_bottom))
# 
# # mout_ras <- raster::raster(rbind(blank_rows_top, 
# #                                  mout, 
# #                                  blank_rows_bottom))
# 
# #rm(d, blank_rows_top, blank_rows_bottom)
# gc()

#-------------------------------------------
# Post-process raster outputs
#------------------------------------

#mout_ras <- terra::rast(mout)

# mout_ras@extent <- rs2@extent
# crs(mout_ras) <- lf.crs

# mask NAs to reduce output file size
#mout_ras <- raster::mask(mout_ras, mout_ras)


# set geospatial attributes
#terra::ext(mout_ras) <- terra::ext(rs2[[1]])
crs(mout_ras) <- lf.crs
mout_ras <- terra::trim(mout_ras) # trim off NA pixels from extent


# inspect
mout_ras
plot(mout_ras)
#freq(mout_ras)

# create output directory
if(!file.exists(glue('{output_dir}raster/'))){
  dir.create(glue('{output_dir}raster/'))
}

# export test raster
writeRaster(mout_ras, 
                   glue('{output_dir}raster/{output_name}_testRows_{testrow1}_{testrow2}_maxpx{max_px}_nT{length(tiles)}.tif'),
                   overwrite = TRUE)


# clear unused memory
#rm(mout, mout_ras)
gc()
