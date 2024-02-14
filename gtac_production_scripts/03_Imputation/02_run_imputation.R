# TreeMap Imputation
# Updated script written by Lila Leatherman (Lila.Leatherman@usda.gov)
# Original script written by Isaac Grenfell, RMRS (igrenfell@gmail.com) 


# PART 2: 
# - Run imputation over input area / target rasters
# - Save outputs as raster tiles


# TO DO: 
# - check for tiles available and then run whatever tiles aren't run


# Last updated: 2/13/2024

###########################################################################
# Set inputs
###########################################################################

# Tiling settings
# ---------------------------------------#
# set dimensions of tile - value is the length of one side
max_px <- 1000

# Test application settings
#-----------------------------------------#

# # supply path to a shapefile to use as subset, or NA
aoi_path <- "//166.2.126.25/TreeMap/01_Data/03_AOIs/UT_Uintas_rect_NAD1983.shp"
aoi_name <- "UT_Uintas_rect"
#aoi_path <- NA

# set number of cores that should be used
ncores <- 20
#ncores <- 3

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
#terraOptions(memfrac = 0.8)

# Other options
# --------------------------------#

# Allow for sufficient digits to differentiate plot cn numbers
# plot CNs aren't present in the imputation portion, however

#options("scipen"=100, "digits"=8)

# Parallelization settings
#--------------------------------------#



# options for future 
options(future.rng.onMisuse = "ignore") # ignore errors with random number generators
options(future.globals.onReference = "error") # give an error if a pointer can't access somehting in a future loop

# set up future session
future::plan("multisession", workers=ncores)

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
  
  # crop and mask
  aoi <- terra::vect(aoi_path) %>% terra::project(lf.crs)
  rs2 <- terra::crop(rs2, aoi, mask = TRUE)
  
  # update output name
  output_name = glue('{output_name}_{aoi_name}')
  
  gc()
  
} else {print("using extent of input raster stack as AOI")} 

#################################################################
# Prep raster inputs
#################################################################

# Calculate northing and easting from aspect
rs2$NORTHING <- terra::app(rs2$ASPECT, function(i) cos((pi/180)*i))
names(rs2$NORTHING) <- "NORTHING"
rs2$EASTING <- terra::app(rs2$ASPECT, function(i) sin((pi/180)*i))
names(rs2$EASTING) <- "EASTING"

# Reclass disturbance to binary 
rs2$disturb_code <- terra::classify(rs2$disturb_code, cbind(2, 1))

# add XY coords to raster
rs2$POINT_X <- terra::init(rs2, 'x')
names(rs2$POINT_X) <- "POINT_X"
rs2$POINT_Y <- terra::init(rs2, 'y')
names(rs2$POINT_Y) <- "POINT_Y"

# remove aspect
rs2$ASPECT <- NULL

gc()

##############################################################################
# Build imputation function
##############################################################################

# # MAKE IMPUTE ROW WORK ON DATA FRAME (representing one row of raster) INSTEAD OF RASTER
# # try out impute.grid?
# 
# impute.row <- function(dat, yai, test)  { 
#   
#   require(yaImpute)
#   
#   # #for testing
#   #currow <- 400
#   #yai <- yai.treelist.bin
#   #ras <- ras
#   #test <- FALSE
#   
#   # Manage inputs
#   #---------------------------------------#
#   
#   #give dat the name we use in this function
#   extract.currow <- data.frame(dat)
#   
#   # handle missing test param
#   if(missing(test)) {
#     test <- FALSE
#   }
#   
#   # # handle a wrapped raster as input
#   # if(is(ras) == "PackedSpatRaster") {
#   #   ras <- terra::unwrap(ras)}
#   
#   
#   
#   # # Get data from raster
#   # #------------------------------------------------#
#   # 
#   # #### Get dimensions of input raster stack
#   # nrows.out <- dim(ras)[1]
#   # ncols.out <- dim(ras)[2]
#   # 
#   # # get cell numbers and raster values for current row
#   # rsvals <- terra::cellFromRowCol(ras, row = currow, col = 1:ncols.out)
#   # rsmat <- ras[rsvals]
#   # extract.currow <- data.frame(rsmat)
#   # 
#   # #### Get coordinates from current row of raster
#   # xycoords <- terra::xyFromCell(ras, rsvals)
#   # xycoords <- data.frame(xycoords)
#   
#   #### Get dimensions of current row
#   colseq <- 1:length(extract.currow[,1])
#   valid.cols <- colseq[as.logical(1-is.na(extract.currow[,1]))]
#   ncols.df <- dim(extract.currow)[2]
#   
#   # #### Get coords of current row
#   # extract.currow$POINT_X <- xycoords$x
#   # extract.currow$POINT_Y <-xycoords$y
#   
#   # Remove NAs
#   extract.currow <- na.exclude(extract.currow)
#   
#   # convert to data frame
#   X.df.temp <- data.frame(extract.currow)
#   
#   # Set up template for output of imputation
#   nrows.orig <- dim(extract.currow)[1] # number of values to impute - without dummy rows for non-appearing evgs
#   nrow.temp <- dim(X.df.temp)[1] # number of values to impute - with dummy rows for non-appearing evs
#   nc.orig <-length(colseq) # number of values in row, including NAs
#   
#   # Default output from imputation - all NAs 
#   impute.out <- rep(NA,nc.orig) 
#   
#   # CHECK FOR NA VALUES
#   #------------------------#
#   if(nrow.temp > 0) { # if there are any non-NA pixels in the row we're imputing
#     
#     # Get data from yai
#     #-----------------------------------------------#
#     id.table <- as.numeric(row.names(yai$xRefs))
#     maxrow <- max(id.table)
#     
#     # EVG handling - 
#     #### Identify EVGs in zone that don't appear in X.df   
#     #-------------------------------------------------------#
#     evg.orig <- levels(yai$xRefs$EVT_GP)
#     evg.val.temp <- X.df.temp$EVT_GP  
#     n.evgs.orig <- length(sort(unique(evg.orig)))  
#     
#     nonappearing.evgs <- evg.orig[-sort(unique(as.numeric(as.character(evg.val.temp))))]  
#     n.dummy.rows <- length(nonappearing.evgs)  
#     
#     # Create dummy rows for non-appearing EVGs
#     if(n.dummy.rows > 0)
#     {    
#       dummy.rows <- X.df.temp[1:n.dummy.rows,]    
#       tempchar <- as.character(X.df.temp$EVT_GP)    
#       X.df.temp$EVT_GP <- tempchar    
#       dummy.rows$EVT_GP <- as.character(nonappearing.evgs)    
#       X.df.temp <- rbind(X.df.temp, dummy.rows)    
#     }
#     
#     # Set factor levels
#     #make sure they match input factor levels in reference data used in model
#     #-------------------------------------------------------#
#     X.df.temp <- 
#       X.df.temp %>%
#       dplyr::mutate(EVT_GP = factor(EVT_GP, levels = levels(yai$xRefs$EVT_GP)),
#                     disturb_code = factor(disturb_code, levels = levels(yai$xRefs$disturb_code))) %>%
#       # put columns in order expected
#       dplyr::select(names(yai$xRefs))
#     
#     # Run imputation
#     #--------------------------------------------------------#
#     
#     # Option for TESTING  - skip imputation
#     if(test == TRUE){
#       # test output - simple extract the same size ans format as impute.row
#       test.out.tmp <- as.numeric(unlist(extract.currow[2]))
#       test.out <- impute.out
#       test.out[valid.cols] <- test.out.tmp
#       
#       return(test.out)
#     }
#     
#     else{
#       # Format row names for X.df.temp - cannot overlap with input X.df
#       colseq.out <- 1:dim(X.df.temp)[1]
#       rownames.all <- colseq.out+maxrow
#       rownames(X.df.temp) <- paste0("T- ", rownames.all)
#       
#       ### Perform imputation
#       # take object from formed random forests model and use X.df.temp dataframe to make predictions
#       temp.newtargs <- yaImpute::newtargets(yai, newdata = X.df.temp)
#       
#       #### Get outputs of interest
#       #out.trgrows <- temp.newtargs$trgRows # row names for target observations
#       #temp.xall <- temp.newtargs$xall # x-variables (predictors) for all observations
#       out.neiIds <- temp.newtargs$neiIdsTrgs # a matrix of reference identifications that correspond to neiDstTrgs (distances between target and ref).
#       
#       #### Format outputs into imputation results
#       yrows <- as.numeric(out.neiIds[,1]) # get list of plotIds; rowname = rowname from X.df.temp - corresponds to cell
#       #id.out <- id.table[yrows] # subset id table to only the ids that appear in the output
#       impute.out[valid.cols] <- yrows[1:nrows.orig] # for each valid column: match it with the output row from imputation
#       
#       # garbage collection
#       gc()
#     }
#   }
#   
#   return(impute.out)
#   
# }

##################################################################
#Apply Imputation Function
###################################################################

# Set up run
# ---------------------------------------------------------- #

# first row to start test on
#test_row <- 1 # adjust this if using a test AOI or tiles vs whole zone

#ntest_rows <- max_px

# # set number of tiles to run
# # if NA, defaults to all tiles in list
ntiles <- NA

# set up rows to run over
row1 <- 1
row2 <- max_px


#test on one row - without parallelizing
# tic()
# out_test <- impute.row(500, yai.treelist.bin, rs2)
# toc()


##############################################################
# Run imputation - furrr on tiles 
# ----------------------------------------------------------

# aggregate - template for making tiles
agg <- terra::aggregate(rs2, fact = max_px)

# subset the raster and create temporary files
# tiles with only NA values are omitted
# the function returns file names for the temporary files
tiles <- rs2 %>%
  terra::makeTiles(agg, paste0(tempfile(), '_.tif'), na.rm = TRUE)

# garbage collector
gc()


# if ntiles is NA, default to number of tiles created
if(is.na(ntiles)) {
  ntiles <- length(tiles)
}

# select tiles to run
tiles_run <- tiles[1:ntiles]
#tiles_run <- tiles[10:15]


#########################################
# APPLY OVER TILES

for(j in 1:length(tiles_run)) {
  
  # for test 
  #j <- 1
  
  # select tile to run
  fn <- tiles_run[j]
  print(glue::glue("working on tile {j} of {length(tiles_run)}"))
  
  # start timer
  tic()
  
  # read raster tile into memory
  ras <- terra::rast(fn)
  
  # get metadata from raster tile
  nrow_r <- nrow(ras)
  ncol_r <- ncol(ras)
  ext_r <- terra::ext(ras)
  crs_r <- terra::crs(ras)
  
  # convert raster to matrix
  mat <- as.matrix(ras)
  
  # convert matrix to list of data frames - one data frame for each row
  rows_in <- NULL
  
  for(r in row1:nrow_r) {
    
    d <- list(data.frame(
      mat[(ncol_r*(r-1)+1):(ncol_r*r),])) # get extracted values from each field for each "row" of input raster
    
    if(is.null(rows_in)){
      rows_in <- d
    } else {
      rows_in <- c(rows_in, d)
    }}
  
  # Do work on tile
  #--------------------------------#

  #wrapper function to track progress
  progressr::with_progress({
    p <- progressr::progressor(steps = length(rows_in))
  
    # work over tile
    rows_out <-
      1:length(rows_in) %>%
      future_imap(function(fn, i, ...) {
        
        # report progress
        p() 
        
        # # for testing
        # i = 1
        # dat = rows_in[i]
        # print(glue::glue("working on row {i}/{length(rows_i)}"))
        
        # impute on row of data - input is named row of data + yai
        row <- impute.row(dat = rows_in[i],
                          yai = yai.treelist.bin, test = FALSE)
        
        return(row)
      }) # end future over rows
    }) # end wrapper function to track progress
  
  #bind rows together
  mout <- do.call(rbind, rows_out)
  
  # turn into a raster tile 
  if(nrow(mout) < nrow_r) {
    
    # this could be broken out into library as a helper function
    #----------------------------------------------------#
    # make_raster_tile <- function(mout, ncol_r, nrow_r) { require(terra) }
    
    
    # make rows with NAs to make full raster of test 
    ncols.out <- ncol_r
    nrows.out <- nrow_r
    d <- rep(NA, ncols.out)
    blank_rows_top <- do.call("rbind", replicate(row1-1, d, simplify = FALSE))
    blank_rows_bottom <- do.call("rbind", replicate(nrows.out-nrow(mout), d, simplify = FALSE))
    
    # will the output raster, with blank rows, be the same size as the input raster?
    #identical(as.numeric(nrows.out), as.numeric(nrow(blank_rows_top) + nrow(mout) + nrow(blank_rows_bottom)))
    
    #bind test rows with NAs to make full raster
    tile_out <- terra::rast(rbind(blank_rows_top,
                                  mout,
                                  blank_rows_bottom))
    
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
              #glue::glue('{output_dir}raster/tiles/{output_name}_maxpx{max_px}_nT{ntiles}_tile{j}.tif'),
              paste0(output_dir,'raster/tiles/',output_name,'_maxpx',max_px,'_nT',ntiles,'_tile',j,'.tif'),
              overwrite = TRUE)
  
  rm(tile_out, rows_in)
  gc() # end of work on tile 
  
  print(glue::glue("done with tile {j} of {ntiles}"))
  print(toc()) # report time elapsed
  
}

print(glue::glue("Done with zone {zone_num}!"))

