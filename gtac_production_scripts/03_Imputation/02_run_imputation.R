# TreeMap Imputation
# Updated script written by Lila Leatherman (Lila.Leatherman@usda.gov)
# Original script written by Isaac Grenfell, RMRS (igrenfell@gmail.com) 


# PART 2: 
# - Run imputation
# - Save outputs as a raster

# Restart R between runs that use parallelization

# TO DO: 
# - get parallelization to work
# - read in rasters as vrt

# Last updated: 12/28/2023

###########################################################################
# Set inputs
###########################################################################

library(glue)

# Zone list
zone_list <- c(16)

#home_dir
home_dir <- "D:/LilaLeatherman/01_TreeMap/"

# Path to X table
xtable_path <- glue("{home_dir}01_Data/01_TreeMap2016_RDA/01_Input/03_XTables/X_table_all_singlecondition.txt")

# Directory where target rasters live
target_dir <- glue("{home_dir}01_Data/01_TreeMap2016_RDA/02_Target/")

# Directory where EVT_GP remap table is located
evt_gp_remap_table_path <- glue("{home_dir}03_Outputs/05_Target_Rasters/02_Vegetation/")

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
evt_gp_remap_table_path = glue('{evt_gp_remap_table_path}/{cur.zone.zero}/EVG_remap_table.csv')


# Model inputs
#----------------------------------#

# Path where model is located
model_path <- glue('{output_dir}/model/{cur.zone.zero}_{output_name}_yai_treelist_bin.RDS')

# load x table and y table

Xdf_path <- glue('{output_dir}/xytables/{cur.zone.zero}_{output_name}_Xdf_bin.csv')
Ydf_path <- glue('{output_dir}/xytables/{cur.zone.zero}_{output_name}_Ydf_bin.csv')

# Xdf_path_orig <- glue('{output_dir}/xytables/{cur.zone.zero}_{output_name}_Xdf_orig.csv')
# Ydf_path_orig <- glue('{output_dir}/xytables/{cur.zone.zero}_{output_name}_Ydf_orig.csv')


# Parallelization settings
#--------------------------------------#

# set percentage of available cores that should be used
ncores <- 10
#nCorefraction <- 0.75

# Test application settings
#-----------------------------------------#

# first row to start test on 
test_row <- 400 # adjust this if using a test AOI vs whole zone

ntest_rows <- 10

# supply path, or NA
aoi_path <- "//166.2.126.25/TreeMap/01_Data/03_AOIs/UT_Uintas_rect_NAD1983.shp"
aoi_name <- "UT_Uintas_rect"
#aoi_path <- NA

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
folders <- dir(tmp_dir, pattern = "Rtmp", full.names = TRUE)
unlink(folders, recursive = TRUE, force = TRUE, expand = TRUE)


#remove unused memory
gc()


# Packages and functions
#---------------------------------#

# packages required
list.of.packages <- c("raster", "yaImpute", "randomForest", 
                      "terra", "tidyverse", "magrittr", "glue", "tictoc",
                      "doParallel", "foreach")

#check for packages and install if needed
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages) > 0) install.packages(new.packages)

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

# Load imputation model and inputs
# ---------------------------------------------------------- #

#load model

yai.treelist.bin <- read_rds(model_path)

# load x table and y table
X.df <- read.csv(Xdf_path)
#Y.df <- read.csv(Ydf_path) # y table isn't used in running imputation

# # original - not binary reclass to disturbance code
# X.df.orig <- read.csv(Xdf_path_orig)
# Y.df.orig <- read.csv(Ydf_path_orig)

# Load target rasters
# --------------------------------------------------------------------#

# list raster files
flist.tif <- list.files(path = target_dir, pattern = "*.tif$", recursive = TRUE, full.names = TRUE)

# load raster files as raster stack
raster.stack <- stack(flist.tif)
p4s.albers <- proj4string(raster.stack)
lf.crs <- crs(raster.stack)

# get raster names 
raster_names <- flist.tif %>%
  str_extract(., "z[0-9][0-9]/([^.])*") %>%
  str_replace("z[0-9][0-9]/", "")

#add names to raster list
names(raster.stack) <- raster_names

# FOR TESTING: crop to aoi
if (!is.na(aoi_path)) {
  
  print("using input shapefile as AOI")
  
  # load aoi subset 
  aoi <- raster::shapefile(aoi_path) 
  aoi <- spTransform(aoi, lf.crs)
  
  # crop raster
  raster.stack <- raster::crop(raster.stack, aoi)
  raster.stack <- raster::mask(raster.stack, aoi)
  
  # update output name
  output_name = glue('{output_name}_{aoi_name}')
    
} 

# Load EVT Group Remap table
# ----------------------------------------------------------#

evt_gp_remap_table <- read.csv(evt_gp_remap_table_path)


# Prep input tables - set factors etc
#-------------------------------------------------------------------------#

X.df %<>%
  mutate(EVT_GP = as.factor(EVT_GP),
         disturb_code = as.factor(disturb_code),
         disturb_year = as.factor(disturb_year))

# Set up inputs to imputation function data
#-------------------------------------------------------------------------#

# number of rows in X.df
maxrow <- max(as.numeric(rownames(X.df)))

#dimensions of raster stack
nrows.out <- dim(raster.stack)[1]
ncols.out <- dim(raster.stack)[2]

# raster stack
rs2 <- raster.stack

# List of Plot IDs
id.table <- X.df$X # comes from saved row names 

# EVG remap
n.evgs <- nrow(evt_gp_remap_table)
evg.in <- as.factor(X.df$EVT_GP)

# Disturbance code levels
lev.dc <- levels(X.df$disturb_code)
#lev.year <- levels(X.df$disturb_year)


##############################################################################
# Build imputation function
##############################################################################

impute.row <- function(currow)  { 
  
  # for testing
  #currow <- 100
  
  # External objects brought into this function:
  # yai.treelist.bin 
  # rs2
  # nrows.out
  # id.table
  # maxrow
  
  ## EVG processing
  # n.evgs
  # evg.in
  # lev.dc

  ## Progress tracking
  # output_dir
  
  #### Load libraries for function
  #library(yaImpute) 
  #library(raster) 
  
  #library(rgdal)
  #raster.coords  <- coordinates(dem.raster)
  #currow.vals <- cellFromRow(dem.raster, currow)
  #coords.currow <- raster.coords[currow.vals,]  
  
  #### Get values from current row of raster
  rsvals <- raster::cellFromRow(rs2, currow)
  rsmat <- rs2[rsvals]
  extract.currow <- data.frame(rsmat)
  
  #### Get coordinates from current row of raster
  xycoords <- raster::xyFromCell(rs2, rsvals)
  xycoords <- data.frame(xycoords)
  
  #### Get dimensions of current row
  colseq <- 1:length(extract.currow[,1])
  valid.cols <- colseq[as.logical(1-is.na(extract.currow[,1]))]
  
  ncols.df <- dim(extract.currow)[2]
  
  #### Process current row extract
  extract.currow$"POINT_X" <- xycoords $x
  extract.currow$"POINT_Y" <-xycoords $y
  extract.currow <- na.exclude(extract.currow)

  X.df.temp <- data.frame(extract.currow)
  #nrow.temp <- dim(X.df.temp)[1]
  
  #### Convert aspect to northing and easing
  aspect.temp <- X.df.temp$ASPECT  
  rad.temp <- (pi/180)*aspect.temp  
  northing.temp <- cos(rad.temp)  
  easting.temp <- sin(rad.temp)
  X.df.temp <- X.df.temp[,-1]  
  X.df.temp$NORTHING <- northing.temp  
  X.df.temp$EASTING <- 	easting.temp
  
  #### Set evg 
  temp.evg <- X.df.temp$'EVT_GP'
  
  # Identify EVGs in zone that don't appear in X.df   
  evg.orig <- 1:n.evgs 
  #evg.orig <- as.numeric(levels(evg.in))
  evg.val <- evg.orig  
  evg.val.temp <- X.df.temp$'EVT_GP'  
  n.evgs.orig <- length(sort(unique(evg.orig)))  
  evg.orig.seq <- 1:n.evgs.orig  
  
  nonappearing.evgs <- evg.val[-sort(unique(as.numeric(as.character(evg.val.temp))))]  
  n.dummy.rows <- length(nonappearing.evgs)  
  
  # Create dummy row for non-appearing EVGs
  if(n.dummy.rows > 0)    
  {    
    dummy.rows <- X.df.temp[1:n.dummy.rows,]    
    tempchar <- as.character(X.df.temp$'EVT_GP')    
    X.df.temp$'EVT_GP' <- tempchar    
    dummy.rows$'EVT_GP' <- as.character(nonappearing.evgs)    
    X.df.temp <- rbind(X.df.temp, dummy.rows)    
  }
  
  #set factor levels for EVT-GP
  temp.fac <- factor(X.df.temp$'EVT_GP', levels = levels(evg.in))  # INPUT OUTSIDE FUNCTION
  X.df.temp$'EVT_GP' <- as.factor(temp.fac)
  
  # Set factor levels for disturbance code 
  dc.code.fac.temp <- factor(X.df.temp$disturb_code, levels=lev.dc)  # INPUT OUTSIDE FUNCTION
  X.df.temp$disturb_code <- dc.code.fac.temp  
  
  # Set factor levels for disturbance year
  #dc.year.fac.temp <- factor( X.df.temp$disturb_year, levels=lev.year)
  #X.df.temp$disturb_year <- dc.year.fac   
  
  # Set up template for output imputation
  nrows.orig <- dim(extract.currow)[1] # number of values to impute - without dummy rows for non-appearing evgs
  nrow.temp <- dim(X.df.temp)[1] # number of values to impute - with dummy rows for non-appearing evs
  
  #nc.orig <- dim(coords.currow)[1]  
  nc.orig <-length(rsvals) # number of values in row, including NAs
  
  impute.out <- rep(NA,nc.orig) # make template for imputation - one NA for each px in input row 
    
  
  if(nrow.temp > 0)  # if there are any non-NA pixels in the row we're imputing  
  { 
    # Format row names for X.df.temp - cannot overlap with input X.df
    colseq.out <- 1:dim(X.df.temp)[1] 
    rownames.all <- colseq.out+maxrow    
    rownames(X.df.temp) <- paste0("T- ", rownames.all) 
    
    # Adjust column names and factor levels on X.df.temp
    X.df.temp$Total_Cover <- X.df.temp$canopy_cover
    X.df.temp$Dominant_Nom_Ht <- X.df.temp$canopy_height
    
    # # Recode disturbance to binary
    # temp.dc <- as.character(X.df.temp$disturb_code)
    # temp.dc[temp.dc!= "0"] <- 1 # recode disturbance to binary
    # temp.dc <- as.factor(temp.dc)
    # X.df.temp$disturb_code <- temp.dc
    
    #### Perform imputation
    # take object from formed random forests model and use X.df.temp dataframe to make predictions    
    temp.newtargs <- newtargets(yai.treelist.bin, newdata = X.df.temp)    
    
    #### Get outputs of interest
    out.trgrows <- temp.newtargs$trgRows # row names for target observations
    temp.xall <- temp.newtargs$xall # x-variables (predictors) for all observations  
    out.neiIds <- temp.newtargs$neiIdsTrgs # a matrix of reference identifications that correspond to neiDstTrgs (distances between target and ref).
    
    #### Format outputs into imputation results
    yrows <- as.numeric(out.neiIds[,1]) # get list of plotIds; rowname = rowname from X.df.temp - corresponds to cell   
    id.out <- id.table[yrows] # subset id table to only the ids that appear in the output 
    impute.out[valid.cols] <- yrows[1:nrows.orig] # for each valid column: match it with the output row from imputation   
  }
  
  # progress tracking
  outval <- currow / nrows.out

  # # write file with percentage completed
  # fout <- "zoneprog.txt"
  # write.table(outval, 
  #             paste0(output_dir, fout))
  
  return(impute.out)  
}

##################################################################
#Apply Imputation Function
###################################################################

# Set up test 
# ---------------------------------------------------------- #

testrow1 <- test_row
testrow2 <- test_row + ntest_rows

# # test on one row - without parallelizing
# tic()
# out_test <- impute.row(testrow1)
# toc()

# Run imputation - with parallelizing
# ----------------------------------------------------#

# Set number of cores
#ncores <- parallel::detectCores()
#ncores <- floor(ncores*nCorefraction)


# Set up cluster for parallel computing
cl <- parallel::makeCluster(ncores, outfile = glue('{tmp_dir}cl_log.txt'))
doParallel::registerDoParallel(cl)

#registerDoSEQ() # register sequential backend - instead of parallel backend

# RUN IMPUTATION ON ROWS
tic()
mout <- foreach(m = testrow1:testrow2, .packages = c("raster", "yaImpute"), 
                .combine="rbind",
                .verbose = TRUE) %dopar% impute.row(m)
toc()


# finish parallel
parallel::stopCluster(cl)
closeAllConnections()
gc()

# Post-process test outputs
#------------------------------------

# make rows with NAs to make full raster of test 
d <- rep(NA, ncols.out)
blank_rows_top <- do.call("rbind", replicate(testrow1-1, d, simplify = FALSE))
blank_rows_bottom <- do.call("rbind", replicate(nrows.out-(testrow2 + 1), d, simplify = FALSE))

#bind test rows with NAs to make full raster
mout_ras <- raster::raster(rbind(blank_rows_top,
                                 mout,
                                 blank_rows_bottom))

rm(blank_rows_top, blank_rows_bottom)
gc()

# Post-process raster outputs
#------------------------------------#

#mout_ras <- terra::rast(mout)

# set geospatial attributes
raster::extent(mout_ras) <- raster::extent(raster.stack[[1]])
crs(mout_ras) <- crs(raster.stack[[1]])


# mout_ras@extent <- raster.stack[[1]]@extent
# raster::crs(mout_ras) <- p4s.albers


# inspect
mout_ras
# plot(mout_ras)
# freq(mout_ras)

# create output directory
if(!file.exists(glue('{output_dir}raster/'))){
  dir.create(glue('{output_dir}raster/'))
}

# export test raster
terra::writeRaster(mout_ras, 
                   glue('{output_dir}raster/{output_name}_testRows_{testrow1}_{testrow2}_doSeq.tif'),
                   overwrite = TRUE)

# clear unused memory
rm(mout, mout_ras)
gc()