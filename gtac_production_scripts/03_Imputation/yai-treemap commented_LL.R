# Original script: Isaac Grenfell, RMRS (igrenfell@gmail.com)
# Updated by Lila Leatherman (Lila.Leatherman@usda.gov)

# Objective: Random Forest Imputation, by zone, to generate TreeMap data products

# Set up a test run for one zone, using Karin's provided target data and x table

# TO DO: 
# - convert to r terra from r raster
# - break into separate scripts for imputation prep and imputation run? 
# - read in rasters as vrt
# - export predicted + ref for left-out plots in RF 

# Load libraries
# -------------------------------#

#original libraries
library(yaImpute)
library(raster)
#library(rgdal)
library(foreign)
library(pryr)
library(parallel)
library(foreach)
library(doParallel)

# added by Lila
library(terra)
library(tidyverse)
library(magrittr)
library(randomForest)
library(tictoc)
library(glue)

# Setup
# -------------------------------#

# Set the zone
cur.zone <-  "z16"
cur.zone.zero <-  "z16" # when zone is only one digit, add zero to the front (e.g., Z07)

###Very important! Always run this first so that you allow for sufficient digits to differentiate plot cn numbers
options("scipen"=100, "digits"=8)

#outfolder <- cur.zone

output_dir <-"//166.2.126.25/TreeMap/03_Outputs/07_Raw_model_outputs/2016_Original_Test/"
if(!file.exists(output_dir)){
  dir.create(output_dir)
}

# Directory where target rasters live
target_dir <- "//166.2.126.25/TreeMap/01_Data/01_TreeMap2016_RDA/02_Target/"

# Set folder paths
target_dir = glue('{target_dir}{cur.zone.zero}/')
output_dir = glue('{output_dir}{cur.zone.zero}/')


# Load target rasters
# ------------------------------------------#

####First input files: Build raster stack of target data
#setwd("G:\\Workspace\\treemap\\treemap2014_rasters2016\\target_data_reclassified_final")
# setwd("//166.2.126.25/TreeMap/01_Data/01_TreeMap2016_RDA/02_Target")
# setwd(cur.zone.zero)
# 
# flist.tif <- Sys.glob("*.tif")

# list raster files
flist.tif <- list.files(path = target_dir, pattern = "*.tif$", recursive = TRUE, full.names = TRUE)

raster.stack <- stack(flist.tif)
p4s.albers <- proj4string(raster.stack)
crs.ras <- crs(raster.stack)
#raster.list <- vector("list", length(flist.tif))
# nrasters <- length(flist.tif)
# for(i in 1:length(flist.tif))  
# {
#   raster.list[[i]] <- raster()  
# }

# get raster names 
raster_names <- flist.tif %>%
  str_extract(., "z[0-9][0-9]/([^.])*") %>%
  str_replace("z[0-9][0-9]/", "")

#add names to raster list
#names(raster.list) <- raster_names
names(raster.stack) <- raster_names

#convert raster stack to terra object
#raster_rast <- terra::rast(raster.stack)

# Load plot coordinates in meters
# ----------------------------------------------------------#

###Second input file
#setwd("G:\\Workspace\\treemap\\treelist_2016\\EVG_remap")
#meters.db <- read.table("metersdb.txt")
###only needed to get plot coordinates in meters

# Load X table
# --------------------------------------#

###Third input file
#allplot.2014 <- read.table("x_table_TreeMap2014_final_EVG_Karin_reclass_plus_loblolly_manual.txt", header = TRUE, sep = ",")

#allplot.2016 <- read.table("x_table_TreeMap2016_final.csv", header = TRUE, sep = ",")

#allplot.2014 <- read.table("X-table-single-cond.txt", header = TRUE, sep = ",")

allplot <- read.csv("//166.2.126.25/TreeMap/01_Data/01_TreeMap2016_RDA/01_Input/03_XTables/X_table_all_singlecondition.txt")

###chose vintage of x table here

#allplot <- allplot.2014
#allplot <- allplot.KRiley

# Convert plot coordinates to meters
#----------------------------------------------------------#
# NOTE: this is currently a stand-in. the allplot table has abbreviated records of the lat and long 
# (to 2 decimal places)

# convert allplot to spatial object
allplot_vect <- terra::vect(cbind(allplot$ACTUAL_LON, allplot$ACTUAL_LAT))

# set input projection
crs(allplot_vect) <- "epsg:4326"

# reproject to desired projection
allplot_vect %<>% terra::project(crs.ras)

# extract lat/long in meters
allplot_xy <- terra::geom(allplot_vect) %>%
  data.frame() %>%
  dplyr::rename("POINT_X" = x,
                "POINT_Y" = y) %>%
  dplyr::select(POINT_X, POINT_Y)

# bind back with allplot table
allplot <- cbind(allplot, allplot_xy)

# EVG remap
# ----------------------------------------#

# ###Fourth input file - evg remap

# setwd("G:\\Workspace\\treemap\\treemap2014_rasters2016\\EVG_remap-2014\\EVG_remap")
# 
# fin <- paste(cur.zone.zero, "_EVG_remap.txt", sep = "")
# 
# remap <- read.table(fin , sep=":")

evt_gp_remap_table <- read.csv("//166.2.126.25/TreeMap/03_Outputs/05_Target_Rasters/02_Vegetation/z16/EVG_remap_table.csv")

#Limit allplot to just the veg types in the remap table
plot.df <- allplot[allplot$EVT_GP %in% evt_gp_remap_table$EVT_GP,]
#plot.df <- allplot

#inspect
dim(plot.df)
plot(terra::vect(cbind(plot.df$POINT_X,plot.df$POINT_Y)))

###Change this!
#dir.create(paste("F:\\Tree_List_c2014\\outputs\\", cur.zone, "_disturb", sep=""))

#write.csv(plot.df, paste("F:\\Tree_List_c2014\\outputs\\", cur.zone, "_disturb\\", cur.zone, "_x_table_allplots_reclass.txt", sep=""), row.names = F)
##plot.df$CN <- as.numeric(plot.df$CN)
# Karin commented out above because it was changing CN)

# Bind with plot coordinates
#merge.df <- merge(plot.df, meters.db, by = "CN")
# # join coords with plot data
#merge.df <- left_join(plot.df, fia_pts_xy,  by = c("ID" = "PLOT") )


plot.df$CN <- factor(plot.df$CN)


# Build X and Y predictor matrices / factors
# ----------------------------------------------#

##Build X predictor matrix
evg.fac <- as.factor(plot.df$EVT_GP)
#evg.fac <- as.factor(plot.df$EVT_GP_reclass)
dc.code.fac <- as.factor(plot.df$disturb_code)
dc.year.fac <- as.factor(plot.df$disturb_year)
dc.year.num <- as.numeric(plot.df$disturb_year)


lev.dc <- levels(dc.code.fac)
lev.year <- levels(dc.year.fac)

#plot.df[,18] <- evg.fac
plot.df$EVT_GP<- evg.fac

##Build Y response matrix
# plot.df$POINT_X <- merge.df$POINT_X
# plot.df$POINT_Y <- merge.df$POINT_Y

# Reclass EVGs in plot.df table
# --------------------------------------------------#

####Reclass evgs
#evg.reclass <- read.table(paste(cur.zone.zero, "_EVG_remap.txt", sep=""), sep=":")
# evg.reclass <- remap
n.evgs <- nrow(evt_gp_remap_table)

#reassign object to evg.reclass
evg.reclass <- evt_gp_remap_table %>%
  dplyr::select(EVT_GP, EVT_GP_remap)

evg.out <- rep(0, dim(plot.df)[1])
evg.vec <- plot.df$"EVT_GP"
for(i in 1:n.evgs)  
{  
  cur.evg <- evg.reclass[i, 1]  # this will need to be changed
  sub.ind <- evg.vec == cur.evg  
  evg.out[sub.ind] <- i  
  
  rm(cur.evg, sub.ind)
}	
evg.in <- as.factor(evg.out)

plot.df$"EVT_GP" <- as.factor(evg.out)

plot.df$disturb_code <- as.factor(plot.df$disturb_code)

# Create X-Table and Y-Table
# ---------------------------------------------#

#Create X Table
#X.df <- plot.df[,5:20] 
X.df <- plot.df %>%
  dplyr::select(SLOPE, ASPECT, ELEV, PARI, PPTI, RELHUMI, TMAXI, TMINI, VPDI,
                disturb_code, disturb_year, canopy_cover, canopy_height, EVT_GP,
                POINT_X, POINT_Y)

# Re-calculate aspect 

aspect.temp <- X.df$ASPECT
rad.temp <- (pi/180)*aspect.temp
northing.temp <- cos(rad.temp)
easting.temp <- sin(rad.temp)
X.df <- X.df[,-2]
#X.df$ASPECT <- NULL
X.df$NORTHING <- northing.temp
X.df$EASTING <- 	easting.temp

rownames(X.df) <- plot.df$ID # add plotid as row names
id.table <-  plot.df$ID

# Create Y table
Y.df <- data.frame(plot.df[,16:18]) # EVC, EVH, EVG
rownames(Y.df) <- plot.df$ID # add plotid as row names
#X.df <- X.df[,-c(9, 10)]

# remove temp files
rm(aspect.temp, rad.temp, northing.temp, easting.temp)

# inspect plot.df table
# -----------------------------------------------------------------------#

# plot.df %>%
#   group_by(ID) %>%
#   count() %>% 
#   filter(n>1)
# 
# plot.df %>%
#   group_by(CN) %>%
#   count() %>% 
#   filter(n>1)
# 
# plot.df %>%
#   dplyr::select(ID, CN) %>%
#   distinct() %>%
#   nrow()


# Build the random forests model (X=all predictors, Y=EVG, EVC, EVH)
# -----------------------------------------------------------------------#


#yai.treelist <- yai(X.df, Y.df, method = "randomForest", ntree = 249)

##Recode Disturbance as 0/1 in X table; add Disturbance to Y table

dc.bin <- as.character(X.df$disturb_code)
dc.bin[dc.bin !="0"] <- "1"

dc.bin <- as.factor(dc.bin)
X.df.orig <- X.df 
Y.df.orig <- Y.df

X.df$disturb_code <- dc.bin
Y.df$disturb_code <- dc.bin

# Export X and Y tables
# ------------------------------------------------------#

#create output directory
if(!file.exists(glue('{output_dir}/xytables'))){
  dir.create(glue('{output_dir}/xytables'))
}


write.csv(X.df, glue('{output_dir}/xytables/{cur.zone.zero}_Xdf_bin.csv'))
write.csv(Y.df, glue('{output_dir}/xytables/{cur.zone.zero}_Ydf_bin.csv'))

write.csv(X.df.orig, glue('{output_dir}/xytables/{cur.zone.zero}_Xdf_orig.csv'))
write.csv(Y.df.orig, glue('{output_dir}/xytables/{cur.zone.zero}_Ydf_orig.csv'))

# Create the imputation model
# -------------------------------------------------------#
tic() # start the clock
set.seed(56789)
yai.treelist.bin <- yai(X.df, Y.df, method = "randomForest", ntree = 400)

toc()

# clear unused memory
gc()

# create model output directory if it does not exists
if(!file.exists(glue('{output_dir}/model/'))){
  dir.create(glue('{output_dir}/model/'))
}

# Export model
write_rds(yai.treelist.bin, glue('{output_dir}/model/{cur.zone.zero}_yai_treelist_bin.RDS'))

# Report model accuracy for Y variables (EVC, EVH, EVG)
# ------------------------------------------------------------------------#

#RF summary
RF_sum <- yaiRFsummary(yai.treelist.bin)

# Confusion matrices
cm_EVC <- yai.treelist.bin$ranForest$canopy_cover$confusion
cm_EVH <- yai.treelist.bin$ranForest$canopy_height$confusion
cm_EVT_GP <- yai.treelist.bin$ranForest$EVT_GP$confusion
cm_DC <- yai.treelist.bin$ranForest$disturb_code$confusion

# variable importance
varImp <- data.frame(RF_sum$scaledImportance)

# process variable importance table for plotting
varImp$outVar <- rownames(varImp)
rownames(varImp) <- NULL

varImp %<>% 
  tidyr::pivot_longer(1:ncol(varImp)-1, names_to = "var")

#plot variable importance
p <- varImp %>%
  ggplot()+
  geom_col(aes(x=var, y = value))+
  coord_flip()+
  facet_wrap(~outVar)+
  theme_bw() + 
  title(glue('RF Variable Importance for {cur.zone.zero}'))

#create output directory
if(!file.exists(glue('{output_dir}/eval'))){
  dir.create(glue('{output_dir}/eval'))
}


# export to file
ggsave(glue('{output_dir}/eval/varImp.png'), width = 7, height = 5)
write.csv(cm_EVC, glue('{output_dir}/eval/CM_canopyCover.csv'))
write.csv(cm_EVH, glue('{output_dir}/eval/CM_canopyHeight.csv'))
write.csv(cm_EVT_GP, glue('{output_dir}/eval/CM_EVT_Group.csv'))
write.csv(cm_DC, glue('{output_dir}/eval/CM_DisturbanceCode.csv'))

# Build dataframes from the raster data
#-------------------------------------------------------#
#raster.coords <- coordinates(raster.stack)
# asp.raster <- raster.stack[[1]]
# dem.raster <- raster.stack[[2]]

#currow.vals <- cellFromRow(dem.raster, 1500)
#coords.currow <- raster.coords[currow.vals,]

#extract.currow <- extract(raster.stack, coords.currow)
p4s.latlong <- CRS("+proj=longlat +datum=NAD83") 

maxrow <- max(as.numeric(rownames(X.df)))

nrows.out <- dim(raster.stack)[1]
ncols.out <- dim(raster.stack)[2]

rs2 <- raster.stack
 
# ###Fifth input file: fixed ppt raster
# ###Get ppt img
# 
# setwd("G:/Workspace/treemap/Spatial_data/biophysical_gradients/d.gradients/national")
# setwd(cur.zone.zero)
# 
# ppt.img <- paste(cur.zone.zero, "ppt.img", sep = "")
# ppt.good.raster <- raster(ppt.img)

#ppt.good.proj <- projectRaster(ppt.good.raster, dem.raster)

Sys.time()

# Set up imputation function
# ------------------------------------------------ #

impute.row <- function(currow)
{
  #### Load libraries for function
  #library(yaImpute) 
  #library(raster) 
  #library(rgdal)
  #raster.coords  <- coordinates(dem.raster)
  #currow.vals <- cellFromRow(dem.raster, currow)
  #coords.currow <- raster.coords[currow.vals,]  
  
  #### Demo row - for testing
  #currow = test_row
  
  # Extract data from input rasters
  # ---------------------------------------------------#
  
  #### Get values from input rasters
  rsvals <- cellFromRow(rs2, currow)
  rsmat <- rs2[rsvals]
  #pptvals  <- cellFromRow(ppt.good.raster, currow)
  #ppt.good <- ppt.good.raster[pptvals]
  
  xycoords <- raster::xyFromCell(rs2, rsvals)
  xycoords <- data.frame(xycoords)
  
  # get data from each row of rasters (coordinates)
  #sp.currow <- SpatialPoints(xycoords , CRS(p4s.albers)) 
  #extract.currow <- extract(rs2,   sp.currow)
  #extract.ppt <- extract(ppt.good.raster, sp.currow)  
  extract.currow <- data.frame(rsmat)
  
  #### make data frame with all values for current row - excluding NAs 
  #extract.currow$"PPTI" <- extract.ppt
  
  colseq <- 1:length(extract.currow[,1]) # list all columns in row
  valid.cols <- colseq[as.logical(1-is.na(extract.currow[,1]))] # list all valid cols in row
  
  # tempname <- paste(cur.zone,"slp_1_2", sep =   "")
  #colseq <- 1:length(currow.vals)
  #valid.cols <- colseq[as.logical(1-is.na(extract.currow[,1]))]
  
  ncols.df <- dim(extract.currow)[2]
  
  # invalid.cols <- colseq[as.logical(is.na(extract.currow[,1]))]
  #extract.currow[invalid.cols,] <- rep(1, ncols.df)
  
  extract.currow <- data.frame(extract.currow)
  extract.currow$"POINT_X" <- xycoords $x
  extract.currow$"POINT_Y" <-xycoords $y
  extract.currow <- na.exclude(extract.currow)
  
  #extract.currow <- extract.currow[ppt.notequal,]
  #valid.cols <- valid.cols[ppt.notequal]  
  
  #### Turn extract into x data frame
  #------------------------------------------------------#
  X.df.temp <- data.frame(extract.currow)
  nrow.temp <- dim(X.df.temp)[1]
  
  #### Convert values from aspect raster into northing and easting
  aspect.temp <- X.df.temp$ASPECT  
  rad.temp <- (pi/180)*aspect.temp  
  northing.temp <- cos(rad.temp)  
  easting.temp <- sin(rad.temp)
  X.df.temp <- X.df.temp[,-1]  
  X.df.temp$NORTHING <- northing.temp  
  X.df.temp$EASTING <- 	easting.temp  
  
  
  #### Account for EVGs that are in the reference data but don't appear in the target data
  #get nonappearing evgs   
  temp.evg <- X.df.temp$'EVT_GP'
  evg.orig <- 1:n.evgs 
  #evg.orig <- as.numeric(levels(evg.in))
  evg.val <- evg.orig  
  evg.val.temp <- X.df.temp$'EVT_GP'  
  n.evgs.orig <- length(sort(unique(evg.orig)))  
  evg.orig.seq <- 1:n.evgs.orig  
  
  nonappearing.evgs <- evg.val[-sort(unique(as.numeric(as.character(evg.val.temp))))]  
  n.dummy.rows <- length(nonappearing.evgs)  
  X.df.temp.old <- X.df.temp
  
  #### create "dummy rows" in X table that represent any non-appearing EVGs
  if(n.dummy.rows > 0)    
  {    
    dummy.rows <- X.df.temp[1:n.dummy.rows,]    
    tempchar <- as.character(X.df.temp$'EVT_GP')    
    X.df.temp$'EVT_GP' <- tempchar    
    dummy.rows$'EVT_GP' <- as.character(nonappearing.evgs)    
    X.df.temp <- rbind(X.df.temp, dummy.rows)    
  }
  
  #### Set factor levels for EVG
  n.rows.orig <- dim(extract.currow)[1]	  
  temp.fac <- factor(X.df.temp$'EVT_GP', levels = levels(evg.in))  # levels taken from outside function
  
  #### Set factor levels for disturbance code
  dc.code.fac.temp <- factor( X.df.temp$disturb_code, levels=lev.dc)  
  #dc.year.fac.temp <- factor( X.df.temp$disturb_year, levels=lev.year)  
  
  X.df.temp$'EVT_GP' <- as.factor(temp.fac)  
  X.df.temp$disturb_code <- dc.code.fac.temp   
  
  #X.df.temp$disturb_year <- dc.year.fac   
  nrow.temp <- dim(X.df.temp)[1]  
  impute.out <- rep(-1, nrow.temp)  
  #X.df.temp <- X.df.temp[,-c(12, 13)]  
  
  #nc.orig <- dim(coords.currow)[1]  
  nc.orig <-length(rsvals)
  
  impute.out <- rep(NA,nc.orig)  
  nrows.orig <- dim(extract.currow)[1]  
  if(nrow.temp > 0)  # if there are any pixels in the row we're imputing  
  { 
    # Set up data frame to hold outputs
    colseq.out <- 1:dim(X.df.temp)[1] # number of inputs, including dummy rows  
    rownames.all <- colseq.out+maxrow    
    rownames(X.df.temp) <- paste("T-", rownames.all) # setting this up to match with imputation outputs?
    
    #rownames(X.df.temp) <- rownames.all[valid.cols]    
    #names(X.df.temp) <- names(X.df)    
    #temp.yai <- impute(yai.treelist, ancilliaryData = X.df.valid)
    
    # Adjust column names and factor levels on X.df.temp
    X.df.temp$Total_Cover <- X.df.temp$canopy_cover
    X.df.temp$Dominant_Nom_Ht <- X.df.temp$canopy_height
    temp.dc <- as.character(X.df.temp$disturb_code)
    temp.dc[temp.dc!= "0"] <- 1 # recode disturbance to binary
    temp.dc <- as.factor(temp.dc)
    X.df.temp$disturb_code <- temp.dc
    
    
    #### Perform imputation
    # take object from formed random forests model and use X.df.temp dataframe to make predictions    
    temp.newtargs <- newtargets(yai.treelist.bin, newdata = X.df.temp)    
  
    #### Format outputs 
    out.trgrows <- temp.newtargs$trgRows # row names for target observations
    temp.xall <- temp.newtargs$xall # x-variables (predictors) for all observations  
    out.neiIds <- temp.newtargs$neiIdsTrgs # a matrix of reference identifications that correspond to neiDstTrgs (distances between target and ref).
        
    yrows <- as.numeric(out.neiIds[,1])    
    id.out <- id.table[yrows]    
    impute.out[valid.cols] <- yrows[1:nrows.orig]    
  }
  outval <- currow / nrows.out
  #setwd("G:\\Workspace\\treemap\\treelist_2016\\Output")
  
  fout <- "zoneprog.txt"
  write.table(outval, 
              paste0(output_dir, fout))
  
  gc()
  return(impute.out)  
}


# Load imputation model and inputs
# ---------------------------------------------------------- #

#if necessary

yai.treelist.bin <- read_rds(file = glue('{output_dir}model/{cur.zone.zero}_yai_treelist_bin.RDS'))

# load x table and y table

X.df <- read.csv(glue('{output_dir}/xytables/{cur.zone.zero}_Xdf_bin.csv'))
Y.df <- read.csv(glue('{output_dir}/xytables/{cur.zone.zero}_Ydf_bin.csv'))


# Set up test 
# ---------------------------------------------------------- #

test_row <- 7500

ntest_rows <- 50

testrow1 <- test_row
testrow2 <- test_row + ntest_rows

# Apply imputation function - test without parallelizing
# ---------------------------------------------------------- #

# test_rows<- foreach(m = testrow1:testrow2, .packages = c("raster", "yaImpute", "glue", "tictoc"), .combine="rbind") %dopar%   impute.row(m)
# 
# # make rows with NAs to make full raster of test
# d <- rep(NA, ncols.out)
# blank_rows_top <- do.call("rbind", replicate(test_row-1, d, simplify = FALSE))
# blank_rows_bottom <- do.call("rbind", replicate(maxrow-(testrow2 + 1), d, simplify = FALSE))
# 
# #bind test rows with NAs to make full raster
# test_rows_ras <- terra::rast(rbind(blank_rows_top,
#                                    test_rows,
#                                    blank_rows_bottom))
# 
# # set geospatial attributes
# ext(test_rows_ras) <- ext(raster.stack)
# crs(test_rows_ras) <- crs(raster.stack)
# 
# # inspect
# test_rows_ras
# plot(test_rows_ras)
# freq(test_rows_ras)
# 
# # export test raster
# writeRaster(test_rows_ras,
#             glue('{output_dir}testRows_{testrow1}_{testrow2}_noPar.tif'))
# 
# # clear unused memory
# rm(test_rows_ras, blank_rows_bottom, blank_rows_)
# gc()


# Run imputation - with parallelizing
# ----------------------------------------------------#

# set proportion of available cores to use
nCorefraction <- 0.25

# Set number of cores
ncores <- parallel::detectCores()
ncores <- floor(ncores*nCorefraction)

# Set up cluster for parallel computing
cl <- makeCluster(ncores)
registerDoParallel(cl)

# RUN IMPUTATION ON ROWS
tic()
mout <- foreach(m = testrow1:testrow2, .packages = c("raster", "yaImpute"), .combine="rbind") %dopar% impute.row(m)
toc()

# finish parallel
stopCluster(cl)
closeAllConnections()
gc()

# Post-process test outputs
#------------------------------------#

# make rows with NAs to make full raster of test 
d <- rep(NA, ncols.out)
blank_rows_top <- do.call("rbind", replicate(testrow1-1, d, simplify = FALSE))
blank_rows_bottom <- do.call("rbind", replicate(nrows.out-(testrow2), d, simplify = FALSE))

# make sure this is the same size
nrow(mout) + nrow(blank_rows_top) + nrow(blank_rows_bottom)
nrows.out

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
            glue('{output_dir}raster/testRows_{testrow1}_{testrow2}.tif'),
            overwrite = TRUE)

# clear unused memory
rm(mout, mout_ras)
gc()

# Run imputation - original
# ---------------------------------------------------#
# mused <- pryr::mem_used()
# mused <- as.numeric(mused)
# mused.gb <- mused / 1e9


#mused.gb <- 3.17
#avail.cores <- floor(110 / mused.gb)
#ncores <- avail.cores - 1

#ncores <- min(60, ncores )

ncores <- parallel::detectCores()
ncores <- floor(ncores*.1)

# Sys.time()
# cl <- makeCluster(ncores)
# registerDoParallel(cl)
# Sys.time()
#mout.2014.rasters.2016 <- foreach(m = 1:nrows.out, .packages = c("raster", "yaImpute"), .combine="rbind") %dopar%   impute.row(m)



# # finish 
# stopCluster(cl)
# closeAllConnections()
# Sys.time()

#mout <- mout.2014.rasters.2016 

#m.raster <-dem.raster

m.raster.out <- raster(mout)
m.raster.out@extent <-dem.raster@extent
m.raster.out@crs <-dem.raster@crs


#setwd("G:\\Workspace\\treemap\\treelist_2016\\Output")

fout <- paste(cur.zone, "_2014-xtable-2016-raster-fixpptsc.tif", sep="")
writeRaster(m.raster.out, fout, overwrite=TRUE)

# 
# subj.str <- paste("Zone ", cur.zone, "Complete!")
# subj.str <- paste(subj.str, as.character(Sys.time()), sep = "\n")
# text.str <- subj.str
# 
# email.complete <- gm_mime() %>%
#   gm_to("igrenfell@gmail.com") %>%
#   gm_from("igrenfell@gmail.com") %>%
#   gm_subject(subj.str) %>%
#   gm_text_body(text.str)
# 
# gm_send_message(email.complete )
# 




