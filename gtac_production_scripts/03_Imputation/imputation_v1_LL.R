# TreeMap Imputation
# Original script written by Isaac Grenfell, RMRS (igrenfell@gmail.com) 
#   and Karin Riley (karin.riley@usda.gov)
# Updated script written by Lila Leatherman (Lila.Leatherman@usda.gov)

# TO DO: 
# - export predicted + ref for left-out plots in RF 
# - read in rasters as vrt

# Last updated: 12/5/2023

###########################################################################
# Set inputs
###########################################################################

# Zone list
zone_list <- c(16)

# Path to X table
xtable_path <- "//166.2.126.25/TreeMap/01_Data/01_TreeMap2016_RDA/01_Input/03_XTables/X_table_all_singlecondition.txt"

# Directory where target rasters live
target_dir <- "//166.2.126.25/TreeMap/01_Data/01_TreeMap2016_RDA/02_Target/"

# Directory where EVT_GP remap table is located
evt_gp_remap_table_path <- "//166.2.126.25/TreeMap/03_Outputs/05_Target_Rasters/02_Vegetation/z16/EVG_remap_table.csv"

# Plot coordinates- shapefile
points_path <- "//166.2.126.25/TreeMap/01_Data/04_FIA/03_FullShp/FIA_US.shp"

# supply path to AOI for testing, or NA
#aoi_path <- "//166.2.126.25/TreeMap/01_Data/03_AOIs/UT_Uintas_rect_NAD1983.shp"
#aoi_name <- "UT_Uintas_rect"
aoi_path <- NA

# Paths for exporting data
#--------------------------------------#

# set path to save output rasters
# this directory will be created if it does not already exist
output_dir <- "//166.2.126.25/TreeMap/03_Outputs/07_Raw_model_outputs/2016_Original_Test/"

# Output imputation name
output_name <- "2016_Orig_Test"

# set tmp directory
tmp_dir <- "D:/tmp/"

# Parallelization settings
#--------------------------------------#

# set percentage of available cores that should be used
cores_frac <- 0.75

###########################################################################
# Set up libraries and directories
###########################################################################

# Packages and functions
#---------------------------------#

# packages required
list.of.packages <- c("raster", "yaImpute", "randomForest", "pryr", 
                      "terra", "tidyverse", "magrittr", "glue", "tictoc",
                      "parallel", "doParallel", "foreach")

#check for packages and install if needed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages)

# load all packages
vapply(list.of.packages, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)

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
#remove unused memory
gc()

# Set up other directories
# ----------------------------------#

if (!file.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Terra options
# --------------------------------#

#increase memory fraction available
terraOptions(memfrac = 0.8)

# Other options
# --------------------------------#

# Allow for sufficient digits to differentiate plot cn numbers
options("scipen"=100, "digits"=8)


####################################################################
# Load data
####################################################################

# Set zone name options
cur.zone <- glue('z{zone_num}')
cur.zone.zero <- if(zone_num < 10) {
  glue('z0{zone_num}') } else {
    cur.zone
  }

# Set folder paths
target_dir = glue('target_dir{cur.zone.zero}')
output_dir = glue('output_dir{cur.zone.zero')

# create output folder if it does not exist
if(!file.exists(output_dir)){
  dir.create(output_dir)
}

# Load target rasters
# --------------------------------------------------------------------#

# list raster files
flist.tif <- list.files(path = target_dir, pattern = "*.tif")

# load raster files as raster stack
raster.stack <- stack(flist.tif)
p4s.albers <- proj4string(raster.stack)
raster.list <- vector("list", length(flist.tif))
nrasters <- length(flist.tif)
for(i in 1:length(flist.tif))  
{
  raster.list[[i]] <- raster()  
}

#add names to raster list
names(raster.list) <- gsub(".tif", "",flist.tif)
names(raster.stack) <- gsub(".tif", "", flist.tif)

# Load X table
# ----------------------------------------------------------#

allplot <- read.csv(xtable_path)

# Load EVT Group Remap table
# ----------------------------------------------------------#

evt_gp_remap_table <- read.csv(evt_gp_remap_table_path)

####################################################################
# Prepare input data
####################################################################

# Convert plot coordinates to meters
#----------------------------------------------------------#
# NOTE: this is currently a stand-in. the allplot table has abbreviated records of the lat and long 
# (to 2 decimal places)

# convert allplot to spatial object
allplot_vect <- terra::vect(cbind(allplot$ACTUAL_LON, allplot$ACTUAL_LAT))

# set input projection
crs(allplot_vect) <- "epsg:4326"

# reproject to desired projection
allplot_vect %<>% terra::project(crs(raster.stack))

# extract lat/long in meters
allplot_xy <- terra::geom(allplot_vect) %>%
  data.frame() %>%
  dplyr::rename("POINT_X" = x,
                "POINT_Y" = y) %>%
  dplyr::select(POINT_X, POINT_Y)

# bind back with allplot table
allplot <- cbind(allplot, allplot_xy)


# Remap EVT Group
# ---------------------------------#

#Limit allplot to just the veg types in the remap table
plot.df <- allplot[allplot$EVT_GP %in% evt_gp_remap_table$EVT_GP,]

####Reclass evgs
n.evgs <- nrow(evt_gp_remap_table)

# get max value of EVG raster
#n.evgs <- minmax(raster_rast$EVT_GP)[2]

#reassign object to evg.reclass
evg.reclass <- evt_gp_remap_table %>%
  dplyr::select(EVT_GP, EVT_GP_remap)

#remap evgs using vector
evg.out <- rep(0, dim(plot.df)[1])
evg.vec <- plot.df$"EVT_GP"
for(i in 1:n.evgs)  
{  
  cur.evg <- evg.reclass[i, 1]  # this will need to be changed
  sub.ind <- evg.vec == cur.evg  
  evg.out[sub.ind] <- i  
}	

# create evg.in - used in imputation
evg.in <- as.factor(evg.out)

# re-assign EVT_GP
plot.df$EVT_GP <- evg.out

#Ensure plot.df variables are factors
#--------------------------------------#

plot.df %<>%
  mutate(CN = factor(CN),
         EVT_GP = factor(EVT_GP),
         disturb_code = factor(disturb_code))

# Re-calculate aspect - to northing and easting
#----------------------------------------------------------#
plot.df %<>%
  mutate(radians = (pi/180)*ASPECT,
         NORTHING = cos(radians),
         EASTING = sin(radians)) %>%
  select(-radians)

# Calculate binary disturbance code
#----------------------------------------------------------#
plot.df %<>%
  mutate(disturb_code_bin = ifelse(disturb_code != 0, 1, disturb_code),
         disturb_code_bin = factor(disturb_code_bin))

# Create X table - orig (aka training table)
# ---------------------------------------------------------#

#Create X Table
X.df_orig <- plot.df %>% dplyr::select(SLOPE, ASPECT, ELEV, PARI, PPTI, RELHUMI, TMAXI, TMINI, VPDI,
              disturb_code, disturb_year, canopy_cover, canopy_height, EVT_GP)

rownames(X.df_orig) <- plot.df$ID


# Create Y table
Y.df_orig <- plot.df %>%
  dplyr::select(canopy_cover, canopy_height, EVT_GP)

rownames(Y.df_orig) <- plot.df$ID

#create id table
id.table <-  plot.df$ID

# Add binary disturbance to X and Y dfs
# ---------------------------------------------------------#

X.df <- X.df_orig
Y.df <- y.df_orig

X.df$disturb_code = plot.df$disturb_code_bin
Y.df$disturb_code = plot.df$disturb_code_bin


# Build the random forests model (X=all predictors, Y=EVG, EVC, EVH)
# -----------------------------------------------------------------------#
set.seed(56789)

#yai.treelist <- yai(X.df.orig, Y.df.orig, method = "randomForest", ntree = 249)
yai.treelist.bin <- yai(X.df, Y.df, method = "randomForest", ntree = 400)

# Export model
write_rds(yai.treelist.bin, glue('{output_dir}/eval/{cur.zone.zero}yai_treelist_bin.RDS'))


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
  theme_bw()

# export to file
ggsave(glue('{output_dir}/eval/varImp.png'))
write.csv(cm_EVC, glue('{output_dir}/eval/CM_canopyCover.csv'))
write.csv(cm_EVH, glue('{output_dir}/eval/CM_canopyHeight.csv'))
write.csv(cm_EVT_GP, glue('{output_dir}/eval/CM_EVT_Group.csv'))
write.csv(cm_DC, glue('{output_dir}/eval/CM_DisturbanceCode.csv'))


# Build dataframes from the raster data
#-------------------------------------------------------------------------#
#raster.coords <- coordinates(raster.stack)
asp.raster <- raster.stack[[1]]
dem.raster <- raster.stack[[2]]

currow.vals <- cellFromRow(dem.raster, 1500)
#coords.currow <- raster.coords[currow.vals,]

#extract.currow <- extract(raster.stack, coords.currow)
p4s.latlong <- CRS("+proj=longlat +datum=NAD83") 

maxrow <- max(as.numeric(rownames(X.df)))

nrows.out <- dim(raster.stack)[1]
ncols.out <- dim(raster.stack)[2]

rs2 <- raster.stack

##############################################################################
# Build imputation function
##############################################################################

impute.row <- function(currow)  
{ 
  # External objects brought into this function" 
  # rs2
  # nrows.out
  # ncols.out
  # ## EVG processing
  # n.evgs
  # evg.in
  # lev.dc
  
  
  #### Load libraries for function
  #library(yaImpute) 
  #library(raster) 
  
  #library(rgdal)
  #raster.coords  <- coordinates(dem.raster)
  #currow.vals <- cellFromRow(dem.raster, currow)
  #coords.currow <- raster.coords[currow.vals,]  
  
  #### Get values from current row of raster
  rsvals <- cellFromRow(rs2, currow)
  rsmat <- rs2[rsvals]
  extract.currow <- data.frame(rsmat)
  
  #### Get coordinates from current row of raster
  xycoords <- xyFromCell(rs2, rsvals)
  xycoords <- data.frame(xycoords)
  
  #### Get dimensions of current row
  colseq <- 1:length(extract.currow[,1])
  valid.cols <- colseq[as.logical(1-is.na(extract.currow[,1]))]
  
  ncols.df <- dim(extract.currow)[2]
  
  #### Process current row
  extract.currow$"POINT_X" <- xycoords $x
  extract.currow$"POINT_Y" <-xycoords $y
  extract.currow <- na.exclude(extract.currow)

  X.df.temp <- data.frame(extract.currow)
  nrow.temp <- dim(X.df.temp)[1]
  
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
  
  #get nonappearing evgs   
  evg.orig <- 1:n.evgs 
  #evg.orig <- as.numeric(levels(evg.in))
  evg.val <- evg.orig  
  evg.val.temp <- X.df.temp$'EVT_GP'  
  n.evgs.orig <- length(sort(unique(evg.orig)))  
  evg.orig.seq <- 1:n.evgs.orig  
  
  nonappearing.evgs <- evg.val[-sort(unique(as.numeric(as.character(evg.val.temp))))]  
  n.dummy.rows <- length(nonappearing.evgs)  
  X.df.temp.old <- X.df.temp
  
  if(n.dummy.rows > 0)    
  {    
    dummy.rows <- X.df.temp[1:n.dummy.rows,]    
    tempchar <- as.character(X.df.temp$'EVT_GP')    
    X.df.temp$'EVT_GP' <- tempchar    
    dummy.rows$'EVT_GP' <- as.character(nonappearing.evgs)    
    X.df.temp <- rbind(X.df.temp, dummy.rows)    
  }
  
  n.rows.orig <- dim(extract.currow)[1]	  
  temp.fac <- factor(X.df.temp$'EVT_GP', levels = levels(evg.in))  
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
  
  if(nrow.temp > 0)    
  {    
    colseq.out <- 1:dim(X.df.temp)[1]    
    rownames.all <- colseq.out+maxrow    
    rownames(X.df.temp) <- paste("T-", rownames.all)    
    #rownames(X.df.temp) <- rownames.all[valid.cols]    
    #names(X.df.temp) <- names(X.df)    
    #temp.yai <- impute(yai.treelist, ancilliaryData = X.df.valid)
    
    # take object from formed random forests model and use X.df.temp dataframe to make predictions    
    X.df.temp$Total_Cover <- X.df.temp$canopy_cover
    X.df.temp$Dominant_Nom_Ht <- X.df.temp$canopy_height
    temp.dc <- as.character(X.df.temp$disturb_code)
    temp.dc[temp.dc!= "0"] <- 1
    temp.dc <- as.factor(temp.dc)
    X.df.temp$disturb_code <- temp.dc
    
    # run imputation
    temp.newtargs <- newtargets(yai.treelist.bin, newdata = X.df.temp)    
    
    # process output
    temp.xall <- temp.newtargs$xall    
    out.neiIds <- temp.newtargs$neiIdsTrgs    
    out.trgrows <- temp.newtargs$trgRows    
    yrows <- as.numeric(out.neiIds[,1])    
    id.out <- id.table[yrows]    
    impute.out[valid.cols] <- yrows[1:nrows.orig]    
  }
  outval <- currow / nrows.out
  #setwd("G:\\Workspace\\treemap\\treelist_2016\\Output")
  
  fout <- "zoneprog.txt"
  write.table(outval, 
              paste0(output_dir, fout))
  
  return(impute.out)  
}

######################################################################
# Apply imputation function
######################################################################

Sys.time()


mused <- mem_used()
mused <- as.numeric(mused)
mused.gb <- mused / 1e9


#mused.gb <- 3.17
avail.cores <- floor(110 / mused.gb)
ncores <- avail.cores - 1

ncores <- min(60, ncores )

Sys.time()
cl <- makeCluster(ncores)
registerDoParallel(cl)
Sys.time()

# Apply imputation function for each row
mout <- foreach(m = 1:nrows.out, .packages = c("raster", "yaImpute"), 
                .combine="rbind") %dopar%   impute.row(m)


# finish 
stopCluster(cl)
closeAllConnections()
Sys.time()

# post-processing 

m.raster <-dem.raster

m.raster.out <- raster(mout)
m.raster.out@extent <-dem.raster@extent
m.raster.out@crs <-dem.raster@crs

# export
filename_out <- glue('{output_dir}{output_name}.tif' )
writeRaster(m.raster.out, filename_out, overwrite=TRUE)

# 
# subj.str <- paste("Zone ", cur.zone, "Complete!")
# text.str <- paste(subj.str, as.character(Sys.time()), sep = "\n")


# email.complete <- gm_mime() %>%
#   gm_to("lila.leatherman@gmail.com") %>%
#   gm_from("lila.leatherman@gmail.com") %>%
#   gm_subject(subj.str) %>%
#   gm_text_body(text.str)
# 
# gm_send_message(email.complete )
# 
