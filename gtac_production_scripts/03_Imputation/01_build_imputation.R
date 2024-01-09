# TreeMap Imputation
# Original script written by Isaac Grenfell, RMRS (igrenfell@gmail.com) 
#   and Karin Riley (karin.riley@usda.gov)
# Updated script written by Lila Leatherman (Lila.Leatherman@usda.gov)

# PART 1: 
# - BUILD x and y tables
# - save x and y tables
# - Build model for a zone
# - Save model evaluation 

# TO DO: 
# - export predicted + ref for left-out plots in RF 
# - read in rasters as vrt

# Last updated: 12/13/2023

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

# Plot coordinates- shapefile
#points_path <- "//166.2.126.25/TreeMap/01_Data/04_FIA/03_FullShp/FIA_US.shp"

# supply path to AOI for testing, or NA
#aoi_path <- "//166.2.126.25/TreeMap/01_Data/03_AOIs/UT_Uintas_rect_NAD1983.shp"
#aoi_name <- "UT_Uintas_rect"
aoi_path <- NA

# Paths for exporting data
#--------------------------------------#

# set path to save output rasters
# this directory will be created if it does not already exist
output_dir <- glue('{home_dir}03_Outputs/07_Raw_model_outputs/2016_Original_Test/')

# Output imputation name
output_name <- "2016_Orig_TestLL"

# set tmp directory
tmp_dir <- "D:/tmp/"

###########################################################################
# Set up libraries and directories
###########################################################################

# Packages and functions
#---------------------------------#

# packages required
list.of.packages <- c("raster", "yaImpute", "randomForest", 
                      "terra", "tidyverse", "magrittr", "glue", "tictoc")

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

# set zone_num
zone_num <- zone_list[1]

# Set zone name options
cur.zone <- glue('z{zone_num}')
cur.zone.zero <- if(zone_num < 10) {
  glue('z0{zone_num}') } else {
    cur.zone
  }

# Set folder paths
target_dir = glue('{target_dir}/{cur.zone.zero}')
output_dir = glue('{output_dir}/{cur.zone.zero}')
evt_gp_remap_table_path = glue('{evt_gp_remap_table_path}/{cur.zone.zero}/EVG_remap_table.csv')

# create output folder if it does not exist
if(!file.exists(output_dir)){
  dir.create(output_dir)
}

# Load target rasters
# --------------------------------------------------------------------#

# list raster files
flist.tif <- list.files(path = target_dir, pattern = "*.tif$", recursive = TRUE, full.names = TRUE)

# load raster files as raster stack
raster.stack <- stack(flist.tif)
p4s.albers <- proj4string(raster.stack)

# get raster names 
raster_names <- flist.tif %>%
  str_extract(., "z[0-9][0-9]/([^.])*") %>%
  str_replace("z[0-9][0-9]/", "")

#add names to raster list

names(raster.stack) <- raster_names

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
  cur.evg <- evg.reclass[i, 1]  
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
  dplyr::mutate(radians = (pi/180)*ASPECT,
         NORTHING = cos(radians),
         EASTING = sin(radians)) %>%
  dplyr::select(-radians)

# Calculate binary disturbance code
#----------------------------------------------------------#
plot.df %<>%
  mutate(disturb_code_bin = ifelse(disturb_code != 0, 1, disturb_code),
         disturb_code_bin = factor(disturb_code_bin))

# Create X table - orig (aka training table)
# ---------------------------------------------------------#

#Create X Table
X.df.orig <- plot.df %>% dplyr::select(SLOPE, ELEV, PARI, PPTI, RELHUMI, TMAXI, TMINI, VPDI,
              disturb_code, disturb_year, canopy_cover, canopy_height, EVT_GP, NORTHING, EASTING)

rownames(X.df.orig) <- plot.df$ID


# Create Y table
Y.df.orig <- plot.df %>%
  dplyr::select(canopy_cover, canopy_height, EVT_GP)

rownames(Y.df.orig) <- plot.df$ID

# Add binary disturbance to X and Y dfs
# ---------------------------------------------------------#

dc.bin <- as.character(X.df.orig$disturb_code)
dc.bin[dc.bin !="0"] <- "1"

dc.bin <- as.factor(dc.bin)

X.df <- X.df.orig
Y.df <- Y.df.orig

X.df %<>% mutate(disturb_code = dc.bin)
Y.df$disturb_code <- dc.bin

# Export X and Y tables
# ------------------------------------------------------#

#create output directory
if(!file.exists(glue('{output_dir}/xytables'))){
  dir.create(glue('{output_dir}/xytables'))
}

write.csv(X.df, glue('{output_dir}/xytables/{cur.zone.zero}_{output_name}_Xdf_bin.csv'))
write.csv(Y.df, glue('{output_dir}/xytables/{cur.zone.zero}_{output_name}_Ydf_bin.csv'))

write.csv(X.df.orig, glue('{output_dir}/xytables/{cur.zone.zero}_{output_name}_Xdf_orig.csv'))
write.csv(Y.df.orig, glue('{output_dir}/xytables/{cur.zone.zero}_{output_name}_Ydf_orig.csv'))


# Build the random forests model (X=all predictors, Y=EVG, EVC, EVH)
# -----------------------------------------------------------------------#
set.seed(56789)

#yai.treelist <- yai(X.df.orig, Y.df.orig, method = "randomForest", ntree = 249)
yai.treelist.bin <- yai(X.df, Y.df, method = "randomForest", ntree = 400)

# Export model
write_rds(yai.treelist.bin, glue('{output_dir}/model/{cur.zone.zero}_{output_name}_yai_treelist_bin.RDS'))


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
  ggtitle(glue('RF Variable Importance for {cur.zone.zero}'))

#create output directory
if(!file.exists(glue('{output_dir}/eval'))){
  dir.create(glue('{output_dir}/eval'))
}


# export to file
ggsave(glue('{output_dir}/eval/{output_name}_varImp.png'), width = 7, height = 5)
write.csv(cm_EVC, glue('{output_dir}/eval/{output_name}_CM_canopyCover.csv'))
write.csv(cm_EVH, glue('{output_dir}/eval/{output_name}_CM_canopyHeight.csv'))
write.csv(cm_EVT_GP, glue('{output_dir}/eval/{output_name}_CM_EVT_Group.csv'))
write.csv(cm_DC, glue('{output_dir}/eval/{output_name}_CM_DisturbanceCode.csv'))

# clear unused memory
gc()

