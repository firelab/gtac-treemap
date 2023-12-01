# Evaluate GTAC Rasters vs Karin Riley rasters

# Idea: that this script can be re-purposed in multiple contexts 
# E.g., to compare: 
# - GTAC 2016 Disturbance vs Karin Riley 2016 Disturbance
# - GTAC LCMS 2016 Disturbance vs Karin Riley 2016 Disturbance
# - GTAC Landfire Target Rasters vs. Karin Riley Target Rasters

# - MAKE THIS INTO A FUNCTION THAT'S INCORPORATED IN A LIBRARY

# Written by: Lila Leatherman (lila.leatherman@usda.gov)
# Redcastle Resources and USFS Geospatial Technology and Applications Center (GTAC)
# Last updated: 11/17/2023

###############################################
# USER INPUTS

# set home dir
home_dir <- "//166.2.126.25/TreeMap/"

# set path to a .prj file with desired projection
proj_path <- "//166.2.126.25/TreeMap/01_Data/02_Landfire/landfire_crs.prj"

# set tmp directory
tmp_dir <- "D:/tmp/"

# set path to "reference" raster
ref_raster <- '//166.2.126.25/TreeMap/01_Data/01_TreeMap2016_RDA/01_Input/01_Disturbance/Spatial_data/disturbance_year/disturbance_year_1999_2016_nodata_reclass.tif'
# set path to "predicted" raster
pred_raster <- "//166.2.126.25/TreeMap/03_Outputs/05_Target_Rasters/01_Disturbance/02_Final/Landfire_Disturbance/1999_2016_LFz16_UtahHighPlateaus_DisturbanceYear.tif"

# set path to points used for extraction, if desired
# else, NA - and uses all values of raster
#points <- "//166.2.126.25/TreeMap/01_Data/04_FIA/03_FullShp/FIA_US.shp"
points <- NA

# set aoi for cropping points, if shapefile
#aoi_path <- paste0(home_dir, "01_Data/03_AOIs/UT_Uintas_rect_NAD1983.shp")
#aoi_name <- "UT_Uintas_subset"

#set aoi for cropping points if landfire zone
aoi_path <- NA
zone_num <- 16

# set dir to save output evaluation
# this directory will be created if it does not exists
eval_out_dir <- "//166.2.126.25/TreeMap/03_Outputs/09_Evaluation/01_LCMS_Landfire_Disturbance_Layer/"

# set name for output evaluation
eval_out_name <- "1999_2016_z16_RileyLandfire_vs_GTACLandfire_values_DistYear"

# name format:
# {startyear}_{endyear}_{zonenum}_{refraster}_vs_{predraster}_{pts or px}_{attribute}"

#####################
# SETUP
######################

# Temp directories 
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

# check if output dir exists; create it if it does not
if (!file.exists(eval_out_dir)){
  dir.create(eval_out_dir)
  
}

# Packages and functions
#---------------------------------#

# packages required
list.of.packages <- c("terra", "tidyverse", "magrittr", "glue", "tictoc", "caret")

# #check for packages and install if needed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages)

# load all packages
vapply(list.of.packages, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)

# make 'notin' function
`%notin%` <- Negate('%in%')


# Terra options
# --------------------------------#

#set memory fraction available
#terraOptions(memfrac = 0.2)

###################################################
# LOAD DATA
###################################################

#load landfire projection
crs <- terra::crs(proj_path)

# Load AOI
# -----------------#

# load LF zone data
LF_zones <- vect(glue('{home_dir}01_Data/02_Landfire/LF_zones/Landfire_zones/refreshGeoAreas_041210.shp'))
LF_zones %<>% project(crs)

if (!is.na(aoi_path)) {
  
  print("using shapefile as aoi")
  # load aoi subset 
  aoi <- vect(aoi_path)
  
  # reassign
  zone <- aoi
  zone_name <- aoi_name
  
} else{ 
  print("using landfire zone as aoi")
  # select single LF zone
  zone <- subset(LF_zones, LF_zones$ZONE_NUM == zone_num) #z16 = Utah High Plateaus
  
  # get name of zone
  zone_name <- paste0("LFz", zone_num, "_", gsub(" ", "", zone$ZONE_NAME))
  
  }

# Load input rasters to evaluation
# -----------------------------------#

# load reference raster
ref <- terra::rast(ref_raster) 

# load predicted raster
pred <- terra::rast(pred_raster) 

# load points
if(!is.na(points)) {
  print("using points for evaluation!")
  pts <- terra::vect(points) %>%
    terra::project(crs) %>% # reproject
    terra::crop(zone) # reproject
  
  #ensure projection is the same
  if(identical(crs(ref), crs(pts))){}
  else{
    print("reprojecting points to crs of ref raster")
    pts %<>% terra::project(crs(ref))}
} else {
  print("using all px in raster for evaluation!")
}

# Input data prep 
# ---------------------------#

# ensure everything is the same projection
if(identical(crs(ref), crs(pred))){} else{
  print("making projection of pred raster align with ref raster")
  pred %<>% terra::project(crs(ref))
}

# ensure pred and ref are for the same aoi
zone %<>% terra::project(crs(ref))

ref %<>% terra::crop(zone, mask = TRUE)
pred %<>% terra::crop(zone, mask = TRUE)

#inspect
plot(ref)
plot(pred)

# make sure nodata values are the same
#pred %<>% terra::classify(cbind(-99,0))
pred %<>% terra::classify(cbind(-99, 99))


###################################################
# Evaluation - confusion matrix
##################################################

#conditionally extract to points. 
#otherwise, use all values of raster.

# this could be made into a function

if(!is.na(points)) {
  # extract values to points
  pt_extract <- cbind(
    terra::extract(pred, pts)[2],
    terra::extract(ref, pts)[2])
  
  # assign to new var
  table <- pt_extract

  } else{
  
  # get all px in raster as values
  pred_values <- terra::values(pred)
  ref_values <- terra::values(ref)
  
  values_table <- as.data.frame(cbind(pred_values, ref_values))
  names(values_table) <- c("pred", "ref")
  table <- values_table
  rm(values_table)
  
}


# SET INPUT TABLE
#################
t <- table

# Function for Evaluation 
# takes a two-column data frame as an input
# produces confusion matrix tables formatted the way I like them :)
#------------------------------------------------------------------------#

eval_cm_function <- function(t, noDataVal) {
  
  #require(c(tidyverse, caret))
  
  #apply column names
  names(t) <- c("pred", "ref")
  
  # set levels for factors
  # get maximum value of table that's not the noDataValue
  tn <- t
  tn[tn == noDataVal] <- NA
  levels_t <- seq(0, max(tn, na.rm = TRUE), 1)
  
  # ensure columns are factors with the same levels
  t %<>%
    mutate(ref = factor(ref, levels = levels_t),
           pred = factor(pred, levels = levels_t)) 
  
  
  # confusion matrix
  cm <- caret::confusionMatrix(t$pred, # pred
                               t$ref # ref
  )
  
  # process data frames for export
  #---------------------------------#
  
  # raw confusion matrix
  cm_raw_out <- as.table(cm)
  cm_raw_out <- addmargins(cm_raw_out)
  
  # make data frame of classes
  cm_t_classes <- data.frame(as.matrix(cm, what = "classes"))
  names(cm_t_classes) <- levels(t$pred)
  cm_t_classes %<>% 
    rownames_to_column(., var = 'metric')
  
  # overall eval stats
  cm_t_overall <- data.frame(as.matrix(cm, what = "overall"))
  names(cm_t_overall) <- c("value")
  
  # format output
  # ---------------------------- #
  out_list <- list(cm_raw_out,
                   cm_t_classes,
                   cm_t_overall)
  names(out_list) <- c("raw", "classes", "overall")
  
  return(out_list)
  
}

# Apply function 
##############################
gc()

results <- eval_cm_function(t, noDataVal = 99)

# inspect confusion matrix 
results$raw
results$classes
results$overall

if(!exists(eval_out_dir)) {
  dir.create(eval_out_dir, recursive = TRUE)
  }

# set export path
export_csv <- glue('{eval_out_dir}/01_csvs/')
export_path <- glue('{export_csv}{eval_out_name}')

# create output dir
if(!file.exists(export_csv)){
  dir.create(export_csv)
}

# export confusion matrix results
write.csv(results$raw, glue('{export_path}_raw.csv'))
write.csv(results$classes, glue('{export_path}_classes.csv'))
write.csv(results$overall, glue('{export_path}_overall.csv'))

#################################################
# Evaluation - subtract rasters of interest
#################################################

# subtract rasters
eval_r <- ref - pred

#inspect
freq <- freq(eval_r)
freq$count_norm <- freq$count/(sum(freq$count))

hist(freq$count/sum(freq$count))

#list unique values in eval raster that are not 0 ( 0 = same)
eval_reclass <- unique(eval_r)[,1]

# get mask for where rasters are different
eval_r_mask <- eval_r %>%
  terra::classify(cbind(0, NA))

# what are the coordinates of the pixels that are different? hard to visually tell
cells_diff <- terra::cells(eval_r_mask)
diff_coords <- xyFromCell(eval_r_mask, cells_diff)

#convert to shp
diff_coords_wgs <- diff_coords %>%
  terra::vect() 
crs(diff_coords_wgs) <- crs(eval_r_mask) # set rcs

#reproject
diff_coords_wgs <-  terra::project(diff_coords_wgs, "EPSG:4326")

#inspect ref raster where different
ref %>%
  terra::mask(eval_r_mask) %>%
  freq()

#inspect pred raster where different
pred %>%
  terra::mask(eval_r_mask) %>%
  freq()

# set export path
export_ras <- glue('{eval_out_dir}/02_rasters/')
export_path <- glue('{export_ras}{eval_out_name}')

# create output dir
if(!file.exists(export_ras)){
  dir.create(export_ras)
}

#export raster
writeRaster(eval_r,
            glue('{export_path}{eval_out_name}_diff.tif'),
            overwrite = TRUE)

# export .shp of different points
writeVector(diff_coords_wgs, 
            glue('{export_path}{eval_out_name}_diffPts.shp'),
            overwrite = TRUE)
