# Prep target rasters for TreeMap Imputation
# Outputs: 
# - Landfire Existing Vegetation Cover (EVC)
# - Forest mask derived from Landfire EVC
# - Landfire Existing Vegetation Type Group
# - Landfire Existing Vegetation Height (EVH)
# EVT_GRP remap table

# Written by Lila Leatherman (lila.leatherman@usda.gov)
# Last Updated: December 2023

# Based on "prep_target_rasters_v2.py" by Karin Riley

##########################################################
# Setup - data
##########################################################
library(glue)

# list landfire zones of interest
zone_list <- c(16)

# Paths to input data
#----------------------------#

# set home dir
home_dir <- "//166.2.126.25/TreeMap/"

# set paths to input landfire rasters 
landfire_path <- '//166.2.126.25/TreeMap/01_Data/02_Landfire/LF_200/'
evc_path <- glue('{landfire_path}EVC/LF2016_EVC_200_CONUS/Tif/LC16_EVC_200.tif')
evh_path <- glue('{landfire_path}EVH/LF2016_EVH_200_CONUS/Tif/LC16_EVH_200.tif')
evt_path <- glue('{landfire_path}EVT/LF2016_EVT_200_CONUS/Tif/LC16_EVT_200.tif')
bps_path <- glue('{landfire_path}BioPhys/LF2016_BPS_200_CONUS/Tif/LC16_BPS_200.tif')

topo_path <-'//166.2.126.25/TreeMap/01_Data/02_Landfire/LF_220/Topo/'
elev_path <- glue('{topo_path}LF2020_Elev_220_CONUS\Tif\LC20_Elev_220.tif')
slopeP_path <- glue('{topo_path}LF2020_SlpP_220_CONUS\Tif\LC20_SlpP_220.tif')
slopeD_path <- glue('{topo_path}LF2020_SlpD_220_CONUS\Tif\LC20_SlpD_220.tif')

# set paths to input disturbance rasters
distYear_path <- ""
distCode_path <- ""

# set path to landfire zones
landfire_zone_path <- glue('{home_dir}01_Data/02_Landfire/LF_zones/Landfire_zones/refreshGeoAreas_041210.shp')

# set projection used for processing landfire rasters
landfire_proj <- "//166.2.126.25/TreeMap/01_Data/02_Landfire/landfire_crs.prj"

# supply path to AOI for testing, or NA
#aoi_path <- "//166.2.126.25/TreeMap/01_Data/03_AOIs/UT_Uintas_rect_NAD1983.shp"
#aoi_name <- "UT_Uintas_rect"
aoi_path <- NA

# set tmp directory
tmp_dir <- "D:/tmp/"

# Paths for exporting data
#--------------------------------------#

# set path to save output rasters
# this directory will be created if it does not already exist
output_dir <- '//166.2.126.25/TreeMap/03_Outputs/05_Target_Rasters/'

##############################################
# SETUP - environments
#############################################

# Packages and functions
#---------------------------------#

# packages required
list.of.packages <- c("terra", "tidyverse", "magrittr", "glue", "tictoc")

#check for packages and install if needed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages)

# load all packages
vapply(list.of.packages, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)

# make 'notin' function
`%notin%` <- Negate('%in%')

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

###################################################
# Load Data
###################################################

# Load raster data
# ------------------------------#

# load LF zone data
LF_zones <- vect(landfire_zone_path)

# load layers of interest
evc <- terra::rast(evc_path)
evh <-terra::rast(evh_path)
evt <- terra::rast(evt_path)

topio

activeCat(evt) <- 7 # evt_gp

# Set up variables to reclassify 
# --------------------------------#

# EVC codes that represent forest
# represented as range start, range end, desired end value
evc_forest_codes_mat <- matrix(c(
  -9999,100,NA,
  110,119,15, 
  120,129,25, 
  130,139,35, 
  140,149,45, 
  150,159,55,
  160,169,65,
  170,179,75, 
  180,189,85, 
  190,199,95,
  200,399,NA), 
  nrow = 11, ncol = 3, byrow = TRUE)

# EVT_GPs that are reclassed to NA
# because there are only a few of these evt_gps
evt_gps_na <- c(
  13,
  14,
  15,
  26,
  60,
  730
)

# EVH classes to reclass
evh_class_mat <- matrix(c(
  101,105,3,
  106,110,8,
  111,125,18,
  126,150,38),
  nrow = 4, ncol = 3, byrow = TRUE)

# get evt levels - to reclassify evt to evt_gp
evt_levels <- levels(evt)[[1]] %>%
  mutate(EVT_GP = as.numeric(EVT_GP))


###################################################
# Iterate over zones
###################################################


#for (z in 1:length(zone_list)) {
  
  tic() # start the clock
  
  #for testing
  z <- 1
  
  zone_num <- zone_list[z]
  
  # status update
  print(glue("working on zone {zone_num}"))
  
  # Prep zone
  #-----------------------------------------#
  
  # select single LF zone
  zone <- subset(LF_zones, LF_zones$ZONE_NUM == zone_num) 
  
  # get name of zone
  zone_name <- glue('LFz{zone_num}_{gsub(" ", "", zone$ZONE_NAME)}')
  
  # inspect
  #LF_zones$ZONE_NAME
  
  if (!is.na(aoi_path)) {
    # load aoi subset - utah uintas only
    aoi <- vect(aoi_path) %>%
      project(crs(evc))
    
    # reassign
    zone <- aoi
    zone_name <- aoi_name
    print("using input shapefile as AOI")
  } else{
    print("using landfire zone as AOI")
  }
  
  #set zone identifiers
  cur.zone <- glue('z{zone_num}')
  cur.zone.zero <- if(zone_num < 10) {
    glue('z0{zone_num}') } else {
      cur.zone
    }
  
  # create output folders
  veg_dir_zone <-glue('{output_dir}02_Vegetation/{cur.zone.zero}')
  if(!file.exists(veg_dir_zone)) {
    dir.create(veg_dir_zone)
  } 
  
  # create output folders
  topobio_dir_zone <- glue('{output_dir}03_TopoBioPhys/{cur.zone.zero}')
  if(!file.exists(topobio_dir_zone)) {
    dir.create(topobio_dir_zone, recursive = TRUE)
  } 
  
  # Start geospatial operations
  # ---------------------------------------#
  
  
  # crop to zone
  evc_z <- terra::crop(evc, zone, mask = TRUE)
  evt_z <- terra::crop(evt, zone, mask = TRUE)
  
  
  
  # reclassify EVC: subset to only forested pixels
  evc_z <- terra::classify(evc_z, evc_forest_codes_mat,
                           right = NA)
  
  # Prepare EVT_GP layer - mask and remap
  # ---------------------------------------- #
  

  evt_gp <- terra::mask(evt_z, evc_z) %>%   # apply forest mask to EVT layer
      terra::classify(evt_levels) %>%     # reclass from EVT to EVT_GP
    # reclassify a few EVT_GPS -
      # there are only a few px for some of the EVT_GPs
      # and/or no plots that keyed to these EVGs
      terra::classify(cbind(evt_gps_na, NA)) 
  
<<<<<<< Updated upstream
=======
  # use EVG raster to mask EVC
  evc_z <- terra::mask(evc_z, evt_gp)
  
  # crop evh to zone, use EVG raster to mask EVH, then reclassify to 2014 conventions
  evh_z <- terra::crop(evh, zone, mask = TRUE) %>%
    terra::mask(evt_gp) %>%
    terra::classify(evh_class_mat, 
                    right = NA)
>>>>>>> Stashed changes
  
  # create reference files to connect EVT_GP with remapped group
  evt_gp_list <- unique(evt_gp)
  evg_remap_table <- data.frame(EVT_GP = evt_gp_list, 
                            EVT_GP_remap = seq(1:nrow(evt_gp_list)))
  
  # remap EVT_GP raster
  evt_gp_remap <- terra::classify(evt_gp, evg_remap_table) 
  
  # export remap table
  write.csv(evg_remap_table, 
            glue('{veg_dir_zone}/EVG_remap_table.csv'), row.names = FALSE)
  

<<<<<<< Updated upstream
  # Apply forest mask to remaining vegetation layers
  # ---------------------------------------- #
  
  #use EVT_GP raster to mask EVC
  evc_z <- terra::mask(evc_z, evt_gp)
  
  # use EVT_GP raster to mask EVH
  # then reclassify EVH raster to 2014 conventions
  evh_z <- terra::mask(evh_z, evt_gp) %>%
    terra::classify(evh_class_mat, 
                    right = NA)
  
  
  # Mask Biophys and topo layers - to forested px for each zone
  # -------------------------------------------------------------#
  
  # Export rasters
  # -------------------------------------------------------- #
  print(glue('exporting rasters for zone {zone_num}'))
=======
  # Export veg rasters
  # --------------------------------------- #
  print(glue('exporting vegetation rasters for zone {zone_num}'))
>>>>>>> Stashed changes
  
  # export canopy cover
  writeRaster(evc_z, 
              glue('{veg_dir_zone}/canopy_cover.tif'),
              overwrite = TRUE)
  
  # canopy height
  writeRaster(evh_z, 
              glue('{veg_dir_zone}/canopy_height.tif'),
              overwrite = TRUE)
  
  # evt_gp
  writeRaster(evt_gp, 
              glue('{veg_dir_zone}/EVT_GP.tif'),
              overwrite = TRUE)
  
  # evt_gp_remap
  writeRaster(evt_gp_remap, 
              glue('{veg_dir_zone}/EVT_GP_Remap.tif'),
              overwrite = TRUE)
  
  #remove unused layers  - keep evt_gp_remap for masking other layers
  rm(evt_gp, evc_z, evh_z)
  gc()
  
  
  # Calculate Northing and Easting from Aspect
  # -------------------------------------------- #
  
  # Mask Biophys and topo layers - to zone
  # -------------------------------------------- #
  
  # Mask Disturbance layers - to zone
  # -------------------------------------------- #
  
  
  print(glue('done with zone {zone_num}!'))
  toc()

#}

