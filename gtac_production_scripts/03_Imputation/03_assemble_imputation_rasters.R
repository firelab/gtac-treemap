# TreeMap Imputation

# PART 3: 
# - Load tiles from output
# - assemble into one raster per zone

# Last updated: 2/7/2024

##################################################
# Set inputs
###################################################

# Set inputs - from input script
# Id where script is located
this.path <- this.path::this.path()

# get path to input script
spl <- stringr::str_split(this.path, "/")[[1]]
input_script.path <- paste( c(spl[c(1:length(spl)-1)],
                              "00_inputs_for_imp.R" ),
                            collapse = "/")

source(input_script.path)

# input raster tile base name
tile_name <- "2016_Orig_Test_keepinbag_UT_Uintas_rect_maxpx500_nT37"

# desired name for output raster
rout_name <- tile_name

###########################################################################
# Set inputs
###########################################################################

# # Test application settings
# #-----------------------------------------#
# 
# # set dimensions of tile - value is the length of one side
# max_px <- 1000
# 
# # first row to start test on 
# test_row <- 1 # adjust this if using a test AOI or tiles vs whole zone
# 
# ntest_rows <- max_px
# 
# # set number of tiles to run
# # if NA, defaults to all tiles in list
# ntiles <- NA

# # # supply path to a shapefile to use as subset, or NA
# aoi_path <- "//166.2.126.25/TreeMap/01_Data/03_AOIs/UT_Uintas_rect_NAD1983.shp"
# aoi_name <- "UT_Uintas_rect"
# #aoi_path <- NA
# 
# # Standard inputs
# #---------------------------------------------#
# 
# # Zone list
# zone_list <- c(16)
# 
# #home_dir
# #home_dir <- "D:/LilaLeatherman/01_TreeMap/"
# home_dir<- "//166.2.126.25/TreeMap/"
# 
# # Directory where target rasters live - for metadata
# target_dir <- glue::glue("{home_dir}01_Data/01_TreeMap2016_RDA/02_Target/")
# 
# # Paths for exporting data
# #--------------------------------------#
# 
# # set path to save output rasters
# # this directory will be created if it does not already exist
# output_dir <- glue::glue('{home_dir}03_Outputs/07_Raw_model_outputs/2016_Original_Test/')
# 
# # Output imputation name
# output_name <- "2016_Orig_TestLL"
# 
# # set tmp directory
# tmp_dir <- "D:/tmp/"
# 
# 
# # set zone_number
# # ----------------------------------------------#
# zone_num <- zone_list[1]
# 
# # Set zone name options
# cur.zone <- glue::glue('z{zone_num}')
# cur.zone.zero <- if(zone_num < 10) {
#   glue::glue('z0{zone_num}') } else {
#     cur.zone
#   }
# 
# # Update output and target dir with zone
# # -----------------------------------------#
# # Set folder paths
# target_dir = glue::glue('{target_dir}/{cur.zone.zero}/')
# output_dir = glue::glue('{output_dir}/{cur.zone.zero}/')
# 
# # Model inputs
# #----------------------------------#
# 
# # Path where model is located
# #model_path <- glue::glue('{output_dir}/model/{cur.zone.zero}_{output_name}_yai_treelist_bin.RDS')



###########################################################################
# Set up libraries and directories
###########################################################################
# 
# # Set up temp directory 
# #----------------------------------#
# 
# # check if tmp directory exists 
# if (file.exists(tmp_dir)){
#   
# } else {
#   # create a new sub directory inside the main path
#   dir.create(tmp_dir)
# }
# 
# # set temp directory - helps save space with R terra
# write(paste0("TMPDIR = ", tmp_dir), file=file.path(Sys.getenv('R_USER'), '.Renviron'))
# 
# #empty temp dir
# do.call(file.remove, list(list.files(tmp_dir, full.names = TRUE)))
# 
# #detect and delete folders with pattern "Rtmp"
# # folders <- dir(tmp_dir, pattern = "Rtmp", full.names = TRUE)
# # unlink(folders, recursive = TRUE, force = TRUE, expand = TRUE)
# 
# 
# #remove unused memory
# gc()
# 
# 
# # Packages and functions
# #---------------------------------#
# 
# # packages required
# list.of.packages <- c("raster", "yaImpute", "randomForest", 
#                       "terra", "tidyverse", "magrittr", "glue", "tictoc",
#                       "furrr")
# 
# #check for packages and install if needed
# # new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# # if(length(new.packages) > 0) install.packages(new.packages)
# 
# # load all packages
# vapply(list.of.packages, library, logical(1L),
#        character.only = TRUE, logical.return = TRUE)
# 
# 
# # Create output dir
# # ---------------------------#
# if (!file.exists(output_dir)) {
#   dir.create(output_dir, recursive = TRUE)
# }
# 
# # Terra options
# # --------------------------------#
# 
# #increase memory fraction available
# terraOptions(memfrac = 0.8)
# 
# # Other options
# # --------------------------------#
# 
# # Allow for sufficient digits to differentiate plot cn numbers
# # plot CNs aren't present in the imputation portion, however
# 
# #options("scipen"=100, "digits"=8)

#####################################################################
# Load supporting data
#####################################################################

# Load target rasters - as reference data
# --------------------------------------------------------------------#

# list raster files
flist.tif <- list.files(path = target_dir, pattern = "*.tif$", recursive = TRUE, full.names = TRUE)

# load raster files as terra raster
# - only load first raster, for reference and metadata
rs2 <- terra::rast(flist.tif[1])

# get raster layer names
layer_names <- flist.tif %>%
  str_extract(., "z[0-9][0-9]/([^.])*") %>%
  str_replace("z[0-9][0-9]/", "")

#add names to raster list
names(rs2) <- layer_names[1]

# get crs
lf.crs <- crs(rs2)

#######################################################################
# Run
#######################################################################

# list tiles from path
tile.list <- list.files(path = tile_dir, pattern = "*.tif$", recursive = TRUE, full.names = TRUE)

# filter to tiles of interest - match tile name
tile.list <- tile.list[str_detect(tile.list, tile_name)]

# load tiles as .vrt
vrt <- terra::vrt(tile.list, filename = glue::glue('{tmp_dir}/t_assemble.tif'),
                  overwrite = TRUE)
# inspect
plot(vrt)



# export as single raster per zone
writeRaster(vrt, 
            glue('{assembled_dir}/01_Imputation/{rout_name}.tif'),
            overwrite = TRUE)


# clear unused memory
gc()
