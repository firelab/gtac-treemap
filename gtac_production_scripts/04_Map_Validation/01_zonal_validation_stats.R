# Zonal Validation Script for TreeMap Outputs

# Written by Lila Leatherman (lila.leatherman@usda.gov)

# Last updated:
#2/13/2024

# Goals: 
# - load preliminary imputation outputs
# - join with x-table on ID 
# - build raster of EVC, EVH, EVT_GP to assess accuracy 
# - join with EVT_GP_remap table to get back to original evt_gps
# - create confusion matrices to compare outputs against Landfire
# - use concat to compare EVC, EVH, etc with landfire layers 

# TO DO: 
# - reclass landfire disturb code to binary
# - try mapping over list instead of lapply
# - update when full lookup table with FIA computed values is available
# - remove unused levels from concat / diff

###########################################################################
# Set inputs
###########################################################################

# name of raster to validate
raster_name <- "2016_Orig_Test_keepinbag_UT_Uintas_rect_maxpx500_nT37"

# list layers to export
layers_export <- c("canopy_cover", "canopy_height", "EVT_GP",
                   "disturb_code", "disturb_year")


###########################################################################
# Set inputs
###########################################################################

# Set inputs - from input script
# Id where script is located
this.path <- this.path::this.path()

# get path to input script with settings for imputation
spl <- stringr::str_split(this.path, "/")[[1]]
input_script.path <- paste( c(spl[c(1:(length(spl)-2))],
                              "03_Imputation/00_inputs_for_imp.R" ),
                            collapse = "/")

source(input_script.path)




###################################################################

# library(glue)
# 
# #home_dir
# #home_dir <- "D:/LilaLeatherman/01_TreeMap/"
# home_dir <- "//166.2.126.25/TreeMap/"
# 
# # Zone list
# zone_list <- c(16)
# 
# # Path to X table
# xtable_path <- glue('{home_dir}01_Data/01_TreeMap2016_RDA/01_Input/03_XTables/X_table_all_singlecondition.txt')
# 
# # path where raw raster output(s) live (zone will be added later)
# raster_dir <- glue('{home_dir}03_Outputs/07_Raw_model_outputs/2016_Original_Test')
# 
# # Directory where EVT_GP remap table is located
# evt_gp_remap_table_dir <- glue('{home_dir}03_Outputs/05_Target_Rasters/02_Vegetation/')
# 
# # desired projection
# prj <- terra::crs(glue('{home_dir}01_Data/02_Landfire/landfire_crs.prj'))
# 
# 
# # path to landfire layers
# landfire_dir <- glue('{home_dir}01_Data/01_TreeMap2016_RDA/02_Target/')
# 
# # path to TreeMap library
# lib_path <- "C:/Users/lleatherman/Documents/GitHub/gtac-treemap/gtac_production_scripts/05_Library/treemapLib.R"
# 
# 
# 
# # Paths for exporting data
# #--------------------------------------#
# 
# # set path to save output rasters
# # this directory will be created if it does not already exist
# output_dir <- raster_dir
# 
# # Output imputation name
# #output_name <- "2016_Orig_Test"
# output_name <- raster_name
# 
# # set tmp directory
# tmp_dir <- "D:/tmp/"
# 
# ###########################################################################
# # Set up libraries and directories
# ###########################################################################
# 
# # Packages and functions
# #---------------------------------#
# 
# # packages required
# list.of.packages <- c("terra", "tidyverse", "magrittr", "glue", "tictoc", "caret")
# 
# #check for packages and install if needed
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages) > 0) install.packages(new.packages)
# 
# # load all packages
# vapply(list.of.packages, library, logical(1L),
#        character.only = TRUE, logical.return = TRUE)
# 
# # load custom library
# source(lib_path)
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
# #empty temp dir
# do.call(file.remove, list(list.files(tmp_dir, full.names = TRUE)))
# #remove unused memory
# gc()

#########################################################################

# Terra options
# --------------------------------#

#increase memory fraction available
terraOptions(memfrac = 0.8)

# Other options
# --------------------------------#

# Allow for sufficient digits to differentiate plot cn numbers
options("scipen"=100, "digits"=8)


#####################################################################
# Load data
####################################################################


# X - table
#------------------------------------------#
xtable <- read.csv(xtable_path)

# Landfire rasters
#---------------------------------------#

# list landfire rasters in dir
target_files <- list.files(target_dir, pattern = ".tif$", recursive = TRUE, full.names = TRUE)

#read in as vrt
lf <- terra::vrt(target_files, filename = "lf.vrt", options = "-separate", overwrite = TRUE)

#apply names
names(lf) <- target_files %>%
  str_extract(., "z[0-9][0-9]/([^.])*") %>%
  str_replace("z[0-9][0-9]/", "")

# reclass landfire disturbance code to binary
lf$disturb_code <- terra::classify(lf$disturb_code, cbind(2, 1))


# Imputed Raster
#--------------------------------------------#

#load raw imputation output raster
ras <- terra::rast(glue('{assembled_dir}/01_Imputation/{raster_name}.tif'))

# trim off NA values
ras <- terra::trim(ras)

# # check projection - reproject to desired proj if it doesn't match
# if(!identical(crs(ras), crs(prj))){
#   #project to desired projection
#   ras %<>% terra::project(prj)
# }

#set name of input column
names(ras) <- c("value")

#convert to integer
ras <- as.int(ras)

# inspect
ras
plot(ras)

#####################################################################
# Assemble derived values from imputation
####################################################################

# get list of IDs present in ras
id_list<- freq(ras)$value %>%
  sort() %>%
  as.data.frame() 
names(id_list) <- "PLOTID"

# join list of ids with x table
# create lookup table that only has IDs present in zone
lookup <- left_join(id_list, xtable, by = c("PLOTID" = "ID")) %>%
  select(PLOTID, CN, canopy_height, canopy_cover, EVT_GP, disturb_code, disturb_year) %>%
  mutate(across(where(is.numeric), ~na_if(., NA)))

# load evt_gp remap table
evt_gp_remap_table <- read.csv(evt_gp_remap_table_path)

# join remap table with lookup table
lookup %<>%
  left_join(evt_gp_remap_table, by = "EVT_GP")

# Apply function
#-----------------------------------------#

# apply function - to only one layer for testing
assembleExport(layer_field = "EVT_GP", raster = ras, lookup = lookup, 
               id_field = "PLOTID", 
               export_path = glue('{assembled_dir}/02_Derived_vars/{raster_name}'))

#lapply
lapply(layers_export, assembleExport, 
       # additional options for function
       raster = ras, lookup = lookup, id_field = "PLOTID",
       export_path = glue('{assembled_dir}/02_Derived_vars/{raster_name}'))

# #

# Evaluation: Calculate confusion matrices of Imputation vs Landfire 
###################################################################


# apply function to one layer - for testing
cm <- assembleCM(layer_field = "EVT_GP", raster = ras, lookup = lookup, id_field = "PLOTID",
                 stackin_compare = lf, stackin_compare_name =  "Landfire", 
                 remapEVT_GP = TRUE, EVT_GP_remap_table =  evt_gp_remap_table )

# apply function to all layers
cms <- layers_export %>%
  map(\(x) assembleCM(x, raster = ras, lookup = lookup, id_field = "PLOTID",
                      stackin_compare = lf, stackin_compare_name =  "Landfire", 
                      remapEVT_GP = TRUE, EVT_GP_remap_table =  evt_gp_remap_table ))
names(cms) <- layers_export

# export as RDS
write_rds(cms, glue::glue('{eval_dir}/01_Map_Validation/CMs_derivedVars.RDS'))

###########################
# Evaluation: Concat for selected fields to see difference when compared vs. landfire
######################################################################


# test function - apply on one input field
#-----------------------------------------#
assembleConcat(layer_field = "canopy_cover", raster = ras, lookup = lookup, 
               id_field = "PLOTID", stackin_compare = lf, stackin_compare_name =  "Landfire", 
               export_path =  glue('{eval_dir}/01_Map_Validation/{output_name}'),
               remapEVT_GP = FALSE, EVT_GP_remap_table =  evt_gp_remap_table)

# assembleConcat("EVT_GP", ras, lookup, "PLOTID",
#              lf, "Landfire", glue('{output_dir}{cur.zone.zero}_{output_name}'),
#              cm = TRUE, remapEVT_GP = TRUE, evt_gp_remap_table)


#lapply  function to selected layers
#-------------------------------------------------#
lapply(layers_export, assembleConcat, # list to apply over, function to apply
       # additional arguments to function
       ras = ras, lookup = lookup, id_field = "PLOTID",
       stackin_compare = lf, stackin_compare_name = "Landfire",
       export_path = glue('{eval_dir}/01_Map_Validation/{output_name}'), 
       remapEVT_GP = TRUE, evt_gp_remap_table)

####################################
  
  
  
