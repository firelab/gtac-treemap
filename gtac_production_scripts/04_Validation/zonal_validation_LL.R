# Zonal Validation Script for TreeMap Outputs

# Written by Lila Leatherman (lila.leatherman@usda.gov)

# Last updated:

# Goals: 
# - load preliminiary imputation outputs
# - join with x-table on ID 
# - build raster of EVC, EVH, EVT_GP to assess accuracy 
# - join with EVT_GP_remap table to get back to original evt_gps
# - use concat to compare EVC, EVH, etc with landfire layers 


###########################################################################
# Set inputs
###########################################################################

# Zone list
zone_list <- c(16)

# Path to X table
xtable_path <- "//166.2.126.25/TreeMap/01_Data/01_TreeMap2016_RDA/01_Input/03_XTables/X_table_all_singlecondition.txt"

# path where raw raster output(s) live (zone will be added later)
raster_dir <- '//166.2.126.25/TreeMap/03_Outputs/07_Raw_model_outputs/2016_Original_Test/'

# raster name
raster_name <- "testRows_7500_7900"

# Directory where EVT_GP remap table is located
evt_gp_remap_table_dir <- "//166.2.126.25/TreeMap/03_Outputs/05_Target_Rasters/02_Vegetation/"

# desired projection
prj <- terra::crs("//166.2.126.25/TreeMap/01_Data/02_Landfire/landfire_crs.prj")

# path to landfire layers
landfire_dir <- "//166.2.126.25/TreeMap/01_Data/01_TreeMap2016_RDA/02_Target/"

# path to TreeMap library
lib_path <- "C:/Users/lleatherman/Documents/GitHub/gtac-treemap/gtac_production_scripts/treemapLib.R"

# list layers to export
layers_export <- c("canopy_cover", "canopy_height", "EVT_GP",
                   "disturb_code", "disturb_year")

# Paths for exporting data
#--------------------------------------#

# set path to save output rasters
# this directory will be created if it does not already exist
output_dir <- "//166.2.126.25/TreeMap/03_Outputs/07_Raw_model_outputs/2016_Original_Test/"

# Output imputation name
#output_name <- "2016_Orig_Test"
output_name <- raster_name

# set tmp directory
tmp_dir <- "D:/tmp/"

###########################################################################
# Set up libraries and directories
###########################################################################

# Packages and functions
#---------------------------------#

# packages required
list.of.packages <- c("terra", "tidyverse", "magrittr", "glue", "tictoc", "caret")

#check for packages and install if needed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages)

# load all packages
vapply(list.of.packages, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)

# load custom library
source(lib_path)

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


# load x table
xtable <- read.csv(xtable_path)


#####################################################################
# Start
####################################################################


#for(i in 1:length(zone_list)) {

  
  zone_num <- zone_list[1]

  # Set zone name options and variables
  #---------------------------------------------------------#
  
  # Set zone name options
  cur.zone <- glue('z{zone_num}')
  cur.zone.zero <- if(zone_num < 10) {
    glue('z0{zone_num}') } else {
      cur.zone
    }
  
  # Set folder paths
  raster_dir = glue('{raster_dir}{cur.zone.zero}/raster/')
  output_dir = glue('{output_dir}{cur.zone.zero}/map_validation/')
  landfire_dir = glue('{landfire_dir}{cur.zone.zero}/')
  
  # create output folder if it does not exist
  if(!file.exists(output_dir)){
    dir.create(output_dir, )
  }

  # Load data
  #--------------------------------------------#

  #load raw output raster
  raster_path = glue('{raster_dir}{raster_name}.tif')
  ras <- terra::rast(raster_path)
  
  # trim off NA values
  ras <- trim(ras)
  
  # # check projection - reproject to desired proj if it doesn't match
  # if(!identical(crs(ras), crs(prj))){
  #   #project to desired projection
  #   ras %<>% terra::project(prj)
  # }
  
  # inspect
  ras
  
  #set name of input column
  names(ras) <- c("value")
  
  #convert to integer
  ras <- as.int(ras)
  
  
  #####
  
  # get list of IDs
  id_list<- freq(ras)$value %>%
    sort() %>%
    as.data.frame() 
  names(id_list) <- "PLOTID"
  
  # join list of ids with x table to create lookup table
  lookup <- left_join(id_list, xtable, by = c("PLOTID" = "ID")) %>%
    select(PLOTID, CN, canopy_height, canopy_cover, EVT_GP, disturb_code, disturb_year) %>%
    mutate(across(where(is.numeric), ~na_if(., NA)))
  
  #####
  
  # load evt_gp remap table
  evt_gp_remap_table_path = glue('{evt_gp_remap_table_dir}/{cur.zone.zero}/EVG_remap_table.csv')
  evt_gp_remap_table <- read.csv(evt_gp_remap_table_path)
  evt_gp_remap_table %<>%
    select(-X)
  
  lookup %<>%
    left_join(evt_gp_remap_table, by = "EVT_GP")
  
  
  # # apply function - to only one layer for testing
  # assembleExport("canopy_height", ras, lookup, "PLOTID",  
  #              glue('{output_dir}{cur.zone.zero}'))
  
  # apply function - to only one layer for testing
  assembleExport("EVT_GP", ras, lookup, "PLOTID",
               glue('{output_dir}{cur.zone.zero}'))
  
  #lapply (can i tidyverse map? )
  lapply(layers_export, assembleExport, 
         # additional options for function
         ras = ras, lookup = lookup, id_field = "PLOTID",
         export_path = glue('{output_dir}{cur.zone.zero}_{output_name}'))
  
  
  
  
  ###########################
  # next up : concats() for selected fields to see diff vs. landfire
  # ----------------------------------------------------#

  # list landfire rasters in dir
  target_files <- list.files(landfire_dir, pattern = ".tif$", recursive = TRUE, full.names = TRUE)

  #read in as vrt
  lf <- terra::vrt(target_files, filename = "lf.vrt", options = "-separate", overwrite = TRUE)

  #apply names
  names(lf) <- target_files %>%
    str_extract(., "z[0-9][0-9]/([^.])*") %>%
    str_replace("z[0-9][0-9]/", "")
  

  ###################
  # test function
  ####################
  assembleConcat("canopy_cover", ras, lookup, "PLOTID",
               lf, "Landfire", glue('{output_dir}{cur.zone.zero}_{output_name}'),
               cm = TRUE, remapEVT_GP = FALSE, evt_gp_remap_table)
  
  # assembleConcat("EVT_GP", ras, lookup, "PLOTID",
  #              lf, "Landfire", glue('{output_dir}{cur.zone.zero}_{output_name}'),
  #              cm = TRUE, remapEVT_GP = TRUE, evt_gp_remap_table)
  
  ##############################################################
  
  #lapply  function
  lapply(layers_export, assembleConcat, # list to apply over, function to apply
         # additional arguments to function
         ras = ras, lookup = lookup, id_field = "PLOTID",
         stackin_compare = lf, stackin_compare_name = "Landfire",
         export_path = glue('{output_dir}{cur.zone.zero}_{output_name}'), 
         cm = TRUE, remapEVT_GP = TRUE, evt_gp_remap_table)
  
  ####################################
  
  
  
#}
