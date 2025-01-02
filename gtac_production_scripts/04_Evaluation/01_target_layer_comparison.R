# Zonal Target layer Comparison for TreeMap Outputs

# Written by Lila Leatherman (lila.leatherman@usda.gov)

# This script accomplishes the following: 
# - load preliminary imputation outputs
# - join with x-table on ID 
# - build rasters of attributes so we can inspect them 
# - create confusion matrices to compare imputatation outputs against target layers
# - Optionally use concat for a visual inspection of differences of imputation outputs vs target layers

# Last updated:
# 8/28/24

###########################################################################
# Set inputs
###########################################################################

# Specific inputs
#----------------------------------------------------------#

#list layers to evaluate, assemble, and export
#eval_vars_cat <- c("evc", "evh", "evt_gp_remap", "evt_gp", "disturb_code", "disturb_code_bin")
#eval_vars_cat <- c(yvars, "evt_gp", "disturb_code") # compare both binary disturbance and original disturbance codes

# Set inputs manually - if running standalone
#-----------------------------------------------------#

standalone <- "N"
cur_zone_zero_standalone <- "z02"
year_standalone <- 2022


#####################################################################
# Load data
####################################################################

# Set inputs manually - if running standalone
#-----------------------------------------------------#

if(standalone == 'Y') {
  
  # assign main variables
  cur_zone_zero <- cur_zone_zero_standalone
  year <- year_standalone
  
  this_proj <- this.path::this.proj()
  this_dir <- this.path::this.dir()
  
  ## load treemap library
  lib_path = glue::glue('{this_proj}/gtac_production_scripts/00_Library/treeMapLib.R')
  source(lib_path)
  
  #load settings for zone
  zone_settings <- glue::glue("{home_dir}/03_Outputs/07_Projects/{year}_Production/01_Raw_model_outputs/{cur_zone_zero}/params/{cur_zone_zero}_{year}_Production_env.RDS")
  
  load(zone_settings)
  
}

message("Loading data for target layer comparison")

# Imputed raster
#-------------------------------------------#
# name of raster to validate
raster_name <- glue::glue("{output_name}_Imputation")

#load raw imputation output raster
ras <- terra::rast(glue::glue("{assembled_dir}/01_Imputation/{raster_name}.tif"))

# trim off NA values
ras <- terra::trim(ras)

# check projection - an inspection
#crs(ras, describe = TRUE)

#set name of input column
names(ras) <- c("value")

# inspect
ras
plot(ras,
     main = glue::glue("Raw imputed ids: Zone {zone_num}"))

# X - df
#------------------------------------------#
X_df <- read.csv(xtable_path_model)

# Load target rasters
#---------------------------------------#

# list landfire rasters in dir
target_files <- list.files(target_dir, pattern = ".tif$",
                           recursive = TRUE, full.names = TRUE)

# filter to include disturbance type we want
target_files <- filter_disturbance_rasters(target_files, dist_layer_type)

# filter to only include eval layers
target_files <- target_files[target_files %>%
    str_detect(eval_vars_cat %>% paste(., collapse = "|"))]

# load
rs2 <- load_and_name_rasters(target_files)

# Prep disturbance code binary target raster
#------------------------------------------------------#

# make binary disturbance code, since this was a target layer
rs2$disturb_code_bin<- rs2$disturb_code
rs2$disturb_code_bin[rs2$disturb_code_bin == 2]<- 1

# Prep evt_gp orig target raster
#-------------------------------------------------------#

# load evt_gp remap table
evt_gp_remap_table <- read.csv(evt_gp_remap_table_path) %>%
  rename_with(tolower) %>%
  dplyr::select(evt_gp_remap, evt_gp) %>%
  as.matrix()

# make evt_gp raster, not remapped
rs2$evt_gp <- terra::classify(rs2$evt_gp_remap, evt_gp_remap_table )


# FOR TESTING: Conditionally crop to aoi
#---------------------------------------------------#
if (!is.na(aoi_path)) {
  
  print("using input shapefile as AOI")
  
  # get desired crs
  lf_crs <- terra::crs(rs2)
  
  # crop and mask
  aoi <- terra::vect(aoi_path) %>% terra::project(lf_crs)
  rs2 <- terra::crop(rs2, aoi, mask = TRUE)
  
  gc()
  
} else { print("using extent of input raster stack as AOI") }

# Keep prepping AOI
#-----------------------------------------------------------------#

#trim away any NAs on the sides - helps match extent with outputs
rs2 %<>% terra::trim()

#crop target data to extent of outputs
rs2 %<>% terra::crop(ext(ras))


# Make sure target and imputation rasters have the same geometry
identical(crs(ras), crs(rs2))
compareGeom(ras, rs2)

# clear memory
#--------------------#
gc()

#####################################################################
# Prep for assembly
####################################################################

message("preparing data to assemble rasters")

# get list of IDs present in imputed raster
id_list <- freq(ras)$value %>%
  sort() %>%
  as.data.frame()
names(id_list) <- "tm_id"

# join list of ids with x table
# create lookup table that only has IDs present in zone
lookup <- left_join(id_list, X_df, by = "tm_id") %>%
  select(tm_id, CN, all_of(eval_vars_cat)) %>%
  mutate(across(where(is.numeric), ~na_if(., NA)))


####################################################################
# Evaluation: Calculate confusion matrices of Imputation vs Target Layers
####################################################################

message("assembling and comparing imputed outputs vs Target Layers")

# if exportTF = true: then exports assembled, imputed raster

# apply function to all layers
cms <- eval_vars_cat %>%
  map(\(x) assembleCM(x, 
                      raster = ras,
                      lookup = lookup,
                      id_field = "tm_id",
                      stackin_compare = rs2,
                      stackin_compare_name =  "Target",
                      exportTF = TRUE,
                      export_path = glue::glue('{assembled_dir}/02_Assembled_vars/{output_name}')

                      ))

names(cms) <- eval_vars_cat

# export as RDS
write_rds(cms, glue::glue('{eval_dir}/01_Target_Layer_Comparison/{output_name}_CMs_TargetLayerComparison.RDS'))

############################################################################

# Below are additional eval steps that can be run if desired

# ############################################################
# # Evaluation: Concat for selected fields to see difference when compared vs. target layers
# ######################################################################
# 
# eval_vars_cat %>%
#   map(\(x) assembleConcat(x, 
#                           ras = ras, 
#                           lookup = lookup, 
#                           id_field = "tm_id",
#                           stackin_compare = rs2, 
#                           stackin_compare_name = "Target",
#                           export_path = glue('{eval_dir}/01_Target_Layer_Comparison/{output_name}'),
#                           ))
# 

 

  
rm(ras, lookup, rs2, id_list)
gc()
