# Zonal Target layer Comparison for TreeMap Outputs

# Written by Lila Leatherman (lila.leatherman@usda.gov)

# Last updated:
# 7/18/24

# In this script, we: 
# - load preliminary imputation outputs
# - join with x-table on ID 
# - build rasters of attributes so we can inspect them 
# - create confusion matrices to compare outputs against Landfire
# - use concat to compare EVC, EVH, etc with landfire layers 

###########################################################################
# Set inputs
###########################################################################

# Set inputs manually - if running standalone
#-----------------------------------------------------#

#this_proj <- this.path::this.proj()
#this_dir <- this.path::this.dir()

# LOAD PROJECT PARAMS RDS

# get zone 

# load zonal inputs RDS

# Specific inputs
#----------------------------------------------------------#

# list layers to evaluate, assemble, and export
#eval_vars_cat <- c("evc", "evh", "evt_gp", "disturb_code", "disturb_code_bin")
eval_vars_cat <- c(yvars, "disturb_code") # compare both binary disturbance and original disturbance codes


#####################################################################
# Load data
####################################################################

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

# Target rasters
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

# make binary disturbance code, since this was a target layer
rs2$disturb_code_bin<- rs2$disturb_code
rs2$disturb_code_bin[rs2$disturb_code_bin == 2]<- 1

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

gc()


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

# get list of IDs present in ras
id_list <- freq(ras)$value %>%
  sort() %>%
  as.data.frame()
names(id_list) <- "PLOTID"

# join list of ids with x table
# create lookup table that only has IDs present in zone
lookup <- left_join(id_list, X_df, by = c("PLOTID" = "X")) %>%
  select(PLOTID, CN, all_of(eval_vars_cat)) %>%
  mutate(across(where(is.numeric), ~na_if(., NA)))

# load evt_gp remap table
evt_gp_remap_table <- read.csv(evt_gp_remap_table_path)

# join remapped EVT_GPs with lookup table
lookup %<>%
  left_join(evt_gp_remap_table %>% dplyr::rename_with(tolower), by = c("evt_gp_remap")) 

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
                      id_field = "PLOTID",
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
# assembleExport() is redundant with assembleCM() in that they both have the ability to export assembled rasters 

# #########################################################
# # Assemble layers- derived from imputed ids matched with X table
# #########################################
# 
# eval_vars_cat %>%
#   map(\(x) assembleExport(x, 
#                           raster = ras, 
#                           lookup = lookup, 
#                           id_field = "PLOTID",
#                           export_path = glue::glue('{assembled_dir}/02_Assembled_vars/{output_name}')
#                           ))
# 
# gc()
# 
# 
# ############################################################
# # Evaluation: Concat for selected fields to see difference when compared vs. target layers
# ######################################################################
# 
# eval_vars_cat %>%
#   map(\(x) assembleConcat(x, 
#                           ras = ras, 
#                           lookup = lookup, 
#                           id_field = "PLOTID",
#                           stackin_compare = rs2, 
#                           stackin_compare_name = "Target",
#                           export_path = glue('{eval_dir}/01_Target_Layer_Comparison/{output_name}'),
#                           ))
# 
# ####################################
#   
#   
  
rm(ras, lookup, rs2, vrt)
gc()
