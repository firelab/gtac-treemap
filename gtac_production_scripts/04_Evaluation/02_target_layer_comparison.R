# Zonal Validation Script for TreeMap Outputs
# Validation, in this instance, means comparing against the LandFire layers used as target data

# Written by Lila Leatherman (lila.leatherman@usda.gov)

# Last updated:
# 6/20/24

# Goals: 
# - load preliminary imputation outputs
# - join with x-table on ID 
# - build rasters of attributes so we can inspect them 
# - create confusion matrices to compare outputs against Landfire
# - use concat to compare EVC, EVH, etc with landfire layers 

# TO DO: 
# - remove unused levels from concat / diff

###########################################################################
# Set inputs
###########################################################################

# Standard Inputs
#-----------------------------------------------#


# Set inputs - from input script used for imputation
#-----------------------------------------------------#

this_proj <- this.path::this.proj()

inputs_for_imputation<- glue::glue('{this_proj}/gtac_production_scripts/03_Imputation/00b_zonal_inputs_for_imp.R')
source(inputs_for_imputation)


# Specific inputs
#----------------------------------------------------------#

# how many digits to round stats to 
round_dig = 4

# list layers to export
eval_vars <- c("canopy_cover", "canopy_height", "EVT_GP",
                   "disturb_code")

#####################################################################
# Load data
####################################################################

# Imputed raster
#-------------------------------------------#
# name of raster to validate
raster_name <- glue::glue("{output_name}")

#load raw imputation output raster
ras <- terra::rast(glue("{assembled_dir}/01_Imputation/{raster_name}.tif"))

# trim off NA values
ras <- terra::trim(ras)

# check projection - an inspection
#crs(ras, describe = TRUE)

#set name of input column
names(ras) <- c("value")

# inspect
ras
plot(ras)

# X - table
#------------------------------------------#
xtable <- read.csv(xtable_path)

# Target rasters
#---------------------------------------#

# list landfire rasters in dir
target_files <- list.files(target_dir, pattern = ".tif$",
                           recursive = TRUE, full.names = TRUE)

# filter to include disturbance type we want
target_files <- filter_disturbance_rasters(target_files, dist_layer_type)

# filter to only include eval layers
target_files <- target_files[target_files %>%
    str_detect(eval_vars %>% paste(., collapse = "|"))]

# load
rs2 <- load_target_rasters(target_files)

#trim away any NAs on the sides - helps match extent with outputs
rs2 %<>% terra::trim()

gc()


# Make sure target and imputation rasters have the same geometry
identical(crs(ras), crs(rs2))
compareGeom(ras, rs2)

# keep landfire code as 1/2 - because output raster values derived from imputation will have 1/2


# clear memory
#--------------------#
gc()

#####################################################################
# Prep for assembly
####################################################################

# get list of IDs present in ras
id_list <- freq(ras)$value %>%
  sort() %>%
  as.data.frame()
names(id_list) <- "PLOTID"

# join list of ids with x table
# create lookup table that only has IDs present in zone
lookup <- left_join(id_list, xtable, by = c("PLOTID" = "ID")) %>%
  select(PLOTID, CN, all_of(eval_vars)) %>%
  mutate(across(where(is.numeric), ~na_if(., NA)))

# load evt_gp remap table
evt_gp_remap_table <- read.csv(evt_gp_remap_table_path)

# join remapped EVT_GPs with lookup table
lookup %<>%
  left_join(evt_gp_remap_table, by = "EVT_GP")

####################################################################
# Evaluation: Calculate confusion matrices of Imputation vs Landfire / reference rasters
####################################################################

# if exportTF = true: then exports assembled, imputed raster

# apply function to all layers
cms <- eval_vars %>%
  map(\(x) assembleCM(x, 
                      raster = ras,
                      lookup = lookup,
                      id_field = "PLOTID",
                      stackin_compare = rs2,
                      stackin_compare_name =  "Target",
                      remapEVT_GP = TRUE,
                      EVT_GP_remap_table =  evt_gp_remap_table,
                      exportTF = TRUE,
                      export_path = glue::glue('{assembled_dir}/02_Assembled_vars/{raster_name}')

                      ))

names(cms) <- eval_vars

# export as RDS
write_rds(cms, glue::glue('{eval_dir}/02_Target_Layer_Comparison/{output_name}_CMs_TargetLayerComparison.RDS'))

############################################################################

# Below are additional eval steps that can be run if desired
# assembleExport() is redundant with assembleCM() in that they both have the ability to export assembled rasters 

# #########################################################
# # Assemble layers- derived from imputed ids matched with X table
# #########################################
# 
# #lapply - change to map? for consistency with next step 
# # lapply(eval_vars, assembleExport, 
# #        # additional options for function
# #        raster = ras, lookup = lookup, id_field = "PLOTID",
# #        export_path = glue::glue('{assembled_dir}/02_Assembled_vars/{raster_name}'))
# 
# eval_vars %>%
#   map(\(x) assembleExport(x, 
#                           raster = ras, 
#                           lookup = lookup, 
#                           id_field = "PLOTID",
#                           export_path = glue::glue('{assembled_dir}/02_Assembled_vars/{raster_name}')
#                           ))
# 
# gc()
# 
# 
# ############################################################
# # Evaluation: Concat for selected fields to see difference when compared vs. target layers
# ######################################################################
# 
# 
# 
# # #lapply  function to selected layers
# # #-------------------------------------------------#
# # lapply(eval_vars, assembleConcat, # list to apply over, function to apply
# #        # additional arguments to function
# #        ras = ras, 
# #        lookup = lookup, 
# #        id_field = "PLOTID",
# #        stackin_compare = rs2, 
# #        stackin_compare_name = "Landfire",
# #        export_path = glue('{eval_dir}/02_LF_Comparison/{output_name}'),
# #        remapEVT_GP = TRUE, 
# #        EVT_GP_remap_table = evt_gp_remap_table)
# 
# eval_vars %>%
#   map(\(x) assembleConcat(x, 
#                           ras = ras, 
#                           lookup = lookup, 
#                           id_field = "PLOTID",
#                           stackin_compare = rs2, 
#                           stackin_compare_name = "Target",
#                           export_path = glue('{eval_dir}/02_Target_Layer_Comparison/{output_name}'),
#                           remapEVT_GP = TRUE, 
#                           EVT_GP_remap_table = evt_gp_remap_table))
# 
# ####################################
#   
#   
  
