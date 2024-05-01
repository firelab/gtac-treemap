# Zonal Validation Script for TreeMap Outputs
# Validation, in this instance, means comparing against the LandFire layers used as target data

# Written by Lila Leatherman (lila.leatherman@usda.gov)

# Last updated:
# 4/18/24

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

# Set inputs - from input script
# Id where script is located
thispath <- this.path::this.path()

# get path to input script with settings for imputation
spl <- stringr::str_split(thispath, "/")[[1]]
input_script_path <- paste(c(spl[c(1:(length(spl) - 1))],
                             "00_inputs_for_evaluation.R"),
                          collapse = "/")

source(input_script_path)

# Specific inputs
#----------------------------------------------------------#

# name of raster to validate
raster_name <- glue::glue("{output_name}_tilesz2000_nT36")

# list layers to export
eval_vars <- c("canopy_cover", "canopy_height", "EVT_GP",
                   "disturb_code")

#eval_vars <- "disturb_code"

#########################################################################

# Terra options
# --------------------------------#

#increase memory fraction available
terraOptions(memfrac = 0.8)

# Other options
# --------------------------------#

# Allow for sufficient digits to differentiate plot cn numbers
options("scipen" = 100, "digits" = 8)


#####################################################################
# Load data
####################################################################

# X - table
#------------------------------------------#
xtable <- read.csv(xtable_path)

# Landfire rasters
#---------------------------------------#

# list landfire rasters in dir
target_files <- list.files(target_dir, pattern = ".tif$",
                           recursive = TRUE, full.names = TRUE)

# filter to only rasters that match layers we want to compare
target_files <- target_files[target_files %>%
    str_detect(eval_vars %>% paste(., collapse = "|"))]

#read in as vrt
lf <- terra::vrt(target_files, filename = glue::glue("{tmp_dir}/lf.vrt"), 
                 options = "-separate", overwrite = TRUE)

#apply names
names(lf) <- target_files %>%
  str_extract(., "z[0-9][0-9]/([^.])*") %>%
  str_replace("z[0-9][0-9]/", "")

#trim away any NAs on the sides - helps match extent with outputs
lf %<>% terra::trim()

gc()


# keep landfire code as 1/2 - because output raster values derived from imputation will have 1/2

# Conditionally load disturbance rasters from another dir
#----------------------------------------------------------#

# conditional
if (!is.na(dist_raster_dir)) {

  # list files
  dist_files <- list.files(dist_raster_dir, full.names = TRUE, recursive = TRUE)

  # filter files if necessary
  dist_files %<>%
    str_subset(pattern = glue::glue("{dist_layer_type}.tif$")) %>%
    str_subset(pattern = glue::glue("{cur_zone_zero}_disturb"))

  # load files in
  dist <- terra::vrt(dist_files, options = "-separate",
                     filename = glue::glue("{tmp_dir}/dist.tif"),
                     overwrite = TRUE)

  # ensure layer names match
  names(dist) <- c("disturb_code", "disturb_year")

  # make sure layers align
  dist %<>% terra::crop(lf)
  
  #inspect
  compareGeom(dist, lf)

  gc()

  # combine replace disturbance layers in target data
  lf$disturb_code <- dist$disturb_code
  lf$disturb_year <- dist$disturb_year

  rm(dist)
  gc()
}


# Imputed Raster
#--------------------------------------------#

#load raw imputation output raster
ras <- terra::rast(glue("{assembled_dir}/01_Imputation/{raster_name}.tif"))

# trim off NA values
ras <- terra::trim(ras)

# check projection - an inspection
crs(ras, describe = TRUE)
#identical(crs(ras), crs(lf))
compareGeom(ras, lf)

#set name of input column
names(ras) <- c("value")

# # inspect
# ras
# plot(ras)

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

# #apply function to one layer - for testing
# cm <- assembleCM(layer_field = "EVT_GP", raster = ras, lookup = lookup, id_field = "PLOTID",
#                  stackin_compare = lf, stackin_compare_name =  "Landfire",
#                  remapEVT_GP = TRUE, EVT_GP_remap_table =  evt_gp_remap_table )

# apply function to all layers
cms <- eval_vars %>%
  map(\(x) assembleCM(x, 
                      raster = ras,
                      lookup = lookup,
                      id_field = "PLOTID",
                      stackin_compare = lf,
                      stackin_compare_name =  "Landfire",
                      remapEVT_GP = TRUE,
                      EVT_GP_remap_table =  evt_gp_remap_table,
                      exportTF = TRUE,
                      export_path = glue::glue('{assembled_dir}/02_Assembled_vars/{raster_name}')
                      ))

names(cms) <- eval_vars

# export as RDS
write_rds(cms, glue::glue('{eval_dir}/02_Target_Layer_Comparison/{output_name}_CMs_TargetLayerComparison.RDS'))

#########################################################
# Assemble layers- derived from imputed ids matched with X table
#########################################

#lapply - change to map? for consistency with next step 
# lapply(eval_vars, assembleExport, 
#        # additional options for function
#        raster = ras, lookup = lookup, id_field = "PLOTID",
#        export_path = glue::glue('{assembled_dir}/02_Assembled_vars/{raster_name}'))

eval_vars %>%
  map(\(x) assembleExport(x, 
                          raster = ras, 
                          lookup = lookup, 
                          id_field = "PLOTID",
                          export_path = glue::glue('{assembled_dir}/02_Assembled_vars/{raster_name}')
                          ))

gc()


############################################################
# Evaluation: Concat for selected fields to see difference when compared vs. landfire
######################################################################



# #lapply  function to selected layers
# #-------------------------------------------------#
# lapply(eval_vars, assembleConcat, # list to apply over, function to apply
#        # additional arguments to function
#        ras = ras, 
#        lookup = lookup, 
#        id_field = "PLOTID",
#        stackin_compare = lf, 
#        stackin_compare_name = "Landfire",
#        export_path = glue('{eval_dir}/02_LF_Comparison/{output_name}'),
#        remapEVT_GP = TRUE, 
#        EVT_GP_remap_table = evt_gp_remap_table)

eval_vars %>%
  map(\(x) assembleConcat(x, 
                          ras = ras, 
                          lookup = lookup, 
                          id_field = "PLOTID",
                          stackin_compare = lf, 
                          stackin_compare_name = "Landfire",
                          export_path = glue('{eval_dir}/02_Target_Layer_Comparison/{output_name}'),
                          remapEVT_GP = TRUE, 
                          EVT_GP_remap_table = evt_gp_remap_table))

####################################
  
  
  
