# Zonal Validation Script for TreeMap Outputs
# Validation, in this instance, means comparing against the LandFire layers used as target data

# Written by Lila Leatherman (lila.leatherman@usda.gov)

# Last updated:
# 3/19/24

# Goals: 
# - load preliminary imputation outputs
# - join with x-table on ID 
# - build rasters of attributes so we can inspect them 
# - create confusion matrices to compare outputs against Landfire
# - use concat to compare EVC, EVH, etc with landfire layers 

# TO DO: 
# - error on assembleCM on disturb_code with LCMS layer - figure out what's happening 
# - update when full lookup table with FIA computed values is available
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
input_script.path <- paste( c(spl[c(1:(length(spl)-2))],
                              "03_Imputation/00_inputs_for_imp.R" ),
                            collapse = "/")

source(input_script.path)

# Specific inputs
#----------------------------------------------------------#

# name of raster to validate
raster_name <- glue::glue('{output_name}_tilesz2000_nT36')

# list layers to export
layers_export <- c("canopy_cover", "canopy_height", "EVT_GP",
                   "disturb_code", "disturb_year")

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

# filter to only raster that match target layers
target_files <- target_files[target_files %>%
                               str_detect(layers_export %>%
                                            paste(., collapse = "|")) 
                             ]

#read in as vrt
lf <- terra::vrt(target_files, filename = "lf.vrt", options = "-separate", overwrite = TRUE)

#apply names
names(lf) <- target_files %>%
  str_extract(., "z[0-9][0-9]/([^.])*") %>%
  str_replace("z[0-9][0-9]/", "")

# reclass landfire disturbance code to binary
lf$disturb_code <- terra::classify(lf$disturb_code, cbind(2, 1))

# # Conditionally load disturbance rasters from another dir
# #----------------------------------------------------------#
# 
# # conditional 
# if(!is.na(dist_raster_dir)) {
#   
#   # list files
#   dist_files <- list.files(dist_raster_dir, full.names = TRUE, recursive = TRUE)
#   
#   # filter files if necessary
#   dist_files %<>% 
#     str_subset(pattern = "LFLCMS.tif$" ) %>%
#     str_subset(pattern = glue::glue('{cur.zone.zero}_disturb'))
#   
#   # load files in 
#   dist <- terra::vrt(dist_files, options = "-separate", 
#                      filename = glue::glue('{tmp_dir}/dist.tif'),
#                      overwrite = TRUE)
#   
#   # ensure layer names match
#   names(dist) <- c("disturb_code", "disturb_year")
#   
#   # reclass disturbance code
#   dist$disturb_code <- terra::classify(dist$disturb_code, cbind(2, 1))
#   
#   # make sure they're in the right projection 
#   dist %<>% terra::project(crs(lf)) 
#   
#   # make sure layers align
#   dist %<>% terra::crop(lf) %>%
#     terra::extend(terra::ext(lf)) %>%
#     terra::crop(lf) %>%
#     terra::resample(lf, method = "near") # check this / maybe make align earlier in processing? off by a fraction of a degree
#   
#   
#   gc()
#   
#   # combine replace disturbance layers in target data
#   lf$disturb_code_lcms <- dist$disturb_code
#   lf$disturb_year_lcms <- dist$disturb_year
#   
#   # lf$disturb_code <- dist$disturb_code
#   # lf$disturb_year <- dist$disturb_year
#   
#   rm(dist)
#   gc()
# }


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

# # inspect
# ras
# plot(ras)

# clear memory
#--------------------#
gc()

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
  select(PLOTID, CN, all_of(layers_export)) %>%
  mutate(across(where(is.numeric), ~na_if(., NA)))

# load evt_gp remap table
evt_gp_remap_table <- read.csv(evt_gp_remap_table_path)

# join remap table with lookup table
lookup %<>%
  left_join(evt_gp_remap_table, by = "EVT_GP")

# Apply function
#-----------------------------------------#

#lapply - change to map? for consistency with next step 
lapply(layers_export, assembleExport, 
       # additional options for function
       raster = ras, lookup = lookup, id_field = "PLOTID",
       export_path = glue::glue('{assembled_dir}/02_Derived_vars/{raster_name}'))
gc()

####################################################################
# Evaluation: Calculate confusion matrices of Imputation vs Landfire 
####################################################################


# apply function to one layer - for testing
# cm <- assembleCM(layer_field = "EVT_GP", raster = ras, lookup = lookup, id_field = "PLOTID",
#                  stackin_compare = lf, stackin_compare_name =  "Landfire", 
#                  remapEVT_GP = TRUE, EVT_GP_remap_table =  evt_gp_remap_table )

# apply function to all layers
cms <- #layers_export %>%
  c("canopy_cover", "canopy_height", "EVT_GP") %>%
  map(\(x) assembleCM(x, raster = ras, lookup = lookup, id_field = "PLOTID",
                      stackin_compare = lf, stackin_compare_name =  "Landfire", 
                      remapEVT_GP = TRUE, EVT_GP_remap_table =  evt_gp_remap_table ))
#names(cms) <- layers_export
names(cms) <- c("canopy_cover", "canopy_height", "EVT_GP")

# export as RDS
write_rds(cms, glue::glue('{eval_dir}/02_LF_Comparison/{output_name}_CMs_derivedVars.RDS'))


############################################################
# Evaluation: Concat for selected fields to see difference when compared vs. landfire
######################################################################



# #lapply  function to selected layers
# #-------------------------------------------------#
# lapply(layers_export, assembleConcat, # list to apply over, function to apply
#        # additional arguments to function
#        ras = ras, lookup = lookup, id_field = "PLOTID",
#        stackin_compare = lf, stackin_compare_name = "Landfire",
#        export_path = glue('{eval_dir}/02_LF_Comparison/{output_name}'), 
#        remapEVT_GP = TRUE, evt_gp_remap_table)

####################################
  
  
  
