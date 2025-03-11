# Prep target rasters for TreeMap Imputation
# Outputs: 
# Masked to forested px for each zone 
# - Landfire Existing Vegetation Cover (EVC)
# - Forest mask derived from Landfire EVC
# - Landfire Existing Vegetation Type Group
# - Landfire Existing Vegetation Height (EVH)
# - Disturbance layers 
# - EVT_GP remap table

# Written by Lila Leatherman (lila.leatherman@usda.gov)
# Last Updated: 7/19/24

# Based on "prep_target_rasters_v2.py" by Karin Riley

# TO DO: 
# - there's def a way to make this look slicker using functions


##########################################################
# SET INPUTS
##########################################################

zone_input <- 5


#get path to inputs script
this_dir <- this.path::this.dir()

inputs_script_a <- glue::glue('{this_dir}/00a_project_inputs_for_targetdata.R')
inputs_script_b <- glue::glue('{this_dir}/00b_zone_inputs_for_targetdata.R')

source(inputs_script_a)
source(inputs_script_b)

# Terra options
# --------------------------------#

#increase memory fraction available
terraOptions(memfrac = 0.8)

###################################################
# Load Data
###################################################

# load LF zone data
LF_zones <- vect(lf_zones_path)

# get crs
landfire_crs <- lf_output_crs

# Prep zone
#-----------------------------------------#
zone_num <- zone_input
# select single LF zone
zone <- subset(LF_zones, LF_zones$ZONE_NUM == zone_num) 

# get name of zone
zone_name <- glue::glue('LFz{zone_num}_{gsub(" ", "", zone$ZONE_NAME)}')

#project
zone <- terra::project(zone, landfire_crs)

# Optional subset
#-----------------------------------------------------#
if (!is.na(aoi_path)) {
  # load aoi subset 
  aoi <- vect(aoi_path) %>%
    project(landfire_crs)
  
  # reassign
  zone <- aoi
  zone_name <- aoi_name
  print("using input shapefile as AOI")
} else{
  print("using landfire zone as AOI")
}

rm(LF_zones)

## VEGETATION
# ###################################################
#-------------------------------------------------------#

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

# Load data
#---------------------------------------------#

# load layers of interest - veg
evc <- terra::rast(evc_path)
evh <- terra::rast(evh_path)
evt <- terra::rast(evt_path)

# set EVT to display EVT_Gp - many layers
activeCat(evt) <- 7 # evt_gp

# get evt levels - to reclassify evt to evt_gp
evt_levels <- levels(evt)[[1]] %>%
  mutate(EVT_GP = as.numeric(EVT_GP))

# project zone to same crs 
zone %<>% terra::project(crs(evc))

## crop to zone and project to desired end crs
evt %<>% terra::crop(zone, mask = TRUE) %>%
  terra::project(landfire_crs)
evh %<>% terra::crop(zone, mask = TRUE) %>%
  terra::project(landfire_crs)

# for evc: crop to zone and project, plus reclassifiy 
evc %<>% terra::crop(zone, mask = TRUE) %>%
  terra::classify(evc_forest_codes_mat, right = NA) %>% # reclassify EVC: subset to only forested pixels
  terra::project(landfire_crs)

# convert zone vector to raster - to help match extents
zone_r <- terra::rasterize(zone, evt) %>%
  terra::project(landfire_crs)

## crop to zone again - this time by raster
evc %<>% terra::crop(zone_r, mask = TRUE)
evt %<>% terra::crop(zone_r, mask = TRUE)
evh %<>% terra::crop(zone_r, mask = TRUE)

# Prepare EVT_GP layer and remap table 
# ---------------------------------------- #

evt_gp <- terra::mask(evt, evc) %>%   # apply forest mask to EVT layer
    terra::classify(evt_levels) %>%     # reclass from EVT to EVT_GP
  # reclassify a few EVT_GPS -
    # there are only a few px for some of the EVT_GPs
    # and/or no plots that keyed to these EVGs
    terra::classify(cbind(evt_gps_na, NA)) 


# create reference files to connect EVT_GP with remapped group
evt_gp_list <- unique(evt_gp)
evg_remap_table <- data.frame(EVT_GP = evt_gp_list, 
                          EVT_GP_remap = seq(1:nrow(evt_gp_list)))

# remap EVT_GP raster
evt_gp <- terra::classify(evt_gp, evg_remap_table) 

# export remap table
write.csv(evg_remap_table, 
          glue::glue('{target_dir_z}/EVG_remap_table.csv'), row.names = FALSE)

gc()

# Apply forest mask to remaining vegetation layers
# ---------------------------------------- #

#use EVT_GP raster to mask EVC
evc <- terra::mask(evc, evt_gp)

# use EVT_GP raster to mask EVH
# then reclassify EVH raster to 2014 conventions
evh <- terra::mask(evh, evt_gp) %>%
  terra::classify(evh_class_mat, 
                  right = NA)


# Export veg rasters
# --------------------------------------- #
print(glue('exporting vegetation rasters for zone {zone_num}'))


# export canopy cover
writeRaster(evc, 
            glue::glue('{target_dir_z}/canopy_cover.tif'),
            overwrite = TRUE)

# canopy height
writeRaster(evh, 
            glue::glue('{target_dir_z}/canopy_height.tif'),
            overwrite = TRUE)

# evt_gp
writeRaster(evt_gp, 
            glue::glue('{target_dir_z}/EVT_GP.tif'),
            overwrite = TRUE)


#remove unused layers  - keep evt_gp for masking other layers
rm(evc, evh, evt)
gc()

## TOPOGRAPHY
####################################################
#-------------------------------------------------------#

# Load topo layers
elev <- terra::rast(elev_path)
slp <- terra::rast(slopeD_path)
asp <- terra::rast(asp_path)

## Crop to zone and mask to forested px
#------------------------------------------------------#

# project zone_r
zone_r %<>% terra::project(crs(elev))

# crop, get to desired end projection, mask 
elev <- terra::crop(elev, zone_r) %>%
  terra::project(landfire_crs) %>%
  terra::mask(evt_gp)
slp <- terra::crop(slp, zone_r) %>%
  terra::project(landfire_crs) %>%
  terra::mask(evt_gp)
asp <- terra::crop(asp, zone_r) %>%
  terra::project(landfire_crs) %>%
  terra::mask(evt_gp)

# Calculate Northing and Easting from Aspect
# -------------------------------------------- #

north <- terra::app(asp, function(i) cos((pi/180)*i))
east <- terra::app(asp, function(i) sin((pi/180)*i))


# Export topo layers
# -------------------------------------------- #

print(glue('exporting topo rasters for zone {zone_num}'))

writeRaster(elev, 
            glue::glue('{target_dir_z}/ELEV.tif'),
            overwrite = TRUE)
writeRaster(slp, 
            glue::glue('{target_dir_z}/SLOPE.tif'),
            overwrite = TRUE)
writeRaster(asp, 
            glue::glue('{target_dir_z}/ASPECT.tif'),
            overwrite = TRUE)
writeRaster(north, 
            glue::glue('{target_dir_z}/NORTHING.tif'),
            overwrite = TRUE)
writeRaster(east, 
            glue::glue('{target_dir_z}/EASTING.tif'),
            overwrite = TRUE)

#remove layers
rm(elev, slp, asp, north, east)
gc()


# ## DISTURBANCE
# # ###################################################
# 
# # # re-load evt_gp as mask
# # evt_gp <- terra::rast(glue::glue('{target_dir_z}/EVT_GP.tif'))
#  
# # # convert zone vector to raster - to help match extents
# # zone_r <- terra::rasterize(zone, evt_gp) %>%
# #   terra::project(landfire_crs)
# 
# 
# # Load disturbance layers
# #------------------------------------------------------#
# disturb_code_lf <- terra::rast(lf_disturb_code_outpath)
# disturb_year_lf <- terra::rast(lf_disturb_year_outpath)
# 
# disturb_year_lflcms <- terra::rast(lcms_disturb_year_outpath)
# disturb_code_lflcms <- terra::rast(lcms_disturb_code_outpath)
# 
# ## Crop to zone and mask to forested px
# #------------------------------------------------------#
# 
# # project zone_r
# zone_r %<>% terra::project(crs(disturb_code_lf))
# 
# # crop, get to desired end projection, mask 
# disturb_code_lf <- terra::crop(disturb_code_lf, zone_r) %>%
#   terra::project(landfire_crs) %>%
#   terra::mask(evt_gp)
# disturb_year_lf <- terra::crop(disturb_year_lf, zone_r) %>%
#   terra::project(landfire_crs) %>%
#   terra::mask(evt_gp)
# 
# zone_r %<>% terra::project(crs(disturb_code_lflcms))
# 
# disturb_code_lflcms <- terra::crop(disturb_code_lflcms, zone_r) %>%
#   terra::project(landfire_crs) %>%
#   terra::mask(evt_gp)
# disturb_year_lflcms <- terra::crop(disturb_year_lflcms, zone_r) %>%
#   terra::project(landfire_crs) %>%
#   terra::mask(evt_gp)
# 
# ## Export
# #--------------------------------------------------------#
# 
# print(glue('exporting disturbance rasters for zone {zone_num}'))
# 
# writeRaster(disturb_code_lf, 
#             glue::glue('{target_dir_z}/disturb_code_LF.tif'),
#             overwrite = TRUE)
# writeRaster(disturb_year_lf, 
#             glue::glue('{target_dir_z}/disturb_year_LF.tif'),
#             overwrite = TRUE)
# 
# writeRaster(disturb_code_lflcms, 
#             glue::glue('{target_dir_z}/disturb_code_LFLCMS.tif'),
#             overwrite = TRUE)
# writeRaster(disturb_year_lflcms, 
#             glue::glue('{target_dir_z}/disturb_year_LFLCMS.tif'),
#             overwrite = TRUE)
# 
# # Remove input / temp disturbance layers
# 
# rm(disturb_code_lf, disturb_year_lf, disturb_code_lflcms, disturb_year_lflcms)

## BIOPHYS
# ###################################################
#-------------------------------------------------------#

# list layers
bio_list <- list.files(biophys_dir_z, full.names = TRUE)

# load layers
biophys <- terra::rast(bio_list)

# Mask Biophys layers - to forested px for each zone
# -------------------------------------------------------------#

for(i in 1:length(bio_list)) {

  # for testing
  #i = 1

  # load raster
  r <- terra::rast(bio_list[i])

  # crop and mask
  r %<>% terra::crop(evt_gp) %>%
    terra::mask(evt_gp) %>%
    terra::project(landfire_crs)

  # set name for output raster
  r_name <- stringr::str_split(bio_list[[i]], "/")[[1]] # get strings for full path
  r_name <- gsub(".img", "", r_name[length(r_name)]) %>% # get string that refers to the last part of the path
    str_replace(cur_zone_zero, "") %>%
    str_remove("NO") %>%
    toupper() %>% # uppercase
    glue::glue(., "I") # add 'I' to the end to match 2016 X table

  # write out
  writeRaster(r,
              glue::glue('{target_dir_z}/{r_name}.tif'),
              overwrite = TRUE)

  gc()
}

# remove layers
rm(r)
gc()



print(glue('done with zone {zone_num}!'))
toc()



