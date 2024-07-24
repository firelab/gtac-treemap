# TreeMap Imputation
# Based on original script written by Isaac Grenfell, RMRS (igrenfell@gmail.com) 
#   and Karin Riley (karin.riley@usda.gov)
# original script: "rmrs_production_scripts/2016_updated_production_scripts/yai-treemap commented.R"
# Updated script written by Lila Leatherman (Lila.Leatherman@usda.gov)

# Last updated: 7/15/2024

# PART 1: 
# - BUILD x and y tables
# - save x and y tables
# - Build model for a zone
# - Save model validation

##################################################
# Set inputs
###################################################

# Set inputs - from input script
# Uncomment this if running a single zone at a time, outside of a loop for all zones
#--------------------------------------------#

# this_dir <- this.path::this.dir()
# 
# inputs_for_imputation<- glue::glue('{this_dir}/00b_zonal_inputs_for_imp.R')
# source(inputs_for_imputation)
  
# Other options
# --------------------------------#

# Allow for sufficient digits to differentiate plot cn numbers

options("scipen" = 100, "digits" = 8)

##########################################################

message("Loading data for imputation")

# Load target rasters - just for metatada
# --------------------------------------------------------------------#

# list raster files
flist_tif <- list.files(path = target_dir, pattern = "*.tif$",
                        recursive = TRUE, full.names = TRUE)

# filter to layers of interest
#inspect
#flist_tif

# load raster files
rs2 <- terra::rast(flist_tif[5])

# get raster names 
raster_names <- flist_tif[5] %>%
  str_extract(., "z[0-9][0-9]/([^.])*") %>%
  str_replace("z[0-9][0-9]/", "")

#add name to raster 
names(rs2) <- raster_names

# Load X table
# ----------------------------------------------------------#

allplot <- read.csv(xtable_path)

# Load coords table
#-----------------------------------------------------------#
coords <- read.csv(coords_path)

# Load EVT Group Remap table
# ----------------------------------------------------------#

evt_gp_remap_table <- read.csv(evt_gp_remap_table_path)

####################################################################
# Prepare input data
####################################################################

# Prepare plot coordinates
#------------------------------------------------------------------#

coords %<>%
  dplyr::rename("POINT_X" = ACTUAL_LON,
                "POINT_Y" = ACTUAL_LAT) 

coords_sp <- terra::vect(coords, geom = c("POINT_X", "POINT_Y"),
                         crs = "epsg:5070")

allplot %<>%
  left_join(coords, by = c("CN" = "PLT_CN"))

# #inspect
# allplot %>%
#   filter(!is.na(POINT_X)) %>%
#   nrow()


# Obtain backup plot coordinates
#----------------------------------------------------------#
# NOTE: this is currently a stand-in. the allplot table has abbreviated records of the lat and long 
# (to 2 decimal places)

# convert allplot to spatial object
allplot_vect <- terra::vect(allplot, geom = c("ACTUAL_LON", "ACTUAL_LAT"),
                            crs = "epsg:5070")

# extract lat/long in meters
allplot_xy <- terra::geom(allplot_vect) %>%
  data.frame() %>%
  dplyr::rename("POINT_X_backup" = x,
                "POINT_Y_backup" = y) %>%
  dplyr::select(POINT_X_backup, POINT_Y_backup)

# bind back with allplot table
allplot <- cbind(allplot, allplot_xy)

# Fill in plot coordinates where they aren't available
#-----------------------------------------------------------------#
allplot %<>%
  mutate(POINT_X = ifelse(is.na(POINT_X), POINT_X_backup, POINT_X),
         POINT_Y = ifelse(is.na(POINT_Y), POINT_Y_backup, POINT_Y)) %>%
  select(-c(POINT_X_backup, POINT_Y_backup))

# Remap EVT Group
# ---------------------------------#

#Limit allplot to just the veg types in the remap table
plot_df <- allplot[allplot$EVT_GP %in% evt_gp_remap_table$EVT_GP,]

####Reclass evgs
n_evgs <- nrow(evt_gp_remap_table)

#reassign object to evg.reclass
evg_reclass <- evt_gp_remap_table %>%
  dplyr::select(EVT_GP, EVT_GP_remap)

#remap evgs using vector
evg_out <- rep(0, dim(plot_df)[1])
evg_vec <- plot_df$EVT_GP
for (i in 1:n_evgs) {
  cur_evg <- evg_reclass[i, 1]
  sub_ind <- evg_vec == cur_evg
  evg_out[sub_ind] <- i
}

# re-assign EVT_GP
plot_df$EVT_GP <- evg_out

#Ensure plot_df variables are factors
#--------------------------------------#

plot_df %<>%
  mutate(EVT_GP = factor(EVT_GP))

# Re-calculate aspect - to northing and easting
#----------------------------------------------------------#
plot_df %<>%
  dplyr::mutate(radians = (pi / 180) * ASPECT,
                NORTHING = cos(radians),
                EASTING = sin(radians)) %>%
  dplyr::select(-radians)


# Calculate binary disturbance code and convert to factor
#----------------------------------------------------------#
plot_df %<>%
  mutate(disturb_code = ifelse(disturb_code > 0, 1, disturb_code),
         disturb_code = factor(disturb_code))



# Create X table - orig (aka training table)
# ---------------------------------------------------------#

#Create X Table
X_df <- plot_df %>% dplyr::select(SLOPE, ELEV, PARI, PPTI, RELHUMI,
                                  TMAXI, TMINI, VPDI, disturb_code,
                                  disturb_year, canopy_cover, canopy_height,
                                  EVT_GP, NORTHING, EASTING, POINT_X, 
                                  POINT_Y)

# add plot id as row names
rownames(X_df) <- plot_df$ID


# Create Y table (aka variables to be predicted)
Y_df <- plot_df %>%
  dplyr::select(canopy_cover, canopy_height, EVT_GP, disturb_code)

rownames(Y_df) <- plot_df$ID


# Export X and Y tables
# ------------------------------------------------------#

#include CN in export so tables can be joined back 
X_df %>%
  mutate(CN = plot_df$CN) %>%
  write.csv(., glue::glue("{raw_outputs_dir}/xytables/{output_name}_Xdf_bin.csv"))

Y_df %>%
  mutate(CN = plot_df$CN) %>%
  write.csv(., glue::glue("{raw_outputs_dir}/xytables/{output_name}_Ydf_bin.csv"))


# Build the random forests model (X=all predictors, Y=EVG, EVC, EVH, disturb_code)
# -----------------------------------------------------------------------#
message("Building imputation model")

set.seed(56789)

yai <- yaImpute::yai(X_df, Y_df,
                        method = "randomForest",
                        ntree = 250,
                        oob = TRUE # this option only available in the dev version of yaImpute found here: https://github.com/jeffreyevans/yaImpute
                        )

# Export model
write_rds(yai, model_path)


# Report model accuracy for Response variables (EVC, EVH, EVG, disturb code)
# ------------------------------------------------------------------------#

message("Computing model accuracy")

#RF summary
RF_sum <- yaiRFsummary(yai)

# for var in response variables, get a full suite of cms 
# and then combine into a list RDS that I can plot the same way I do the others
response_vars <- names(Y_df)

cms_list <- NULL

for (i in seq_along(response_vars)) {
  
  # for testing
  #i = 3
  
  var = response_vars[i]
  
  # get random forest model
  rf_in <- yai$ranForest[var]
  
  # get predicted and ref table from RF model and X table
  p_r<- get_pr_RF(rf_in, X_df)
  
  # get confusion matrices desired
  cm <- eval_cm_function(p_r)
  
  # append to a list to write out
  cms_list <- c(cms_list, cm)
  
}

# name 
names(cms_list) <- response_vars

saveRDS(cms_list, file = glue::glue("{raw_outputs_dir}/model_eval/{output_name}_CMs_ResponseVariables.RDS"))


#########################

# Get variable importance
varImp <- data.frame(RF_sum$scaledImportance)

# process variable importance table for plotting
varImp$outVar <- rownames(varImp)
rownames(varImp) <- NULL

varImp %<>% 
  tidyr::pivot_longer(1:ncol(varImp) - 1, names_to = "var")

#plot variable importance
p <- varImp %>%
  ggplot() +
  geom_col(aes(x = var, y = value)) +
  coord_flip() +
  facet_wrap(~outVar) +
  theme_bw() +
  ggtitle(glue::glue("RF Variable Importance for {cur_zone_zero}"))


# export to file
ggsave(glue::glue("{raw_outputs_dir}/model_eval/{output_name}_varImp.png"),
       width = 7, height = 5)


# clear unused memory
gc()

# remove objects? 
rm(allplot, allplot_vect, allplot_xy, cm, cms_list, coords, coords_sp, evg_reclass, p, p_r, rf_in, RF_sum, X_df, Y_df, varImp, plot_df)