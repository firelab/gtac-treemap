# TreeMap Imputation
# Original script written by Isaac Grenfell, RMRS (igrenfell@gmail.com) 
#   and Karin Riley (karin.riley@usda.gov)
# Updated script written by Lila Leatherman (Lila.Leatherman@usda.gov)

# Last updated: 4/1/2024

# PART 1: 
# - BUILD x and y tables
# - save x and y tables
# - Build model for a zone
# - Save model validation

# TO DO: 
# - replace coords here (currently  buffered from FIA db) with full coords


##################################################
# Set inputs
###################################################

# Set inputs - from input script
this.path <- this.path::this.path() # Id where THIS script is located

# get path to input script
spl <- stringr::str_split(this.path, "/")[[1]]
input_script_path <- paste( c(spl[c(1:(length(spl)-1))],
                              "00_inputs_for_imp.R" ),
                            collapse = "/")

source(input_script_path)


# Other options
# --------------------------------#

# Allow for sufficient digits to differentiate plot cn numbers

options("scipen" = 100, "digits" = 8)

##########################################################

# Load target rasters - just for metatada
# --------------------------------------------------------------------#

# list raster files
flist_tif <- list.files(path = target_dir, pattern = "*.tif$",
                        recursive = FALSE, full.names = TRUE)

# filter to layers of interest

# load raster files as raster stack
rs2 <- terra::rast(flist_tif[1])

# get raster names 
raster_names <- flist_tif[1] %>%
  str_extract(., "z[0-9][0-9]/([^.])*") %>%
  str_replace("z[0-9][0-9]/", "")

#add names to raster list
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
  write.csv(., glue::glue("{output_dir}/xytables/{cur_zone_zero}_{output_name}_Xdf_bin.csv"))

Y_df %>%
  mutate(CN = plot_df$CN) %>%
  write.csv(., glue::glue("{output_dir}/xytables/{cur_zone_zero}_{output_name}_Ydf_bin.csv"))


# Build the random forests model (X=all predictors, Y=EVG, EVC, EVH, disturb_code)
# -----------------------------------------------------------------------#
set.seed(56789)

tic()
yai_treelist_bin <- yai(X_df, Y_df,
                        method = "randomForest",
                        ntree = 250)
toc()

# Export model
write_rds(yai_treelist_bin, model_path)


# Report model accuracy for Y variables (EVC, EVH, EVG)
# ------------------------------------------------------------------------#

#RF summary
RF_sum <- yaiRFsummary(yai_treelist_bin)

# Confusion matrices
cm_EVC <- yai_treelist_bin$ranForest$canopy_cover$confusion
cm_EVH <- yai_treelist_bin$ranForest$canopy_height$confusion
cm_EVT_GP <- yai_treelist_bin$ranForest$EVT_GP$confusion
cm_DC <- yai_treelist_bin$ranForest$disturb_code$confusion

# variable importance
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
ggsave(glue::glue("{output_dir}/model_eval/{output_name}_varImp.png"),
       width = 7, height = 5)
write.csv(cm_EVC,
          glue::glue("{output_dir}/model_eval/{output_name}_CM_canopyCover.csv"))
write.csv(cm_EVH,
          glue::glue("{output_dir}/model_eval/{output_name}_CM_canopyHeight.csv"))
write.csv(cm_EVT_GP,
          glue::glue("{output_dir}/model_eval/{output_name}_CM_EVT_Group.csv"))
write.csv(cm_DC,
          glue::glue("{output_dir}/model_eval/{output_name}_CM_DisturbanceCode.csv"))

# clear unused memory
gc()
