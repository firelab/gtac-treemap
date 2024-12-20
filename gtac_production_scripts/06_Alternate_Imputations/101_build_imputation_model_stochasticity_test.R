# TreeMap Imputation
# Based on original script written by Isaac Grenfell, RMRS (igrenfell@gmail.com) and Karin Riley (karin.riley@usda.gov)
# original script: "rmrs_production_scripts/2016_updated_production_scripts/yai-treemap commented.R"
# Updated script written by Lila Leatherman (Lila.Leatherman@usda.gov)
# With contributions from Abhinav Shrestha (abhinav.shrestha@usda.gov) and Scott Zimmer (scott.zimmer@usda.gov)

# Last updated: 8/28/2024

# This script accomplishes the following tasks: 
# - BUILD and save x and y tables
# - Build and save model for a zone
# - Execute and save model validation

##################################################
# Set inputs
###################################################

# all inputs are set by zone and year in the 00b_zonal_inputs_for_imp.R script

##########################################################

message("Loading data for imputation")

####################################################################
# Prepare input data
####################################################################

# Load X table
# ----------------------------------------------------------#
xtable <- read.csv(xtable_path) %>%
  # Get distinct rows 
  distinct()


# Prepare plot coordinates
#------------------------------------------------------------------#

# Load coords table
coords <- read.csv(coords_path)

# project coords into same coordinate system 
coords <- terra::vect(coords, geom = c("ACTUAL_LON", "ACTUAL_LAT"), crs = "epsg:4269") %>%
  terra::project(output_crs)

# reassign to coords object
coords <- cbind(data.frame(coords), data.frame(terra::geom(coords)))

# select fields of interest
coords %<>%
  dplyr::rename("point_x" = x,
                "point_y" = y) %>%
  select(PLT_CN, point_x, point_y)

# Join x table with coords into new table : plot_df 
#------------------------------------------------------------#

# create plot_df
plot_df <- xtable %>%
  left_join(coords, by = "PLT_CN")

#inspect - check that all points have coords
message(glue::glue("number of plots without coordinates: {plot_df %>% filter(is.na(point_x)) %>% nrow()}"))


# Prep EVT Group
# ---------------------------------#

# Load EVT Group Remap table
evt_gp_remap_table <- read.csv(evt_gp_remap_table_path) 

# Join with evt remap table to reclass EVT_GPs
# And convert EVT-GP to factor
plot_df %<>% 
  left_join(evt_gp_remap_table, by = "EVT_GP") %>%
  dplyr::mutate(EVT_GP_remap = as.factor(EVT_GP_remap)) 


# Address issues with slope and aspect
#----------------------------------------------------------#
plot_df %<>%
  # calculate northing and easting from aspect
  dplyr::mutate(radians = (pi / 180) * ASPECT,
                NORTHING = cos(radians),
                EASTING = sin(radians)) %>%
  # convert slope from percent to degrees, to match target layer
  dplyr::mutate(SLOPE = atan(SLOPE / 100) * 180 / pi)

#Address no aspect issue by setting easting and northing to 0 anywhere with 0 slope and 0 aspect
plot_df$EASTING[plot_df$SLOPE == 0 & plot_df$ASPECT == 0]<- 0
plot_df$NORTHING[plot_df$SLOPE == 0 & plot_df$ASPECT == 0]<- 0

# Rename all other vars
#----------------------------------------------------------#

# Because target layers for 2020/22 are all lower case, change field names to all lower case
# And change other necessary column names
plot_df %<>%
  dplyr::rename_with(tolower) %>%
  dplyr::rename("elevation" = elev,
                "evc" = canopy_cover,
                "evh" = canopy_height)

# Calculate binary disturbance code and convert to factor
#----------------------------------------------------------#
plot_df %<>%
  mutate(disturb_code_bin = ifelse(disturb_code > 0, 1, disturb_code),
         disturb_code_bin = factor(disturb_code_bin)
  )

# Replace row names with plot id
#------------------------------------------------------------------#

row.names(plot_df) <- NULL
row.names(plot_df) <- plot_df$tm_id


# Create X table - orig (aka training table)
# ---------------------------------------------------------#

#Create X Table
X_df <- plot_df %>% dplyr::select(all_of(xvars))
row.names(X_df) <- plot_df$tm_id


# Create Y table (aka response variables)
#-----------------------------------------------------------#

Y_df <- plot_df %>% dplyr::select(all_of(yvars))
row.names(Y_df) <- plot_df$tm_id

# Get ready to build and export models ----

# Make directories
dir.create(gsub("model/","model_stochasticity_test",model_dir))
dir.create(gsub("model/","model_stochasticity_test/model/",model_dir))
dir.create(gsub("model/","model_stochasticity_test/cms",model_dir))


message("Building imputation models and computing model accuracy")


model_nums<- seq(1,20,1)

for(j in seq_along(model_nums)){
  
  set.seed(j)
  assign(paste0("yai",j), yaImpute::yai(X_df, Y_df,
                                        method = "randomForest",
                                        ntree = 300,
                                        mtry = 5))
  
  yai<- eval(parse(text=paste0("yai",j)))

  #RF summary
  RF_sum <- yaiRFsummary(yai)

  # for var in response variables, get a full suite of cms 
  # and then combine into a list RDS that I can plot the same way I do the others

  cms_list <- NULL

  for (i in seq_along(c(yvars))) {
  
    # for testing
    #i = 3
  
    var = yvars[i]
  
    # get random forest model
    rf_in <- yai$ranForest[var]
  
    # get predicted and ref table from RF model and X table
    #p_r <- get_pr_RF(rf_in, X_df, var)
    preds <- rf_in[[var]]$predicted
    refs <- rf_in[[var]]$y
    p_r <- as.data.frame(cbind(preds, refs))
  
    # get confusion matrices desired
    cm <- list(eval_cm_function(p_r))
  
    # append to a list to write out
    cms_list <- c(cms_list, cm)
  
  }


  # name 
  names(cms_list) <- yvars
  assign(paste0("cms_list",j), cms_list)
  
  # Save out
  saveRDS(yai, gsub("model/",paste0("model_stochasticity_test/model/rf_model_",j,".rds"),model_dir))
  
  #
  print(paste0("Finished model ",j, " of ", max(model_nums)))
}


# Get names of all cms
cms_names <- paste0("cms_list", model_nums)

# Assemble all cms
all_cms<- lapply(cms_names, get)


# Save out
saveRDS(all_cms, gsub("model/","model_stochasticity_test/cms/all_cms.R",model_dir))


# Clean up
#-----------------------------------------------------------------#

# remove objects 
rm(coords, xtable, plot_df, X_df, Y_df, RF_sum, var, rf_in, p_r, cm, cms_list)

# clear unused memory
gc()
