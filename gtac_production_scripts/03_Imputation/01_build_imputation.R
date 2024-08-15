# TreeMap Imputation
# Based on original script written by Isaac Grenfell, RMRS (igrenfell@gmail.com) 
#   and Karin Riley (karin.riley@usda.gov)
# original script: "rmrs_production_scripts/2016_updated_production_scripts/yai-treemap commented.R"
# Updated script written by Lila Leatherman (Lila.Leatherman@usda.gov)

# Last updated: 8/15/2024

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

# Load X table
# ----------------------------------------------------------#
xtable <- read.csv(xtable_path)

# Load coords table
#-----------------------------------------------------------#
coords <- read.csv(coords_path)

# Load EVT Group Remap table
# ----------------------------------------------------------#
evt_gp_remap_table <- read.csv(evt_gp_remap_table_path)

####################################################################
# Prepare input data
####################################################################

# Get distinct rows
xtable %<>% distinct()

# Prepare plot coordinates
#------------------------------------------------------------------#

coords %<>%
  dplyr::rename("point_x" = ACTUAL_LON,
                "point_y" = ACTUAL_LAT) %>%
  select(-c(STATECD, COUNTYCD, MaxOfINVYR, PLOT))

# join x table with coords into new table : plot_df 
plot_df <- xtable %>%
  left_join(coords, by = "PLT_CN")

#inspect - check that all points have coords
print("number of plots without coordinates:")
print(plot_df %>%
  filter(is.na(point_x)) %>%
  nrow())

# Remap EVT Group
# ---------------------------------#

# Join with evt remap table to reclass EVT_GPs
# And convert EVT-GP to factor
plot_df %<>% 
  left_join(evt_gp_remap_table, by = "EVT_GP") %>%
  dplyr::mutate(EVT_GP_remap = as.factor(EVT_GP_remap)) %>%
  select(-EVT_GP)


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
         disturb_code_bin = factor(disturb_code))


# Replace row names with plot id
#------------------------------------------------------------------#

row.names(plot_df) <- NULL
row.names(plot_df) <- plot_df$tm_id


# Create X table - orig (aka training table)
# ---------------------------------------------------------#

#Create X Table
X_df <- plot_df %>% dplyr::select(all_of(xvars))
row.names(X_df) <- plot_df$tm_id


# Create Y table (aka variables to be predicted)
#-----------------------------------------------------------#

Y_df <- plot_df %>% dplyr::select(all_of(yvars))
row.names(Y_df) <- plot_df$tm_id

#############################################################
## Build and export the model
#############################################################

# Build the random forests model (X=all predictors, Y=EVG, EVC, EVH, disturb_code_bin)
# -----------------------------------------------------------------------#
message("Building imputation model")

set.seed(56789)

yai <- yaImpute::yai(X_df, Y_df,
                     method = "randomForest",
                     ntree = 300,
                     mtry = 5)

# Export model
write_rds(yai, model_path)

# Export X and Y tables
# ------------------------------------------------------#

# include CN in export so tables can be joined back
# also include original disturbance code
# row numbers, aka treemap id, are saved as X, or row number, in these csv outputs
X_df %>%
  mutate(CN = plot_df$plt_cn,
         tm_id = plot_df$tm_id,
         disturb_code = plot_df$disturb_code) %>%
  # remove x and y coords for confidentiality
  select(-c(point_x, point_y)) %>%
  write.csv(., xtable_path_model)

Y_df %>%
  mutate(CN = plot_df$plt_cn,
         tm_id = plot_df$tm_id,
         disturb_code = plot_df$disturb_code) %>%
  write.csv(., ytable_path_model)

###########################################################################
# Compute model accuracy
###########################################################################

# Report model accuracy for Response variables (EVC, EVH, EVG, disturb code)
# ------------------------------------------------------------------------#

message("Computing model accuracy")

#RF summary
RF_sum <- yaiRFsummary(yai)

# for var in response variables, get a full suite of cms 
# and then combine into a list RDS that I can plot the same way I do the others

cms_list <- NULL

for (i in seq_along(yvars)) {
  
  # for testing
  #i = 1
  
  var = yvars[i]
  
  # get random forest model
  rf_in <- yai$ranForest[var]
  
  # get predicted and ref table from RF model and X table
  p_r <- get_pr_RF(rf_in, X_df, var)
  
  # get confusion matrices desired
  cm <- list(eval_cm_function(p_r))
    
  # append to a list to write out
  cms_list <- c(cms_list, cm)
  
}

# name 
names(cms_list) <- yvars

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

# remove objects? 
rm(coords, xtable, plot_df, X_df, Y_df, RF_sum, var, rf_in, p_r, p, cm, cms_list , varImp )

# clear unused memory
gc()
