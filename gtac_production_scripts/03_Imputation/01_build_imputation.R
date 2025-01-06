# TreeMap Imputation
# Based on original script written by Isaac Grenfell, RMRS (igrenfell@gmail.com) and Karin Riley (karin.riley@usda.gov)
# original script: "rmrs_production_scripts/2016_updated_production_scripts/yai-treemap commented.R"
# Updated script written by Lila Leatherman (Lila.Leatherman@usda.gov)
# With contributions from Abhinav Shrestha (abhinav.shrestha@usda.gov) and Scott Zimmer (scott.zimmer@usda.gov)

# Last updated: 12/30/2024

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
  terra::project(zone_output_crs)

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

# Inspect input variables - check for values in expected ranges
#-------------------------------------------------------------------#

##### PLOT
message(glue::glue("plotting x table variables for pre-model QA: exporting to {raw_outputs_dir}"))

facet_n = ceiling(sqrt(length(xvars)))

png(glue::glue("{raw_outputs_dir}/model_eval/{cur_zone_zero}_xtable_summary.png"),
    width =2000, height = 2000)

par(mar=c(3,3,3,3),
    mfrow = c(facet_n, 4))

for(i in 1:length(xvars)) {
  
  hist(as.numeric(plot_df[,xvars[i]]),
       xlab = "n",
       ylab = xvars[i],
       main = xvars[i])
  
}
dev.off()


#### EXPORT CSV USING SKIMR package
skim_out <- plot_df %>%
  mutate(evt_gp_remap = as.numeric(evt_gp_remap),
         disturb_code_bin = as.numeric(disturb_code_bin)) %>%
  skimr::skim()%>%
  select(skim_variable, 
         numeric.mean, numeric.sd, numeric.p0, numeric.p25, numeric.p50, numeric.p75, numeric.p100) %>%
  filter(skim_variable %notin% c("tm_id", "zone", "plt_cn")) %>%
  as_tibble() %>%
  rename("variable" = skim_variable) %>%
  rename_with(., ~ gsub("numeric.", "", .x))

#str(skim_out)

write.csv(skim_out, glue::glue("{raw_outputs_dir}/model_eval/{cur_zone_zero}_xtable_summary.csv"), row.names = FALSE)

# Create X table - orig (aka training table)
# ---------------------------------------------------------#

#Create X Table
X_df <- plot_df %>% dplyr::select(all_of(xvars))
row.names(X_df) <- plot_df$tm_id


# Create Y table (aka response variables)
#-----------------------------------------------------------#

Y_df <- plot_df %>% dplyr::select(all_of(yvars))
row.names(Y_df) <- plot_df$tm_id

#############################################################
## Build and export the model
#############################################################

# Build the random forests model (X=all predictors, Y= evc, evh, evt_gp_remap,disturb_code_bin)
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

# Add fields to include in exported table: 
# CN
# tm_id
# disturb_code (original, non-binary disturbance code)
# evt_gp (original, non-remapped evt gp)
# include CN in export so tables can be joined back
# also include original disturbance code and non-remapped evt_gp

X_df %>%
  mutate(CN = plot_df$plt_cn,
         tm_id = plot_df$tm_id,
         disturb_code = plot_df$disturb_code, 
         evt_gp = plot_df$evt_gp) %>%
  # remove x and y coords from export
  select(-c(point_x, point_y)) %>%
  write.csv(., xtable_path_model)

Y_df %>%
  mutate(CN = plot_df$plt_cn,
         tm_id = plot_df$tm_id,
         disturb_code = plot_df$disturb_code,
         evt_gp = plot_df$evt_gp) %>%
  write.csv(., ytable_path_model)

#########################################################################
# Compute model accuracy
###########################################################################

# Report model accuracy for Response variables (evc, evh, evt_gp_remap,disturb_code_bin)
# ------------------------------------------------------------------------#

message("Computing model accuracy")

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
  
  if (var == "evt_gp_remap"){
  
    p_r_new <- p_r # initialize dataframe with original dataframe (to maintain shape)
    p_r_new[] <- evt_gp_remap_table$EVT_GP[match(unlist(p_r), evt_gp_remap_table$EVT_GP_remap)]
    p_r <- p_r_new # pass new remapped dataframe to original df variable
    
  }
  
  # get confusion matrices desired
  cm <- list(eval_cm_function(p_r))
    
  # append to a list to write out
  cms_list <- c(cms_list, cm)
  
}


# name 
names(cms_list) <- yvars
names(cms_list)[which(names(cms_list) == "evt_gp_remap")] <- "evt_gp" # change name back to evt_gp

saveRDS(cms_list, file = glue::glue("{raw_outputs_dir}/model_eval/{output_name}_CMs_ResponseVariables.RDS"))


# Calculate variable importance
#------------------------------------------------------------#

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


# Clean up
#-----------------------------------------------------------------#

# remove objects 
rm(coords, xtable, plot_df, X_df, Y_df, RF_sum, var, rf_in, p_r, p, cm, cms_list , varImp )

# clear unused memory
gc()
