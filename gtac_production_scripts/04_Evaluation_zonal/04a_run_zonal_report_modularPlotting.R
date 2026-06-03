# Generate zonal report from Rmd file
# Written by Lila Leatherman (Lila.Leatherman@usda.gov)
# Updated by Abhinav Shrestha (abhinav.shrestha@usda.gov)

# Last updated: 12/23/2024

#==========================================================#
#                                                          # 
#         Specific inputs (CHANGE VARIABLES HERE)          #  
#                                                          #    
#==========================================================#

# IF RUNNING STANDALONE: 
#------------------------------------------#
standalone <- "N"
cur_zone_zero_standalone <- "z01"
year_standalone <- 2020
project_standalone <- glue::glue("{year_standalone}_Production_newXtable")
eval_type_standalone <- "TargetLayerComparison"



# VARIABLES TO EVALUATE
#------------------------------------------#

# list variables to evaluate
# - confusion matrices (CMs) for these variables are calculated in the 01-03 scripts
# eval_vars_cat <- yvars
# list names of attribute vars to evaluate - these come from RAT table or similar; are not included in imputation
# attributevars <- c("BALIVE", "GSSTK", "QMD_RMRS", "SDIPCT_RMRS",
#                    "CANOPYPCT", "CARBON_D", "CARBON_L", "CARBON_DOWN_DEAD",
#                    "TPA_DEAD", "TPA_LIVE")
eval_vars_cat <- c("evc", "evh", "evt_gp", "disturb_code_bin", "disturb_code", "disturb_year" ) # without "evt_gp_remap"
eval_vars_cat_cont <- c(eval_vars_cat, attributevars) 
#eval_vars_cat_cont <- eval_vars_cat

# Eval report for OOB or derived vars
# Options: "model_eval", "TargetLayerComparison", "OOB_manual", "CV"

eval_type <- eval_type_in
#eval_type <- "TargetLayerComparison"
#eval_type <- "OOB" 
#eval_type <- "CV"

# PLOTS and TABLES TO INCLUDE; EXPORT OPTIONS
#---------------------------------------------#

# TABLES
#~~~~~~~~~~~#

# Table of parameters used in the zonal imputation
include_paramTable <- "Y" # (Y/N)


# PLOTS
#~~~~~~~~~~~#

# Bar plot of raw frequency; observed vs. predicted for each class
barPlot_raw <- "Y" # (Y/N)

# Bar plot of normalized frequency; observed vs. predicted vs. for each class
barPlot_norm <- "Y" # (Y/N)

# Scatterplot of # class in reference data vs. classwise accuracy
scatterPlot_refVClassAcc <- "Y" # (Y/N)

# Scatterplot: Total obs of class vs. classwise accuracy
scatterPlot_obsVClassAcc <- "Y" # (Y/N)


# SAVE PLOTS and EXPORT REPORT
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

save_Plot <- "F" # Save plots as png (T/F)

exportDir <- "path-to-export" # Set path to export plots to. Required ONLY if `save_Plot == T`.

report_format <- "word_document" # options: pdf_document, word_document, html_document

document_formatExtensionDict <- c("pdf_document" = ".pdf", 
                                  "word_document" = ".docx", 
                                  "html_document" = ".html")


#-------------------------------------------#
#                                           #
# Standard inputs (less likely to change)   #
#                 ~~~~~~~~~~~~~~~~~~~~~~    #
#-------------------------------------------#


# Other settings
#------------------------------------------#

# set number of digits to round to
round_dig <- 4


# Other options
# --------------------------------#

# Allow for sufficient digits to differentiate plot cn numbers

options("scipen" = 100, "digits" = 8)

# Set inputs manually - if running standalone
#-----------------------------------------------------#

if(standalone == 'Y') {
  
  # assign main variables
  cur_zone_zero <- cur_zone_zero_standalone
  year <- year_standalone
  eval_type <- eval_type_standalone
  
  # load directories
  this_proj <- this.path::this.proj()
  this_dir <- this.path::this.dir()
  
  ## load treemap library
  lib_path = glue::glue('{this_proj}/gtac_production_scripts/00_Library/treeMapLib.R')
  source(lib_path)
  
  # load(project_settings)
  
  #load settings for zone
  zone_settings <- glue::glue("{home_dir}/03_Outputs/07_Projects/{project_standalone}/01_Raw_model_outputs/{cur_zone_zero}/params/{cur_zone_zero}_{project_standalone}_env.RDS")
  
  load(zone_settings)
  
  # load library again in case functions have been updated since initial creation of RDS
  #source(lib_path)
}

# Prep constructed paths
#------------------------------------------#

this_dir <- this.path::this.dir()

# get path to rmd
rmd_path <- glue::glue("{this_dir}/04b_zonal_eval_report_generator_modularPlotting.Rmd")

# set dir for temporary outputs - needs to be a place w/ write permissions for R (network drives aren't allowed)
tmpout_dir <- tmp_dir

# set default plot labels - these will be updated for each evaluation type
plot_labels <- c("Imputed", "Observed")

# set default labels for confusion matrix - these will be updated for each evaluation type
cm_labels <- c("Predicted", "Reference")


#===========================================================#
#                                                           #
# EVAL REPORT GENERATION SCRIPT START (DO NOT EDIT BELOW)   #
#                                     ~~~~~~~~~~~~~~~~~~~   #
#===========================================================#


# Load evaluation data
#------------------------------------------#

# load raw imputation output raster
ras <- terra::rast(glue::glue("{assembled_dir}/01_Imputation/{output_name}_Imputation.tif"))

# Unique plots imputed in zone
unique_pltsZone <- length(freq(ras)$value)

# conditional loads and variables based on evaluation type: 

#   - load RDS of cm files
#   - label reference type
#   - make labels for plots - Predicted, Reference

if(eval_type == "TargetLayerComparison") {
    
    cms_path <- glue::glue("{eval_dir}/01_Target_Layer_Comparison/{output_name}_CMs_{eval_type}.RDS")
    plot_labels <- c("Imputed", "Target")
    cm_labels <- c("Imputed", "Target")
    
} else if(eval_type == "OOB_manual") {
  
  cms_path <- glue::glue("{eval_dir}/02_OOB_Manual_Evaluation/{output_name}_CMs_{eval_type}.RDS")
  plot_labels <- c("Imputed (OOB)", "Observed (RAT; FIA)")
  cm_labels <- c("Imputed (OOB)", "Observed (RAT; FIA)")
  
} else if(eval_type == "model_eval") {
  
  cms_path <- glue::glue("{raw_outputs_dir}/model_eval/{output_name}_CMs_ResponseVariables.RDS")
  plot_labels <- c("Predicted - RF", "Reference - RF")
  cm_labels <- c("Predicted - RF", "Reference - RF")
  
} else if(eval_type == "CV") {
    cms_path <- glue::glue("{eval_dir}/03_Cross_Validation/{output_name}_CMs_{eval_type}.RDS")
    plot_labels <- c("Imputed (CV)", "Observed (RAT; FIA)")
    cm_labels <- c("Imputed (CV)", "Observed (RAT; FIA)")
  }
  
cms_all <- readRDS(cms_path)

# Load Landfire Zones
#------------------------------------------#

# load LF zone data
LF_zones <- terra::vect(zones_path)

# select single LF zone
zone <- terra::subset(LF_zones, LF_zones$ZONE_NUM == zone_num)

#project
zone <- terra::project(zone, crs(ras))

# get name of zone
zone_name <- glue::glue("LFz{zone_num}_{gsub(' ', '', zone$ZONE_NAME)}")

# Load EVT metadata
#------------------------------------------#
evt_gp_metadata <- read.csv(glue::glue("{home_dir}/01_Data/02_Landfire/LF_230/Vegetation/EVT/LF2022_EVT_230_CONUS/CSV_Data/LF22_EVT_230.csv")) %>% 
  rename_with(tolower) %>%
  dplyr::select(evt_gp, evt_gp_n) %>%
  dplyr::mutate(evt_gp = factor(evt_gp),
                # make a shortened evt_gp_name 
                evt_gp_n_short = paste0(evt_gp, "-", substr(evt_gp_n, 1, 15)) ) %>% 
  arrange(evt_gp) %>%
  distinct()


# Load X table
#------------------------------------------#

# load X_df 
X_df <- read.csv(xtable_path_model)

# Plots available in zone
numPltsZone_XdfModel <- nrow(X_df)

# Percent of available plots imputed
percent_avlbPlts_imputed <- round((100 * (unique_pltsZone/numPltsZone_XdfModel)), 2)

# Load raster attribute table and points
#------------------------------------------#

message("Importing raster attribute table...")
# Using function in 00_Library/load_RAT.R
rat <- load_RAT(rat_path, 
                CN_column = "PLT_CN", 
                ID_column = "TM_ID")

# Join RAT and X_df into rat_x
#-----------------------------------------------------------#

X_df %<>% 
  dplyr::rename("TM_ID" = tm_id)

# join RAT with X df using CN - because TM_ID varies between versions of TreeMap
# joining by TM_ID will cause an error if joining with an older RAT
rat_x <- rat %>%
  select(-TM_ID) %>%
  right_join(X_df, by = "CN") %>%
  select(c(CN, TM_ID, all_of(eval_vars_cat_cont))) %>%
  # filter to plots with values
  #filter(!is.na(BALIVE)) %>%
  arrange(TM_ID)

# Spatial Filter for RAT - Include only points that fall within the zone
#--------------------------------------------------------------------------#

# load model to get xy data
yai <- readRDS(model_path)

# get xy data from model
X_xy <- yai$xRefs %>%
  dplyr::select(point_x, point_y) %>%
  tibble::rownames_to_column(var = "TM_ID")

# convert xy data to spatial points
X_pts <- terra::vect(X_xy, geom = c("point_x", "point_y"), crs = zone_output_crs)

# mask to pts within zone
zone_pts <- terra::mask(X_pts, zone)
zone_pts$TM_ID <- as.numeric(zone_pts$TM_ID)

# filter to pts within zone
rat_x <- rat_x %>%
  dplyr::filter(TM_ID %in% as.numeric(zone_pts$TM_ID))

# Create a data.frame to store summaries of the categorical vars seen in `rat_x`
rat_x_catVarsSummary_df <- data.frame(unclass(summary(rat_x[which((names(rat_x) %in% eval_vars_cat))])), check.names = FALSE, stringsAsFactors = TRUE, row.names = NULL)
names(rat_x_catVarsSummary_df) <- str_replace_all(names(rat_x_catVarsSummary_df), c(" " = "")) #remove any spaces in column names


# Calc frequency for all vars in RAT_x
#------------------------------------------#

rat_freq_all <- list()

for (i in seq_along(eval_vars_cat)) {

  var_in = eval_vars_cat[i]

# get frequency table
  f_out <- rat_x %>%
    dplyr::select(all_of(var_in)) %>%
    table() %>%
    data.frame() 
  
  # add normalized frequency
  f_out$Freq_norm = f_out$Freq/sum(f_out$Freq)
  
  # join with other outputs
  rat_freq_all = c(rat_freq_all, list(f_out))
  
}

names(rat_freq_all) <- eval_vars_cat

# # inspect
# rat_freq_all


# Render report
#------------------------------------------#

rmarkdown::render(rmd_path,
                  output_format = report_format,
                  output_file = glue::glue("{output_name}_eval_report_{eval_type}"),
                  output_dir = tmpout_dir,
                  #build df of params to share with rmd
                  params = list(raster_name = output_name,
                                zone_num = zone_num,
                                eval_type = eval_type,
                                eval_vars= eval_vars_cat,
                                cms_path = cms_path, 
                                unique_pltsZone = unique_pltsZone, 
                                numPltsZone_XdfModel = numPltsZone_XdfModel, 
                                percent_avlbPlts_imputed = percent_avlbPlts_imputed, 
                                rat_x_catVarsSummary_df = rat_x_catVarsSummary_df)
)

if(eval_type %in% c("OOB", "CV")){
    file.remove(tmp_figs_list)
}

# File Organization
#------------------------------------------#
#

fileExtension <- document_formatExtensionDict[[report_format]]

message("\n\nMoving rendered report from tmp directory to output directory...")


# move report from tmpout dir to desired out dir
file.copy(from=glue::glue("{tmpout_dir}/{output_name}_eval_report_{eval_type}{fileExtension}"),
          #to= glue::glue("{tmp_output_dir}/"), 
          to = glue::glue("{eval_dir}/04_Eval_Reports/"),
          overwrite = TRUE, recursive = FALSE,
          copy.mode = TRUE)

# remove file from tmp location
file.remove(glue::glue("{tmpout_dir}/{output_name}_eval_report_{eval_type}{fileExtension}"))

message(glue::glue("Report render complete! Output file: {eval_dir}/04_Eval_Reports/{output_name}_eval_report_{eval_type}{fileExtension}\n\n"))

#Sys.time() - ptm

