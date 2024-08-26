# Generate zonal report from Rmd file
# Written by Lila Leatherman (Lila.Leatherman@usda.gov)
# Updated by Abhinav Shrestha (abhinav.shrestha@usda.gov)

# Last updated: 08/14/2024


# TODO:
# add continuous vars to report - from plots already made 
# - for OOB
# - for CV



#==========================================================#
#                                                          # 
#         Specific inputs (CHANGE VARIABLES HERE)          #  
#                                                          #    
#==========================================================#

# VARIABLES TO EVALUATE
#------------------------------------------#

# list variables to evaluate
# - confusion matrices (CMs) for these variables are calculated in the 01-03 scripts
#eval_vars_cat <- yvars
eval_vars_cat <- c(yvars, "disturb_code") #FIXME:, "evt_gp")
eval_vars_cat_cont <- c(eval_vars_cat, attributevars) 

# Eval report for OOB or derived vars
# - options: "TargetLayerComparison" or "OOB" or "CV"

#eval_type <- eval_type_in
eval_type <- "TargetLayerComparison"
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


# Set inputs - from input script used for imputation
#-----------------------------------------------------#

#this_dir <- this.path::this.dir()

#inputs_for_evaluation <- glue::glue('{this_dir}/00_inputs_for_evaluation.R')
#source(inputs_for_evaluation)

# Other settings
#------------------------------------------#

# set number of digits to round to
round_dig <- 4


# Other options
# --------------------------------#

# Allow for sufficient digits to differentiate plot cn numbers

options("scipen" = 100, "digits" = 8)

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

# conditional loads and variables based on evaluation type: 

#   - load RDS of cm files
#   - label reference type
#   - make labels for plots - Predicted, Reference

if(eval_type == "TargetLayerComparison") {
    
    cms_path <- glue::glue("{eval_dir}/01_Target_Layer_Comparison/{output_name}_CMs_{eval_type}.RDS")
    plot_labels <- c("Imputed", "Target")
    cm_labels <- c("Imputed", "Target")
    
} else if(eval_type == "OOB") {
  
  cms_path <- glue::glue("{eval_dir}/02_OOB_Evaluation/{output_name}_CMs_{eval_type}.RDS")
  plot_labels <- c("Imputed (OOB)", "Observed (FIA)")
  cm_labels <- c("Predicted", "Reference")

  
} else if(eval_type == "CV") {
    cms_path <- glue::glue("{eval_dir}/03_Cross_Validation/{output_name}_CMs_{eval_type}.RDS")
    plot_labels <- c("Imputed (CV)", "Observed (FIA)")
    cm_labels <- c("Predicted", "Reference")
  }
  
cms_all <- readRDS(cms_path)

# Load Landfire Zones
#------------------------------------------#

# load LF zone data
LF_zones <- terra::vect(lf_zones_path)

# select single LF zone
zone <- terra::subset(LF_zones, LF_zones$ZONE_NUM == zone_num)

#project
zone <- terra::project(zone, crs(ras))

# get name of zone
zone_name <- glue::glue("LFz{zone_num}_{gsub(' ', '', zone$ZONE_NAME)}")

# Load X table
#------------------------------------------#

# load X_df 
X_df <- read.csv(xtable_path_model) 

# Load raster attribute table and points
#------------------------------------------#

message("Importing raster attribute table...")
rat_tif <- terra::rast(rat_path)
rat <- data.frame(cats(rat_tif))


# Prep Raster Attribute Table
#-----------------------------------------------------------------#

message("Preparing raster attribute table...")
rm(rat_tif)

# identify eval_vars_cont that are not from RMRS - we handle NAs differently
eval_vars_cont_RMRS <- stringr::str_subset(names(rat), "RMRS")
eval_vars_cont_nonRMRS <- stringr::str_subset(names(rat %>% dplyr::select(where(is.numeric))), "RMRS", negate = TRUE)

# prep rat table
rat %<>%
  # rename shortened field names
  dplyr::rename("SDIPCT_RMRS" = SDIPCT_RMR,
                "CARBON_DOWN_DEAD" = CARBON_DWN) %>%
  # convert CN to numeric
  dplyr::mutate(CN = as.numeric(CN)) %>%
  # round and fix NA values for RMRS and non RMRS vars
  dplyr::mutate(across(any_of(eval_vars_cont_nonRMRS), ~ round(.x, digits = round_dig))) %>%
  dplyr::mutate(across(any_of(eval_vars_cont_nonRMRS), ~ ifelse(.x == -99.000, 0, .x))) %>%
  dplyr::mutate(across(any_of(eval_vars_cont_RMRS), ~ dplyr::na_if(.x, -99))) %>%
  # calculate TPA_DEAD_LIVE_RATIO
  dplyr::mutate(TPA_DEAD_LIVE_RATIO = TPA_DEAD/TPA_LIVE) %>%
  # remove columns
  dplyr::select(-c(Value, tm_id))


# join RAT with X df using CN
rat_x <- rat %>%
  right_join(X_df, by = "CN") %>%
  select(c(CN, tm_id, all_of(eval_vars_cat_cont))) %>%
  # filter to plots with values
  #filter(!is.na(BALIVE)) %>%
  arrange(tm_id)

# Spatial Filter for RAT - Include only points that fall within the zone
#--------------------------------------------------------------------------#

# load model to get xy data
yai <- readRDS(model_path)

# get xy data from model
X_xy <- yai$xRefs %>%
  dplyr::select(point_x, point_y) %>%
  tibble::rownames_to_column(var = "tm_id")

# convert xy data to spatial points
X_pts <- terra::vect(X_xy, geom = c("point_x", "point_y"), crs = "epsg:4269")

# project
X_pts <- terra::project(X_pts, crs(zone))

# mask to pts within zone
zone_pts <- terra::mask(X_pts, zone)

# filter to pts within zone
rat_x <- rat_x %>%
  dplyr::filter(tm_id %in% as.numeric(zone_pts$tm_id))

# Calc frequency for all vars in RAT_x
#------------------------------------------#

rat_freq_all <- list()

for (i in seq_along(eval_vars_cat)) {

  var = eval_vars_cat[i]

# get frequency table
  f_out <- rat_x %>%
    dplyr::select(all_of(var)) %>%
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
                                cms_path = cms_path)
)

#file.remove(tmp_figs_list)

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

