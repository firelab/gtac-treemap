# Generate zonal report from Rmd file
# Written by Lila Leatherman (Lila.Leatherman@usda.gov)
# Updated by Abhinav Shrestha (abhinav.shrestha@usda.gov)

# Last updated: 06/21/2024


# TODO:
# add continuous vars to report - from plots already made 


#==========================================================#
#                                                          # 
#         Specific inputs (CHANGE VARIABLES HERE)          #  
#                                                          #    
#==========================================================#

# VARIABLES TO EVALUATE
#------------------------------------------#

# list variables to evaluate
# - confusion matrices (CMs) for these variables are calculated in the 01-03 scripts
eval_vars <- c("canopy_cover", 
               "canopy_height", 
               "EVT_GP", 
               "disturb_code")

# Eval report for OOB or derived vars
# - options: "OOB" or "TargetLayerComparison" 

eval_type <- "TargetLayerComparison"
# eval_type <- "OOB" 
# FIXME: NO OOB RDS for 2016_GTAC_LCMSDist project


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

this_dir <- this.path::this.dir()

inputs_for_evaluation <- glue::glue('{this_dir}/00_inputs_for_evaluation.R')
source(inputs_for_evaluation)

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
rmd_path <- glue::glue("{this_dir}/04b_zonal_eval_report_generator_modularPloting.Rmd")

# set dir for temporary outputs - needs to be a place w/ write permissions for R (network drives aren't allowed)
tmpout_dir <- tmp_dir

plot_labels <- c("Imputed", "Observed")



#===========================================================#
#                                                           #
# EVAL REPORT GENERATION SCRIPT START (DO NOT EDIT BELOW)   #
#                                     ~~~~~~~~~~~~~~~~~~~   #
#===========================================================#


# Load evaluation data
#------------------------------------------#

# CACHING -- checking if the same raster was already imported
if (exists("ras")){
  
  if(ras@ptr$get_sourcenames() == raster_nameCompare){
  
    message("Using preloaded raster from previous run...")
  
  } else {
    
    # ras exists but not the same as previous one
    # load raw imputation output raster
    ras <- terra::rast(glue::glue("{assembled_dir}/01_Imputation/{output_name}.tif"))
    raster_nameCompare <- output_name
  }
} else {
  
  # load raw imputation output raster
  ras <- terra::rast(glue::glue("{assembled_dir}/01_Imputation/{output_name}.tif"))
  raster_nameCompare <- output_name
  
}

# conditional loads and variables based on evaluation type: 

#   - load RDS of cm files
#   - label reference type
#   - make labels for plots

if(eval_type == "OOB") {
  
  cms_path <- glue::glue("{eval_dir}/01_OOB_Evaluation/{output_name}_CMs_{eval_type}.RDS")
  plot_labels <- c("Imputed", "Observed (Out-of-bag)")

} else if(eval_type == "TargetLayerComparison") {
  
  cms_path <- glue::glue("{eval_dir}/02_Target_Layer_Comparison/{output_name}_CMs_{eval_type}.RDS")
  plot_labels <- c("Imputed", "Observed (Target Layers)")
}

cms_all <- readRDS(cms_path)

# Load zone
#------------------------------------------#

if (exists("LF_zones")){
  
  if(lf_zones_pathCompare == lf_zones_path) { # check if its the same rat as before
    
    message("Using previously loaded lf zones vector...")
    
  } else {
    
    message("`lf_zones` exists but not the same as previously loaded data, importing lf_zones...")
    LF_zones <- terra::vect(lf_zones_path)
    lf_zones_pathCompare <- lf_zones_path
    
  }
  
} else {
  
  message("Importing lf_zones...")
  LF_zones <- terra::vect(lf_zones_path)
  lf_zones_pathCompare <- lf_zones_path
  
}



# load LF zone data
# LF_zones <- terra::vect(lf_zones_path)

# select single LF zone
zone <- terra::subset(LF_zones, LF_zones$ZONE_NUM == zone_num)

#project
zone <- terra::project(zone, crs(ras))

# get name of zone
zone_name <- glue::glue("LFz{zone_num}_{gsub(' ', '', zone$ZONE_NAME)}")

# Load X table
#------------------------------------------#

# load X_df
X_df <- read.csv(xtable_path) %>%
  dplyr::rename(PLOTID = X)

# Reclass EVT_GP
#------------------------------------------#

#load evt_gp remap table
evt_gp_remap_table <- read.csv(evt_gp_remap_table_path)

# join to reclass 
X_df %<>%
  dplyr::rename(EVT_GP_remap = EVT_GP) %>%
  dplyr::left_join(evt_gp_remap_table, by = c("EVT_GP_remap"))

# Load raster attribute table and points
#------------------------------------------#

# load rat
 
if (exists("rat")){
  
  if(rat_pathCompare == rat_path) { # check if its the same rat as before
    
    message("Using previously loaded raster attribute table...")
    
  } else {
    
    message("`rat` exists but not the same as previously loaded data, importing rat...")
    rat_tif <- terra::rast(glue::glue("{rat_path}TreeMap2016.tif"))
    rat_pathCompare <- rat_path

  }
  
} else {
  
  message("Importing raster attribute table...")
  rat_tif <- terra::rast(glue::glue("{rat_path}TreeMap2016.tif"))
  rat_pathCompare <- rat_path

}


rat <- data.frame(cats(rat_tif)) %>%
  dplyr::rename("SDIPCT_RMRS" = SDIPCT_RMR,
                "CARBON_DOWN_DEAD" = CARBON_DWN)


# identify eval_vars_cont that are not from RMRS - we handle NAs differently
eval_vars_cont_RMRS <- stringr::str_subset(names(rat), "RMRS")
eval_vars_cont_nonRMRS <- stringr::str_subset(names(rat %>% dplyr::select(where(is.numeric))), "RMRS", negate = TRUE)

# prep rat table
rat %<>%
  dplyr::mutate(CN = as.numeric(CN)) %>%
  dplyr::mutate(across(any_of(eval_vars_cont_nonRMRS), ~ round(.x, digits = 3))) %>%
  dplyr::mutate(across(any_of(eval_vars_cont_nonRMRS), ~ ifelse(.x == -99.000, 0, .x))) %>%
  dplyr::mutate(across(any_of(eval_vars_cont_RMRS), ~ dplyr::na_if(.x, -99))) %>%
  dplyr::select(-Value) %>%
  dplyr::mutate(TPA_DEAD_LIVE_RATIO = TPA_DEAD/TPA_LIVE)

# join with X df  - limit to plots in X df
rat %<>%
  dplyr::right_join(X_df, by = c("CN" = "CN", "tm_id" = "PLOTID")) 



# Calc frequency for all vars in RAT
#------------------------------------------#

rat_freq_all <- list()

for (i in seq_along(eval_vars)) {

#var = names(rat_z)[i]
  var = eval_vars[i]

# get frequency table
  f_out <- rat %>%
    dplyr::select(all_of(var)) %>%
    table() %>%
    data.frame() 
  
  # add normalized frequency
  f_out$Freq_norm = f_out$Freq/sum(f_out$Freq)
  
  # join with other outputs
  rat_freq_all = c(rat_freq_all, list(f_out))
  
}

names(rat_freq_all) <- eval_vars

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
                                eval_vars= eval_vars,
                                cms_path = cms_path)
)

# File Organization
#------------------------------------------#
#

fileExtension <- document_formatExtensionDict[[report_format]]

message("\n\nCopying rendered report from tmp directory to output directory...")

message(glue::glue("-- Output directory: {tmpout_dir}\n\n"))

# move report from tmpout dir to desired out dir
file.copy(from=glue::glue("{tmpout_dir}/{output_name}_eval_report_{eval_type}{fileExtension}"),
          #to= glue::glue("{tmp_output_dir}/"), 
          to = glue::glue("{eval_dir}/04_Eval_Reports/"),
          overwrite = TRUE, recursive = FALSE,
          copy.mode = TRUE)

message("Copied report, removing file from tmp directory...")

# remove file from tmp location
file.remove(glue::glue("{tmpout_dir}/{output_name}_eval_report_{eval_type}{fileExtension}"))
message("Render complete!")

Sys.time() - ptm
