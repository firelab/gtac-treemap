setwd(this.path::here())

# Clear workspace
rm(list = ls())
gc()


#################################################################
# Control script INPUTS
#################################################################

# Initialize projects (years) and zones
year_input <- 2020

#zones_list <- c(12)   # testing
# # path to priority zone list
# priority_zones <- read.csv("//166.2.126.25/TreeMap/03_Outputs/07_Projects/2022_Production/00_Prioritization/priority_forest_wcs.csv")
# priority_list <- priority_zones[,'ZONE_NUM']
# 
# # get odd indices to run in a list - F8V75K3
# zones_list <- priority_list[seq(1,length(priority_list),2)]
# zones_list <- zones_list[18:34] 


# get even indices to run in a list - abhi
#zones_list <- priority_list[seq(0,length(priority_list),2)]

# manually list zones
zones_list <- c(seq(from = 1, to = 10, by = 1), # all CONUS zones, skipping zone 11
                seq(from = 12, to = 66, by = 1),
                98, 99)
#
zones_list <- c(1,10,16,57,66)
 

# Types of evaluation to run and prepare reports for 
# Options: "TargetLayerComparison", "OOB", "CV"
#eval_type_list <- c("TargetLayerComparison", "OOB", "CV")

# Script inputs - changed less frequently 
########################################################

# Initialize paths to scripts
this_dir <- this.path::this.dir()
this_proj <- this.path::this.proj()

# Imputation scripts
project_inputScript <- glue::glue("{this_dir}/00a_project_inputs_for_imp.R")
zone_inputScript <- glue::glue("{this_dir}/00b_zonal_inputs_for_imp.R")
buildImputation_script <-  glue::glue("{this_dir}/101_build_imputation_model_stochasticity_test.R")
runImputation_script <- glue::glue("{this_dir}/102_run_imputation_sample_model_stochasticity_test.R")
#assembleImputation_script <- glue::glue("{this_dir}/03_assemble_imputation_rasters.R")

# Evaluation scripts
#eval_inputScript <- glue::glue("{this_proj}/gtac_production_scripts/04_Evaluation/00_inputs_for_evaluation.R")
#targetLayerComparison_script <- glue::glue("{this_proj}/gtac_production_scripts/04_Evaluation/01_target_layer_comparison.R")
#OOBEvaluation_script <- glue::glue("{this_proj}/gtac_production_scripts/04_Evaluation/02_oob_evaluation.R")
#CV_script <- glue::glue("{this_proj}/gtac_production_scripts/04_Evaluation/03_cross_validation.R")

#reportGenerator_script <- glue::glue("{this_proj}/gtac_production_scripts/04_Evaluation/04a_run_zonal_report_modularPlotting.R")

# packages required
list.of.packages <- c("glue", "this.path", "rprojroot", "terra", "tidyverse", 
                      "magrittr", "tictoc", "caret", "randomForest", 
                      "Metrics", "foreach", "doParallel", "yaImpute", "docstring",
                      "stringr", "stringi", "devtools")

################################################################
# END USER INPUTS
################################################################

#################################################################
# Make sure required packages are installed
#################################################################

# #check for packages and install if needed
new.packages <- tryCatch(
  
  # try to get list of packages installed
  {suppressWarnings(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]) }, 
  
  error= function(cond) {
    message("Can't access list of packages")
    message("Here's the original error message:")
    message(conditionMessage(cond))
    
    # return value in case of error:
    NA
  }
  
)

if(length(new.packages) > 0) install.packages(new.packages)

# remove unused objects
rm(list.of.packages, new.packages)


##############################################################################
# Do the work
##############################################################################

ptm_start <- Sys.time() # Processing time: Start

message(paste0("Running imputation preparation for year: ", year_input))

# PASS variables to `00a_project_inputs_for_imp.R` to SET project directory for each year
source(project_inputScript)
zone_input=2

# LOOP by zones (runs x66 for all zones in CONUS)
for (zone_input in zones_list){

  # zone_input <- zones_list[1] # for testing
  
  ptm_zone_imp <- Sys.time()
  
  # load project inputs RDS
  load(glue::glue('{this.path::this.dir()}/params/{project_name}_imputation_inputs.RDS'))
  
  # PASS variables to `00b_zone_inputs_imp.R` to SET variables by zone
  source(zone_inputScript)
  
  #### Run modified Imputation Script, running 
  
  
  # SOURCE script 01 to build the imputation model for the zone
  message(paste0("Building many imputation models for zone: ", zone_input))
  source(buildImputation_script)
  
  # SOURCE script 02 to run the imputation model for each zone
  message(paste0("Applying imputation model for random sample of pixels in zone: ", zone_input))
  source(runImputation_script)
  
  #############################################################################
  message(paste0("Imputation complete for zone: ", zone_input))
  print(Sys.time() - ptm_zone_imp)
  message("---------------------------------------------------------------------------")
  message("===========================================================================")
  
  # Remove all variables except the ones in the list (described below)
  rm(list=setdiff(ls(), c("year_input",
                          "zones_list",
                          "eval_type_list",
                          "this_dir",
                          "this_proj",
                          "project_inputScript",
                          "zone_inputScript",
                          "buildImputation_script",
                          "runImputation_script",
                          "assembleImputation_script",
                          "eval_inputScript",
                          "targetLayerComparison_script",
                          "OOBEvaluation_script",
                          "reportGenerator_script",
                          "project_name",
                          "ptm_start")))
  gc()
  
} # end of loop over zone


# Code block to catch warnings, although removing files from tmp almost always causes warning messages.
message("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
message("Warning messages (if any:)")
scriptWarnings <- warnings()
if (is.null(scriptWarnings) == TRUE){
  message("There were no warning messages during script execution.")
} else {
  scriptWarnings
}

message("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
message("!==================================================================!")
message("!    Imputation and Evaluation complete, script run successfully!      !")
message("!==================================================================!")

print(Sys.time() - ptm_start)