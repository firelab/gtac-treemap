# Clear workspace
rm(list = ls())
gc()

#################################################################
# Set Inputs
#################################################################

# Initialize projects (years) 
#years_list <- c(2020, 2022)
years_list <-c(2020, 2022)

# List zones to run for
# zones_list <- c(2, 16, 57) # testing
# ###CONUS
#zones_list <- c(seq(from = 1, to = 10, by = 1), 
#                seq(from = 12, to = 66, by = 1), # skipping zone 11 
#                98, 99)
# ### AK and HI
zones_list <- c(seq(67, 80, 1))


# Initialize directories
this_dir <- this.path::this.dir()
project_inputScript <- glue::glue("{this_dir}/00a_project_inputs_for_targetdata.R")
zone_inputScript <- glue::glue("{this_dir}/00b_zone_inputs_for_targetdata.R")
createLFDist_inputs_02a_Script <- glue::glue("{this_dir}/02a_create_LF_Disturbance_inputs.R")
mergeLFDist_layers_03a_Script <- glue::glue("{this_dir}/03a_merge_LF_disturbance_layers.R")



#################################################################
# Load required packages
#################################################################

# packages required
list.of.packages <- c("glue", "this.path", "rprojroot", "terra", "tidyverse", 
                      "magrittr", "tictoc", "caret", "randomForest", 
                      "Metrics", "foreach", "doParallel", "yaImpute", "docstring",
                      "stringr", "stringi")

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

# load all packages
vapply(list.of.packages, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)

# remove unused objects
rm(list.of.packages, new.packages)


#################################################################
# Control script START
#################################################################

ptm_start <- Sys.time() # Processing time: Start


# LOOP by year (outer loop) 
for (year_input in years_list){
  
  # year_input <- years_list[1] # for testing
  ptm_year <- Sys.time()
  
  message(paste0("Running disturbance data preparation for year: ", year_input))
  
  # PASS variables to `00a_project_inputs_for_targetdata.R` to SET project directory for each year
  source(project_inputScript)
  
  # LOOP by zones (inner loop)
  for (zone_input in zones_list){
   
    #zone_input <- zones_list[1] # for testing
    
    ptm_zone <- Sys.time()
   
    # PASS variables to `00b_zone_inputs_for_targetdata.R` to SET variables by zone
    source(zone_inputScript)

    # SOURCE script 01b to download LF data (not required, already downloaded)
    
    # SOURCE script 02a to create LF disturbance inputs
    message(paste0("Creating LF disturbance inputs for zone: ", zone_input))
    source(createLFDist_inputs_02a_Script)
    
    # SOURCE script 03a to merge the LF disturbance layers
    message(paste0("Merging and exporting LF disturbance layers for zone: ", zone_input))
    source(mergeLFDist_layers_03a_Script)
    
    message(paste0("Disturbance data prep complete for zone: ", zone_input, ". Moving to next zone..."))
    print(Sys.time() - ptm_zone)
    message("---------------------------------------------------------------------------")
    
    # Remove all variables except the ones in the list (described below)
    rm(list=setdiff(ls(), c("this_dir", 
                            "project_inputScript", 
                            "zone_inputScript", 
                            "createLFDist_inputs_02a_Script", 
                            "mergeLFDist_layers_03a_Script",
                            "year_input",
                            "zone_input",
                            "years_list", 
                            "zones_list", 
                            "project_name", 
                            "target_data_version",
                            "year",
                            "start_year", 
                            "end_year",
                            "model_year",
                            "year_list",
                            "landfire_version_topo",
                            "landfire_year_topo",
                            "landfire_year_veg",
                            "landfire_version_veg",
                            "ptm_zone",
                            "ptm_year", 
                            "ptm_start")))
    gc()
    
  }
  
  message(paste0("Disturbance data prep complete for year: ", year_input))
  Sys.time() - ptm_year
  message("===========================================================================")
  message("===========================================================================")
}

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
message("!    Disturbance data prep complete, script run successfully!      !")
message("!==================================================================!")

print(Sys.time() - ptm_start)