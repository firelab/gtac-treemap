# This script checks that all outputs for a given zone have been completed
# It looks for the presence of all expected output files in the output directory

check_imputation_completion <- function(zone, 
                                        output_dir, 
                                        complete_zone = 1 # zone known to be complete
                                        ) {
  
  library(glue)
  library(dplyr)
  
  # zone = 12
  # complete_zone = 1

  # Define output directories
  raw_model_outputs_dir <- paste0(outputs_folder, "01_Raw_model_outputs")
  assembled_model_outputs_dir <- paste0(outputs_folder, "02_Assembled_model_outputs")
  evaluation_outputs_dir <- paste0(outputs_folder, "03_Evaluation")

  # get project name from last part of output_dir separated by "/"
  project_name <- tail(unlist(strsplit(output_dir, "/")), n=1)

  # set zone with leading zeros
  cur_zone_zero <- ifelse(zone < 10, 
                          paste0("z0", zone), 
                          paste0("z", zone))
  
  # get list of expected files - assumes zone 1 is complete
  #-------------------------------------------------------------------------------#
  complete_zone_zero <- ifelse(complete_zone < 10, 
                          paste0("z0", complete_zone), 
                          paste0("z", complete_zone))

  expected_files_raw <- list.files(paste0(raw_model_outputs_dir,"/", complete_zone_zero), 
                               recursive = TRUE)
  # remove anything in the raster/tiles folder, since we assemble those later
  expected_files_raw <- expected_files_raw[!grepl("raster/tiles/", expected_files_raw)]
  expected_files_assembled <- list.files(paste0(assembled_model_outputs_dir,"/", complete_zone_zero),recursive = TRUE)
  expected_files_evaluation <- list.files(paste0(evaluation_outputs_dir,"/", complete_zone_zero, "/04_Eval_Reports"),recursive = TRUE)[1:3]
  
  expected_files <- c(expected_files_raw,
                      expected_files_assembled,
                      expected_files_evaluation)
  # replace expected_files zone with current zone
  expected_files <- gsub(complete_zone_zero, cur_zone_zero, expected_files)
  
  # get list of existing files for the given zone
  existing_files_raw <- list.files(paste0(raw_model_outputs_dir,"/", cur_zone_zero), 
                               recursive = TRUE)
# remove anything in the raster/tiles folder, since we assemble those later
  existing_files_raw <- existing_files_raw[!grepl("raster/tiles/", existing_files_raw)]
  existing_files_assembled <- list.files(paste0(assembled_model_outputs_dir,"/", cur_zone_zero),recursive = TRUE)
  existing_files_evaluation <- list.files(paste0(evaluation_outputs_dir,"/", cur_zone_zero, "/04_Eval_Reports"),recursive = TRUE)[1:3]

  existing_files <- c(existing_files_raw,
                      existing_files_assembled,
                      existing_files_evaluation)


  # check for presence of expected files
  missing_files <- setdiff(expected_files, existing_files)
  
  if(length(missing_files) == 0) {
    message(glue::glue("All expected files are present for zone {zone}"))
    return(TRUE)
  } else {
    message(glue::glue("Missing files for zone {zone}:"))
    print(missing_files)
    return(FALSE)
  }
}

# Example usage: 

# # comparison project + zone
# zones_list <- c(seq(from = 2, to = 10, by = 1), # all CONUS zones, skipping zone 11
#                 seq(from = 12, to = 66, by = 1),
#                 98, 99)

# output_dir=  "//166.2.126.25/TreeMap/03_Outputs/07_Projects/2023_Production/"
# for(zone in zones_list) {
#   check_imputation_completion(zone = zone,
#                               output_dir = output_dir,
#                               complete_zone = 1)
# }
