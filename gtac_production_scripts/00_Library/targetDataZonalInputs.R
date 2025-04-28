##### PROJECT INPUTS FOR TARGET DATA

### This function ( library?) sets up commonly used inputs for the preparation of target data -- at the project level


# Written by Lila Leatherman (lila.leatherman@usda.gov)

# Last updated: 4/25/25

# TO DO:
# - add a switch for LF only disturbance vs LCMS+LF disturbance ? 

###########################################################################
# Set inputs
###########################################################################

# SETUP
# Required libraries
list.of.packages <- c("docstring",
                      "glue",
                      "this.path",
                      "terra",
                      "tidyverse")

#check for packages and install if needed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages)

# load all packages
vapply(list.of.packages, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)

targetDataZonalInputs <- function(zone_input,
                                  aoi_name = NA,
                                  aoi_path = NA) {
  
  #' Create environment variables to facilitate target data preparation
  #'
  #' @param zone_input Number; Landfire zone currently being worked on
  #'
  #' @return Environment variables for target data preparation
  #' @export
  #'
  #' @examples 
  #' 

  
  ###########################################################################
  # Set user inputs
  ###########################################################################
  
  zone = zone_input
  #zone = 71 # uncomment to run standalone
  
  # path to an RDS file containing parameters, or NA - NA runs 00a_project_inputs_for_target_data.R
  # path is relative to script location
  #target_prep_params_path <- glue::glue("/params/{target_data_version}_target_data_inputs.RDS")
  #target_prep_params_path <- NA
  
  # Inputs for testing
  # -----------------------------------------------#
  # supply path to a shapefile to use as a subset, or NA
  # aoi_path <- "//166.2.126.25/TreeMap/01_Data/03_AOIs/UT_Uintas_rect_NAD1983.shp"
  # aoi_name <- "UT_Uintas_rect_"
  # aoi_path <- NA
  # aoi_name <- NA
  

  
  ##########################################
  # Run
  ##########################################
  
  # Packages and functions
  #---------------------------------#
  
  # load library 
  this_proj = this.path::this.proj()
  lib_path = glue::glue('{this_proj}/gtac_production_scripts/00_Library/treeMapLib.R')
  source(lib_path)
  
  # Terra options
  # --------------------------------#
  
  #increase memory fraction available
  #terraOptions(memfrac = 0.8)
  
  # # Load pre-existing params, if available
  # #--------------------------------------------#
  # 
  # this_dir <- this.path::this.dir()
  # 
  # if(!is.na(target_prep_params_path)) {
  #   # load params
  # 
  #   params_script_path <- glue::glue('{this_dir}/{target_prep_params_path}')
  # 
  #   # load(params_script_path) # un-comment to run independently from the control script
  # 
  #   } else {
  # 
  #     inputs_for_target_data <- glue::glue('{this_dir}/00a_project_inputs_for_targetdata.R')
  # 
  #     source(inputs_for_target_data)
  # 
  # }
  
  
  ##################################################################
  # CREATE ZONE-SPECIFIC VARIABLES AND PATHS
  ##################################################################
  
  # Prep zone
  #-----------------------------------------#
  
  zone_num <- zone
  
  # Set zone identifiers 
  cur_zone <- glue::glue('z{zone_num}') 
  cur_zone_zero <- if(zone_num < 10) {
    glue::glue('z0{zone_num}') } else {
      cur_zone
    }
  
  # Load zone metadata
  #----------------------------------------#
  
  # load zone metadata
  LF_zone_metadata <- read.csv(zone_metadata_path)
  
  # identify which geographic area zone is in 
  study_area <- LF_zone_metadata %>%
    dplyr::filter(ZONE_NUM == zone_num) %>%
    dplyr::select(STUDY_AREA) %>%
    toString()
  
  # Update dirs with zone
  # -----------------------------------------#
  
  # Set folder paths
  target_dir_premask_z <<- glue::glue('{target_dir_premask}/{cur_zone_zero}/')
  # Set folder paths
  target_dir_postmask_z <<- glue::glue('{target_dir_postmask}/{cur_zone_zero}/')
  
  # Directory where EVT_GP remap table will be located
  evt_gp_remap_table_path <<- target_dir_premask_z
  
  # set aoi_name field if it doesn't already exist via aoi subset
  if(is.na(aoi_name)) {
    aoi_name <- ""
  }
  
  # Export data paths- disturbance
  #---------------------------------------------------------------#
  # create output file names
  LF_fire_years_outpath <<- glue::glue('{target_dir_premask_z}/{start_year}_{end_year}_{cur_zone_zero}_{aoi_name}LFDist_Fire_Years.tif')
  LF_fire_binary_outpath <<- glue::glue('{target_dir_premask_z}/{start_year}_{end_year}_{cur_zone_zero}_{aoi_name}LFDist_Fire_Binary.tif')
  
  LF_ind_years_outpath <<- glue::glue('{target_dir_premask_z}/{start_year}_{end_year}_{cur_zone_zero}_{aoi_name}LFDist_InsectDisease_Years.tif')
  LF_ind_binary_outpath <<- glue::glue('{target_dir_premask_z}/{start_year}_{end_year}_{cur_zone_zero}_{aoi_name}LFDist_InsectDisease_Binary.tif')
  
  
  lcms_slowloss_years_outpath <- glue::glue('{target_dir_premask_z}/{start_year}_{end_year}_{cur_zone_zero}_{aoi_name}LCMSDist_SlowLoss_Years.tif')
  lcms_slowloss_binary_outpath <- glue::glue('{target_dir_premask_z}/{start_year}_{end_year}_{cur_zone_zero}_{aoi_name}LCMSDist_SlowLoss_Binary.tif')
  
  lf_disturb_code_outpath <<- glue::glue('{target_dir_premask_z}/disturb_code_LF.tif')
  lf_disturb_year_outpath <<- glue::glue('{target_dir_premask_z}/disturb_year_LF.tif')
  
  lcms_disturb_code_outpath <- glue::glue('{target_dir_premask_z}/disturb_code_LFLCMS.tif')
  lcms_disturb_year_outpath <- glue::glue('{target_dir_premask_z}/disturb_year_LFLCMS.tif')
  
  
  # Input parameters for LCMS Disturbance
  #-----------------------------------------------------------#
  
  # Set variables
  LCMS_NAvalue <- -32768
  
  # set threshold for probability of slow loss from LCMS
  slow_loss_thresh <- 14 # default value for LCMS processing: 14
  
  # set probability thresholds for change
  LCMS_change_thresholds <- c(29, # fast loss; default = 29
                              slow_loss_thresh, # slow loss; default = 14
                              20 # gain; default = 20
  )
  
  # LCMS file paths
  #---------------------------------------------------------------#
  lcms_slowloss_years_outpath <- glue::glue('{target_dir_premask_z}/{start_year}_{end_year}_{cur_zone_zero}_{aoi_name}LCMSDist_SlowLoss_Years.tif')
  lcms_slowloss_binary_outpath <- glue::glue('{target_dir_premask_z}/{start_year}_{end_year}_{cur_zone_zero}_{aoi_name}LCMSDist_SlowLoss_Binary.tif')
  
  lcms_disturb_code_outpath <- glue::glue('{target_dir_premask_z}/disturb_code_LFLCMS.tif')
  lcms_disturb_year_outpath <- glue::glue('{target_dir_premask_z}/disturb_year_LFLCMS.tif')
  
  # Temp directories 
  #----------------------------------#
  
  # check if tmp directory exists 
  
  message("Checking for temporary directory...")
  if (file.exists(tmp_dir)){
    message(paste0("Temporary directory exists: ", tmp_dir))
  } else {
    # create a new sub directory inside the main path
    dir.create(tmp_dir, recursive = TRUE)
    message(paste0("Creating temporary directory: ", tmp_dir))
  }
  
  # set temp directory - allows you to inspect files in progress more easily
  write(paste0("TMPDIR = ", tmp_dir), file=file.path(Sys.getenv('R_USER'), '.Renviron'))
  
  # create tmp dir folder for LCMS tiles
  # if (!file.exists(glue::glue('{tmp_dir}/lcms/'))) {
  #   dir.create(glue::glue('{tmp_dir}/lcms/'))
  # }
  
  # create tmp dir folder for LF tiles
  if (!file.exists(glue::glue('{tmp_dir}/lf/'))) {
    dir.create(glue::glue('{tmp_dir}/lf/'))
  }
  
  #empty temp dir
  message("empyting tmp dir")
  do.call(file.remove, list(list.files(tmp_dir, full.names = TRUE, recursive = TRUE)))
  
  
  # Create all directories
  # ----------------------------------#
  
  if(!file.exists(target_dir_premask_z)) {
    dir.create(target_dir_premask_z, recursive = TRUE)
  }
  
  if(!file.exists(target_dir_postmask_z)) {
    dir.create(target_dir_postmask_z, recursive = TRUE)
  }
  
  
  
  # Remove unused objects
  #------------------------------------------------#
  #rm(list.of.packages, new.packages)
  
  
  # Make RDS of input parameters used
  #---------------------------------------------------------#
  #save(list = ls(), file = glue::glue('{target_dir_z}/params/{cur_zone_zero}_env.RDS'))

}
