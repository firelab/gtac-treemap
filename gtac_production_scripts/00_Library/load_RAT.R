# Load Raster Attribute Table (RAT)

# Function to load and prepare RAT from a file path
# Conditionally handles different years and types of RAT

# As well as different field names present in iterations

library(docstring)

load_RAT <- function(rat_path, CN_column, ID_column, round_dig = 4)  { 
  
  #' Function to make new imputation predictions given a data frame input
  #' 
  #' @param rat_path STRING - The path to the Raster Attribute Table (RAT). Can be a .csv, or a .tif when the RAT is a literal raster attribute table. 
  #' @param CN_column STRING - the name of the column in the RAT that has the CN
  #' @param ID_column STRING - the name of the column in the RAT that has the TM_ID
  #' @param round_dig NUMERIC - how many decimal places should values be rounded to? Default value = 4
  #' @return data frame of Raster Attribute Table with appropriate formatting applied
  #' @export
  #'
  #' @examples 
  
  require(tidyverse)
  require(magrittr)
  require(stringr)
  require(terra)
  
  # Allow for sufficient digits to differentiate plot cn numbers
  options("scipen" = 100, "digits" = 8)
  
  # rat_path <- "//166.2.126.25/TreeMap/03_Outputs/06_Reference_Data/v2020/03_Raster_attributes/TM2020RAT_tmid.csv"
  # #rat_path <- ("//166.2.126.25/TreeMap/01_Data/01_TreeMap2016_RDA/RDS-2021-0074_Data/Data/TreeMap2016.tif")
  # 
  # CN_column <- "PLT_CN"
  # ID_column <- "TM_ID"
  # 
  # round_dig = 4
  
  ############################################################################
  # LOAD RAT
  ############################################################################
  
  # handle RAT input cases - if it's a TIF vs CSV
  #--------------------------------------------------------------------------#

  if(stringr::str_detect(rat_path, ".tif")) {
    
    message("loading RAT from TIF")

    # load rat
    rat <- terra::rast(rat_path)
    rat <- data.frame(terra::cats(rat))

    # 2016 - specific
    if(str_detect(rat_path, "2016")) {

      rat %<>%
        # rename shortened field names
        dplyr::rename("SDIPCT_RMRS" = SDIPCT_RMR,
                      "CARBON_DOWN_DEAD" = CARBON_DWN) %>%
        select(-Value)
    }
  } else if(stringr::str_detect(rat_path, ".csv")) {
    
    message("loading RAT from CSV")
    
    # load rat
    rat <- read.csv(rat_path)
    
  }
  
  ############################################################################
  # PREP RAT
  ############################################################################
  
  # Rename fields
  #----------------------------------------------------------------------------#
  
  # rename CN and TM_ID columns to be standard
  rat %<>%
    dplyr::rename("CN" = all_of(CN_column)) %>%
    dplyr::rename("TM_ID" = all_of(ID_column)) 
  
  # remove column titled X - row name - if present
  if("X" %in% names(rat)) {
    rat$X <- NULL
  }
  
  if("Value" %in% names(rat)) {
    rat$Value <- NULL
  }
  
  if("SDIPCT_RMR" %in% names(rat)) {
    rat %<>%
      # rename shortened field names
      dplyr::rename("SDIPCT_RMRS" = SDIPCT_RMR)
  }
  
  if("CARBON_DWN" %in% names(rat)) {
    rat %<>%
      # rename shortened field names
      dplyr::rename("CARBON_DOWN_DEAD" = CARBON_DWN) 
  }
  
  
  # General table prep 
  #-----------------------------------------------------------------------------#
  
  # identify eval_vars_cont that are not from RMRS - we handle NAs differently
  eval_vars_cont_RMRS <- stringr::str_subset(names(rat), "RMRS")
  eval_vars_cont_nonRMRS <- stringr::str_subset(names(rat %>% dplyr::select(where(is.numeric))), "RMRS", negate = TRUE)
  
  # prep rat table
  rat %<>%
    # convert CN to numeric
    dplyr::mutate(CN = as.numeric(CN)) %>%
    # round and fix NA values for RMRS and non RMRS vars
    dplyr::mutate(across(any_of(eval_vars_cont_nonRMRS), ~ round(.x, digits = round_dig))) %>%
    dplyr::mutate(across(any_of(eval_vars_cont_nonRMRS), ~ ifelse(.x == -99.000, 0, .x))) %>%
    dplyr::mutate(across(any_of(eval_vars_cont_RMRS), ~ dplyr::na_if(.x, -99))) %>%
    # calculate TPA_DEAD_LIVE_RATIO
    dplyr::mutate(TPA_DEAD_LIVE_RATIO = TPA_DEAD/TPA_LIVE) 
 
  names_select <- names(rat)[names(rat) != "CN" & names(rat) != "TM_ID" ]
  
  # order column names for export
  rat %<>%
    dplyr::select(TM_ID, CN, all_of(names_select))
  
  # return at end of function 
  rat 
}