library(tidyverse)

xtable_path <- "C:/Users/abhinavshrestha/OneDrive - USDA/Documents/02_TreeMap/TM_2022_prod_QA/X_table_v2022_allZones.csv"

xTable_df_input <- data.frame(read.csv(xtable_path))

lookUp_TM_ID <- 21348

lookUp_Zone <- 33


xTable_df %>% 
  dplyr::filter(Zone == lookUp_Zone) %>% 
  dplyr::filter(TM_ID == lookUp_TM_ID)

get_xTableRow <- function(lookUp_Zone=NULL, lookUp_TM_ID=NULL, xTable_df=NULL){
  
  if (any(sapply(list(lookUp_Zone,lookUp_TM_ID, xTable_df), is.null)) | !all(sapply(list(lookUp_Zone,lookUp_TM_ID), is.numeric))){
    message("Please enter valid lookUp_Zone, lookUp_TM_ID, and XTable_df. `lookUp_Zone` and `lookUp_TM_ID` must be numeric and `XTable_df` must be a `data.frame`.")
  } else {
    options(scipen = 100000)
    row <- xTable_df %>% 
            dplyr::filter(Zone == lookUp_Zone) %>% 
            dplyr::filter(TM_ID == lookUp_TM_ID)

    if(nrow(row) == 0){
      message("Function returned with zero (0) rows, please check for valid inputs!")
    } else{
      return(row)
    }
    
  }
 
  
}

get_xTableRow(lookUp_Zone = 32,
              lookUp_TM_ID = 21348,
              xTable_df = xTable_df_input)

print("sourced")