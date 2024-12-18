# packages required
list.of.packages <- c("terra", "tidyverse", "magrittr", "glue", "tictoc", "foreach", "doParallel", "this.path", "shiny", "rsconnect", "bslib", "data.table", "DT", "plotly")

# #check for packages and install if needed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages)

# load all packages
vapply(list.of.packages, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)

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

# NOTE code assumes X table csv is in the app folder (copy csv from NAS drive //166.2.126.25/TreeMap/07_Documentation/01_Validation/02_Eval_tools/)
xtable_path <- file.path("F:/TreeMap2020/Validation/X_table_v2020_allZones.csv")
xTable_df_input <<- data.frame(read.csv(xtable_path))

runApp(launch.browser=TRUE)