# packages required
list.of.packages <- c("terra", "tidyverse", "magrittr", "glue", "tictoc", "foreach", "doParallel", "this.path", "shiny", "rsconnect", "bslib", "data.table", "DT", "plotly", "lazyeval")

# #check for packages and install if needed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages)

# load all packages
vapply(list.of.packages, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)

get_xTableRow <<- function(lookUp_Zone=NULL, lookUp_TM_ID=NULL, xTable_df=NULL){
  #' Get X Table row
  #'
  #' @param lookUp_Zone `numeric`. Zone number to filter the CONUS X table (reference table)
  #' @param lookUp_TM_ID `numeric`. TreeMap ID (TM ID) used to extract row from filtered CONUS X table (reference table)
  #' @param xTable_df `data.frame`. CONUS X table (reference table) read in and converted to a `data.frame`
  #'
  #' @return `data.frame`. Single row extracted from the CONUS X table (reference table) `data.frame` input
  #' @export
  #'
  #' @examples
  
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

xtable_path <- file.path(getwd(), "X_table_v2023_allZones.csv")
if (!file.exists(xtable_path)){
  cat("\n")
  cat("\n")
  message("!-----------------------------------------------------------------------------------------------------------------------!")
  message(glue::glue("! {xtable_path} is not present in app directory. Please move the file to app directory and re-run the app. !"))
  message("!-----------------------------------------------------------------------------------------------------------------------!")
  cat("\n")
  cat("\n")
} else {
  cat("\n")
  cat("\n")
  message("---------------------------------------------------------------------------------------------------------------------")
  message("                         LOADING X TABLE DATA... EVAL APP WILL LAUNCH ON THE BROWSER...                              ")
  message("---------------------------------------------------------------------------------------------------------------------")
  cat("\n")
  cat("\n")
}



xTable_df_input <<- data.frame(read.csv(xtable_path))
runApp(launch.browser=TRUE)