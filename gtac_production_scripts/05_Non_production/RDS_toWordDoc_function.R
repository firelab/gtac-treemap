RDS_toDocx <- function(paramsObj, exportDocName=NULL, outDir){
  #' Convert variables loaded via RDS as a Word document table
  #'
  #' @param paramsObj Object containing parameters loaded via RDS. The default name is `params_out` when using `load(RDS-path)`
  #' @param exportDocName Default is `NULL`. When NULL, export name = `<project name>_<zone number>_PARAMS.docx`.
  #' @param outDir Output directory to save the Word document table
  #'
  #' @return
  #' @export Word document table in the `outDir` directory
  #'
  #' @examples
  
  # inspo: https://datafortress.github.io/en/2018/06/word-tables-with-r/ 
  
  # Required libraries
  library(tidyverse)
  library(flextable)
  library(officer)
  
    
  # Convert params object to data.frame
  params_df <- as.data.frame(paramsObj)
  
  # Convert df to flextable and autofit contents
  params_df_toConvert <- params_df %>% 
    flextable::regulartable() %>% 
    flextable::autofit()
  
  # Create empty word doc
  params_doc <- officer::read_docx()
  
  # Add title for table in empty word doc
  params_doc <- officer::body_add_par(params_doc, 
                                      value = glue::glue("{project_name}, {cur_zone_zero} paramerters"), 
                                      style = "Normal")
  
  # Custom function to autofit table to word doc
  FitFlextableToPage <- function(ft, pgwidth = 6){
    
    ft_out <- ft %>% autofit()
    
    ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
    return(ft_out)
  }
  
  if (is.null(exportDocName)){
    exportDoc_name <- glue::glue("{project_name}_{cur_zone_zero}_PARAMS.docx")
  } else {
    exportDoc_name <- exportDocName
  }
  
  # Export
  params_doc %>% 
    flextable::body_add_flextable(FitFlextableToPage(params_df_toConvert)) %>% 
    print(target = glue::glue("{out_dir}/{exportDoc_name}"))  
  
  
}

library(docstring)
?RDS_toDocx


# EXAMPLE:

# params_RDS_path <- "//166.2.126.25/TreeMap/03_Outputs/07_Projects/2016_GTAC_Test/01_Raw_model_outputs/z16/params/z16_2016_Orig_Test_params.RDS"
# load(params_RDS_path)
# 
# out_dir <- "YOUR-PATH-HERE" # change this
# 
# RDS_toDocx(paramsObj = params_out,
#                outDir = out_dir)