# SETUP
# Required libraries
list.of.packages <- c("tidyverse",
                      "dplyr",
                      "flextable",
                      "ggplot2",
                      "officer", 
                      "docstring", 
                      "roxygen2")

#check for packages and install if needed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages)

# load all packages
vapply(list.of.packages, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)

# ----------------------------------------------------------------------------------------

# Bar plot: Raw frequency, observed vs. predicted for each class 
  
plot_barchart_raw <- function(df, var_in, zone_num, save_Plot, exportDir){
    #' Create barchart of raw frequency values
    #'
    #' @param df `freq` data frame assembled from `cms$freq`
    #' @param var_in Variable that is evaluated, set `var_in = var_in` for report generation
    #' @param zone_num Zone number for evaluation, set `zone_num = zone_num` for report generation
    #' @param savePlot Boolean. If `T`, plot will be saved in the export dir
    #' @param exportDir Path to export plot to only if `savePlot = T`
    #'
    #' @return ggplot object
    #' @export
    #'
    #' @examples 
    
    plot_labels <- c("Imputed", "Observed")

    barchart1 <- df %>% # format frequency table for plotting
                    tidyr::pivot_longer(!class, names_to = "dataset") %>%
                    dplyr::rename(frequency = "value") %>%
                    dplyr::arrange(dataset, class) %>% 
                    ggplot() + # plot
                    geom_col(aes(x = class, y = frequency, fill = factor(dataset)), position = "dodge") +
                    scale_fill_manual(name = "Dataset", labels = c("Ground (FIA)", plot_labels),values = c("springgreen4", "aquamarine3", "chartreuse4"))+
                    theme_bw() +
                    ggtitle(glue::glue("Frequency of classes
                            Zone: z{zone_num} ; Attribute: {var_in}"))

    # conditionally flip axis text labeling
    if(var_in == "EVT_GP") {
        barchart1 <- barchart1 + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    }

    

    if (save_Plot == "T"){
        ggplot2::ggsave(filename = glue::glue("barchart_rawValues_{var_in}_{zone_num}.png"),
                        barchart1, 
                        path = exportDir)
    }
    
    return(barchart1)
}

# ----------------------------------------------------------------------------------------

# Bar plot: Normalized frequency, observed vs. predicted vs. for each class

plot_barchart_norm <- function(df, var_in, zone_num, save_Plot, exportDir){
    #' Create barchart of normalized frequency values
    #'
    #' @param df `freq_norm` data frame assembled from normalizing `cms$freq` by pred and ref totals
    #' @param var_in Variable that is evaluated, set `var_in = var_in` for report generation
    #' @param zone_num Zone number for evaluation, set `zone_num = zone_num` for report generation
    #' @param savePlot Boolean. If `T`, plot will be saved in the output dir
    #' @param exportDir Directory to export plots to IF `savePlot == T`
    #' 
    #' @return ggplot object
    #' @export
    #'
    #' @examples  

    plot_labels <- c("Imputed", "Observed")
    
    barchart2 <- df %>% # format frequency table for plotting
                    tidyr::pivot_longer(!class, names_to = "dataset") %>%
                    dplyr::rename(frequency = "value") %>%
                    dplyr::mutate(dataset= factor(dataset)) %>%
                    dplyr::arrange(dataset, class) %>%
                    ggplot() + # plot
                    geom_col(aes(x = class, y = frequency, fill = factor(dataset)), position = "dodge") +
                    scale_fill_manual(name = "Dataset", labels = c("Ground (FIA)", plot_labels),values = c("springgreen4", "aquamarine3", "chartreuse4"))+
                    theme_bw() +
                    ggtitle(glue::glue("Normalized frequency of classes
                            Zone: z{zone_num} ; Attribute: {var_in}"))

    # conditionally flip axis text labeling
    if(var_in == "EVT_GP") {
        barchart2 <- barchart2 + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    }
        
    

    if (save_Plot == "T"){
        ggplot2::ggsave(filename = glue::glue("barchart_normalized_{var_in}_{zone_num}.png"),
                        barchart2, 
                        path = exportDir)
    }

    return(barchart2)
    
}

# ----------------------------------------------------------------------------------------

# Scatterplot: N of class in reference data vs. classwise accuracy

plot_scatter_refVClassAcc <- function(df, var_in, zone_num, save_Plot, exportDir){
  #' Title Create scatterplot of reference data vs classwise accuracy
  #'
  #' @param df `n_acc` data frame created from the `X_df` data frame of the reference dataset 
  #' @param var_in Variable that is evaluated, set `var_in = var_in` for report generation
  #' @param zone_num Zone number for evaluation, set `zone_num = zone_num` for report generation
  #' @param save_Plot Boolean. If `T`, plot will be saved in the output dir
  #' @param exportDir Directory to export plots to IF `savePlot == T` 
  #'
  #' @return ggplot object
  #' @export
  #'
  #' @examples

    scatter_plot1 <- df %>%
                        ggplot()+
                        geom_point(aes(x = n, y = value), size = 2) + 
                        theme_bw() +
                        labs(x = "total N of class in reference data", y = "Balanced Accuracy") + 
                        ggtitle(glue("Accuracy vs. N Reference pts per class
                                Zone: z{zone_num} ; Attribute: {var_in}"))

    

    if (save_Plot == "T"){
        
        ggplot2::ggsave(filename = glue::glue("scatterplot_refVClasswiseAcc_{var_in}_{zone_num}.png"),
                        scatter_plot1, 
                        path = exportDir)
      
      print(glue::glue("Plot exported to: {exportDir}"))
        
    }    
    
    return(scatter_plot1)
    
}

# ----------------------------------------------------------------------------------------

# Scatterplot: Total obs of class vs. classwise accuracy


plot_scatter_obsVClassAcc <- function(df, var_in, zone_num, save_Plot, exportDir){
  #' Title
  #'
  #' @param df `freq_acc` data frame created from the `freq` table
  #' @param var_in Variable that is evaluated, set `var_in = var_in` for report generation
  #' @param zone_num Zone number for evaluation, set `zone_num = zone_num` for report generation
  #' @param save_Plot Boolean. If `T`, plot will be saved in the output dir
  #' @param exportDir Directory to export plots to IF `savePlot == T` 
  #'
  #' @return ggplot object
  #' @export
  #'
  #' @examples

    scatter_plot2 <- df %>%
                        ggplot()+
                        geom_point(aes(x = n, y = value), size = 2) + 
                        theme_bw() +
                        labs(x = "total N of class in Imputed observations", y = "Balanced Accuracy") + 
                        ggtitle(glue("Accuracy vs. N Imputed Obs per class
                                Zone: z{zone_num} ; Attribute: {var_in}")) 

    if (save_Plot == "T"){
        
        ggplot2::ggsave(filename = glue::glue("scatterplot_obsVClasswiseAcc_{var_in}_{zone_num}.png"),
                        scatter_plot2, 
                        path = exportDir)
      
      print(glue::glue("Plot exported to: {exportDir}"))
        
    } 
    
    return(scatter_plot2)
    
}

# ----------------------------------------------------------------------------------------

RDS_toTable <- function(paramsObj){
  #' Convert variables loaded via RDS as a Word document table
  #'
  #' @param paramsObj Object containing parameters loaded via RDS. The default name is `params_out` when using `load(RDS-path)`
  #'
  #' @return
  #' @export Flextable (formatted)
  #'
  #' @examples
  
  # inspo: https://datafortress.github.io/en/2018/06/word-tables-with-r/ 
  
  # Convert params object to data.frame
  params_df <- data.frame(paramsObj) %>% 
                dplyr::rename(Parameter = param, 
                  Value = value)
  
  
  # Convert df to flextable and autofit contents
  params_df_toConvert <- flextable::flextable(data = params_df) %>%
    flextable::font(fontname = "Cambria", i = 1, j = NULL) %>% 
    flextable::bold(i = 1, j = NULL, bold = TRUE, part = "header") %>% 
    flextable::width(j = 1, width = 0.25, unit= "in") %>% 
    flextable::font(fontname = "Consolas", j = 1) %>% 
    flextable::font(fontname = "Calibri", j = 2)
  # flextable::autofit()
  
  
  # # Create empty word doc
  # params_doc <- officer::read_docx()
  # 
  # # Add title for table in empty word doc
  # params_doc <- officer::body_add_par(params_doc, 
  #                                     value = glue::glue("{project_name}, {cur_zone_zero} parameters"), 
  #                                     style = "Normal") %>% 
  #               officer::body_add_par(value = "\n")
  
  # Custom function to autofit table to word doc
  FitFlextableToPage <- function(ft, pgwidth = 6){
    
    ft_out <- ft 
    
    ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
    return(ft_out)
  }
  
  # if (is.null(exportDocName)){
  #   exportDoc_name <- glue::glue("{project_name}_{cur_zone_zero}_PARAMS.docx")
  # } else {
  #   exportDoc_name <- exportDocName
  # }
  
  # Export
  param_table <- FitFlextableToPage(params_df_toConvert)
  
  return(param_table)
  
}