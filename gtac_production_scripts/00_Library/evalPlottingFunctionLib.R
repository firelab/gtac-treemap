# SETUP
# Required libraries
list.of.packages <- c("tidyverse",
                      "dplyr",
                      "flextable",
                      "ggplot2",
                      "officer", 
                      "docstring", 
                      "roxygen2",
                      "glue", 
                      "ggrepel")

#check for packages and install if needed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages)

# load all packages
vapply(list.of.packages, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)

# ----------------------------------------------------------------------------------------

# Bar plot: Raw frequency, observed vs. predicted for each class 
  
plot_barchart_raw <- function(df, var_in, zone_num, plot_labels = c("Imputed", "Observed"), 
                              save_Plot, exportDir){
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

    if(zone_num <10) {
      zone_num <- paste0("0", zone_num)
    }
  
    barchart1 <- df %>% # format frequency table for plotting
                    tidyr::pivot_longer(!class, names_to = "dataset") %>%
                    dplyr::rename(frequency = "value") %>%
                    dplyr::arrange(dataset, class) %>% 
                    ggplot() + # plot
                    geom_col(aes(x = class, y = frequency, fill = factor(dataset)), position = "dodge") +
                    scale_fill_manual(name = "Dataset", labels = c("Reference (X-table)", plot_labels),values = c("springgreen4", "aquamarine3", "chartreuse4"))+
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

plot_barchart_norm <- function(df, var_in, zone_num, plot_labels = c("Imputed", "Observed"),
                               save_Plot, exportDir ){
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

  if(zone_num <10) {
    zone_num <- paste0("0", zone_num)
  }
  
  barchart2 <- df %>% # format frequency table for plotting
                  tidyr::pivot_longer(!class, names_to = "dataset") %>%
                  dplyr::rename(frequency = "value") %>%
                  dplyr::mutate(dataset= factor(dataset)) %>%
                  dplyr::arrange(dataset, class) %>%
                  ggplot() + # plot
                  geom_col(aes(x = class, y = frequency, fill = factor(dataset)), position = "dodge") +
                  scale_fill_manual(name = "Dataset", labels = c("Reference (X-table)", plot_labels),values = c("springgreen4", "aquamarine3", "chartreuse4"))+
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

  if(zone_num <10) {
    zone_num <- paste0("0", zone_num)
  }
  
    scatter_plot1 <- df %>%
                        ggplot()+
                        geom_point(aes(x = n, y = value), size = 2) + 
                        geom_label_repel(aes(x = n, y = value, label = class),
                          box.padding   = 0.6, 
                          point.padding = 0,
                          label.padding = 0.2,
                          segment.color = 'grey50',
                          size=3.3,
                          min.segment.length = 0,
                          max.time = 3) +
                        theme_bw() +
                        labs(x = "total N of class in reference data", y = "Balanced Accuracy") + 
                        ggtitle(glue::glue("Accuracy vs. N Reference pts per class
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

  if(zone_num <10) {
    zone_num <- paste0("0", zone_num)
  }
  
    scatter_plot2 <- df %>%
                        ggplot()+
                        geom_point(aes(x = n, y = value), size = 2) + 
                        geom_label_repel(aes(x = n, y = value, label = class),
                          box.padding   = 0.6, 
                          point.padding = 0,
                          label.padding = 0.2,
                          segment.color = 'grey50',
                          size=3.3,
                          min.segment.length = 0,
                          max.time = 3) +
                        theme_bw() +
                        labs(x = "total N of class in Imputed observations", y = "Balanced Accuracy") + 
                        ggtitle(glue::glue("Accuracy vs. N Imputed Obs per class
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

# Normalized Frequency Density Plot


plot_density <- function(df, var_in, zone_num, save_Plot, exportDir){
  #' Title
  #'
  #' @param df `freq_t2` data frame created from normalizing the `freq` table
  #' @param var_in Variable that is evaluated, set `var_in = var_in` for report generation
  #' @param zone_num Zone number for evaluation, set `zone_num = zone_num` for report generation
  #' @param save_Plot Boolean. If `T`, plot will be saved in the output dir
  #' @param exportDir Directory to export plots to IF `savePlot == T` 
  #'
  #' @return ggplot object
  #' @export
  #'
  #' @examples
  
  if(zone_num <10) {
    zone_num <- paste0("0", zone_num)
  }

  density_plot <- df %>%
    ggplot(aes(x=as.numeric(as.character(class)),weight=frequency, color=dataset))+
    geom_density(size=1.2, alpha=0.4, fill="grey50")+
    scale_color_brewer(palette="Set2", labels = c("Reference (X-table)", plot_labels))+
    labs(x = "EVC (%)", y = "Density", 
         fill = "Data")+
    theme_bw()+
    theme(legend.title=element_blank())+
    labs(x = var_in, y = "Density")+
    ggtitle(glue::glue("Density Comparison
                                Zone: z{zone_num} ; Attribute: {var_in}")) 
  
  if (save_Plot == "T"){
    
    ggplot2::ggsave(filename = glue::glue("distribution_density_{var_in}_{zone_num}.png"),
                    density_plot, 
                    path = exportDir)
    
    print(glue::glue("Plot exported to: {exportDir}"))
    
  }
  
  return(density_plot)
  
}



# Report Rendering
########################################################

FitFlextableToPage <- function(ft, pgwidth){
  
  ft_out <- ft %>% autofit()
  
  ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
  return(ft_out)
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
  
  
  # if (is.null(exportDocName)){
  #   exportDoc_name <- glue::glue("{project_name}_{cur_zone_zero}_PARAMS.docx")
  # } else {
  #   exportDoc_name <- exportDocName
  # }
  
  # Export
  param_table <- FitFlextableToPage(params_df_toConvert, pgwidth = 6)
  
  return(param_table)
  
}

#---------------------------------------------------------------------------#

paramsCSV_toFlexTable <- function(csv){
  #' Convert params variables loaded via CSV to a Word document table
  #'
  #' @param paramsObj Object containing parameters loaded via CSV. The default name is `params_out` when using `load(RDS-path)`. Required column names are "param" and "value".
  #'
  #' @return
  #' @export Flextable (formatted)
  #'
  #' @examples
  
  # inspo: https://datafortress.github.io/en/2018/06/word-tables-with-r/ 
  
  # Convert params object to data.frame
  params_df <- data.frame(csv) %>% 
    dplyr::rename(Parameter = param, 
                  Value = value)
  
  # remove row.name column ("X"; if it exists)
  if (names(params_df[1]) == "X"){
      params_df = params_df[-1]
  }
  
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
  
  
  # if (is.null(exportDocName)){
  #   exportDoc_name <- glue::glue("{project_name}_{cur_zone_zero}_PARAMS.docx")
  # } else {
  #   exportDoc_name <- exportDocName
  # }
  
  # Export
  param_table <- FitFlextableToPage(params_df_toConvert, pgwidth = 6)
  
  return(param_table)
  
}

evalRDS_toFlexTables <- function(paramsObj){
    #' Convert eval report stats RDS to flextables
    #'
    #' @param paramsObj Object containing parameters loaded from the eval report stats RDS
    #'
    #' @return `list` of `flextables` formatted to insert in rendered documents
    #' @export 
    #'
    #' @examples
    
    # inspo: https://datafortress.github.io/en/2018/06/word-tables-with-r/ 
    
    # initialize vectors to compile as columns for `params_df` data.frame
    Parameter <- c()
    Value <- c()
    
    # Parse through the RDS object to separate the 1:many key-value (i.e., nested table of accuracies) from the 1:1 key-values (i.e., other parameters)
    for (item in 1:length(paramsObj)){
        
        if(is.data.frame(paramsObj[[item]]) == TRUE){
            oaTable_df <- paramsObj[[item]] %>%
                dplyr::mutate_all(as.character)
            
        } else {
            Parameter <- c(Parameter, names(paramsObj[item]))
            Value <- c(Value, paramsObj[[item]])
        }
        
    }
    
    # Data.frame of eval report parameters and stats
    params_df <- data.frame(Parameter, Value)    

    # Convert df to flextable and autofit contents
    params_df_toConvert <- flextable::flextable(data = params_df) %>%
        flextable::font(fontname = "Cambria", i = 1, j = NULL) %>% 
        flextable::bold(i = 1, j = NULL, bold = TRUE, part = "header") %>% 
        flextable::width(j = 1, width = 0.25, unit= "in") %>% 
        flextable::font(fontname = "Consolas", j = 1) %>% 
        flextable::font(fontname = "Calibri", j = 2)
    # flextable::autofit()
    
    # Convert OA table data.frame to formatted `flextable`    
    oaTable_df_toConvert <- flextable::flextable(data = oaTable_df) %>%
        flextable::font(fontname = "Cambria", i = 1, j = NULL) %>% 
        flextable::bold(i = 1, j = NULL, bold = TRUE, part = "header") %>% 
        flextable::width(j = 1, width = 0.25, unit= "in") %>% 
        flextable::font(fontname = "Consolas", j = 1) %>% 
        flextable::font(fontname = "Calibri", j = 2) 
    
    
    
    # Export
    param_table_forRMD <- FitFlextableToPage(params_df_toConvert, pgwidth = 6)
    
    OA_table_forRMD <- FitFlextableToPage(oaTable_df_toConvert, pgwidth = 6)
    
    
    return(list(param_table_forRMD, OA_table_forRMD))
    
}


listObjects_toWord <- function(listObjects, exportDir, exportName, project_name, cur_zone_zero, eval_type){
  #' Place flextables and variables in a Word document
  #'
  #' @param listObjects `list` of flextables to be placed into the Word document
  #' @param exportDir `string`; output directory
  #' @param exportName `string`; output Word document name
  #' @param project_name `output_name` variable while running TreeMap production script (for file naming)
  #' @param cur_zone_zero `cur_zone_zero` variable while running TreeMap production script (for file naming)
  #' @param eval_type `eval_type_in` variable while running TreeMap production script (for file naming)
  #'
  #' @return exports word document in the `exportDir` as "`exportName`.docx"
  #' @export
  #'
  #' @examples
  
  # Create empty word doc
  export_doc <- officer::read_docx()
  
  # Add title for table in empty word doc
  export_doc <- officer::body_add_par(export_doc,
                                      value = glue::glue("{project_name}, {cur_zone_zero} '{eval_type}' evaluation stats"),
                                      style = "centered") %>%
                officer::body_add_par(value = "\n")
  
  for (item in 1:length(listObjects)){
    
    if (class(listObjects[[item]]) == "flextable"){
      export_doc <- flextable::body_add_flextable(export_doc, value = listObjects[[item]]) %>%
                      officer::body_add_par(value = "\n")
    } else {
      export_doc <- flextable::body_add_par(export_doc, 
                                            value = listObjects[[item]], 
                                            style = "Normal") %>%
                                officer::body_add_par(value = "\n")
      
    }
    
  }
  
  
  if (is.null(exportName)){
    exportName <- glue::glue("{project_name}_{cur_zone_zero}_model_eval_stats")
  } else {
    exportName <- exportName
  }
  
  exportPATH <- glue::glue("{exportDir}/{exportName}.docx")
  
  print(export_doc, target = exportPATH)
  
}