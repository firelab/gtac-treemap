server = function(input, output, session){
  
  # Initialize flag variable to check if RDS is loaded
  loadEvalRDS_flag <- ""
  
  # LOAD EVAL DATA BASED ON EVAL PARAMS
  observeEvent(input$load, {
    
    withProgress(
      
      message='LOADING EVALUATION DATA...',
      
      value=0, {
        n <- 4
        # Passed inputs
        #---------------------------------------------#
        
        zone <<- input$zone_num
        
        # Project name
        project_name <<- input$project_name
        
        # output name - name of raster and CM outputs
        # output_name <<- input$output_Name
        
        if (project_name == "2020_Production"){
          output_name = project_name
        } else if (project_name == "2022_Production"){
          output_name = "2022_GTAC_Production"
        }
        
        # eval type
        eval_type <<- input$eval_type
        
        
        # Prep and construct paths
        #------------------------------------------#
        
        
        # Create dictionary for eval_RDS_dir per by evaluation type # "model_eval", "TargetLayerComparison", "OOB_manual", "CV"
        eval_type_dir_dict <<- c("model_eval"            = "00_Model_Evaluation", 
                                 "TargetLayerComparison" = "01_Target_Layer_Comparison", 
                                 "OOB_manual"            = "02_OOB_Manual_Evaluation", 
                                 "CV"                    = "03_Cross_Validation")
        
        
        # Load library
        #-------------------------------------------------------#
        
        this.path <- this.path::this.path()
        
        # get path to library script
        spl1 <- stringr::str_split(this.path, "/")[[1]]
        lib_path <- paste(c(spl1[c(1:(length(spl1) - 4))],
                            "00_Library/treemapLib.R"),
                          collapse = "/")
        
        source(lib_path)
        
        # Initialize home dir
        #-----------------------------------------------#
        # Id where THIS script is located
        this.path <- this.path::this.path()
        
        # get path to input script
        spl <- stringr::str_split(this.path, "/")[[1]]
        setup_dirs.path <- paste( c(spl[c(1:(length(spl)-4))],
                                    "00_Library/setup_dirs.R" ),
                                  collapse = "/")
        
        source(setup_dirs.path)
        
        # get path to evaluation plotting function library
        evalPlotFunctions_lib_path <- paste(c(spl[c(1:(length(spl) - 4))],
                                              "00_Library/evalPlottingFunctionLib.R"),
                                            collapse = "/")
        
        source(evalPlotFunctions_lib_path)
        
        raw_outputs_dir <- glue::glue("{home_dir}03_Outputs/07_Projects/{project_name}/01_Raw_model_outputs/")
        
        # Other settings
        #--------------------------------------------------#
        
        # set number of digits to round to
        round_dig <<- 4
        
        #------------------------------------------------------#
        #                   Load evaluation data
        #------------------------------------------------------#
        
        # Set zone name options
        cur_zone <- glue::glue('z{zone}')
        cur_zone_zero <<- if(zone < 10) {
          glue::glue('z0{zone}') 
        } else {
          cur_zone
        }
        
        # Production eval directory
        eval_dir <<- glue::glue("{home_dir}03_Outputs/07_Projects/{project_name}/03_Evaluation/")
        varImpPlot_path <<- glue::glue("{raw_outputs_dir}{cur_zone_zero}/model_eval/{cur_zone_zero}_{output_name}_varImp.png")
        
        # Zonal eval directory:
        evalDir_ofZone <<- glue::glue("{eval_dir}{cur_zone_zero}/")
        
        eval_type_Dir_ofZone <<- glue::glue("{evalDir_ofZone}{eval_type_dir_dict[[eval_type]]}/")
        
        # Load evaluation type statistics RDS
        evalTypeStatsRDS_Path <<- glue::glue("{eval_type_Dir_ofZone}{cur_zone_zero}_{output_name}_{eval_type}_STATS.RDS")
        
        # Load evaluation CMs RDS
        if (eval_type == "model_eval"){
          evalTypeCMsRDS_Path <- glue::glue("{raw_outputs_dir}{cur_zone_zero}/model_eval/{cur_zone_zero}_{output_name}_CMs_ResponseVariables.RDS")
          plot_labels <<- c("Predicted - RF", "Reference - RF")
          cm_labels <<- c("Predicted - RF", "Reference - RF")
        } else {
          evalTypeCMsRDS_Path <- glue::glue("{eval_type_Dir_ofZone}{cur_zone_zero}_{output_name}_CMs_{eval_type}.RDS")
          plot_labels <<- c("Imputed", "Target")
          cm_labels <<- c("Imputed", "Target")
        }
        
        incProgress(1/n, message = paste("Inputs processed, loading eval RDS..."))
        
        evalTypeStatsRDS <<- readRDS(evalTypeStatsRDS_Path)
        evalTypeCMsRDS_all <<- readRDS(evalTypeCMsRDS_Path)
        
        # incProgress(1/n, message = paste("RDS loaded, rendering var imp plot..."))
        # # Load RF imp plot (.png)
        # output$RF_varImpPlot <- renderImage({list(src = normalizePath(varImpPlot_path), 
        #                                           alt = "Variable importance plot for imputation model", 
        #                                           width = 800,
        #                                           height = 600)
        #                                     }, 
        #                                     deleteFile = FALSE)
        
        # OUPUT general evaluation stats
        
        output$run_name                             <- renderText(evalTypeStatsRDS$run_name)
        output$unique_pltsInZone                    <- renderText(evalTypeStatsRDS$unique_pltsInZone)
        output$plts_availableInZone                 <- renderText(evalTypeStatsRDS$plts_availableInZone)
        output$percent_availablePlts_imputedInZone  <- renderText(paste0(evalTypeStatsRDS$percent_availablePlts_imputedInZone, "%"))
        output$oaSummaryTable <- renderTable(evalTypeStatsRDS$OA_table, rownames = FALSE)
        
        loadEvalRDS_flag <<- "1"
        
        incProgress(1/n, message = paste("Eval RDS is loaded, please proceed to steps 2. and 3.!"))
      })
    
  })
  
  # FILTER TO EVALUATION VARIABLE OF CHOICE
  observeEvent(input$create, {
    if(loadEvalRDS_flag == ""){
      showModal(modalDialog(title ="Warning!!!", "Please complete '1. Evaluation parameters:' section first. The 2. and 3. sections require the evaluation data to be loaded!!!"))
    } else {
      # React to eval var selected:
      # eval var
      eval_vars <<- input$eval_vars
      
      # Prep frequency table
      #---------------------------------------------#
      
      evalTypeCMsRDS <- evalTypeCMsRDS_all[[eval_vars]]
      
      freq <-
        evalTypeCMsRDS$freq %>%
        #select fields of interest
        dplyr::select(class, ref, pred) %>%
        dplyr::mutate(class = factor(class))
      
      n_classes <- nrow(freq)
      
      # Calculate normalized frequency table 
      #--------------------------------------------#
      
      # calc total to use in normalizing
      total_pred = sum(freq$pred)
      total_ref = sum(freq$ref)
      
      # add normalized frequency
      freq_norm <- freq %>%
        dplyr::mutate(pred = pred/total_pred, 
                      ref = ref/total_ref)
      
      # Prep CM Classes
      #---------------------------------------------#
      
      # get single cm for classes
      cm_classes <- evalTypeCMsRDS$classes
      
      #manipulate
      cm_classes %<>%
        tidyr::pivot_longer(!metric, names_to = "class") %>%
        dplyr::mutate(class = factor(class)) %>%
        dplyr::mutate(value = round(value, round_dig))
      
      # get number of classes
      nclass = length(unique(cm_classes$class))
      
      
      # Prep confusion matrix for display
      # -------------------------------------------------#
      # 
      
      # Prep raw CM
      #------------------------------------------------#
      
      #load cm_raw table
      cm_raw <- as.data.frame.matrix(evalTypeCMsRDS$raw)
      
      # Prep cm overall metrics table
      #------------------------------------------------------#
      
      cm_overall <- evalTypeCMsRDS$overall
      
      #format
      cm_overall %<>% 
        dplyr::mutate(value = round(value, round_dig))
      
      row.names(cm_overall) <- NULL
      
      
      # Prep confusion matrix for display
      # -------------------------------------------------#
      
      #get overall accuracy 
      oa <- cm_overall %>%
        dplyr::filter(metric == "Accuracy") %>%
        dplyr::select(value) 
      
      oa %<>%
        round(round_dig)
      
      print(glue::glue("*Overall accuracy: {oa*100}%*"))    
      
      #get overall accuracy 
      oa <- cm_overall %>%
        dplyr::filter(metric == "Accuracy") %>%
        dplyr::select(value) 
      
      oa %<>%
        round(round_dig)
      
      print(glue::glue("*Overall accuracy: {oa*100}%*"))
      
      # Get producer's and user's accuracy for CM
      #---------------------------------------------------------------------------------------#
      
      # get producer's accuracy for each class - precision
      pa <- cm_classes %>%
        dplyr::filter(metric == "Precision") %>%
        dplyr::mutate(value = round(value, round_dig))  %>%
        dplyr::select(value) %>%
        # replace NA with character
        dplyr::mutate(value = as.character(value)) %>%
        tidyr::replace_na(list(value = "NA")) %>%
        #rename
        dplyr::rename("Prod. Acc" = value) %>%
        # add extra row to be able to bind
        rbind(c("NA"))
      
      # get user's accuracy for each class - recall
      ua <- cm_classes %>%
        dplyr::filter(metric == "Recall") %>% 
        dplyr::mutate(value = round(value, round_dig))  %>%
        dplyr::select(value) %>%
        # replace NA with character
        dplyr::mutate(value = as.character(value)) %>%
        tidyr::replace_na(list(value = "NA")) 
      
      # join producer's accuracy
      cm_display <- cbind(cm_raw, pa)
      
      #format user's accuracy to be able to bind
      # create list of length that will make it match size of cm_display
      b <- data.frame(value = rep("NA", ncol(cm_display)- nrow(ua)))
      ua <-
        #c("User's Acc") %>%
        rbind(ua, b) %>%
        # rename
        dplyr::rename("User's Acc" = value) %>%
        t() %>%
        data.frame()
      
      # add names to ua be able to bind
      names(ua) <- names(cm_display)
      
      #bind ua with cm
      cm_display <- rbind(cm_display, ua)
      
      # make row names a column
      cm_display$Imputed <- row.names(cm_display)
      row.names(cm_display) <- NULL
      
      # bind row name column back with df
      cm_display <- cbind(Imputed = cm_display$Imputed, cm_display[1:ncol(cm_display)-1])
      
      # substitute desired column name
      names(cm_display) <- c(cm_labels[1], names(cm_display)[2:ncol(cm_display)])
      
      # Format flextable
      # - Add contrast background colors for accuracy
      # - Highlight diagonal values
      
      
      table_cm <- flextable::flextable(cm_display) %>%
        # add header and footer
        flextable::add_header_row(values = c("", cm_labels[2]), colwidths = c(1, ncol(cm_display)-1)) %>%
        flextable::add_footer_row(values = c("Overall accuracy", oa), colwidths = c(ncol(cm_display)-1, 1)) %>%
        # add background color for producer's and user's acc rows
        flextable::bg(i = 1:nclass, j = nclass+3, bg = "darkseagreen1") %>%
        flextable::bg(i = nclass+2, j = 1:nclass+1, bg = "darkseagreen1") %>%
        # bold text
        flextable::style(i = -1:nclass+2, j = c(1,nclass+3), fp_text_default(bold = TRUE)) %>%
        flextable::style(i = nclass+2, j = 1:nclass+2, fp_text_default(bold = TRUE)) %>%
        flextable::bold(part = "header") %>%
        flextable::bold(part = "footer") %>%
        # add background color for overall accuracy
        flextable::bg(j = nclass + 3,  bg = "green4", part = "footer")%>%
        # make font smaller
        flextable::fontsize(size = 8, part = "all") %>%
        # add borders to body cells
        flextable::surround(border = fp_border_default(color = "gray", width = 1), part = "body") %>%
        # add title
        flextable::set_caption(glue::glue("Confusion Matrix
             Zone: z{zone} ; Attribute: {eval_vars}"))
      
      # highlight diagonal values
      for (r in 1:(nrow(cm_display)-1)) {
        table_cm %<>%
          flextable::surround(i = r, j = r+1, border = flextable::fp_border_default(color = "cornflowerblue", width = 1.5), part = "body")
      }
      
      
      
      # fit to page
      table_cm <- FitFlextableToPage(ft = table_cm,
                                     pgwidth = 7)
      
      
      output$outputCM <- renderUI({table_cm %>% 
          htmltools_value()})
      
      if (eval_vars == "evt_gp"){
        
        if (project_name == "2020_Production"){
          
          LF_evt_gp_numNameCSV_path <- glue::glue('{home_dir}07_Documentation/01_Validation/02_Eval_tools/LF20_EVT_220_forJoin.csv')
          evt_gp_remapTable_path <- glue::glue('{home_dir}03_Outputs/05_Target_Rasters/v2020/post_mask/{cur_zone_zero}/evt_gp_remap.csv')
          
        } else if (project_name == "2022_Production"){
          
          LF_evt_gp_numNameCSV_path <- glue::glue('{home_dir}07_Documentation/01_Validation/02_Eval_tools/LF23_EVT_240_forJoin.csv')
          evt_gp_remapTable_path <- glue::glue('{home_dir}03_Outputs/05_Target_Rasters/v2022/post_mask/{cur_zone_zero}/evt_gp_remap.csv')
          
        }
        
        
        LF_evt_gp_numName_joinTable <- as.data.frame(read.csv(LF_evt_gp_numNameCSV_path))
        evt_gp_remapTable <- as.data.frame(read.csv(evt_gp_remapTable_path))
        
        # rearrange columns
        evt_gp_remapTable <- evt_gp_remapTable %>% dplyr::select("EVT_GP_remap", "EVT_GP")
        
        # Join with joinTable
        evt_gp_remapTable_joined_DT <- DT::datatable(dplyr::inner_join(x = evt_gp_remapTable, 
                                                                       y = LF_evt_gp_numName_joinTable, 
                                                                       by = c(EVT_GP = "EVT_GP")) %>% 
                                                       dplyr::select("EVT_GP", "EVT_GP_N") %>% 
                                                       dplyr::distinct() %>% 
                                                       dplyr::rename("EVT_GP name" = "EVT_GP_N"), 
                                                     options = list(iDisplayLength = 25), 
                                                     rownames = FALSE)
        
        
        output$evt_gp_remapTable <- DT::renderDataTable(evt_gp_remapTable_joined_DT, rownames = FALSE)
      } else {
        evt_gp_remapTable_joined_DT <- NULL
        output$evt_gp_remapTable <- DT::renderDataTable(evt_gp_remapTable_joined_DT, rownames = FALSE)
      }
      
    }
  })
  
  # DISPLAY ADDITIONAL DATA FROM SELECTED EVAL VARIABLE
  observeEvent(input$display, {
    if(loadEvalRDS_flag == ""){
      showModal(modalDialog(title ="Warning!!!", "Please complete '1. Evaluation parameters:' section first. The 2. and 3. sections require the evaluation data to be loaded!!!"))
    } else {
      # pass selected metric
      metric_type <- input$metric
      
      # subset RDS object to metric and pass into data frame
      subsetRDS_df <- as.data.frame(evalTypeCMsRDS_all[[eval_vars]][[metric_type]])
      
      if (metric_type %in% c("freq", "freq_norm")){
        subsetRDS_df <- subsetRDS_df %>% relocate(class)
      } else if (metric_type == "overall") {
        subsetRDS_df <- subsetRDS_df %>% relocate(metric)
      }
      
      if (metric_type == "classes"){
        subsetRDS_df_longer <<- subsetRDS_df %>% pivot_longer(cols = c(2:ncol(subsetRDS_df)))
      }
      
      
      #  a data table
      subsetRDS_dt <- DT::datatable(subsetRDS_df, 
                                    options = list(iDisplayLength = 25),
                                    rownames = FALSE)
      
      output$displayMetricTable <- DT::renderDataTable(subsetRDS_dt, rownames = FALSE)
    }
  })
  
  observeEvent(input$generate, {
    if(input$metric != "classes"){
      showModal(modalDialog(title ="Warning!!!", "This feature is currently only available for the 'classes' option Please regenerate metric table with 'classes' to produce the interactive plot!!!"))
    } else {
      
      output$classesPlot <- plotly::renderPlotly({p <- ggplot(data = subsetRDS_df_longer, aes(x = factor(name, level = c(unique(name))), y = value, group = metric, color = metric)) + 
        geom_point() + 
        geom_line(size = 1) + 
        xlab(eval_vars) +
        theme_bw()
      
      plotly::ggplotly(p)
      
      })
    }
    
  })
}