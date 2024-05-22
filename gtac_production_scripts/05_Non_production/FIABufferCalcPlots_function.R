FIABufferCalcPlots <- function(df, plotVARs, filterYear=2016, facetDistCode=F) {
  #' FIA Buffer evaluation plots
  #'
  #' @param df An object of class "data.frame". Output .csv from FIA Buffer Calc evaluation script
  #' @param plotVARs An object of class vector (1-D array). List of variables to be plot
  #' @param filterYear Type numeric. Year to filter the dataset with, might return errors if no data for given filter year is present
  #' @param facetDistCode Boolean (T or F). If TRUE plot will facet by disturbance code (None, Fire, Slow Loss)
  #'
  #' @return Printed ggplots. Barplots for catagorical and scatterplots for continuous
  #' @export
  #'
  #' @examples
  
  
  # Assign inputs to local variables (in function)
  p_r <- df
  plotVARs <- plotVARs
  filterYear <- filterYear
  facetDistCode <- facetDistCode
  
  
  # Housekeeping 
  
  # load libraries
  library(terra)
  library(tidyverse)
  library(magrittr)
  library(ggplot2)
  library(modeest)
  
  # Allow for sufficient digits to differentiate plot cn numbers
  options("scipen"=100, "digits"=8)
  
  # Define catagorical and continuous variables to be evaluated
  eval_vars_cat <- c("canopy_cover", "canopy_height", "EVT_GP", 
                     "disturb_year", "disturb_code")
  
  eval_vars_cont <- c("GSSTK", "QMD_RMRS", "SDIPCT_RMRS", 
                      "CANOPYPCT", "CARBON_L", "CARBON_D", 
                      "CARBON_DOWN_DEAD", "DRYBIO_L", "DRYBIO_D", 
                      "TPA_DEAD", "TPA_LIVE", "BALIVE")
  
  # Initialize the order of factor variables (for plotting)
  p_r$disturb_code <- factor(p_r$disturb_code, levels = c("None", "Fire", "Slow Loss"))
  p_r$dataset <- factor(p_r$dataset, levels = c("Reference", "LFOrig", "LCMSDist"))
  
  barplot_legendCols <- c("Reference" = "#00BA38",
                          "LFOrig" = "#F8766D", 
                          "LCMSDist" = "#619CFF")
  
  for (var_name in plotVARs) {
    
    if (var_name %in% eval_vars_cat){
    
      # PLOT CATEGORICAL VARS
      
      ggObjList <- list()
      
      if (var_name == "disturb_code"){
        
        distCode_frequencyDF <- p_r %>%
          filter(MaxOfINVYR >= filterYear) %>%
          # filter(var == var_name) %>%
          select(-c(var, PLOTID, CN_plot)) %>%
          group_by(ID, dataset, disturb_code)
        
        distCode_frequencyPlot <- ggplot(data = distCode_frequencyDF, aes(x = disturb_code, fill = dataset)) +
          geom_bar(position = position_dodge2(preserve = "single")) +
          labs(title = glue::glue('Variation in {var_name}'),
               subtitle = "FIA buffer plot (40 m)") +
          xlab(var_name) +
          theme_bw()
        
        print(distCode_frequencyPlot)
        
        
      } else {
        
        
      # FIA Buffer plots eval: modal value for categorical variable
      p_r_mode <- p_r %>%
        filter(MaxOfINVYR >= filterYear) %>%
        filter(var == var_name) %>%
        select(-c(var, PLOTID, CN_plot)) %>%
        group_by(ID, dataset, disturb_code) %>%
        reframe(Modal_value = modeest::mlv(value, method = "mfv")) %>%  # aggregate to modal value
        ungroup()
      
      
      
      
      p <- p_r_mode  %>% 
        ggplot(aes(x=as.factor(Modal_value),  fill=dataset)) + 
        geom_bar(position= position_dodge2(preserve = "single")) + # https://stackoverflow.com/questions/38101512/the-same-width-of-the-bars-in-geom-barposition-dodge
        {if(facetDistCode)facet_wrap(~disturb_code)} +
        labs(title = glue::glue('Variation in {var_name} by disturbance code (Modal count)'),
             # subtitle = "FIA buffer plot evaluation (40 m radius): any cell touching buffer"
        ) +
        xlab(var_name) +
        theme(plot.title = element_text(size = 14),
              # plot.subtitle = element_text(size = 10), 
              axis.title = element_text(size = 12), 
              axis.text =  element_text(size = 8)) +
        scale_fill_manual(name = "Dataset", 
                          values = barplot_legendCols, 
                          breaks =  c("Reference", "LFOrig", "LCMSDist"))+
        theme_bw()
      
      print(p)
      
  
      }
  
    
    } else if (var_name %in% eval_vars_cont) {
      
      text_size <- 4 # 5 for indvidual plots 3 for all plots in grid
      percent_x_textPos <- 0.50 # 0.4 for individual plots
      percent_y_textPos1 <- 0.99 # 0.96 for individual plots
      percent_y_textPos2 <- 0.78 # 0.96 for individual plots
      textBoxFill_ratioX <- 0.25
      textBoxFill_ratioY <- 0.04
      
        
        # Change NA values to 0 for continuous data 
        p_r1 <- p_r %>% 
          filter(var == var_name) %>%
          mutate(value = ifelse(is.na(value), 0, value))
        
        low_lim <- quantile((p_r1 %>% filter(var == var_name))$value, probs = 0.10, na.rm = TRUE)[[1]] # 20th quantile/percentile
        up_lim <- quantile((p_r1 %>% filter(var == var_name))$value, probs = 0.90, na.rm = TRUE)[[1]] # 80th quantile/percentile
        
        # plot as scatterplot
        p_r2 <- p_r1 %>%
          filter(MaxOfINVYR >= filterYear) %>%
          filter(var == var_name) %>%
          filter(value >= low_lim & value <= up_lim) %>%
          select(-c(var, PLOTID, CN_plot)) %>%
          ungroup() %>%
          pivot_wider(names_from = dataset, values_from = value, values_fn = ~ mean(.x, na.rm = TRUE)) %>% # aggregate to mean value
          arrange(ID) %>% 
          drop_na()
       
       
         # Create linear model
        lm_LF <- lm(LFOrig ~ Reference, data = p_r2)
        
        lm_LCMS <- lm(LCMSDist ~ Reference, data = p_r2)
        
        
        # Annotate with r-sq, root mean square error (RMSE), mean absolute error (MAE)  
        # Parsing the information saved in the model to create the equation to be added to the scatterplot as an expression # https://r-graphics.org/recipe-scatter-fitlines-text
        eqn_LF <- sprintf(
          "italic(r)^2 ~ '=' ~ %.2g * ',' ~~ RMSE ~ '=' ~  %.3g * ',' ~~ MAE ~ '=' ~  %.3g",
          summary(lm_LF)$r.squared,  # r-squared 
          sqrt(mean(lm_LF$residuals^2)), # https://www.statology.org/extract-rmse-from-lm-in-r/
          mean(abs(lm_LF$residuals)) # mean absolute error (MAE)
        )
        
        eqn_LCMS <- sprintf(
          "italic(r)^2 ~ '=' ~ %.2g * ',' ~~ RMSE ~ '=' ~  %.3g * ',' ~~ MAE ~ '=' ~  %.3g",
          summary(lm_LCMS)$r.squared,  # r-squared 
          sqrt(mean(lm_LCMS$residuals^2)), # https://www.statology.org/extract-rmse-from-lm-in-r/
          mean(abs(lm_LCMS$residuals)) # mean absolute error (MAE)
        )
        
        # Manually set legend colors
        
        legendColors <- c("LFOrig" = "purple", 
                          "LCMSDist" = "darkgreen")
        
        
        p2 <- p_r2 %>%
          ggplot(aes(x = Reference)) +
          geom_abline(intercept = 0, color = "red", linewidth = 0.5 ) +
          geom_point(aes(y = LFOrig, color = "LFOrig"), alpha = 0.25) +
          geom_point(aes(y = LCMSDist, color = "LCMSDist"), alpha = 0.25) +
          geom_smooth(method = "lm", formula = y~x,  aes(y = LFOrig, color = "LFOrig"), alpha = 0.15) +
          geom_smooth(method = "lm", formula = y~x, aes(y = LCMSDist, color = "LCMSDist"), alpha = 0.15) +
          # facet_wrap(~disturb_code) +
          scale_color_manual(values = legendColors, breaks = c("LFOrig", "LCMSDist")) +
          guides(color = guide_legend(title = "Dataset",
                                      keylength = 2,
                                      keyheight = 2,
                                      title.theme = element_text(size = 14),
                                      label.theme = element_text(size =12))) +
          guides(color = "none") + # un-comment to remove plot legend
          scale_x_continuous(expand = c(0, 0), limits = c(low_lim, up_lim)) + # starts x-axis from 0 and labels 0
          scale_y_continuous(expand = c(0, 0), limits = c(low_lim, up_lim)) + # starts y-axis from 0 and labels 0
          labs(x = "Reference (Ground_FIA)", 
               y = "Imputed") + 
          theme_bw() +
          # ggtitle(glue::glue("{var_name}: FIA buffer plot (40 m radius) evaluation")) +
          ggtitle(glue::glue("{var_name}")) +
          theme(axis.title = element_text(size = 16),
                plot.title = element_text(size = 18)) +
          annotate(geom="rect",
                   xmin = ((low_lim + up_lim)/2) - ((up_lim - low_lim)*textBoxFill_ratioX),
                   xmax = ((low_lim + up_lim)/2) + ((up_lim - low_lim)*textBoxFill_ratioX),
                   ymin = (percent_y_textPos1*max(p_r2$LCMSDist, na.rm = TRUE)) - ((up_lim - low_lim)*textBoxFill_ratioY),
                   ymax = (percent_y_textPos1*max(p_r2$LCMSDist, na.rm = TRUE)) + ((up_lim - low_lim)*textBoxFill_ratioY),
                   fill = "beige") +
          annotate(geom="text",
                   x = (low_lim + up_lim)/2,
                   y = (percent_y_textPos1*max(p_r2$LCMSDist, na.rm = TRUE)),
                   label = as.character(eqn_LF),
                   parse = TRUE,
                   color = "purple",
                   size = text_size) +
          annotate(geom="rect",
                   xmin = (low_lim + up_lim)/2 - ((up_lim - low_lim)*textBoxFill_ratioX),
                   xmax = (low_lim + up_lim)/2 + ((up_lim - low_lim)*textBoxFill_ratioX),
                   ymin = (percent_y_textPos2*max(p_r2$LCMSDist, na.rm = TRUE)) - ((up_lim - low_lim)*textBoxFill_ratioY),
                   ymax = (percent_y_textPos2*max(p_r2$LCMSDist, na.rm = TRUE)) + ((up_lim - low_lim)*textBoxFill_ratioY),
                   fill = "cadetblue1") +
          annotate(geom="text",
                   x = (low_lim + up_lim)/2,
                   y = (percent_y_textPos2*max(p_r2$LCMSDist, na.rm = TRUE)),
                   label = as.character(eqn_LCMS),
                   parse = TRUE,
                   color = "darkgreen",
                   size = text_size)
        
        print(p2)
        
    }
  }
   
} 
