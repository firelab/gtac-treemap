# Written by Lila Leatherman (lila.leatherman@usda.gov)

# Objective: Compare output rasters - extract values to 40 m buffered FIA plots (polygons)

# Last update: 04/24/24 (by Abhinav Shrestha; abhinav.shrestha@usda.gov)


########################################################
# Setup
########################################################

# Initialize home dir
#-----------------------------------------------#
# Id where THIS script is located
this.path <- this.path::this.path()

# get path to input script
spl <- stringr::str_split(this.path, "/")[[1]]
setup_dirs.path <- paste( c(spl[c(1:(length(spl)-2))],
                              "00_Library/setup_dirs.R" ),
                            collapse = "/")

source(setup_dirs.path)

# path to first raster to compare
r1_path <- glue::glue("{home_dir}/03_Outputs/07_Projects/2016_GTAC_Test/02_Assembled_model_outputs/z16/01_Imputation/2016_Orig_Test_keepinbag_ntree250_tilesz2000_nT36.tif")
r1_name <- "LFOrig"

# path to second raster to compare
r2_path <- glue::glue("{home_dir}/03_Outputs/07_Projects/2016_GTAC_LCMSDist/02_Assembled_model_outputs/z16/01_Imputation/2016_GTAC_LCMSDist_tilesz2000_nT36.tif")
r2_name <- "LCMSDist"

# path to save output figures
export_fig_path <- glue::glue("{home_dir}03_Outputs/07_Projects/2016_GTAC_LCMSDist/03_Evaluation/z16/04_Model_Comparison/")

# export_fig_path <- glue::glue("C:/Users/abhinavshrestha/OneDrive - USDA/Documents/02_TreeMap/temp_dir/") # for testing

# list variables to evaluate
eval_vars_cat <- c("canopy_cover", "canopy_height", "EVT_GP", 
                   "disturb_year", "disturb_code")

eval_vars_cont <- c("GSSTK", "QMD_RMRS", "SDIPCT_RMRS", 
                    "CANOPYPCT", "CARBON_L", "CARBON_D", 
                    "CARBON_DOWN_DEAD", "DRYBIO_L", "DRYBIO_D", 
                    "TPA_DEAD", "TPA_LIVE", "BALIVE")

# path to shapefile or coords of points
pts_path <- glue::glue("{FIA_dir}/03_FullShp/FIA_US.shp")

# path to xtable or similar 
xtable_path <- glue::glue("{home_dir}/03_Outputs/06_Reference_Data/v2016_RMRS/X_table_all_singlecondition.txt")

# set location of raster attribute table
rat_path <- glue::glue("{home_dir}01_Data/01_TreeMap2016_RDA/RDS-2021-0074_Data/Data/")

# path to evt_gp metadata
evt_path <- glue::glue("{home_dir}01_Data/02_Landfire/LF_200/EVT/LF2016_EVT_200_CONUS/CSV_Data/LF16_EVT_200.csv")

# path to coords
coords_path <- glue::glue("{FIA_dir}/06_Coordinates/select_TREEMAP2022_2send/select_TREEMAP2022_2send.csv")

# crs raster data are in 
landfire_crs <- terra::crs(glue::glue('{home_dir}/01_Data/02_Landfire/landfire_crs.prj'))


# libraries
#--------------------------------------------------------#

# install dev version
#remotes::install_github("corybrunson/ggalluvial@main", build_vignettes = TRUE)
library(ggalluvial)

# load libraries
library(terra)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(modeest)

# Other options
# --------------------------------#

# Allow for sufficient digits to differentiate plot cn numbers

options("scipen"=100, "digits"=8)

#########################################################
# Run
#########################################################

# Load data
#----------------------------------------------#

# load raster 1 for comparison
r1 <- terra::rast(r1_path)
names(r1) <- "PLOTID"

# load raster 2 for comparison
r2 <- terra::rast(r2_path)
names(r2) <- "PLOTID"

# load shp / pts to extract to 
# pts <- terra::vect(pts_path)

# load x.df
X.df <- read.csv(xtable_path) %>%
  rename(PLOTID = ID)

# load evt dat
evt_dat <- read.csv(evt_path) 

# load coords
coords <- read.csv(coords_path)

# Load raster attribute table
#-------------------------------------------------#
rat <- terra::rast(glue::glue('{rat_path}TreeMap2016.tif'))
rat <- data.frame(cats(rat))

rat %<>% 
  rename("SDIPCT_RMRS" = SDIPCT_RMR,
         "CARBON_DOWN_DEAD" = CARBON_DWN) %>%
  mutate(CN = as.numeric(CN)) %>%
  select(-Value)

#names(rat)

# prep evt data
#------------------------------------------------------#
evt_dat %<>%
  select(EVT_GP, EVT_GP_N, RED, GREEN, BLUE) %>%
  filter(!is.na(EVT_GP)) %>%
  distinct() %>%
  group_by(EVT_GP) %>%
  slice_head()  

# Prep pts
#----------------------------------#

# OLD CODE -- NOT NEEDED (?)

# project pts 
# pts %<>% terra::project(r1)
# 
# # convert to data frame
# pts_df <- data.frame(pts)

# Prep coords data
#----------------------------------------------------------------#
coords %<>%
  filter(PLT_CN %in% rat$CN) # limit to CNs with reference values in RAT

# separate out into data frame
coords_df <- coords


# convert coords to spatial 
coords <- terra::vect(coords, geom = c("ACTUAL_LON", "ACTUAL_LAT"), crs = "epsg:4269")

coords_subset <- coords[5500]

# Set buffer width
buffer_width <- 40

# convert FIA points to polygon with <buffer width> radius 
coords_buffer <- terra::buffer(coords, width = buffer_width)

# Store attributes of FIA plots (polygons) in a data frame for joining later
coords_buffer_df <- data.frame(coords_buffer)


## For testing
# coords_bufferSubset <- coords_buffer[5799]
# coords_bufferSubset_df <- data.frame(coords_bufferSubset)

# Prep data to plot
#----------------------------------------------------------#

# Data Dictionary for extracts / Outputs:
# ID: row number of FIA point in original coords
# PLOTID: imputed treemap plot id
# CN_pt : CN of FIA point in original coords
# CN_plot: CN of plot imputed to fia point

# extract  values to points - imputed plot ID at original FIA point
# -----------------------------------------------#

# Extract r1 values of cell centers (default setting) that fall in polygon to data.frame
# - `ID = TRUE` parameter ensures the index of the polygon (coords_buffer) is added to the data.frame (used to join later)
# - `touches = TRUE` parameter enables extraction from any raster cells that touch the polygon (the boundary of fall within)
r1_ex_buffer <- terra::extract(r1, coords_buffer, ID = TRUE) 

# Add a new attribute `CN_pt` to the extract raster
r1_ex_buffer$CN_pt <- NA

for (i in 1:nrow(r1_ex_buffer)){
  
  if (!is.na(r1_ex_buffer$PLOTID[i])) {
    
  # Add CN_pt of FIA plot from coords_buffer_df to extract data.frame using polygon Index (`ID`)  
  r1_ex_buffer$CN_pt[i] <- coords_buffer_df$PLT_CN[r1_ex_buffer$ID[i]]
  
  }
}

r1_ex_buffer <- r1_ex_buffer %>%
                  filter(!is.na(PLOTID)) # remove NAs



# Extract raster cell values over polygon
r2_ex_buffer <- terra::extract(r2, coords_buffer, ID = TRUE) 

# Add a new attribute `CN_pt` to the extract raster
r2_ex_buffer$CN_pt <- NA

for (i in 1:nrow(r2_ex_buffer)){
  
  if (!is.na(r2_ex_buffer$PLOTID[i])) {
    
    # Join with CN_pt of FIA plot data frame to raster cells using polygon Index (`ID`)
    r2_ex_buffer$CN_pt[i] <- coords_buffer_df$PLT_CN[r2_ex_buffer$ID[i]]
    
  }
}

r2_ex_buffer <- r2_ex_buffer %>%
  filter(!is.na(PLOTID)) # remove NAs


# join extracts with x table and RAT - join ri_ex$PLOTID to RAT$tm_id
# to get values of the imputed plot

r1_ex_buffer %<>% left_join(X.df,
                           by = c("PLOTID" = "PLOTID")) %>%
  left_join(rat, by = c("CN" = "CN", "PLOTID" = "tm_id")) %>%
  rename(CN_plot = "CN") %>%
  select(c(ID, CN_pt, CN_plot, PLOTID, any_of(c(eval_vars_cat, eval_vars_cont)))) %>%
  mutate(dataset = r1_name) # add name of source dataset in the `dataset` column


r2_ex_buffer %<>% left_join(X.df,
                            by = c("PLOTID" = "PLOTID")) %>%
  left_join(rat, by = c("CN" = "CN", "PLOTID" = "tm_id")) %>%
  rename(CN_plot = "CN") %>%
  select(c(ID, CN_pt, CN_plot, PLOTID, any_of(c(eval_vars_cat, eval_vars_cont)))) %>%
  mutate(dataset = r2_name) # add name of source dataset in the `dataset` column

# prep reference values - from "X.df / reference table" 

refs_buffer <- 
  r1_ex_buffer %>% 
  select(ID, CN_pt) %>%
  left_join(X.df, by = c("CN_pt" = "CN")) %>%
  left_join(rat, by = c("CN_pt" = "CN")) %>%
  mutate(CN_plot = as.numeric(NA)) %>%
  mutate(PLOTID = as.numeric(NA)) %>%
  select(c(ID, CN_pt, CN_plot, PLOTID, any_of(c(eval_vars_cat, eval_vars_cont)))) %>%
  mutate(dataset = "Reference") 

# join

p_r_buffer <- bind_rows(r1_ex_buffer, r2_ex_buffer, refs_buffer) %>%
  # pivot longer
  pivot_longer(!c(ID, CN_pt, CN_plot, PLOTID, dataset, disturb_code), names_to = "var", values_to = "value") %>%
  mutate(var = factor(var),
         # value = na_if(value, -99.00000), # update NA values from RAT
         disturb_code = factor(disturb_code, 
                               labels = c("None", "Fire", "Slow Loss"))) %>%
  group_by(disturb_code) %>% 
  left_join(coords_df %>% select(PLT_CN, MaxOfINVYR), by = c("CN_pt" = "PLT_CN"))


## EXPORT buffer extraction data as a .csv
# - buffer extractions takes substantial compu


# save as .csv
# write.csv(p_r_buffer, file.path(export_fig_path, "FIA_BufferPlots_LF_LCMS_2_noNA.csv"), row.names = FALSE)

# load saved buffer plot csv
p_r <- data.frame(read.csv(file.path(export_fig_path, "FIA_BufferPlots_LF_LCMS.csv")))


# Initialize the order of factor variables (for plotting)

p_r$disturb_code <- factor(p_r$disturb_code, levels = c("None", "Fire", "Slow Loss"))
p_r$dataset <- factor(p_r$dataset, levels = c("Reference", "LFOrig", "LCMSDist"))

barplot_legendCols <- c("Reference" = "#00BA38",
                        "LFOrig" = "#F8766D", 
                        "LCMSDist" = "#619CFF")

# Select year to filter with (inclusive)
filterYear <- 2016



# =================================== PLOT ===================================
#-----------------------------------------------------------#
# plot as desired


# grouped violin plots by disturbance code


# PLOT CATEGORICAL VARS
# ---------------------------------------------------#

ggObjList <- list()

for(i in 1:(length(eval_vars_cat)) {
  
  # for testing
  # i = 1
  
  var_name <- eval_vars_cat[i]

  # FIA Buffer plots eval: modal value for categorical variable
  p_r_mode <- p_r %>%
    filter(MaxOfINVYR >= filterYear) %>%
    # filter(var == var_name) %>%
    select(-c(var, PLOTID, CN_plot)) %>%
    group_by(ID, dataset, disturb_code) %>%
    reframe(Modal_value = modeest::mlv(value, method = "mfv")) %>%  # aggregate to modal value
    ungroup()
  
  
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
  
  # ggsave(glue::glue('{export_fig_path}/FIABufferPlotEval/cell_centriod_withinBuffer/Barplots_categorical_vars/{r1_name}_vs_{r2_name}_vs_ref_{var_name}_FIABufferPlotEval_actualCount.png'),
  #        plot = distCode_frequencyPlot,
  #        width = 14,
  #        height = 9)
  # 
    
  

  p <- p_r_mode  %>% 
    ggplot(aes(x=as.factor(Modal_value),  fill=dataset)) + 
    geom_bar(position= position_dodge2(preserve = "single")) + # https://stackoverflow.com/questions/38101512/the-same-width-of-the-bars-in-geom-barposition-dodge
    facet_wrap(~disturb_code) + 
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
                      breaks =  c("Reference", "LFOrig", "LCMSDist")) + 
    theme(legend.position = "none")
  
  print(p)
  
  # ggObjList[[i]] <- p
  
  # save
  # ggsave(glue::glue('{export_fig_path}/FIABufferPlotEval/anyCell_touchingBuffer/Barplots_categorical_vars/{r1_name}_vs_{r2_name}_vs_ref_{var_name}_FIABufferPlotEval_anyCellTouchingBuffer.png'),
  #        plot = p,
  #        width = 14,
  #        height = 9)


}


# p1_2 <- gridExtra::grid.arrange(grobs = ggObjList, nrow = 2, ncol = 2)
# 
# ggsave(glue::glue("{export_fig_path}/FIABufferPlotEval/cell_centriod_withinBuffer/Barplots_categorical_vars/{r1_name}_vs_{r2_name}_vs_ref_ALL_VARS.png"),
#        plot = p1_2,
#        width = 24,
#        height = 13.5)

# PLOT CONTINUOUS VARS
# ---------------------------------------------------#

dodge <- position_dodge(width = 0.6)

ggObjList <- list()

text_size <- 4 # 5 for indvidual plots 3 for all plots in grid
percent_x_textPos <- 0.50 # 0.4 for individual plots
percent_y_textPos1 <- 0.99 # 0.96 for individual plots
percent_y_textPos2 <- 0.78 # 0.96 for individual plots
textBoxFill_ratioX <- 0.25
textBoxFill_ratioY <- 0.04

for(i in 1:(length(eval_vars_cont))) {
  
  # for testing
  # i = 8
  
  var_name <- eval_vars_cont[i]
  
  # Change NA values to 0 for continuous data 
  p_r1 <- p_r %>% 
        # filter(var == var_name) %>% 
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
  
  TPA_deaddf <- p_r1 %>%
    filter(MaxOfINVYR >= filterYear) %>%
    filter(var == "TPA_DEAD") %>%
    filter(value >= quantile(value, probs = 0.20, na.rm = TRUE)[[1]] & value <=quantile(value, probs = 0.80, na.rm = TRUE)[[1]]) %>% 
    select(-c(var, PLOTID, CN_plot)) %>%
    ungroup() %>%
    pivot_wider(names_from = dataset, values_from = value, values_fn = ~ mean(.x, na.rm = TRUE)) %>% # aggregate to mean value
    arrange(ID) %>% 
    drop_na() %>% 
    rename(LFOrig_TPAD = LFOrig, LCMSDist_TPAD = LCMSDist, Reference_TPAD = Reference)
  
  TPA_livedf <- p_r1 %>%
    filter(MaxOfINVYR >= filterYear) %>%
    filter(var == "TPA_LIVE") %>%
    filter(value >= quantile(value, probs = 0.20, na.rm = TRUE)[[1]] & value <=quantile(value, probs = 0.80, na.rm = TRUE)[[1]]) %>% 
    select(-c(var, PLOTID, CN_plot)) %>%
    ungroup() %>%
    pivot_wider(names_from = dataset, values_from = value, values_fn = ~ mean(.x, na.rm = TRUE)) %>% # aggregate to mean value
    arrange(ID) %>% 
    drop_na() %>% 
    rename(LFOrig_TPAL = LFOrig, LCMSDist_TPAL = LCMSDist, Reference_TPAL = Reference)
  
  
  merge_df <- TPA_deaddf %>% 
                left_join(TPA_livedf, by = c("ID", "CN_pt"))
  
  
  merge_df$LF_TPA_D2L <- merge_df$LFOrig_TPAD/merge_df$LFOrig_TPAL
  merge_df$LCMS_TPA_D2L <- merge_df$LCMSDist_TPAD/merge_df$LCMSDist_TPAL
  merge_df$Ref_TPA_D2L <- merge_df$Reference_TPAD/merge_df$Reference_TPAL
  
  # Create linear model
  lm_LF <- lm(LF_TPA_D2L ~ Ref_TPA_D2L, data = merge_df)
  
  lm_LCMS <- lm(LCMS_TPA_D2L ~ Ref_TPA_D2L, data = merge_df)
  
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
  
  merge_df %>%
    ggplot(aes(x = Ref_TPA_D2L)) +
    geom_abline(intercept = 0, color = "red", linewidth = 0.5 ) +
    geom_point(aes(y = LF_TPA_D2L, color = "LFOrig"), alpha = 0.25) +
    geom_point(aes(y = LCMS_TPA_D2L, color = "LCMS_TPA_D2L"), alpha = 0.25) +
    geom_smooth(method = "lm", formula = y~x,  aes(y = LF_TPA_D2L, color = "LFOrig"), alpha = 0.15) +
    geom_smooth(method = "lm", formula = y~x, aes(y = LCMS_TPA_D2L, color = "LCMS_TPA_D2L"), alpha = 0.15) +
    # facet_wrap(~disturb_code) +
    scale_color_manual(values = legendColors, breaks = c("LFOrig", "LCMS_TPA_D2L")) +
    guides(color = guide_legend(title = "Dataset",
                                keylength = 2,
                                keyheight = 2,
                                title.theme = element_text(size = 14),
                                label.theme = element_text(size =12))) +
    # guides(color = "none") + # un-comment to remove plot legend
    scale_x_continuous(expand = c(0, 0), limits = c(low_lim, up_lim)) + # starts x-axis from 0 and labels 0
    # scale_y_continuous(expand = c(0, 0), limits = c(low_lim, up_lim)) + # starts y-axis from 0 and labels 0
    labs(x = "TPA_DEAD:TPA_LIVE (Reference)", 
         y = "TPA_DEAD:TPA_LIVE (Imputed)") + 
    theme_bw() +
    # ggtitle(glue::glue("{var_name}: FIA buffer plot (40 m radius) evaluation")) +
    ggtitle(glue::glue("TPA dead to live ratio")) +
    theme(axis.title = element_text(size = 16),
          plot.title = element_text(size = 18)) +
    # annotate(geom="rect",
    #          xmin = ((low_lim + up_lim)/2) - ((up_lim - low_lim)*textBoxFill_ratioX),
    #          xmax = ((low_lim + up_lim)/2) + ((up_lim - low_lim)*textBoxFill_ratioX),
    #          ymin = (percent_y_textPos1*max(merge_df$LCMS_TPA_D2L, na.rm = TRUE)) - ((up_lim - low_lim)*textBoxFill_ratioY),
    #          ymax = (percent_y_textPos1*max(merge_df$LCMS_TPA_D2L, na.rm = TRUE)) + ((up_lim - low_lim)*textBoxFill_ratioY),
    #          fill = "beige") +
    annotate(geom="text",
             x = (max(merge_df$Ref_TPA_D2L))/2,
             y = (percent_y_textPos1*max(merge_df$LCMS_TPA_D2L, na.rm = TRUE)),
             label = as.character(eqn_LF),
             parse = TRUE,
             color = "purple",
             size = text_size) +
    # annotate(geom="rect",
    #          xmin = (low_lim + up_lim)/2 - ((up_lim - low_lim)*textBoxFill_ratioX),
    #          xmax = (low_lim + up_lim)/2 + ((up_lim - low_lim)*textBoxFill_ratioX),
    #          ymin = (percent_y_textPos2*max(merge_df$LCMS_TPA_D2L, na.rm = TRUE)) - ((up_lim - low_lim)*textBoxFill_ratioY),
    #          ymax = (percent_y_textPos2*max(merge_df$LCMS_TPA_D2L, na.rm = TRUE)) + ((up_lim - low_lim)*textBoxFill_ratioY),
    #          fill = "cadetblue1") +
    annotate(geom="text",
             x = (max(merge_df$Ref_TPA_D2L))/2,
             y = (percent_y_textPos2*max(merge_df$LCMS_TPA_D2L, na.rm = TRUE)),
             label = as.character(eqn_LCMS),
             parse = TRUE,
             color = "darkgreen",
             size = text_size)
  

  # Create linear model
  lm_LF <- lm(LFOrig ~ Reference, data = p_r2)
  
  lm_LCMS <- lm(LCMSDist ~ Reference, data = p_r2)
  
  
  # Manual annotation using `annotate()`
  
  # Annotate with equation of linear model, r-sq, RMSE  
  # Parsing the information saved in the model to create the equation to be added to the scatterplot as an expression # https://r-graphics.org/recipe-scatter-fitlines-text
  # eqn_LF <- sprintf(
  #   "italic(y) == %.3g + %.3g * italic(x) * ',' * ~~ italic(r)^2 ~ '=' ~ %.2g * ',' ~~ RMSE ~ '=' ~  %.3g * ',' ~~ MAE ~ '=' ~  %.3g",
  #   coef(lm_LF)[1],
  #   coef(lm_LF)[2],
  #   summary(lm_LF)$r.squared,  # r-squared 
  #   sqrt(mean(lm_LF$residuals^2)), # https://www.statology.org/extract-rmse-from-lm-in-r/
  #   mean(abs(lm_LF$residuals)) # mean absolute error (MAE)
  # )
  # 
  # eqn_LCMS <- sprintf(
  #   "italic(y) == %.3g + %.3g * italic(x) * ',' * ~~ italic(r)^2 ~ '=' ~ %.2g * ',' ~~ RMSE ~ '=' ~  %.3g * ',' ~~ MAE ~ '=' ~  %.3g",
  #   coef(lm_LCMS)[1],
  #   coef(lm_LCMS)[2],
  #   summary(lm_LCMS)$r.squared,  # r-squared 
  #   sqrt(mean(lm_LCMS$residuals^2)), # https://www.statology.org/extract-rmse-from-lm-in-r/
  #   mean(abs(lm_LCMS$residuals)) # mean absolute error (MAE)
  # )
  
  
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
    
  # print(p2)
  
  # save
  
  # export_fig_path <- "C:/Users/abhinavshrestha/OneDrive - USDA/Documents/02_TreeMap/temp_dir" # testing
  # 
  # ggsave(glue::glue('{export_fig_path}/FIABufferPlotEval/cell_centriod_withinBuffer/Scatterplots_continuous_vars/{r1_name}_vs_{r2_name}_vs_ref_{var_name}_FIABufferPlotEval.png'),
  #        plot = p2,
  #        width = 16,
  #        height = 9)

  
  # interactive plot
  
  # library(plotly)
  # library(htmlwidgets)
  
  # p_plotly <- p_r2 %>%
  #   # filter(LFOrig < 200) %>%
  #   # filter(LCMSDist < 200) %>%
  #   ggplot(aes(x = Reference)) +
  #   geom_abline(intercept = 0, color = "red", linewidth = 0.5 ) +
  #   geom_point(aes(y = LFOrig, color = "LFOrig"), alpha = 0.25) +
  #   geom_point(aes(y = LCMSDist, color = "LCMSDist"), alpha = 0.25) +
  #   geom_smooth(method = "lm", formula = y~x,  aes(y = LFOrig, color = "LFOrig"), alpha = 0.15) +
  #   geom_smooth(method = "lm", formula = y~x, aes(y = LCMSDist, color = "LCMSDist"), alpha = 0.15) +
  #   # facet_wrap(~disturb_code) +
  #   scale_color_manual(values = legendColors, breaks = c("LFOrig", "LCMSDist")) +
  #   # guides(color = "none") + # comment out to remove plot legend
  #   guides(color = guide_legend(title = "Target data",
  #                               keylength = 2,
  #                               keyheight = 2,
  #                               title.theme = element_text(size = 14),
  #                               label.theme = element_text(size =12))) +
  #   # scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) + # starts x-axis from 0 and labels 0
  #   # scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) + # starts y-axis from 0 and labels 0
  #   labs(x = "Reference (Ground_FIA)", 
  #        y = "Imputed") + 
  #   theme_bw() +
  #   theme(axis.title = element_text(size = 12)) +
  #   
  #   ggtitle(var_name)
  # 
  # plotlyObj <- plotly::ggplotly(ggObjList[[10]])
  # 
  # htmlwidgets::saveWidget(plotlyObj, glue::glue('{export_fig_path}FIABufferPlotEval/cell_centriod_withinBuffer/Scatterplots_continuous_vars/{r1_name}_vs_{r2_name}_vs_ref_{var_name}_cellCentroidWithinBuffer.html'))
  # 
  
  ggObjList[[i]] <- p2
}

library(gridExtra)
p3 <- gridExtra::grid.arrange(grobs = ggObjList, nrow = 3, ncol = 4)

# Export 

export_fig_path <- "C:/Users/abhinavshrestha/OneDrive - USDA/Documents/02_TreeMap/temp_dir" # testing
ggsave(glue::glue("{export_fig_path}/FIABufferPlotEval/cell_centriod_withinBuffer/Scatterplots_continuous_vars/{r1_name}_vs_{r2_name}_vs_ref_ALL_VARS.png"),
       plot = p3,
       width = 24,
       height = 13.5)


### AGREEMENT TEST: Similar to Karin's old code 

p_r_agreementDF <- p_r %>%
  select(-c(PLOTID, CN_plot)) %>%
  ungroup() %>%
  pivot_wider(names_from = dataset, values_from = value, values_fn = list) %>% # aggregate to list of all values
  arrange(ID) %>% 
  drop_na()


p_r_agreementDF$Agreement_LF <- NA
p_r_agreementDF$Agreement_LCMS <- NA


cont_evalTH_perc <- 0.1


for (i in (1:nrow(p_r_agreementDF))){
  
  if (p_r_agreementDF$var[i] %in% eval_vars_cat){
    
    # NA sieve
    if (is.na((mean(p_r_agreementDF$Reference[[i]])))) {
      next
      
    } else {
      
      # Check agreement with LF
      if (mean(p_r_agreementDF$Reference[[i]]) %in% p_r_agreementDF$LFOrig[[i]]) {
        
        p_r_agreementDF$Agreement_LF[i] <- 1 # Agreement
        
      } else {
        
        p_r_agreementDF$Agreement_LF[i] <- 0 # No agreement
        
      }
      
      # Check agreement with LCMS
      if (mean(p_r_agreementDF$Reference[[i]]) %in% p_r_agreementDF$LCMSDist[[i]]) {
        
        p_r_agreementDF$Agreement_LCMS[i] <- 1 # Agreement
        
      } else {
        
        p_r_agreementDF$Agreement_LCMS[i] <- 0 # No agreement
        
      }
    }
  }
  
  if (p_r_agreementDF$var[i] %in% eval_vars_cont){
    
    # NA sieve
    if (is.na((mean(p_r_agreementDF$Reference[[i]])))) {
      next
      
    } else {
      
      # Thresholds for checking agreement in continuous variable
      contVar_lowLim <- (p_r_agreementDF$Reference[[i]][1] - (cont_evalTH_perc*p_r_agreementDF$Reference[[i]][1]))
      contVar_upLim <- (p_r_agreementDF$Reference[[i]][1] + (cont_evalTH_perc*p_r_agreementDF$Reference[[i]][1]))
      
      # Check agreement with LF
      if (any(c(na.omit(p_r_agreementDF$LFOrig[[i]])) >= contVar_lowLim & c(na.omit(p_r_agreementDF$LFOrig[[i]])) <= contVar_upLim)) {
        
        p_r_agreementDF$Agreement_LF[i] <- 1 # Agreement
        
      } else {
        
        p_r_agreementDF$Agreement_LF[i] <- 0 # No agreement
        
      }
      
      # Check agreement with LCMS
      if (any(c(na.omit(p_r_agreementDF$LCMSDist[[i]])) >= contVar_lowLim & c(na.omit(p_r_agreementDF$LCMSDist[[i]])) <= contVar_upLim)) {
        
        p_r_agreementDF$Agreement_LCMS[i] <- 1 # Agreement
        
      } else {
        
        p_r_agreementDF$Agreement_LCMS[i] <- 0 # No agreement
        
      }
    }
  }
  
  
}

# Intialize data frame:
agreementSummary_df <- data.frame(Variable = as.character(),
                                  agreement_LF = as.numeric(), 
                                  agreement_LCMS = as.numeric())

all_vars <- c(eval_vars_cat, eval_vars_cont)
all_vars <- all_vars[-(which(all_vars == "disturb_code"))]

for (var_name in all_vars){
  
  summaryTable_temp_df <- p_r_agreementDF %>% 
    filter(MaxOfINVYR >= filterYear) %>%
    filter(var == var_name) %>%
    drop_na()
  
  total <- nrow(summaryTable_temp_df)
  
  LF_agreement_num <- sum(summaryTable_temp_df$Agreement_LF, na.rm = TRUE)
  LCMS_agreement_num <- sum(summaryTable_temp_df$Agreement_LCMS, na.rm = TRUE)
  
  agreementSummary_df <- rbind(agreementSummary_df, data.frame(Variable = var_name,
                                                               agreement_LF = (LF_agreement_num/total)*100, 
                                                               agreement_LCMS = as.numeric(LCMS_agreement_num/total)*100))
  
}


