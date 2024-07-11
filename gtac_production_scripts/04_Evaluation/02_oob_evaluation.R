# TreeMap Evaluation
# Written by Lila Leatherman (lila.leatherman@usda.gov)

# Out-of-bag stats and validation
# - Extract model validation stats from yai object
# - Report out model accuracy for vars

# TO DO:

# TALK MORE ABOUT OOB EVALUATION SO WE UNDERSTAND IT

# Last updated: 7/8/2024

###########################################################################
# Set inputs
###########################################################################

# Specific inputs
#---------------------------------------------#

# list variables to evaluate

eval_vars_cat <- c("canopy_cover", "canopy_height", "EVT_GP", 
                   "disturb_code")

eval_vars_cont <- c("BALIVE", "GSSTK", "QMD_RMRS", "SDIPCT_RMRS", 
                    "CANOPYPCT", "CARBON_D", "CARBON_L", "CARBON_DOWN_DEAD", 
                    "TPA_DEAD", "TPA_LIVE")

eval_vars <- c(eval_vars_cat, eval_vars_cont)

# Standard inputs
#---------------------------------------------#

# Set inputs - from input script
thispath <- this.path::this.path() # Id where THIS script is located

# get path to input script
spl <- stringr::str_split(thispath, "/")[[1]]
input_script_path <- paste(c(spl[c(1:(length(spl) - 1))],
                             "00_inputs_for_evaluation.R"),
                           collapse = "/")

source(input_script_path)

############################################################

# Terra options
# --------------------------------#

#increase memory fraction available
#terraOptions(memfrac = 0.8)

# Other options
# --------------------------------#

# Allow for sufficient digits to differentiate plot cn numbers

options("scipen" = 100, "digits" = 8)


####################################################################
# Load data
####################################################################

# Load imputation model
# ---------------------------------------------------------- #

#load model
yai <- readr::read_rds(model_path)

# Load x table
# ------------------------------------------------------------#

# load X_df
X_df <- read.csv(xtable_path) %>%
  rename(PLOTID = X)

# Load raster attribute table 
#-------------------------------------------------#

# load rat
rat <- terra::rast(glue::glue("{rat_path}TreeMap2016.tif"))
rat <- data.frame(cats(rat))

rat %<>%
  rename("SDIPCT_RMRS" = SDIPCT_RMR,
         "CARBON_DOWN_DEAD" = CARBON_DWN) %>%
  mutate(CN = as.numeric(CN)) %>%
  # replace -99 (na value) with NA 
  mutate_at(eval_vars_cont, ~na_if(.x, -99)) %>%
  mutate_at(eval_vars_cont, ~na_if(.x, -99.00)) %>%
  mutate_at(eval_vars_cont, ~na_if(.x, -99.00000)) %>%
  # replace NA values with O for certain vars
  mutate(TPA_DEAD = ifelse(is.na(TPA_DEAD), 0, TPA_DEAD ),
         CARBON_D = ifelse(is.na(CARBON_D), 0 ,CARBON_D )) %>%
  select(-Value) 

# join with X df  
rat %<>%
  right_join(X_df, by = c("CN" = "CN", "tm_id" = "PLOTID")) %>%
  rename("PLOTID" = tm_id) %>%
  # filter to plots with values
  filter(!is.na(FORTYPCD))


######################################################################
# Get and prep validation data - out of bag observations
######################################################################

# Data dictionary for OOB predicted and reference: 
# oob_id : row number of original oob observation
# plotid_ref : treemap plot id for the reference plot in the imputation/forest
# plotid_pred : treemap plotid for the predicted plot in the imputation/forest

# run custom function to get out of bag predictions - 1 for each ref for each forest
# this takes some time to run 
oobs <- get_OOBs_yai(yai) #%>%
  #rename("ref_id" = ref,
  #       "pred_id" = pred) %>%
oobs %<>% mutate(oob_id = row_number())


# make Join OOB predicted and reference with Xdf and RAT to get variable values
# --------------------------------------------#

# join oobs with x df - reference
refs <-
  left_join(oobs, rat,
            by = c("ref_id" = "PLOTID")) %>%
  select(c(oob_id, ref_id, pred_id,
           any_of(eval_vars))) %>%
  mutate(dataset = "ref")
  
# join oobs with X_df - predicted
preds <- left_join(oobs, rat,
                   by = c("pred_id" = "PLOTID")) %>%
  select(c(oob_id, ref_id, pred_id,
           any_of(eval_vars))) %>%
  mutate(dataset = "pred") 

# clear memory
gc()

# Join Predicted and Reference
#-----------------------------------------#

p_r <- bind_rows(preds, refs) %>%
  # pivot longer
  pivot_longer(!c(oob_id, ref_id, pred_id, dataset), 
               names_to = "var", values_to = "value") %>%
  mutate(var = factor(var),
         value = na_if(value, -99.0000),
         value = round(value, round_dig)) %>%
  arrange(oob_id)


# Make confusion matrices for each categorical var
#-----------------------------------------------#

# loop over vars
# using a for loop bc ¯\_(ツ)_/¯

cms <- NULL

for (i in eval_vars_cat) {
  
  var_in <- i
  
  print(glue::glue("working on {var_in}"))
  
  # subset to var of interest
  # and transform as necessary 
  d <- 
  p_r %>%
    dplyr::filter(var == var_in) %>%
    select(-c(ref_id, pred_id, var)) %>%
    pivot_wider(id_cols = oob_id, 
                names_from = dataset, 
                values_from = value) %>%
    dplyr::select(pred, ref)
  
  # get confusion matrices
  cm <- eval_cm_function(d, NA)
  
  # prep outputs
  cm <- list(cm)
  names(cm) <- var_in
  
  if (is.null(cms)) {
    cms <- cm
  } else {
    cms <- c(cms, cm)
  }

}

# save cms as RDS
write_rds(cms, file = 
            glue::glue('{eval_dir}/01_OOB_Evaluation/{output_name}_CMs_OOB_ORIG_wrong.RDS'))

gc()


####################################################
# CONTINUOUS VARS EVAL
####################################################

# PLOT CONTINUOUS VARS
# ---------------------------------------------------#

dodge <- position_dodge(width = 0.6)
text_size <- 4 # 5 for indvidual plots 3 for all plots in grid
percent_x_textPos <- 0.50 # 0.4 for individual plots
percent_y_textPos1 <- 0.99 # 0.96 for individual plots
percent_y_textPos2 <- 0.78 # 0.96 for individual plots
textBoxFill_ratioX <- 0.25
textBoxFill_ratioY <- 0.04

for(i in 1:(length(eval_vars_cont))) {
  
  # for testing
  #i = 6
  
  var_name <- eval_vars_cont[i]
  
  # Violin plots
  #------------------#
  
  p <- 
    p_r %>%
    filter(var == var_name) %>%
    select(-c(ref_id, pred_id)) %>%
    drop_na() %>%
    ggplot(aes(x = dataset, y = value, fill = dataset))+
    geom_violin(position = dodge)+
    geom_boxplot(width=.1, outlier.colour=NA, position = dodge) + 
    labs(title = glue::glue('Variation in {var_name} by dataset')) + 
    xlab(var_name) + 
    theme_bw()
  
  print(p)
  
  gc()
  
  # Scatterplots
  #-------------------#
  
  # prep dataset
  p_r2 <- 
  p_r %>%
    filter(var == var_name) %>%
    select(-c(ref_id, pred_id, var)) %>%
    pivot_wider(id_cols = oob_id, 
                names_from = dataset, 
                values_from = value) %>%
    drop_na()
  
  # build an LM
  lm <- lm(pred ~ ref, data = p_r2)
  # lm$coefficients
  # sum <- summary(lm)
  # 
  # sum$r.squared
  
  
  # Parsing the information saved in the model to create the equation to be added to the scatterplot as an expression # https://r-graphics.org/recipe-scatter-fitlines-text
  eqn <- sprintf(
    "italic(y) == %.3g + %.3g * italic(x) * ',' * ~~ italic(r)^2 ~ '=' ~ %.2g * ',' ~~ RMSE ~ '=' ~  %.3g * ',' ~~ MAE ~ '=' ~  %.4g",
    coef(lm)[1],
    coef(lm)[2],
    summary(lm)$r.squared,  # r-squared 
    sqrt(mean(lm$residuals^2)), # https://www.statology.org/extract-rmse-from-lm-in-r/
    mae(na.omit(p_r2$pred), predict(lm))
  )
  
  eqn
  
  p2 <- p_r2 %>%
    ggplot(aes(x = ref, y = pred)) + 
    geom_abline(intercept = 0, color = "red", linewidth = 1, linetype = 2) + 
    geom_point(alpha = 0.25) +
    geom_smooth(method = "lm", formula = y~x) +
    labs() + 
    theme_bw() + 
    ggtitle(glue::glue("OOB predicted vs. ref for {var_name}")) + 
    annotate(geom="text",
             x = (max(ref)/2),
             y = (percent_y_textPos1*max(y, na.rm = TRUE)),
             label = as.character(eqn_LF),
             parse = TRUE,
             color = "purple",
             size = text_size) 
  
  print(p2)
  
  # save
  ggsave(glue::glue('{eval_dir}/01_OOB_Evaluation/figs/OOB_Imputed_vs_ref_{var_name}_violin_ORIG_wrong.png'),
         plot = p,
         width = 7, 
         height = 4.5)    
  
  # save
  ggsave(glue::glue('{eval_dir}/01_OOB_Evaluation/figs/OOB_Imputed_vs_ref_{var_name}_scatter_ORIG_wrong.png'),
         plot = p2,
         width = 7, 
         height = 4.5) 
  
  gc()
}
