# TreeMap Evaluation
# Written by Lila Leatherman (lila.leatherman@usda.gov)

# Out-of-bag stats and validation
# - Use yaImpute::predict() to get putative "Out-of-bag" (OOB) Predictions
# - We have not verified that these truly represent OOB prediction
# - Assumption that these are OOB is following the logic present in the Random Forests predict.randomForest() function
# - Returns confusion matrices for categorical response vars
# - Returns scatterplots for continuous attributes
# ### REQUIRES CURRENT RAT FOR ACCURACY OF CONTINUOUS ATTRIBUTES


# Last updated: 8/15/2024

###########################################################################
# Set inputs
###########################################################################

# Specific inputs
#---------------------------------------------#

# list variables to evaluate

#eval_vars_cat <- c("evc", "evh", "evt_gp_remap", 
#                   "disturb_code")
eval_vars_cat <- yvars

eval_vars_cont <- c("BALIVE", "GSSTK", "QMD_RMRS", "SDIPCT_RMRS", 
                    "CANOPYPCT", "CARBON_D", "CARBON_L", "CARBON_DOWN_DEAD", 
                    "TPA_DEAD", "TPA_LIVE")

eval_vars_cat_cont <- c(eval_vars_cat, eval_vars_cont)

# Standard inputs
#---------------------------------------------#

# this_proj <- this.path::this.proj()
# this_dir <- this.path::this.dir()
# 
# ## load treemap library
# lib_path = glue::glue('{this_proj}/gtac_production_scripts/00_Library/treeMapLib.R')
# source(lib_path)


####################################################################
# Load data
####################################################################

message("loading data for OOB evaluation")

# Load imputation model
# ---------------------------------------------------------- #

#load model
yai <- readr::read_rds(model_path)


# Load raster attribute table 
#-------------------------------------------------#

# load rat
rat <- terra::rast(rat_path)
rat <- data.frame(cats(rat))

# Prep X table
#-------------------------------------------------------------#

# get x and y dfs
X_df1 <- yai$xall %>%
  mutate(X = as.numeric(row.names(yai$xall)))
X_df2 <- read.csv(xtable_path_model) %>% arrange(X)


# join tables so we have tm_id, cn, and point_x and point_y
X_df <- left_join(X_df1, 
                  X_df2 %>% select(c(X, tm_id, CN)), by = "X")


# remove unused tables
rm(X_df1, X_df2)

# apply appropriate row names
# important to do this AFTER all other data frame processing
row.names(X_df) <- X_df$tm_id

# Prep Raster Attribute Table
#-----------------------------------------------------------------#

# identify eval_vars_cont that are not from RMRS - we handle NAs differently
eval_vars_cont_RMRS <- stringr::str_subset(names(rat), "RMRS")
eval_vars_cont_nonRMRS <- stringr::str_subset(names(rat %>% dplyr::select(where(is.numeric))), "RMRS", negate = TRUE)

# prep rat table
rat %<>%
  # rename shortened field names
  dplyr::rename("SDIPCT_RMRS" = SDIPCT_RMR,
                "CARBON_DOWN_DEAD" = CARBON_DWN) %>%
  # convert CN to numeric
  dplyr::mutate(CN = as.numeric(CN)) %>%
  # round and fix NA values for RMRS and non RMRS vars
  dplyr::mutate(across(any_of(eval_vars_cont_nonRMRS), ~ round(.x, digits = round_dig))) %>%
  dplyr::mutate(across(any_of(eval_vars_cont_nonRMRS), ~ ifelse(.x == -99.000, 0, .x))) %>%
  dplyr::mutate(across(any_of(eval_vars_cont_RMRS), ~ dplyr::na_if(.x, -99))) %>%
  # calculate TPA_DEAD_LIVE_RATIO
  dplyr::mutate(TPA_DEAD_LIVE_RATIO = TPA_DEAD/TPA_LIVE) %>%
  # remove columns
  dplyr::select(-c(Value, tm_id))

# Join RAT and X_df into rat_x
#-----------------------------------------------------------#

# join RAT with X df using CN
rat_x <- rat %>%
  right_join(X_df, by = "CN") %>%
  select(c(CN, tm_id, any_of(eval_vars_cat_cont))) %>%
  # filter to plots with values
  filter(!is.na(BALIVE)) %>%
  # make factors into numeric so that we can make a long data frame
  mutate(across(c(evt_gp_remap, disturb_code), as.numeric))

######################################################################
# Get and prep validation data - out of bag predictions
######################################################################

message("getting out-of-bag predictions")

# Data dictionary for OOB predicted and reference: 
# oob_id : row number of original oob observation
# ref_id : treemap plot id for the reference plot 
# pred_id : treemap plotid for the predicted plot 

# run custom function to get out of bag predictions - 1 for each reference 
# this takes some time to run 
oobs <- get_OOBs_yai_predict(yai) 
oobs %<>% 
  mutate(oob_id = row_number())


# make Join OOB predicted and reference with Xdf and RAT to get variable values
# ----------------------------------------------------------------------------#

# join oobs with x df - reference
refs <-
  left_join(oobs, rat_x,
            by = c("ref_id" = "tm_id")) %>%
  select(c(oob_id, ref_id, pred_id,
           any_of(eval_vars_cat_cont))) %>%
  mutate(dataset = "ref")
  
# join oobs with X_df - predicted
preds <- left_join(oobs, rat_x,
                   by = c("pred_id" = "tm_id")) %>%
  select(c(oob_id, ref_id, pred_id,
           any_of(eval_vars_cat_cont))) %>%
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

message("performing evaluation on OOB predictions")

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
            glue::glue('{eval_dir}/02_OOB_Evaluation/{output_name}_CMs_OOB.RDS'))

gc()


# PLOT continuous vars
# ---------------------------------------------------#

# set parameters for continuous var plotting
dodge <- position_dodge(width = 0.6)
text_size <- 4 # 5 for indvidual plots 3 for all plots in grid
percent_x_textPos <- 0.50 # 0.4 for individual plots
percent_y_textPos1 <- 0.99 # 0.96 for individual plots
percent_y_textPos2 <- 0.78 # 0.96 for individual plots
textBoxFill_ratioX <- 0.25
textBoxFill_ratioY <- 0.04
alpha <- 0.1
export_width <- 7 # in inches
export_height <- 4.5 # in inches

# loop over continuous vars 

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
  
  # inspect LM
  # lm$coefficients
  # sum <- summary(lm)
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
    geom_point(alpha = alpha) +
    geom_smooth(method = "lm", formula = y~x) +
    labs() + 
    theme_bw() + 
    ggtitle(glue::glue("OOB predicted vs. ref for {var_name}")) + 
    annotate(geom="text",
             x = (max(p_r2$ref)/2),
             y = (percent_y_textPos1*max(p_r2$pred, na.rm = TRUE)),
             label = as.character(eqn),
             parse = TRUE,
             color = "purple",
             size = text_size) 
  
  print(p2)
  
  # save
  ggsave(glue::glue('{eval_dir}/02_OOB_Evaluation/figs/OOB_Imputed_vs_ref_{var_name}_violin.png'),
         plot = p,
         width = export_width, 
         height = export_height)    
  
  # save
  ggsave(glue::glue('{eval_dir}/02_OOB_Evaluation/figs/OOB_Imputed_vs_ref_{var_name}_scatter.png'),
         plot = p2,
         width = export_width, 
         height = export_height) 
  
  gc()
}

#rm(lm, p, p2)
