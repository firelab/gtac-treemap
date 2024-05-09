# TreeMap Evaluation
# Written by Lila Leatherman (lila.leatherman@usda.gov)

# Out-of-bag stats and validation
# - Extract model validation stats from yai object
# - Report out model accuracy for vars

# TO DO:
# - add continuous vars 

# Last updated: 4/19/2024

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
  mutate(PLOTID = ID)

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
  select(-Value)

# join with X df  
rat %<>%
  right_join(X_df, by = c("CN" = "CN", "tm_id" = "PLOTID")) %>%
  rename("PLOTID" = tm_id)


######################################################################
# Get and prep validation data - out of bag observations
######################################################################

# Data dictionary for OOB predicted and reference: 
# oob_id : row number of original oob observation
# plotid_ref : treemap plot id for the reference plot in the imutation/tree
# plotid_pred : treemap plotid for the predicted plot in the imputatio/tree

# run custom function to get out of bag observations
# this takes some time to run - ~15 secs
oobs <- get_OOBs_yai(yai) %>%
  rename("ref_id" = ref,
         "pred_id" = pred) %>%
  mutate(oob_id = row_number())


# make Join OOB predicted and reference with Xdf and RAT to get layer values
# --------------------------------------------#

# join oobs with x df - reference
refs <-
  left_join(oobs, rat,
            by = c("ref_id" = "ID")) %>%
  select(c(oob_id, ref_id, pred_id, inbag_count,
           any_of(eval_vars))) %>%
  mutate(dataset = "ref")
  
# join oobs with X_df - predicted
preds <- left_join(oobs, rat,
                   by = c("pred_id" = "ID")) %>%
  select(c(oob_id, ref_id, pred_id, inbag_count,
           any_of(eval_vars))) %>%
  mutate(dataset = "pred") 

# clear memory
gc()

# Join Predicted and Reference
#-----------------------------------------#

p_r <- bind_rows(preds, refs) %>%
  filter(inbag_count == 0) %>% # filter to only OOB
  select(-inbag_count) %>%
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
            glue::glue('{eval_dir}/01_OOB_Evaluation/{output_name}_CMs_OOB.RDS'))

gc()


####################################################
# CONTINUOUS VARS EVAL
####################################################

# PLOT CONTINUOUS VARS
# ---------------------------------------------------#

dodge <- position_dodge(width = 0.6)

for(i in 1:(length(eval_vars_cont))) {
  
  # for testing
  i = 5
  
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
  lm$coefficients
  sum <- summary(lm)
  
  sum$r.squared
  
  
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
    ggtitle(glue::glue("OOB predicted vs. ref for {var_name}"))
  
  print(p2)
  
  # save
  ggsave(glue::glue('{eval_dir}/01_OOB_Evaluation/figs/OOB_Imputed_vs_ref_{var_name}_violin.png'),
         plot = p,
         width = 7, 
         height = 4.5)    
  
  # save
  ggsave(glue::glue('{eval_dir}/01_OOB_Evaluation/figs/OOB_Imputed_vs_ref_{var_name}_scatter.png'),
         plot = p2,
         width = 7, 
         height = 4.5) 
  
  gc()
}
