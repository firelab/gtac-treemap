# Zonal TreeMap Cross-Validation
# Written by Lila Leatherman (lila.leatherman@usda.gov)

# Last updated: 8/15/2024


# In this script: 
# Performs k-fold cross-validation on yaImpute model
# Returns confusion matrices for categorical response vars
# Returns scatterplots for continuous attribues
# ### REQUIRES CURRENT RAT FOR COMPLETE ACCURACY OF CONTINUOUS ATTRIBUTES

###########################################################################
# Set inputs
###########################################################################

# Specific inputs
#-----------------------------------------------------#

# remove any CNs with EVT_GPs that are < thresh of total
evt_pct_thresh <- .3

# choose number of folds
k = 10

# list variables to evaluate
eval_vars_cat <- c("evc", "evh", "evt_gp", "disturb_code", "disturb_code_bin", "disturb_year")
#eval_vars_cat <- c(yvars, "disturb_code", "evt_gp")

eval_vars_cont <- attributevars

eval_vars_cat_cont <- c(eval_vars_cat, eval_vars_cont)

# Set inputs manually - if running standalone
#-----------------------------------------------------#

standalone <- "N"
cur_zone_zero_standalone <- "z01"
year_standalone <- 2020
project_name_standalone <- glue::glue("{year_standalone}_Production_newXtable")


#####################################################################
# Load data
####################################################################

# Set inputs manually - if running standalone
#-----------------------------------------------------#

if(standalone == 'Y') {
  
  # assign main variables
  cur_zone_zero <- cur_zone_zero_standalone
  year <- year_standalone
  
  this_proj <- this.path::this.proj()
  this_dir <- this.path::this.dir()
  
  ## load treemap library
  lib_path = glue::glue('{this_proj}/gtac_production_scripts/00_Library/treeMapLib.R')
  source(lib_path)
  
  #load settings for zone
  zone_settings <- glue::glue("{home_dir}/03_Outputs/07_Projects/{project_name_standalone}/01_Raw_model_outputs/{cur_zone_zero}/params/{cur_zone_zero}_{project_name_standalone}_env.RDS")
  
  load(zone_settings)
  
  # load library again in case functions have been updated since initial creation of RDS
  source(lib_path)
}

message("Loading data for cross-validation")

# Load imputation model
# ---------------------------------------------------------- #


# load model
yai <- readr::read_rds(model_path)


# Load X and Y dfs
# ---------------------------------------------------------- #


# load X_df 
X_df <- read.csv(xtable_path_model) 

# load Y_df 
Y_df <- read.csv(ytable_path_model)  

# Identify Groups that comprise less than the threshold pct of total
gps_to_drop <- X_df %>%
  group_by(evt_gp_remap) %>%
  summarize(n = n(),
            total = nrow(X_df), 
            pct = 100 * (n/total)) %>%
  filter(pct < evt_pct_thresh) %>%
  select(evt_gp_remap) %>% 
  mutate(evt_gp_remap = as.integer(evt_gp_remap)) %>%
  as.list()

# remove EVT-GPs from Xdf and Ydf and drop levels
X_df %<>% filter(evt_gp_remap %notin% gps_to_drop) %>%
  droplevels()

Y_df %<>% filter(evt_gp_remap %notin% gps_to_drop) %>%
  droplevels()

# apply row names - must be treemap id
# important to do this AFTER all other data frame processing
row.names(X_df) <- X_df$tm_id
row.names(Y_df) <- Y_df$tm_id


# Load and prep Raster Attribute Table
#-----------------------------------------------------------------#

message("Importing raster attribute table...")
# Using function in 00_Library/load_RAT.R
rat <- load_RAT(rat_path, 
                CN_column = "PLT_CN", 
                ID_column = "TM_ID")

# Join RAT and X_df into rat_x
#-----------------------------------------------------------#

X_df %<>% 
  dplyr::rename("TM_ID" = tm_id)

# join RAT with X df using CN - because TM_ID varies between versions of TreeMap
# joining by TM_ID will cause an error if joining with an older RAT
rat_x <- rat %>%
  select(-TM_ID) %>%
  right_join(X_df, by = "CN") %>%
  select(c(CN, TM_ID, all_of(eval_vars_cat_cont))) %>%
  # filter to plots with values
  #filter(!is.na(BALIVE)) %>%
  arrange(TM_ID)


######################################################################
# Run cross-validation
######################################################################


# Set up and run CV model
#-------------------------------------------------#
message(glue::glue("running {k}-fold cross-validation"))

# make folds
folds <- caret::createMultiFolds(y = row.names(X_df), k = k, times = 1)

cv <- data.frame()

for (i in seq_along(folds)) {
  
  # for testing
  #i = 1
  print(glue::glue("working on fold {i}"))
  
  # get the subset / fold of obs to include
  fold <- unlist(folds[i])
  
  # subset input data frames
  X_fold <- X_df[fold,]
  Y_fold <- Y_df[fold,]
  
  # fit the model
  yai_fold <- yaImpute::yai(X_fold, Y_fold,
                            method = "randomForest",
                            ntree = 250)
  
  # get imputed ids for each id
  yai_out <- yaImpute::foruse(yai_fold, kth = 1)
  
  # rename fields
  yai_out %<>%
    data.frame() %>%
    mutate(ref_id = as.numeric(row.names(yai_out)),
           pred_id = as.numeric(use), 
           fold = i) %>%
    select(ref_id, pred_id, dist, fold) %>%
    data.frame()
  
  # bind to data frame
  cv <- rbind(cv, yai_out)
  
  gc()
  
}

# add id to help with restructuring data
cv$cv_id = row.names(cv)


# get values of attributes 
#--------------------------------------------------------------#

# get predicted attributes
cv_att_pred <- 
  left_join(cv, rat_x,
            by = c("pred_id" = "TM_ID")) %>%
  mutate(dataset = "pred")

cv_att_ref <- 
  left_join(cv, rat_x,
            by = c("ref_id" = "TM_ID")) %>%
  mutate(dataset = "ref") 


# join predicted and reference
p_r <- bind_rows(cv_att_pred, cv_att_ref) %>%
  select(-c(CN)) %>%
  # pivot longer
  pivot_longer(!c(cv_id, ref_id, pred_id, dist, fold, dataset), 
               names_to = "var", values_to = "value") %>%
  mutate(var = factor(var),
         value = round(value, round_dig)) %>%
  arrange(cv_id) 

# #########################################################
# Perform evaluation on results of cross-validation
############################################################

message("performing evaluation for cross-validation")

# Make confusion matrices for each categorical var
#-----------------------------------------------#

# make a container to hold output confustion matrices
cms <- NULL

# loop over variables named at teh top of the script
for (i in eval_vars_cat) {
  
  var_in <- i
  
  print(glue::glue("working on {var_in}"))
  
  # subset to var of interest
  # and transform as necessary 
  d <- 
    p_r %>%
    dplyr::filter(var == var_in) %>%
    select(-c(pred_id, dist, fold, var)) %>%
    pivot_wider(id_cols = cv_id, 
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

#inspect
cms

# save cms as RDS
write_rds(cms, file = 
            glue::glue('{eval_dir}/03_Cross_Validation/{output_name}_CMs_CV.RDS'))

gc()

# Plot continuous variables
# ---------------------------------------------------#

print("working on continuous variables")

# set parameters for continuous var plotting
dodge <- position_dodge(width = 0.6)
text_size <- 4 # 5 for indvidual plots 3 for all plots in grid
percent_x_textPos <- 0.50 # 0.4 for individual plots
percent_y_textPos1 <- 0.99 # 0.96 for individual plots
percent_y_textPos2 <- 0.78 # 0.96 for individual plots
textBoxFill_ratioX <- 0.25
textBoxFill_ratioY <- 0.04
alpha <- 0.05
export_width <- 7 # in inches
export_height <- 4.5 # in inches

for (i in eval_vars_cont) {
  
  # for testing
  #i = 1
  #var_in <- eval_vars_cont[i]
  
  var_in <- i
  
  # Violin plots
  #------------------#
  
  p <- 
    p_r %>%
    filter(var == var_in) %>%
    select(-c(ref_id, pred_id)) %>%
    drop_na() %>%
    ggplot(aes(x = dataset, y = value, fill = dataset))+
    geom_violin(position = dodge)+
    geom_boxplot(width=.1, outlier.colour=NA, position = dodge) + 
    labs(title = glue::glue('{year_input} {cur_zone_zero}: Variation in {var_in} by dataset')) + 
    xlab(var_in) + 
    theme_bw()
  
  print(p)
  
  gc()
  
  # Scatterplots
  #-------------------#
  
  # prep dataset
  p_r2 <- 
    p_r %>%
    filter(var == var_in) %>%
    select(-c(ref_id, pred_id, var)) %>%
    pivot_wider(id_cols = cv_id, 
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
  
  #eqn
  
  p2 <- p_r2 %>%
    ggplot(aes(x = ref, y = pred)) + 
    geom_abline(intercept = 0, color = "red", linewidth = 1, linetype = 2) + 
    geom_point(alpha = alpha) +
    geom_smooth(method = "lm", formula = y~x) +
    labs() + 
    theme_bw() + 
    ggtitle(glue::glue("{year_input} {cur_zone_zero}: CV predicted vs. ref for {var_in}")) + 
    annotate(geom="text",
             x = (max(p_r2$ref)/2),
             y = (percent_y_textPos1*max(p_r2$pred, na.rm = TRUE)),
             label = as.character(eqn),
             parse = TRUE,
             color = "purple",
             size = text_size) 
  
  print(p2)
  
  # save
  ggsave(glue::glue('{eval_dir}/03_Cross_Validation/figs/{year_input}_{cur_zone_zero}_CV_{var_in}_violin.png'),
         plot = p,
         width = export_width, 
         height = export_height)    
  
  # save
  ggsave(glue::glue('{eval_dir}/03_Cross_Validation/figs/{year_input}_{cur_zone_zero}_CV_{var_in}_scatter.png'),
         plot = p2,
         width = export_width, 
         height = export_height) 
  
  gc()
}

message("done with cross-validation!")

#remove unused objects
rm(yai, yai_fold, Y_fold, X_fold, cm, cms, cv, cv_att_pred, cv_att_ref, folds, lm, p, p_r, p_r2, p2, gps_to_drop) # evt_metadata
