# Zonal Cross-Validation for TreeMap Outputs
# Written by Lila Leatherman (lila.leatherman@usda.gov)

# Last updated: 8/15/2024


# In this script: 
# Performs k-fold cross-validation on yaImpute model
# Returns confusion matrices for categorical response vars
# Returns scatterplots for continuous attribues
# ### REQUIRES CURRENT RAT FOR ACCURACY OF CONTINUOUS ATTRIBUTES

###########################################################################
# Set inputs
###########################################################################

# Specific inputs
#-----------------------------------------------------#

# remove any CNs with EVT_GPs that are < thresh of total
evt_pct_thresh <- .3

# choose number of folds
k = 10

# Set inputs manually - if running standalone
#--------------------------------------------------------------#

# cur_zone_zero <- "z07"
# year <- 2022
# 
# this_proj <- this.path::this.proj()
# this_dir <- this.path::this.dir()
# 
# 
# lib_path = glue::glue('{this_proj}/gtac_production_scripts/00_Library/treeMapLib.R')
# source(lib_path)
# 
# 
# imputation_settings <- glue::glue("{home_dir}/03_Outputs/07_Projects/{year}_Production/01_Raw_model_outputs/{cur_zone_zero}/params/{cur_zone_zero}_{year}_Production_env.RDS")
# 
# load(imputation_settings)
# 
# inputs_for_evaluation <- glue::glue('{this_dir}/00_inputs_for_evaluation.R')
# source(inputs_for_evaluation)
#project_name <- "2020_ImputationPrep"

#output_name <- "z16_2020_ImputationPrep"

# Evaluation dir
#eval_dir <- glue::glue('{home_dir}/03_Outputs/07_Projects/{project_name}/03_Evaluation/z16/')

# number of digits to round to 
#round_dig <- 4




#####################################################################
# Load data
####################################################################

message("Loading data for cross-validation")

# set inputs
###################################################

# list layers to evaluate, assemble, and export
#eval_vars_cat <- c("evc", "evh", "evt_gp_remap", "disturb_code")
eval_vars_cat <- yvars


eval_vars_cont <- c("BALIVE", "GSSTK", "QMD_RMRS", "SDIPCT_RMRS", 
                    "CANOPYPCT", "CARBON_D", "CARBON_L", "CARBON_DOWN_DEAD", 
                    "TPA_DEAD", "TPA_LIVE")

eval_vars_cat_cont <- c(eval_vars_cat, eval_vars_cont)

# load input data
####################################################

# load model
#yai <- readRDS("//166.2.126.25/TreeMap/03_Outputs/07_Projects/2020_ImputationPrep/01_Raw_model_outputs/z16/model/z16_2020_GTAC_ImputationPrep_yai_treelist_bin.RDS")

yai <- readRDS(model_path)

# load rat
#rat_path <- glue::glue("{home_dir}01_Data/01_TreeMap2016_RDA/RDS-2021-0074_Data/Data/")

rat_tif <- terra::rast(rat_path)
rat <- data.frame(cats(rat_tif)) 

rm(rat_tif)

# evt_gp_remap table
evt_gp_remap_table <- read.csv(evt_gp_remap_table_path) %>%
  rename_with(tolower)

# evt metadata table
evt_metadata <- read.csv(glue::glue("{home_dir}/01_Data/02_Landfire/LF_230/Vegetation/EVT/LF2022_EVT_230_CONUS/CSV_Data/LF22_EVT_230.csv")) %>%
  rename_with(tolower)

# prep data
#########################################################

# get x and y dfs
X_df1 <- yai$xall  %>% mutate(X = as.numeric(row.names(yai$xall)))
X_df2 <- read.csv(xtable_path_model) %>% arrange(X)
Y_df1 <- yai$yRefs %>%  mutate(X = as.numeric(row.names(yai$yRefs)))
Y_df2 <- read.csv(ytable_path_model) %>% arrange(X)

# join tables so we have tm_id, cn, and point_x and point_y
X_df <- left_join(X_df1, 
                  X_df2 %>% select(c(X, tm_id, CN)), by = "X")
Y_df <- left_join(Y_df1, 
                  Y_df2 %>% select(c(X, tm_id, CN)), by = "X")

# remove unused tables
rm(X_df1, X_df2, Y_df1, Y_df2)

# limit to first 1000 rows for testing
#X_df <- X_df[1:1000,]
#Y_df <- Y_df[1:1000,]


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

# apply appropriate row names
# important to do this AFTER all other data frame processing
row.names(X_df) <- X_df$tm_id
row.names(Y_df) <- Y_df$tm_id


# # inspect
# summary(X_df$evt_gp_remap)
# str(X_df$evt_gp_remap)
# row.names(X_df)


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


# join RAT with X df using CN
rat_x <- rat %>%
  right_join(X_df, by = "CN") %>%
  select(c(CN, tm_id, all_of(eval_vars_cat_cont))) %>%
  # filter to plots with values
  filter(!is.na(BALIVE))

# join evt_gp_remap table with metadata
evt_gp_remap_table %<>%
  rename_with(tolower) %>%
  left_join(evt_metadata, by = "evt_gp") %>%
  #select(evt_gp, evt_gp_remap, evt_gp_n) %>%
  select(evt_gp, evt_gp_remap) %>%
  distinct() %>%
  mutate(evt_gp_remap = factor(evt_gp_remap, levels = levels(X_df$evt_gp_remap)))

# join evt_gp metadata to rat_x
rat_x %<>% left_join(evt_gp_remap_table, by = "evt_gp_remap") %>%
  mutate(across(c(evt_gp_remap, disturb_code), as.numeric))

######################################################################
# run cross-validation
######################################################################

message(glue::glue("running {k}-fold cross-validation"))

# make folds
folds <- caret::createMultiFolds(y= row.names(X_df), k = k, times = 1)

cv <- data.frame()

for(i in seq_along(folds)) {
  
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
  left_join(cv, rat_x %>% mutate(across(c(evt_gp_remap, disturb_code), as.numeric)), 
            by = c("pred_id" = "tm_id")) %>%
  mutate(dataset = "pred")

cv_att_ref <- 
  left_join(cv, rat_x %>% mutate(across(c(evt_gp_remap, disturb_code), as.numeric)),
            by = c("ref_id" = "tm_id")) %>%
  mutate(dataset = "ref") 


# join predicted and reference
p_r <- bind_rows(cv_att_pred, cv_att_ref) %>%
  select(-CN) %>%
  # pivot longer
  pivot_longer(!c(cv_id, ref_id, pred_id, dist, fold, dataset), 
               names_to = "var", values_to = "value") %>%
  mutate(var = factor(var),
         value = round(value, round_dig)) %>%
  arrange(cv_id) 

# #########################################################
# Do evaluation
############################################################

message("performing evaluation for cross-validation")

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
    select(-c( pred_id, dist, fold, var)) %>%
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

for(i in eval_vars_cont) {
  
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
    labs(title = glue::glue('Variation in {var_in} by dataset')) + 
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
  
  eqn
  
  p2 <- p_r2 %>%
    ggplot(aes(x = ref, y = pred)) + 
    geom_abline(intercept = 0, color = "red", linewidth = 1, linetype = 2) + 
    geom_point(alpha = alpha) +
    geom_smooth(method = "lm", formula = y~x) +
    labs() + 
    theme_bw() + 
    ggtitle(glue::glue("CV predicted vs. ref for {var_in}")) + 
    annotate(geom="text",
             x = (max(p_r2$ref)/2),
             y = (percent_y_textPos1*max(p_r2$pred, na.rm = TRUE)),
             label = as.character(eqn),
             parse = TRUE,
             color = "purple",
             size = text_size) 
  
  print(p2)
  
  # save
  ggsave(glue::glue('{eval_dir}/03_Cross_Validation/figs/CV_pred_vs_ref_{var_in}_violin.png'),
         plot = p,
         width = export_width, 
         height = export_height)    
  
  # save
  ggsave(glue::glue('{eval_dir}/03_Cross_Validation/figs/CV_pred_vs_ref_{var_in}_scatter.png'),
         plot = p2,
         width = export_width, 
         height = export_height) 
  
  gc()
}

message("done with cross-validation!")

#remove unused objects
rm(yai, yai_fold, Y_fold, X_fold, cm, cms, cv, cv_att_pred, cv_att_ref, evt_metadata, folds, lm, p, p_r, p_r2, p2)
