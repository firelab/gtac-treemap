# TreeMap Evaluation
# Written by Lila Leatherman (lila.leatherman@usda.gov)

# Out-of-bag stats and validation 
# - Extract model validation stats from yai object
# - Report out model accuracy for vars

# TO DO: 

# Last updated: 3/6/2024

###########################################################################
# Set inputs
###########################################################################

# Standard inputs
#---------------------------------------------#

# Set inputs - from input script
this.path <- this.path::this.path() # Id where THIS script is located

# get path to input script
spl <- stringr::str_split(this.path, "/")[[1]]
input_script.path <- paste( c(spl[c(1:(length(spl)-2))],
                              "03_Imputation/00_inputs_for_imp.R" ),
                            collapse = "/")

source(input_script.path)

############################################################

# Terra options
# --------------------------------#

#increase memory fraction available
#terraOptions(memfrac = 0.8)

# Other options
# --------------------------------#

# Allow for sufficient digits to differentiate plot cn numbers

options("scipen"=100, "digits"=8)


####################################################################
# Load data
####################################################################

# Load imputation model 
# ---------------------------------------------------------- #

#load model
yai <- readr::read_rds(model_path)

# Load x table
# ------------------------------------------------------------#
X.df <- read.csv(xtable_path)

X.df %<>% 
  mutate(
    # disturb_code = factor(disturb_code),
    # disturb_year = factor(disturb_year),
    # EVT_GP = factor(EVT_GP),
    # canopy_cover = factor(canopy_cover),
    # canopy_height = factor(canopy_height),
    PLOTID = ID)

# Load lookup table
#--------------------------------------------------------------#
# load lookup table - table with all ref vars, 
# including those calculated from FIA
# don't have this YET


######################################################################
# Get Validation data
######################################################################

# run custom function to get out of bag observations
# this takes some time to run - ~15 secs
oobs <- get_OOBs_yai(yai)

oobs %<>%
  rename("ref_id" = ref,
         "pred_id" = pred)

# list variables to evaluate
eval_vars <- c("canopy_cover", "canopy_height", "EVT_GP", 
               "disturb_code", "disturb_year")


# make reference and predicted table
# --------------------------------------------#
refs <- 
  left_join(oobs, X.df, 
                  by = c("ref_id" = "ID")) %>%
  select(c(PLOTID, ref_id, pred_id, tree_num, inbag_count, all_of(eval_vars))) %>%
  pivot_longer(!c(PLOTID, ref_id, pred_id, tree_num, inbag_count), names_to = "var", values_to = "ref") %>%
  mutate(var = factor(var)) %>%
  rename("PLOTID_ref" = PLOTID)

# join oobs with X.df
preds <- left_join(oobs, X.df,
                   by = c("pred_id" = "ID")) %>%
  select(c(PLOTID, ref_id, pred_id, tree_num, inbag_count, all_of(eval_vars))) %>%
  pivot_longer(!c(PLOTID, ref_id, pred_id, tree_num, inbag_count), names_to = "var", values_to = "pred") %>%
  mutate(var = factor(var)) %>%
  rename("PLOTID_pred" = PLOTID)

# join refs and preds - could i just cbind? 

# check to make sure cols we expect to be the same are the same
# identical(refs$tree_num, preds$tree_num)
# identical(refs$ref_id, preds$ref_id)
# identical(refs$pred_id, preds$pred_id)
# identical(preds$PLOTID_pred, refs$pred_id)
# identical(refs$inbag_count, preds$inbag_count)
# 
# identical(preds$PLOTID_pred, preds$pred_id)

p_r <- cbind(refs, 
             preds %>% select(pred))


# p_r <- inner_join(refs, preds, by = c( "ref_id", "pred_id", "tree_num","inbag_count", "var")) %>%
#   filter(inbag_count != 0) # get only OOB observations

gc()

# Make confusion matrices for each var
#-----------------------------------------------#

# loop over vars
# using a for loop bc ¯\_(ツ)_/¯

cms <- NULL

for(i in eval_vars) {
  
  var_in <- i
  
  print(glue::glue("working on {var_in}"))
  
  # subset to var of interest
  d <- p_r %>%
    dplyr::filter(var == var_in) %>%
    dplyr::select(pred, ref)
  
  # get confusion matrices
  cm <- eval_cm_function(d, 99)
  
  # prep outputs
  cm <- list(cm)
  names(cm) <- var_in
  
  if(is.null(cms)) { cms <- cm } else { 
    cms <- c(cms, cm) }
  
}

# save cms as RDS 
write_rds(cms, file = 
            glue::glue('{eval_dir}/01_OOB_Evaluation/{output_name}_CMs_OOB.RDS'))

gc()