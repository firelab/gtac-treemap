# explore yai object

###########################################################################
# Set inputs
###########################################################################

library(glue)

# Zone list
zone_list <- c(16)

#home_dir
#home_dir <- "D:/LilaLeatherman/01_TreeMap/"
home_dir<- "//166.2.126.25/TreeMap/"

# Directory where target rasters live
target_dir <- glue("{home_dir}01_Data/01_TreeMap2016_RDA/02_Target/")

# Paths for exporting data
#--------------------------------------#

# set path to save output rasters
# this directory will be created if it does not already exist
output_dir <- glue('{home_dir}03_Outputs/07_Raw_model_outputs/2016_Original_Test/')

# Output imputation name
output_name <- "2016_Orig_TestLL"

# set tmp directory
tmp_dir <- "D:/tmp/"

# set zone_num
####################
zone_num <- zone_list[1]

# Set zone name options
cur.zone <- glue('z{zone_num}')
cur.zone.zero <- if(zone_num < 10) {
  glue('z0{zone_num}') } else {
    cur.zone
  }
##################

# Update output and target dir with zone
# -----------------------------------------#
# Set folder paths
target_dir = glue('{target_dir}/{cur.zone.zero}/')
output_dir = glue('{output_dir}/{cur.zone.zero}/')


# Model inputs
#----------------------------------#

# Path where model is located
model_path <- glue('{output_dir}/model/{cur.zone.zero}_{output_name}_yai_treelist_bin.RDS')


#################################
# Load

yai <- readr::read_rds(model_path)

yai <- yai.treelist.bin

###########################
# explore
# Confusion matrices
cm_EVC <- yai$ranForest$canopy_cover$confusion
cm_EVH <- yai$ranForest$canopy_height$confusion
cm_EVT_GP <- yai$ranForest$EVT_GP$confusion
cm_DC <- yai$ranForest$disturb_code$confusion


yai$call
yai$obsDropped
yai$yDrop
yai$bootstrap
yai$trgRows
yai$cancor
yai$theFormula
yai$ftest

yai$ranForest$canopy_cover$forest$treemap
yai$ranForest$canopy_cover$oob.times

summary(yai$ranForest$canopy_cover)

yai$ranForest$canopy_cover$y
yai$ranForest$canopy_cover$oob.times

d <- cbind(yai$ranForest$canopy_cover$oob.times,
      yai$ranForest$canopy_cover$y,
      yai$ranForest$canopy_cover$predicted) %>%
  data.frame() %>%
  mutate(X2 = as.factor(X2),
         X3 = as.factor(X3))

caret::confusionMatrix(d$X2, d$X3)

yai$
  