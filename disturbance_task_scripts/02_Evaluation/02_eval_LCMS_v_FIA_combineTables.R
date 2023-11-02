# Eval LCMS vs FIA data to determine thresholds
# Part two - process exports to determine thresholds for all zones combined

############################
# USER INPUTS
############################

#select year range (LCMS available for 1985-2022)
start_year <- 1999
end_year <- 2016

#list landfire zones of interest
zone_list <- sort(c(
  15,
  16,
  19,
  21,
  28,
  17,
  18,
  23
))

# list slow loss thresholds to use
slow_loss_thresh_list <- c(5, 10, 14, 20, 25)
#slow_loss_thresh_list <- c(14)

# Default thresholds: 
fastLossThresh = 27
slowLossThresh = 14
##gainThresh = 30
gainThresh = 10

# set home dir
home_dir <- ("//166.2.126.25/TreeMap/")

# set tmp directory
tmp_dir <- "E:/tmp"

# path to 2016 treemap data
treemap_path <- paste0(home_dir, "01_Data/01_TreeMap2016_RDA/RDS-2021-0074_Data/Data/TreeMap2016.tif")

# set path to lcms raw prob rasters
lcms_dir <- '//166.2.126.25/TreeMap/01_Data/05_LCMS/01_Threshold_Testing/'
lcms_dir_rawprob <- paste0(lcms_dir, "01_Raw/02_Raw_Probabilities/")

# path to FIA data
fia_path <- paste0(home_dir, "01_Data/04_FIA/SlowFast_StatVars_ActualLL.csv")

#set path to save evaluation data
eval_dir <- paste0(home_dir, "01_Data/05_LCMS/01_Threshold_Testing/05_Evaluation/")

# aoi path - if different from landfire zone
# supply path, or NA
#aoi_path <- paste0(home_dir, "01_Data/03_AOIs/UT_Uintas_rect_NAD1983.shp")
aoi_path <- NA
#aoi_name <- "UT_Uintas_subset"

#####################
# SETUP
######################

# check if tmp directory exists 
if (file.exists(tmp_dir)){
  
} else {
  
  # create a new sub directory inside
  # the main path
  dir.create(tmp_dir)
  
}

# set temp directory - helps save space with R terra
write(paste0("TMPDIR = ", tmp_dir), file=file.path(Sys.getenv('R_USER'), '.Renviron'))
#empty temp dir
do.call(file.remove, list(list.files(tmp_dir, full.names = TRUE)))
#remove unused memory
gc()

# load libraries
library(tidyverse)
library(magrittr)
library(terra)
library(caret)
library(modeest)

#make %notin% function
`%notin%` <- Negate('%in%')


##############################################
##### EVAL for all AOIs run 
##############################################

# load in all csv extracts
extract_files <- list.files(path = paste0(eval_dir, '01_csvs/02_extracts/'), full.names = TRUE)

extracts_all <- lapply(extract_files, read.csv)
extracts_all <- do.call(rbind, extracts_all)

do.call("rbind", sapply(filenames, read.csv, simplify = FALSE))

#inspect
str(extracts_all)

#rename to simplify following code
dt_fia <- extracts_all

#hard code factors
dt_fia %<>%
  # convert change classes to factors
  mutate(changeType = factor(changeType, levels = c(seq(1,5,1), 'NA')),
         ShannonClassNum = factor(ShannonClassNum, levels = levels(changeType)),
         MortStatClassNum = factor(MortStatClassNum, levels = levels(changeType))) 

# join with change classes
change_classes_df <- data.frame(changeType = as.factor(c(1,2,3,4,5,6)),
                                changeClass = factor(c("Stable", "Slow Loss", "Fast Loss", "Gain", "NPArea", "NA"), 
                                                        levels = c("Stable", "Slow Loss", "Fast Loss", "Gain", "NPArea", "NA")))

dt_fia %<>%
  left_join(change_classes_df)

# update zone name
zone_name <- paste0("z",paste(unlist(zone_list), collapse = "_"))

#####################################################
# Run eval / cm over all zones
###################################################

#create blank data frame to append into 
eval_t <- data.frame()


for(m in 1:length(unique(dt_fia$changeMethod))) {
  
  
  #for testing
  #m <- 1
  
  changeMethod_select <- unique(dt_fia$changeMethod)[m]
  
  #print(paste0("evaluating change method ", changeMethod_select))
  
  dt_fia_t <- dt_fia %>%
    filter(changeMethod == changeMethod_select)
  
  for(i in 1:length(slow_loss_thresh_list)) {
    
    #for testing
    #i = 1
    
    # set thresh
    thresh = slow_loss_thresh_list[i]
    #thresh = 14
    
    #print(paste0("evaluating threshold = ", thresh))
    
    dt_fia_t_in <- 
      dt_fia_t %>%
      filter(slowLossThresh == thresh)
    
    dt_fia_t_in %>%
      group_by(ShannonClassNum) %>%
      count()
    
    ######
    #generate confusion matrix
    
    FIAIndex <- "ShannonClass"
    
    cm <- caret::confusionMatrix(dt_fia_t_in$changeType, # pred
                                 dt_fia_t_in$ShannonClassNum # ref
    )
    
    cm
    
    
    # extract accuracy measures
    addmargins(cm$table)
    cm$byClass
    
    # classwise accuracy for slow loss: 
    cm$byClass[2,11]
    
    # make data frame of classes
    cm_t_classes <- data.frame(as.matrix(cm, what = "classes"))
    names(cm_t_classes) <- levels(dt_fia_t$changeType)
    names(cm_t_classes) <- c("Stable", "Slow Loss", "Fast Loss", "Gain", "NPArea", "NA")
    cm_t_classes %<>% 
      rownames_to_column(., var = 'metric') %>%
      pivot_longer(2:7,
                   names_to = 'changeClass',
                   values_to = 'value') %>%
      # add overall accuracy and other metrics
      mutate(overallAccuracy = as.numeric(as.numeric(cm$overall[1])),
             slowLossThresh = thresh,
             changeMethod = changeMethod_select,
             FIAIndex = FIAIndex)
    
    #inspect
    cm_t_classes
    
    #bind to data frame
    eval_t <- rbind(eval_t, cm_t_classes)
    
    # do the same thing for MortStat Class
    ###########################################
    
    FIAIndex <- "MortStatClass"
    
    cm <- caret::confusionMatrix(dt_fia_t_in$changeType, # pred
                                 dt_fia_t_in$MortStatClassNum # ref
    )
    
    cm
    
    
    # extract accuracy measures
    addmargins(cm$table)
    cm$byClass
    
    # classwise accuracy for slow loss: 
    cm$byClass[2,11]
    
    cm_t_classes <- data.frame(as.matrix(cm, what = "classes"))
    names(cm_t_classes) <- levels(dt_fia_t$changeType)
    names(cm_t_classes) <- c("Stable", "Slow Loss", "Fast Loss", "Gain", "NPArea", "NA")
    cm_t_classes %<>% 
      rownames_to_column(., var = 'metric') %>%
      pivot_longer(2:7,
                   names_to = 'changeClass',
                   values_to = 'value') %>%
      # add overall accuracy and other metrics
      mutate(overallAccuracy = as.numeric(as.numeric(cm$overall[1])),
             slowLossThresh = thresh,
             changeMethod = changeMethod_select,
             FIAIndex = FIAIndex)
    
    #bind to data frame
    eval_t <- rbind(eval_t, cm_t_classes)
    
    
    ##############################################################
    # Export - Mort Stat Class
    ##############################################################
    
    # raw confusion matrix
    cm_raw_out <- as.table(cm)
    cm_raw_out <- addmargins(cm_raw_out)
    
    cm_t_overall <- data.frame(as.matrix(cm, what = "overall"))
    names(cm_t_overall) <- c("value")
    
    cm_t_classes <- data.frame(as.matrix(cm, what = "classes"))
    names(cm_t_classes) <- levels(dt_fia_t$changeType)
    names(cm_t_classes) <- c("Stable", "Slow Loss", "Fast Loss", "Gain", "NPArea", "NA")
    
    #inspect
    cm_raw_out
    cm_t_overall
    cm_t_classes
    
    # check export name
    
    
    print(paste0(eval_dir, "01_csvs/", start_year, "_", end_year, "_", zone_name, "_t", thresh, "_", changeMethod_select, "_MortStat"))
    export_name <- paste0(eval_dir, "01_csvs/01_cms/", start_year, "_", end_year, "_", zone_name, "_t", thresh, "_", changeMethod_select, "_MortStat")
    
    # export
    write.csv(cm_raw_out, file = paste0(export_name, "_cm_raw", ".csv"))
    write.csv(cm_t_overall, file = paste0(export_name, "_cm_overall", ".csv"))
    write.csv(cm_t_classes, file = paste0(export_name, "_cm_classes", ".csv"))
    
    # Stop the clock
    #print(paste0("time elapsed: ", proc.time() - ptm))
    
    gc()
    
  }}

# add name of zone
eval_t$zone = zone_name

# export eval table
write.csv(eval_t, 
          file = paste0(eval_dir, "01_csvs/02_eval/", start_year, "_", end_year, "_", zone_name, ".csv"))

# PLOTTING
####################################################
# Plotting
######################################################

# inspect
str(eval_t)
summary(eval_t)
unique(eval_t$changeClass)
unique(eval_t$metric)
unique(eval_t$FIAIndex)
unique(eval_t$changeMethod)

#set levels for change method for plotting
eval_t %<>%
  mutate(changeMethod = factor(changeMethod, levels = c('Change_min', 'Change_mode', 'Change_max', 'Change_min_F', 'Change_mode_F', 'Change_max_F'))) 

# how does classwise accuracy for slow loss vary over thresholds? 
eval_t %>%
  filter(FIAIndex == "ShannonClass") %>%
  filter(changeClass %in% c("Slow Loss", "Fast Loss", "Stable")) %>%
  filter(metric %in% c("Sensitivity", "Specificity", "Balanced Accuracy", "Precision", "Recall")) %>%
  ggplot() +
  geom_point(aes(x = slowLossThresh, y = value, color = metric )) + 
  geom_line(aes(x = slowLossThresh, y = value, color = metric )) + 
  geom_vline(xintercept = 14) + 
  #facet_wrap(changeMethod~changeClass) +
  facet_grid(rows = vars(changeMethod), cols = vars(changeClass)) + 
  labs(title = paste0("LCMS Slow Loss Thresholds - ", zone_name, " ", start_year, "-", end_year, "
                        vertical line = default Slow Loss Threshold = 14")) +
  theme_bw()

#export
ggsave(filename = paste0(eval_dir, "02_imgs/", start_year, "_", end_year, "_", zone_name, ".png"),
       width = 9, 
       height = 7,
       units = "in",
)

# compare mort shannonClass and Mort Stat Class
eval_t %>%
  filter(changeMethod == "Change_min_F") %>%
  #filter(FIAIndex == "ShannonClass") %>%
  filter(changeClass %in% c("Slow Loss", "Fast Loss", "Stable")) %>%
  filter(metric %in% c("Sensitivity", "Specificity", "Balanced Accuracy", "Precision", "Recall")) %>%
  ggplot() +
  geom_point(aes(x = slowLossThresh, y = value, color = metric , shape = factor(FIAIndex))) + 
  geom_line(aes(x = slowLossThresh, y = value, color = metric , linetype = factor(FIAIndex)), linewidth = 1) + 
  geom_vline(xintercept = 14) + 
  facet_wrap(changeMethod~changeClass) +
  #facet_grid(rows = vars(changeMethod), cols = vars(changeClass)) + 
  labs(title = paste0("LCMS Slow Loss Thresholds - ", zone_name, " ", start_year, "-", end_year, "
                        vertical line = default Slow Loss Threshold = 14")) +
  theme_bw()

#export
ggsave(filename = paste0(eval_dir, "02_imgs/", start_year, "_", end_year, "_", zone_name, "_byFIAIndex.png"),
       width = 9, 
       height = 5,
       units = "in",
)

#############################################################
# Look at change_min_F to get ideal threshold for slow loss in each zone
##########################################################################


dt_fia %>%
  filter(changeMethod == "Change_min_F") %>%
  filter(changeClass == "Slow Loss") %>%
  str()






# clear memory
gc()