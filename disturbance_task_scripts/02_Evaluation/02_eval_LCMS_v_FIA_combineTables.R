# Eval LCMS vs FIA data to determine thresholds
# Part two - process exported evaluation tables to find best threshold for all zones

############################
# USER INPUTS
############################

# #select year range (LCMS available for 1985-2022)
start_year <- 1999
end_year <- 2016
# 
# #list landfire zones of interest
# zone_list <- sort(c(
#   15,
#   16,
#   19,
#   21,
#   28,
#   17,
#   18,
#   23
# ))
# 
# # list slow loss thresholds to use
# slow_loss_thresh_list <- c(5, 10, 14, 20, 25)
# #slow_loss_thresh_list <- c(14)
# 
# # Default thresholds: 
# fastLossThresh = 27
# slowLossThresh = 14
# ##gainThresh = 30
# gainThresh = 10

# set home dir
home_dir <- ("//166.2.126.25/TreeMap/")

# set tmp directory
tmp_dir <- "D:/tmp"

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
# library(terra)
# library(caret)
# library(modeest)
library(PlaneGeometry)

#make %notin% function
`%notin%` <- Negate('%in%')


##############################################
##### EVAL for all AOIs run 
##############################################

# load in all confusion matrices of interest 
# e.g.: 1999_2016_LFz15_MogollonRim_t5_Change_min_F_MortStat_cm_classes
extract_files <- list.files(path = paste0(eval_dir, '01_csvs/03_eval/'), full.names = TRUE)

# bind all files together, add file name as row name
extracts_all <- do.call("rbind", sapply(extract_files, read.csv, simplify = FALSE))

#inspect
str(extracts_all)
#head(rownames(extracts_all))

# add zones and thresh values as columns, from file name
row_zone <- str_split_i(rownames(extracts_all), "z", 2)

# set row names as null
row.names(extracts_all) <- NULL

# rename
eval_all <- 
  extracts_all %>%
    mutate(zone_num = str_split_i(row_zone, "_", 1),
           #zone_name = str_split_i
           ) %>%
  select(-X)

#inspect
str(eval_all)

unique(eval_all$zone)

#############################################################
# Look at change_min_F to get ideal threshold for slow loss in each zone
##########################################################################

# PLOT to inspect

# how does classwise accuracy for slow loss vary over thresholds, for different zones?
prelim_plot <- 
  eval_all %>%
    filter(FIAIndex == "ShannonClass") %>%
    #filter(changeClass %in% c("Slow Loss", "Fast Loss", "Stable")) %>%
    filter(changeClass == "Slow Loss") %>%
    filter(metric %in% c("Precision", "Recall")) %>%
    filter(changeMethod == "Change_min_F") %>%
    ggplot() +
    geom_point(aes(x = slowLossThresh, y = value, color = metric )) + 
    geom_line(aes(x = slowLossThresh, y = value, color = metric )) + 
    geom_vline(xintercept = 14) + 
    #facet_wrap(changeMethod~changeClass) +
    #facet_grid(cols = vars(changeClass)) + 
    facet_wrap(~zone) + 
    labs(title = paste0("LCMS Slow Loss Thresholds - ", " ", start_year, "-", end_year, " by zone.
                        vertical line = default Slow Loss Threshold = 14")) +
    theme_bw()



# group by zone to get two coordinate pairs for precision and recall that are the closest, 
# then figure out where those lines cross. 
# plot those lines over precision and recall to check we aren't too far off

intersect_pts <- 
eval_all %>%
  filter(changeClass == "Slow Loss") %>% # filter to only include fields and parameters of interest
  filter(changeMethod == "Change_min_F") %>%
  filter(metric %in% c("Precision", "Recall")) %>%
  filter(FIAIndex == "ShannonClass") %>%
  select(-c(changeClass, changeMethod, FIAIndex, overallAccuracy)) %>%
  # pivot wider on metric
  pivot_wider(names_from = 'metric', values_from = 'value') %>%
  # calculate difference between precision and recall
  mutate(PR_diff = abs(Precision-Recall)) %>%
  group_by(zone) %>%
  arrange(zone, PR_diff) %>%
  slice_head(n=2) 

#inspect intersect pts
prelim_plot + 

  geom_point(data = intersect_pts, aes(x = slowLossThresh, y = Precision), color = "blue") + 
  geom_line(data = intersect_pts,aes(x = slowLossThresh, y = Precision), color = "blue") + 
  geom_point(data = intersect_pts,aes(x = slowLossThresh, y = Recall), color = "red") + 
  geom_line(data = intersect_pts,aes(x = slowLossThresh, y = Recall), color = "red")




# get table of x and y coords
xy_precision <- 
  intersect_pts %>%
  group_by(zone) %>%
  mutate(p = row_number()) %>% # add row number for assigning to separate pts
  ungroup() %>%
  mutate(Precision1 = ifelse(p == 1, Precision, NA),
         Precision2 = ifelse(p == 2, Precision, NA),
         Recall1 = ifelse(p == 1, Recall, NA),
         Recall2 = ifelse(p == 2, Recall, NA),
         x1 = ifelse(p == 1, slowLossThresh, NA),
         x2 = ifelse(p == 2, slowLossThresh, NA)) %>%
  select(zone, x1,x2, Precision1, Precision2, Recall1, Recall2) %>%
  group_by(zone) %>% 
  summarise(across(everything(), ~ paste(unique(.x[!is.na(.x)]), collapse = ", "))) %>% 
  mutate(across(c(2:7), as.numeric)) %>%
  ungroup() 

#inspect
xy_precision %>%
  str()


# calculate intersect using plane geometry packages
#vignette
# line1 <- Line$new(A = c(0,0), B = c(1,1))
# line2 <- Line$new(A = c(0,2), B = c(4,2))
# intersectionLineLine(line1, line2)[2]


line_intx <- function(x1, y1,
                      x2, y2,
                      x3, y3,
                      x4, y4) {
  # x1 = 1
  # y1 = 2
  # x2 = 2
  # y2 = 1
  # x3 = 0
  # y3 = 0
  # x4 = 0
  # y4 = 5
  line1 <- Line$new(A = c(x1, y1), B = c(x2, y2))
  line2 <- Line$new(A = c(x3, y3), B = c(x4, y4))
  x = intersectionLineLine(line1, line2)[1]
  return(x)
  
}
  


ints <- xy_precision %>%
  group_by(zone) %>%
  #as_tibble() %>%
  group_map(~ broom::tidy(line_intx(x1 = .x$x1, y1 = .x$Precision1,
                                    x2 = .x$x2, y2 = .x$Precision2,
                                    x3 = .x$x1, y3 = .x$Recall1,
                                    x4 = .x$x2, y4 = .x$Recall2)))

optimal_slow_loss_thresh <- ints %>% as.data.frame() %>% 
  pivot_longer(cols= everything()) %>%
  cbind(xy_precision$zone) %>%
  rename(optimal_thresh = value,
         zone = `xy_precision$zone`) %>%
  mutate(optimal_thresh_2= round(optimal_thresh, 2)) %>%
  select(-name)

write.csv(optimal_slow_loss_thresh, paste0(eval_dir, "01_csvs/04_threshs/", start_year, end_year, "_ZonesOptimalSlowLossThresh.csv"))


#plot threshs on existing plot
prelim_plot + 
  geom_vline(data = optimal_slow_loss_thresh, aes(xintercept = optimal_thresh), color = "blue", linewidth = 1)+
  geom_label(data = optimal_slow_loss_thresh, aes(x = 25, y = 0.9, label = optimal_thresh_2))

#export
ggsave(filename = paste0(eval_dir, "02_imgs/", start_year, "_", end_year, "_allZones_optimalThresh.png"),
       width = 9, 
       height = 7,
       units = "in",
)
