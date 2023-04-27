# Eval LCMS slow loss layer against FIA data

# basically, do a point extract from FIA db sheet that John shared
# threshold for slow loss
# and make a confusion matrix

# compare accuracy across different thresholds for determining slow loss from FIA data

############################
# USER INPUTS
############################

#select year range (LCMS available for 1985-2021)
start_year <- 2010
end_year <- 2016

# give name for outputs 
zone_name <- "LF_z16_UT_High_Plateaus" 
#zone_name <- "UT_Uintas_subset"

# select landfire zone
zone_num <- 16

# set home dir
home_dir <- ("//166.2.126.25/TreeMap/")

# set tmp directory
tmp_dir <- "E:/tmp"

# path to 2016 treemap data
treemap_path <- paste0(home_dir, "01_Data/01_TreeMap2016_RDA/RDS-2021-0074_Data/Data/TreeMap2016.tif")

# get path to change rasters - LCMS
lcms_dir <- ("//166.2.126.227/lcms/Projects/11_LCMS_NationalProduct/06_DataDelivery/Conterminous_United_States/v2021-7/Change/Annual/")

# path to LCMS slow loss layers
slowloss_dir <- paste0(home_dir, "03_Outputs/01_LCMS_Slow_Loss/01_Rasters/")

# path to FIA data
fia_path <- paste0(home_dir, "01_Data/04_FIA/SlowFast_StatVars_ActualLL.csv")

#select file to evaluate - can adjust later / inspect vs diff years
#slowloss_filename <- paste0(start_year, "_", end_year, "_UT_Uintas_subset_LCMS_SlowLoss.tif")
slowloss_filename <- paste0(start_year, "_", end_year, "_LF_z16_UT_High_Plateaus_LCMS_SlowLoss.tif")

#set path to save evaluation data
eval_dir <- paste0(home_dir, "03_Outputs/01_LCMS_Slow_Loss/02_Evaluation/")

#eval_filename <- paste0(start_year, "_", end_year, "_UT_Uintas_subset_LCMS_SlowFastStableEval.tif")

# aoi path - if different from landfire zone
# supply path, or NA
#aoi_path <- paste0(home_dir, "01_Data/03_AOIs/UT_Uintas_rect_NAD1983.shp")
aoi_path <- NA

#####################
# SETUP
######################

# set desired end crs 
#crs <- crs("epsg:5070") # tree map output data is in NAD83 Albers

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

#####################
# LOAD DATA
######################

#load any lcms change raster - to get spatial specs; doesn't load values into memory yet
lcms <- terra::rast(paste0(lcms_dir, "LCMS_CONUS_v2021-7_Change_2020.tif"))

# get desired crs from LCMS
crs <- crs(lcms)

# load slow loss layer
slowloss <- terra::rast(paste0(slowloss_dir, "01_SlowLoss/", slowloss_filename))

# load slow loss eval layer
#slowloss_eval <- terra::rast(paste0(slowloss_dir, "02_Eval/", eval_filename))

# load FIA data
fia <- read.csv(fia_path)

#rename vars as needed
fia %<>% 
  rename(CN = ï..CN)

#####################
###### PREP AOI
#####################

# load LF zone data
LF_zones <- vect(paste0(home_dir, "01_Data/02_Landfire/LF_zones/Landfire_zones/refreshGeoAreas_041210.shp"))

# inspect
#LF_zones$ZONE_NAME

# select single LF zone
zone <- subset(LF_zones, LF_zones$ZONE_NUM == zone_num) #z16 = Utah High Plateaus

if (!is.na(aoi_path)) {
  # load aoi subset - utah uintas only
  aoi <- vect(aoi_path)
  
  # reassign
  zone <- aoi
} else{}


# project
zone <- project(zone, crs)

zone
plot(zone)
##########################
# EDA - FIA data
##########################

str(fia)
fia$ShannonClass = factor(fia$ShannonClass)
fia$MortStatClass = factor(fia$MortStatClass)

str(fia)

plot(fia$LON_ACTUAL_NAD83, fia$LAT_ACTUAL_NAD83)


#various EDA

fia %>%
  filter(!is.na(ShannonClass)) %>%
  ggplot()+
  geom_boxplot(aes(x = ShannonClass, y = ShannonH))

fia %>%
  filter(!is.na(ShannonClass)) %>%
  ggplot()+
  geom_boxplot(aes(x = MortStatClass, y = StDevOfMORTYR))

fia %>%
  ggplot() + 
  geom_point(aes(x = StDevOfMORTYR, y = ShannonH, color = ShannonClass), alpha = 0.5)

fia %>%
  ggplot() + 
  geom_point(aes(x = StDevOfMORTYR, y = ShannonH, color = MortStatClass), alpha = 0.5)

fia %>%
  ggplot() + 
  geom_point(aes(x = BALIVEp, y = ShannonH, color = ShannonClass), alpha = 0.5)

fia %>%
  ggplot() + 
  geom_point(aes(x = BAp_Mort, y = ShannonH, color = ShannonClass), alpha = 0.5)

fia %>%
  ggplot() + 
  geom_point(aes(x = MortPct, y = ShannonH, color = ShannonClass), alpha = 0.5)


##########################
# PROCESS
##########################

################# PREP LCMS 
#############################

#inspect
slowloss

# convert slow loss layer to binary 0/1
m <- c(0, 2025, 1)
m <- matrix(m, ncol = 3, byrow= TRUE)
slowloss_bin <- terra::classify(slowloss, m)

#inspect
plot(slowloss_bin, col = c("red"))

# convert LCMS layer to a 3x3 window 
#modal function expands area of slow loss 
slowloss_bin_focal <- terra::focal(slowloss_bin, 
                                   w = 3,
                                   fun = "modal") ### function here 
#inspect
slowloss_bin_focal
plot(slowloss_bin_focal, col = c("white", "red"))

# ################## PREP LCMS EVAL - not used
# #################################
# 
# #inspect
# slowloss_eval
# 
# #create year range
# year_list <- seq(start_year, end_year, 1)
# 
# # make 3x3 for slow, fast, stable each over time period
# nlyr(slowloss_eval)
# 
# # make empty raster for each
# slow_r <- terra::rast(crs = crs, ext(slowloss_eval), res = res(slowloss_eval))
# slow_r <- setValues(slow_r, 0)
# fast_r <- slow_r
# stable_r <- slow_r
# 
# #set values to reclassify 
# # classes are ordered from 1-n in order: Stable, Slow Loss, Fast Loss, Gain, NP Area Mask
# 
# for (i in 1:nlyr(slowloss_eval)){
#   
#   # for testing
#   #i = 2
#   
#   # get year
#   year <- year_list[i]
#   
#   # status update
#   print(paste0("working on ", year))
#   
#   #isolate layer
#   lyr <- slowloss_eval[[i]]
#   
#   # get stable values
#   st <- terra::classify(lyr, cbind(c(2,3,4,5,NA), NA))
#   #update nas to 0
#   st <- subst(st, NA, 0)
#   #append
#   stable_r <- stable_r + st
#   stable_r <- subst(stable_r, 1, year)
#   
#   # get slow values
#   sl <- terra::classify(lyr, cbind(c(1,3,4,5,NA), NA))
#   #update nas to 0
#   sl <- subst(sl, NA, 0)
#   #update values of interest to 1
#   sl <- subst(sl,2, 1)
#   #append
#   slow_r <- slow_r + sl
#   slow_r <- subst(slow_r, 1, year)
#   
#   # get fast values
#   f <- terra::classify(lyr, cbind(c(1,2,4,5,NA), NA))
#   #update nas to 0
#   f <- subst(f, NA, 0)
#   #update values of interest to 1
#   f <- subst(f,3, 1)
#   #append
#   fast_r <- fast_r + f
#   fast_r <- subst(fast_r, 1, year)
#   
#   
# }
# 
# # #update 0s back to NA
# # stable_r <- subst(stable_r, 0, NA)
# # slow_r <- subst(slow_r, 0, NA)
# # fast_r <- subst(fast_r, 0, NA)
# 
# # inspect
# stable_r
# slow_r
# fast_r
# 
# 
# #inpsect
# par(mfrow = c(2,2))
# plot(stable_r)
# plot(slow_r)
# plot(fast_r)
# 
# par(mfrow = c(1,1))
# 
# #### convert eval layers to binar moving window
# # convert slow loss layer to binary 0/1
# m <- c(0, 2025, 1)
# m <- matrix(m, ncol = 3, byrow= TRUE)
# stable_r_bin_focal <- terra::focal(
#                                   terra::classify(stable_r, m),
#                                   w = 3,
#                                   fun = "modal")
#                                  
# fast_r_bin_focal <- terra::focal(
#                                   terra::classify(fast_r, m),
#                                   w = 3,
#                                   fun = "modal")
# 
# 

################ PREP FIA
#####################################

##############################
## PREP FIA DATA
##########################################

# filter to appropriate year range 
# fia %<>%
#   filter(MEASYEAR >= start_year & MEASYEAR <= end_year )

# filter spatially - crop 

#convert fia data to spatvector
fia_sp<- fia %>%
  select(CN, LAT_ACTUAL_NAD83, LON_ACTUAL_NAD83) %>%
  rename(y = LAT_ACTUAL_NAD83, 
         x = LON_ACTUAL_NAD83) %>%
  terra::vect(geom = c("x", "y"), crs = "epsg:4629")

# #inspect
# plot(fia_sp)
# 
#project
fia_sp <- terra::project(fia_sp, crs)
# 
# #inspect
# plot(fia_sp)

# crop to AOI
fia_sp_aoi <- terra::crop(fia_sp, zone)

#inspect
plot(fia_sp_aoi)

# #inspect
# plot(fia_sp_aoi)


################ EXTRACT

# extract 3x3 moving window slow loss data to fia points
fia_sp_aoi$slowloss <- terra::extract(slowloss_bin_focal, fia_sp_aoi )[2]
# extract stable and fast loss for eval
#fia_sp_aoi$stable <- terra::extract(stable_r_bin_focal, fia_sp_aoi)[2]
#fia_sp_aoi$fastloss <- terra::extract(stable_r_bin_focal, fia_sp_aoi)[2]

#inspect
fia_sp_aoi

#join with full FIA data set
fia_extract <- data.frame(fia_sp_aoi) %>%
  left_join(fia, by = "CN")
str(fia_extract)


# ##### inspect to see overlap between slow / stable/focal from LCMS
# fia_extract %>%
#   ggplot(aes(x = slowloss, y = fastloss, color = stable))+
#   geom_point()

# convert to observed / predicted format
# slowloss = 1 -> slowloss

fia_extract2 <- 
  fia_extract %>%
  #filter(ShannonClass != "B") %>%
  filter(is.na(MinOfMORTYR) | MinOfMORTYR >= start_year | MaxOfMORTYR <= end_year) %>%
  filter(ShannonClass != "") %>%
  filter(!is.na(ShannonClass)) %>%
  mutate(MortStatClass = factor(MortStatClass),
         ShannonClass = factor(ShannonClass, levels = levels(MortStatClass)),
         slowlossClass = ifelse(is.na(slowloss), "N", ifelse(slowloss == 1, "S", ifelse(slowloss == 0, "N", NA))),
         slowlossClass = factor(slowlossClass, levels = levels(MortStatClass))) 


#inspect
str(fia_extract2)

fia_extract2 %>%
  group_by(ShannonClass) %>%
  count()

fia_extract2 %>%
  group_by(MortStatClass) %>%
  count()

fia_extract2 %>%
  group_by(slowlossClass) %>%
  count()

# generate confusion matrix
cm <- caret::confusionMatrix(fia_extract2$slowlossClass, # pred
                       fia_extract2$ShannonClass # ref
                       )

cm

# extract accuracy measures
addmargins(cm$table)
cm$byClass

# classwise accuracy for slow loss: 
cm$byClass[4,11]

# do the same thing for MortStat Class

# generate confusion matrix
cm <- caret::confusionMatrix(fia_extract2$slowlossClass, # pred
                             fia_extract2$MortStatClass # ref
)

cm

# extract accuracy measures
addmargins(cm$table)
cm$byClass

# classwise accuracy for slow loss: 
cm$byClass[4,11]


##############################################################
# Export
##############################################################

# raw confusion matrix
cm_raw_out <- as.table(cm)
cm_raw_out <- addmargins(cm_raw_out)

cm_t_overall <- data.frame(as.matrix(cm, what = "overall"))
names(cm_t_overall) <- c("value")

cm_t_classes <- data.frame(as.matrix(cm, what = "classes"))

#inspect
cm_raw_out
cm_t_overall
cm_t_classes

# check export name


print(paste0(eval_dir, start_year, "_", end_year, "_", zone_name))
export_name <- paste0(eval_dir, start_year, "_", end_year, "_", zone_name)

# export
write.csv(cm_raw_out, file = paste0(export_name, "_cm_raw", ".csv"))
write.csv(cm_t_overall, file = paste0(export_name, "_cm_overall", ".csv"))
write.csv(cm_t_classes, file = paste0(export_name, "_cm_classes", ".csv"))




#######################################
# additional eval
########################################

# First tier secondary eval: 
#   - Vary % of LCMS px that show slow loss
#   - Use different functions for focal 


# convert LCMS layer to a 3x3 window 
slowloss_bin_focalmin <- terra::focal(slowloss_bin, 
                                   w = 3,
                                   fun = "min") ### function here 

# convert LCMS layer to a 3x3 window 
slowloss_bin_focalmax <- terra::focal(slowloss_bin, 
                                      w = 3,
                                      fun = "max") ### function here 


# extract FIA data to points indicated by 3x3 moving window
fia_sp_aoi$slowlossfocalmin <- terra::extract(slowloss_bin_focalmin, fia_sp_aoi )[2]
fia_sp_aoi$slowlossfocalmax <- terra::extract(slowloss_bin_focalmax, fia_sp_aoi )[2]

# 
fia_extract <- data.frame(fia_sp_aoi) %>%
  left_join(fia)
str(fia_extract)

#
fia_extract %<>%
  mutate(slowlossClass_focalMode = ifelse(is.na(slowloss), "N", ifelse(slowloss == 1, "S", NA)),
         slowlossClass_focalMin = ifelse(is.na(slowlossfocalmin), "N", ifelse(slowloss == 1, "S", NA)),
         slowlossClass_focalMax = ifelse(is.na(slowlossfocalmax), "N", ifelse(slowloss == 1, "S", NA)),
         slowlossClass_focalMode = factor(slowlossClass_focalMode, levels = levels(ShannonClass)),
         slowlossClass_focalMax = factor(slowlossClass_focalMax, levels = levels(ShannonClass)),
         slowlossClass_focalMin = factor(slowlossClass_focalMin, levels = levels(ShannonClass)))





#################################################

# Second tier secondary eval:
#   - Vary threshold for Shannon H that determines slow vs fast
# - Vary threshold for standard dev that determines slow vs fast
# 
# Additional options: 
#   - compare vs Landfire disturbance layer 
#   - Pull in LCMS fast loss for additional comparison, see if that's where any errors are coming up
# 	- + also indicate where commission error is occurring 
# 	- Eval 2016 disturbance layer
# 	- Eval 2020 disturbance layer
# 	- Eval for other Landfire regions 


##############################
# compare accuracy by varying basal mortality, percent mortality
###############################

# to do this--
# calculate quartiles, separate quartiles, and compare accuracy across quartiles? 
