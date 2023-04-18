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

# set home dir
home_dir <- ("//166.2.126.25/TreeMap/")

# set tmp directory
tmp_dir <- "E:/tmp"

# path to 2016 treemap data
treemap_path <- paste0(home_dir, "01_Data/01_TreeMap2016_RDA/RDS-2021-0074_Data/Data/TreeMap2016.tif")

# path to LCMS slow loss layers
slowloss_dir <- paste0(home_dir, "03_Outputs/01_LCMS_Slow_Loss/01_Rasters/")

# path to FIA data
fia_path <- paste0(home_dir, "01_Data/04_FIA/SlowFast_StatVars_ActualLL.csv")

#select file to eval - can adjust later / inspect vs diff years
slowloss_filename <- paste0(start_year, "_", end_year, "_LF_z16_UT_High_Plateaus_LCMS_SlowLoss.tif")


#####################
# SETUP
######################

# set desired end crs 
crs <- crs("epsg:5070") # tree map output data is in NAD83 Albers

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

# load slow loss layer
slowloss <- terra::rast(paste0(slowloss_dir, slowloss_filename))

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
zone <- subset(LF_zones, LF_zones$ZONE_NUM == 16) #z16 = Utah High Plateaus

# load aoi subset - utah uintas only
aoi <- vect(paste0(home_dir, "01_Data/03_AOIs/UT_Uintas_rect_NAD1983.shp"))

# reassign
zone <- aoi

# project
zone <- project(zone, crs)

##########################
# EDA
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

# convert slow loss layer to binary 0/1
m <- c(0, 2025, 1)
m <- matrix(m, ncol = 3, byrow= TRUE)
slowloss_bin <- terra::classify(slowloss, m)

#inspect
plot(slowloss_bin, col = "red")

# convert LCMS layer to a 3x3 window 
slowloss_bin_focal <- terra::focal(slowloss_bin, 
                                   w = 3,
                                   fun = "modal") ### function here 
#inspect
slowloss_bin_focal
plot(slowloss_bin_focal, col = "red")

#modal function expands area of slow loss 

################ PREP FIA

# filter to appropriate year range 
fia %<>%
  filter(MEASYEAR >= start_year & MEASYEAR <= end_year )

# filter spatially - crop 

#convert fia data to spatvector
fia_sp<- fia %>%
  select(CN, LAT_ACTUAL_NAD83, LON_ACTUAL_NAD83) %>%
  rename(y = LAT_ACTUAL_NAD83, 
         x = LON_ACTUAL_NAD83) %>%
  terra::vect(geom = c("x", "y"), crs = crs)

#inspect
plot(fia_sp)

# crop to AOI
fia_sp_aoi <- terra::crop(fia_sp, zone)

# filter and make everything factors
fia_sp_aoi %<>% 
  filter(ShannonClass != "") %>%
  mutate(ShannonClass = factor(ShannonClass),
         MortStatClass = factor(MortStatClass))

#inspect
plot(fia_sp_aoi)


################ EXTRACT

# extract FIA data to points indicated by 3x3 moving window
fia_sp_aoi$slowloss <- terra::extract(slowloss_bin_focal, fia_sp_aoi )[2]

#inspect
fia_sp_aoi
plot(fia_sp_aoi, col = fia_sp_aoi$slowloss)

#join with full FIA data set
fia_extract <- data.frame(fia_sp_aoi) %>%
  left_join(fia)
str(fia_extract)

# convert to observed / predicted format
# slowloss = 1 -> slowloss

fia_extract %<>%
  mutate(slowlossClass = ifelse(is.na(slowloss), "N", ifelse(slowloss == 1, "S", NA)),
         slowlossClass = factor(slowlossClass, levels = levels(ShannonClass))) 

#inspect
str(fia_extract)

# generate confusion matrix
cm <- caret::confusionMatrix(fia_extract$ShannonClass, # pred
                       fia_extract$slowlossClass # ref
                       )

cm

# extract accuracy measures
addmargins(cm$table)
cm$byClass

# classwise accuracy for slow loss: 
cm$byClass[4,11]

# what if 

# append to data frame


# export eval stats

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
