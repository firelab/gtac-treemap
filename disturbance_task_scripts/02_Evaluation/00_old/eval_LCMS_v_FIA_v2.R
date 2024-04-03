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
#zone_name <- "LF_z16_UT_High_Plateaus" 
zone_name <- "UT_Uintas_subset"

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
slowloss_filename <- paste0(start_year, "_", end_year, "_UT_Uintas_subset_LCMS_SlowLoss.tif")

#get path to all LCMS data for desired time period
eval_filename <- paste0(start_year, "_", end_year, "_UT_Uintas_subset_LCMS_SlowFastStableEval.tif")

# aoi path - if different from landfire zone
# supply path, or NA
aoi_path <- paste0(home_dir, "01_Data/03_AOIs/UT_Uintas_rect_NAD1983.shp")
#aoi_path <- NA

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

#create year list
year_list <- seq(start_year, end_year, 1)

#####################
# LOAD DATA
######################

#load any lcms change raster - to get spatial specs; doesn't load values into memory yet
lcms <- terra::rast(paste0(lcms_dir, "LCMS_CONUS_v2021-7_Change_2020.tif"))

# get desired crs from LCMS
crs <- crs(lcms)

# load slow loss layer
slowloss <- terra::rast(paste0(slowloss_dir, "01_SlowLoss/", slowloss_filename))

# load eval layers - slow, fast, and stable for start year - end year
lcms_eval <- terra::rast(paste0(slowloss_dir, "02_Eval/", eval_filename))

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

# #inspect
# zone
# plot(zone)
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
  filter(!is.na(ShannonH)) %>%
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


######################################
################ PREP FIA
#####################################

# filter to appropriate year range - filters on measurement year, not mortality year
# fia %<>%
#   filter(MEASYEAR >= start_year & MEASYEAR <= end_year )

# filter spatially - crop 

#convert fia data to spatvector
fia_sp<- fia %>%
  select(CN, LAT_ACTUAL_NAD83, LON_ACTUAL_NAD83) %>%
  rename(y = LAT_ACTUAL_NAD83, 
         x = LON_ACTUAL_NAD83) %>%
  terra::vect(geom = c("x", "y"), crs = "epsg:4629")

#inspect
#plot(fia_sp)

#project
fia_sp <- terra::project(fia_sp, crs)

#inspect
#plot(fia_sp)

# crop to AOI
fia_sp_aoi <- terra::crop(fia_sp, zone)

#inspect
plot(fia_sp_aoi)

# convert to data frame
# and join with full fia data set
fia_aoi <- data.frame(fia_sp_aoi) %>%
  left_join(fia, by = "CN")

#inspect
str(fia_aoi)

##########################
# PROCESS
##########################

################# PREP LCMS 
#############################

#inspect
slowloss

# convert slow loss layer to binary 0/1
# where 1 indicates slow loss over any year from start year to end year
m <- c(0, 2025, 1)
m <- matrix(m, ncol = 3, byrow= TRUE)
slowloss_bin <- terra::classify(slowloss, m)

#inspect
plot(slowloss_bin, col = "red")

# convert LCMS layer to a 3x3 window 
#modal function expands area of slow loss 
slowloss_bin_focal <- terra::focal(slowloss_bin, 
                                   w = 3,
                                   fun = "modal") ### function here 
#inspect
slowloss_bin_focal
plot(slowloss_bin_focal, col = c("white", "red"))

################## PREP LCMS EVAL
#################################

# Take 1:
# make binary 3x3 for slow, fast, stable each over time period
###################################################################

#inspect
lcms_eval

#create year range
year_list <- seq(start_year, end_year, 1)


nlyr(lcms_eval)

# make empty raster for each
slow_r <- terra::rast(crs = crs, ext(lcms_eval), res = res(lcms_eval))
slow_r <- setValues(slow_r, 0)
fast_r <- slow_r
stable_r <- slow_r

#set values to reclassify 
# classes are ordered from 1-n in order: Stable, Slow Loss, Fast Loss, Gain, NP Area Mask

for (i in 1:nlyr(lcms_eval)){
  
  # for testing
  #i = 2
  
  # get year
  year <- year_list[i]
  
  # status update
  print(paste0("working on ", year))
  
  #isolate layer
  lyr <- lcms_eval[[i]]
  
  # get stable values
  st <- terra::classify(lyr, cbind(c(2,3,4,5,NA), NA))
  #update nas to 0
  st <- subst(st, NA, 0)
  #append
  stable_r <- stable_r + st
  stable_r <- subst(stable_r, 1, year)
  
  # get slow values
  sl <- terra::classify(lyr, cbind(c(1,3,4,5,NA), NA))
  #update nas to 0
  sl <- subst(sl, NA, 0)
  #update values of interest to 1
  sl <- subst(sl,2, 1)
  #append by adding
  slow_r <- slow_r + sl
  slow_r <- subst(slow_r, 1, year)
  
  # get fast values
  f <- terra::classify(lyr, cbind(c(1,2,4,5,NA), NA))
  #update nas to 0
  f <- subst(f, NA, 0)
  #update values of interest to 1
  f <- subst(f,3, 1)
  #append
  fast_r <- fast_r + f
  fast_r <- subst(fast_r, 1, year)
  
  
}

# #update 0s back to NA
# stable_r <- subst(stable_r, 0, NA)
# slow_r <- subst(slow_r, 0, NA)
# fast_r <- subst(fast_r, 0, NA)

# inspect
stable_r
slow_r
fast_r


#inpsect
par(mfrow = c(2,2))
plot(stable_r)
plot(slow_r)
plot(fast_r)

par(mfrow = c(1,1))

#### convert eval layers to binar moving window
# convert slow loss layer to binary 0/1
m <- c(0, 2025, 1)
m <- matrix(m, ncol = 3, byrow= TRUE)
stable_r_bin_focal <- terra::focal(
                                  terra::classify(stable_r, m),
                                  w = 3,
                                  fun = "modal")
                                 
fast_r_bin_focal <- terra::focal(
                                  terra::classify(fast_r, m),
                                  w = 3,
                                  fun = "modal")



# mask with tree mask - same as slowloss_bin_focal
stable_r_bin_focal <- terra::mask(stable_r_bin_focal, slowloss_bin_focal)
fast_r_bin_focal <- terra::mask(fast_r_bin_focal, slowloss_bin_focal)


# Take 2:
# Extract for each plot, in each year, in seq min-max years
###########################################

# Prep LCMS layers
# load as a stack for year list
############################################


#create empty raster
r <- rast(crs = crs, ext(slowloss), res = res(slowloss))
r <- setValues(r, 0)
r <- mask(r, slowloss)

lcms_yrs <- r

for(i in 1:length(year_list)){
  
  #for testing
  #i = 1
  
  # iterate through change rasters by year
  year <- year_list[i]
  
  print(paste0("working on ", year))
  
  #load annual lcms change raster
  lcms_yr <- terra::rast(paste0(lcms_dir, "LCMS_CONUS_v2021-7_Change_", year, ".tif"))
  
  #crop lcms change raster
  lcms_yr <- crop(lcms, zone)
  
  # stack
  lcms_yrs <- c(lcms_yrs, lcms_yr)
  
  #rename layer
  names(lcms_yrs)[i+1] <- year
  
  
  
}

lcms_yrs

# remove empty first raster, retain all others
lcms_yrs <- lcms_yrs[[1:length(year_list)+1]]

#remove single year raster to save space
rm(lcms_yr)

#inspect
lcms_yrs
names(lcms_yrs)

# apply 3x3 moving window - focal mode
lcms_yrs_focal <- terra::focal(
  lcms_yrs,
  w = 3,
  fun = "modal")

#reassign 
lcms_yrs <- lcms_yrs_focal

#remove unused memory
rm(lcms_yrs_focal)
gc()

##############################################
## Extract
#########################################

#for each plot, each year in seq min-max years:
# is there fast loss? buffer- same year or year after, 
# is there slow loss? buffer- min and max fia plot years  


# whatever you end up doing, give the example of a single plot 
# getting it out of raster sooner, dealing with tables to crosswalk 


# smash into a max reducer if ANY OF These have change / meet criteria
# reducer - any time there's agreement

#set up basic cols
fia_aoi$fastloss <- NA
fia_aoi$slowloss <- NA
fia_aoi$stable <- NA

# set up annual cols
for (j in 1:length(year_list)) {
  
  year <- year_list[j]

  #rename column to year
  fia_aoi[, paste("fastloss", year, sep = "_")] <- fia_aoi$fastloss
  
  #rename column to year
  fia_aoi[, paste("slowloss", year, sep = "_")] <- fia_aoi$slowloss
  
  #rename column to year
  fia_aoi[, paste("stable", year, sep = "_")] <- fia_aoi$stable
}

#########
# get fast loss, slow loss, and stable for each point for each year

#for (i in 1:nrow(fia_aoi)){
for (i in 1:25){
  
  # for testing
  #i = 20
  
  #status update
  print(paste0("working on plot ", i, " of ", nrow(fia_aoi)))
  
  #get point
  pt <- fia_sp_aoi[i]
  
  # get minimum mortality year
  minMortYear <- as.numeric(fia_aoi$MinOfMORTYR[i])
  
  # get maximum mortality year
  maxMortYear <- as.numeric(fia_aoi$MaxOfMORTYR[i])

  # get seq mort years that overlap with our years of interest
  if (is.na(minMortYear)) {
    mortYear_list <- NA
  } else {
    mortYear_list <- seq(minMortYear, maxMortYear, 1)
    mortYear_list <- intersect(mortYear_list, year_list)
  }
  
  
  for (j in 1:length(year_list)){
    
    # for testing
    #j = 1
    
    # get year
    year = year_list[j]
    
    ########
    ###### Is there fast loss? 
    #Logic: Return 1 if there is fast loss in fast loss year = year or fast loss year = year + 1
    ########
    
    # classes are ordered from 1-n in order: Stable, Slow Loss, Fast Loss, Gain, NP Area Mask
    # 1 Stable, 2 Slow loss, 3 Fast loss, 4 Gain, 5 NP Area mask,
    
    FL1 <- as.numeric(ifelse(
      terra::extract(lcms_yrs[[j]], pt)[2] == 3, 1, 0))
    if (year != end_year){
      FL2 <- as.numeric(ifelse(
        terra::extract(lcms_yrs[[j+1]], pt)[2]==3, 1, 0))
    } else {
      FL2 <- 0
    }
    
    FL_final <- max(FL1, FL2)
    
    fia_aoi[, paste("fastloss", year, sep = "_")][i] <- FL_final
    
    #####################
    ##### Is there slow loss?
    # Logic: slow loss in a given year if:
    #    a) given year is in min-max of mort years that overlap with years of layer
    # +  b) there is slow loss in any year from in mort year - max mort year
    ###################

    if (!is.na(minMortYear) & year >= minMortYear & year <= maxMortYear){

      SL2 <- 0

      for (k in length(mortYear_list)) {
        
        # for testing
        #k = 5
        
        #
        y <- mortYear_list[k]

        SL1 <- as.numeric(ifelse(
          terra::extract(lcms_yrs[[j]], pt)[2] == 2, 1, 0))

        SL2 <- max(SL1, SL2)
        }
      } else {
        SL1 <- 0
        SL2 <- 0
    }

    SL_final <- max(SL1, SL2)

    #append to data frame
    #fia_aoi$slowloss[i] <- SL_final

    fia_aoi[, paste("slowloss", year, sep = "_")][i] <- SL_final

    ###### Is it stable? 
    # Logic: it is stable if:
    #   a) there is stable in that year
    # + b) we voted no for slow loss for that year
    
    ST1 <- as.numeric(ifelse(
      terra::extract(lcms_yrs[[k]], pt)[2] == 1, 1, 0)) 
    ST2 <- ifelse(SL_final == 1, 0, 1)
    
    ST_final <- min(ST1, ST2)
    
    #append to data frame
    fia_aoi[, paste("stable", year, sep = "_")][i] <- ST_final
    
    #save memory
    gc()
    
  }
}
  

#bin final class
#### Logic 
  
  





####################################
################ EXTRACT to FIA PTS
######################################

# extract 3x3 moving window slow loss data to fia points
fia_sp_aoi$slowloss <- terra::extract(slowloss_bin_focal, fia_sp_aoi )[2]
# extract stable and fast loss for eval
fia_sp_aoi$stable <- terra::extract(stable_r_bin_focal, fia_sp_aoi)[2]
fia_sp_aoi$fastloss <- terra::extract(stable_r_bin_focal, fia_sp_aoi)[2]

fia_sp_aoi$slowloss_yr <- terra::extract(slowloss, fia_sp_aoi)[2]
fia_sp_aoi$stable_yr <- terra::extract(stable_r, fia_sp_aoi)[2]
fia_sp_aoi$fastloss_yr <- terra::extract(fast_r, fia_sp_aoi)[2]

#inspect
fia_sp_aoi
head(data.frame(fia_sp_aoi))

#join with full FIA data set
fia_extract <- data.frame(fia_sp_aoi) %>%
  left_join(fia)
str(fia_extract)



##### inspect to see overlap between slow / stable/focal from LCMS
fia_extract %>%
  ggplot()+
  geom_density(aes(slowloss, color = factor(fastloss_yr)), adjust = 1, linewidth = 1) + 
  theme_bw() + facet_wrap(~factor(stable))

fia_extract %>%
  ggplot()+
  geom_density(aes(slowloss, color = factor(stable_yr)), adjust = 1, linewidth = 1) + 
  theme_bw() + facet_wrap(~factor(fastloss))

addmargins(table(fia_extract$slowloss, fia_extract$fastloss))

addmargins(table(fia_extract$slowloss_yr, fia_extract$fastloss))


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
