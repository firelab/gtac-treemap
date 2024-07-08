# Eval LCMS slow loss layer against FIA data

# basically, do a point extract from FIA db sheet that John shared
# threshold for slow loss
# and make a confusion matrix

# compare accuracy across different thresholds for determining slow loss from FIA data

#v3: vary thresholds used for determining change type in LCMS

# Updated: 6/12/2024

############################
# USER INPUTS
############################

#select year range (LCMS available for 1985-2021)
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
tmp_dir <- "D:/tmp"

# path to 2016 treemap data
treemap_path <- paste0(home_dir, "01_Data/01_TreeMap2016_RDA/RDS-2021-0074_Data/Data/TreeMap2016.tif")

# set path to lcms raw prob rasters
lcms_dir <- '//166.2.126.25/TreeMap/01_Data/05_LCMS/01_Threshold_Testing/'
lcms_dir_rawprob <- paste0(lcms_dir, "01_Raw/02_Raw_Probabilities/")

# path to FIA data
fia_path <- paste0(home_dir, "01_Data/04_FIA/04_LCMS_SlowLoss_Validation/SlowFast_StatVars_ActualLL.csv")

#set path to save evaluation data
eval_dir <- paste0(home_dir, "03_Outputs/07_Projects/Disturbance_Task/01_Evaluation/")

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

#####################
# LOAD DATA
######################

#load any lcms change raster - to get spatial specs; doesn't load values into memory yet
lcms <- terra::rast("//166.2.126.227/lcms/Production/00_CONUS/07_CONUS_2022-8/04_Final_Products/LCMS_CONUS_v2022-8_Change_2020.tif")

# get desired crs from LCMS
crs <- crs(lcms)

# load FIA data
fia <- read.csv(fia_path)

# load LF zone data
LF_zones <- vect(paste0(home_dir, "01_Data/02_Landfire/LF_zones/Landfire_zones/refreshGeoAreas_041210.shp"))

#make year list 
year_list <- seq(start_year, end_year, 1)

################### 
# LOOP OVER LANDFIRE ZONES
##########################


for (z in 1:length(zone_list)) {
  
  #for testing
  #z <- 1
  
  zone_num <- zone_list[z]
  
  # Start the clock!
 #ptm <- proc.time()
  
  # status update
  print(paste0("working on zone ", zone_num))
  

  #####################
  ###### PREP AOI
  #####################

  # select single LF zone
  zone <- subset(LF_zones, LF_zones$ZONE_NUM == zone_num) #z16 = Utah High Plateaus
  
  # get name of zone
  zone_name <- paste0("LFz", zone_num, "_", gsub(" ", "", zone$ZONE_NAME))
  
  if (!is.na(aoi_path)) {
    # load aoi subset - utah uintas only
    aoi <- vect(aoi_path)
    
    # reassign
    zone <- aoi
    zone_name <- aoi_name
    
  } else{}
  
  
  # project
  zone <- project(zone, crs)
  
  #zone
  #plot(zone)
  
  
  ##############################
  ## PREP FIA DATA
  ##########################################
  
  
  # filter to appropriate year range - 
  # plots that were measured in years of interest OR possibly had mortality in the years of interest
  fia %<>%
    filter((MEASYEAR >= start_year & MEASYEAR <= end_year) | MinOfMORTYR >= start_year | MaxOfMORTYR <= end_year)
  
  #rename cols where needed
  names(fia)[1] <- "CN"
  
  # filter spatially - crop 
  
  #convert fia data to spatvector
  fia_sp<- fia %>%
    select(CN, LAT_ACTUAL_NAD83, LON_ACTUAL_NAD83) %>%
    rename(y = LAT_ACTUAL_NAD83, 
           x = LON_ACTUAL_NAD83) %>%
    terra::vect(geom = c("x", "y"), crs = "epsg:4629")
  
  # #inspect
  plot(fia_sp)
  
  #project
  fia_sp <- terra::project(fia_sp, crs)
  
  # #inspect
  # plot(fia_sp)
  
  # crop to AOI
  fia_sp_aoi <- terra::crop(fia_sp, zone)
  
  # convert to buffered poly - to approximate moving window
  fia_sp_aoi_buff <- terra::buffer(fia_sp_aoi, 50) # 50 to approximate 3x3 moving window
  
  #inspect
  #plot(fia_sp_aoi)
  #plot(fia_sp_aoi_buff)
  
  # convert IDs back to data frame
  fia_sp_aoi_df <- data.frame(fia_sp_aoi)
  
  # #inspect
  # fia_sp_aoi_df$CN %>%
  #   unique() %>%
  #   length()
  
  # filter original data set to records contained within AOI
  fia_aoi <- fia %>%
    filter(CN %in% fia_sp_aoi_df$CN)
  
  # #########################################
  # # EDA - FIA data for zone
  # #########################################
  # 
  # str(fia)
  # fia$ShannonClass = factor(fia$ShannonClass)
  # fia$MortStatClass = factor(fia$MortStatClass)
  # 
  # str(fia)
  # 
  # plot(fia$LON_ACTUAL_NAD83, fia$LAT_ACTUAL_NAD83)
  # 
  # 
  # #various EDA
  # 
  # fia %>%
  #   filter(!is.na(ShannonClass)) %>%
  #   ggplot()+
  #   geom_boxplot(aes(x = ShannonClass, y = ShannonH))
  # 
  # fia %>%
  #   filter(!is.na(ShannonClass)) %>%
  #   ggplot()+
  #   geom_boxplot(aes(x = MortStatClass, y = StDevOfMORTYR))
  # 
  # fia %>%
  #   ggplot() + 
  #   geom_point(aes(x = StDevOfMORTYR, y = ShannonH, color = ShannonClass), alpha = 0.5)
  # 
  # fia %>%
  #   ggplot() + 
  #   geom_point(aes(x = StDevOfMORTYR, y = ShannonH, color = MortStatClass), alpha = 0.5)
  # 
  # fia %>%
  #   ggplot() + 
  #   geom_point(aes(x = BALIVEp, y = ShannonH, color = ShannonClass), alpha = 0.5)
  # 
  # fia %>%
  #   ggplot() + 
  #   geom_point(aes(x = BAp_Mort, y = ShannonH, color = ShannonClass), alpha = 0.5)
  # 
  # fia %>%
  #   ggplot() + 
  #   geom_point(aes(x = MortPct, y = ShannonH, color = ShannonClass), alpha = 0.5)
  # 
  
  
  ##################################################
  ## loop over years to extract raw probability data
  ###################################################
  
  # create data frame to bind raw probability extract to 
  d <- data.frame()
  
  for (y in 1:length(year_list)) {
    
    ################# PREP LCMS 
    #############################
    
    #for testing
    #year = 1999
    
    year = year_list[y]
    
    print(paste0("working on ", year))
    
    #  list all raw probability files for each year
    year_files <- list.files(lcms_dir_rawprob, pattern = paste0(year, '.+.tif$'), full.names = TRUE)
    
    print("building VRT")
    
    # build vrt  for raw probability
    vrt <- vrt(x = year_files, 
               filename = paste0(lcms_dir, "04_VRTs/", year, "_RawProb.vrt"), 
               options = 'separate', overwrite = TRUE)
    
    # rename layers
    names(vrt) <- c("FastLoss_Raw_Prob", "SlowLoss_Raw_Prob", "Gain_Raw_Prob")
    
    print("cropping")
    
    # crop to zone
    zone <- terra::project(zone, crs(vrt))
    vrt_crop <- terra::crop(vrt, zone)
    
    print("extracting")
    
    # extract to fia pts 
    ext_test <- terra::extract(vrt_crop, fia_sp_aoi_buff, ID = TRUE)
    
    # add year
    ext_test$year <- year
    
    #inspect
    # ext_test %>%
    #   group_by(ID) %>%
    #   count()
    # 
    # ext_test %>%
    #   filter(is.na(FastLoss_Raw_Prob)) %>%
    #   count()
    
    # bind years together
    d <- rbind(d, ext_test)
    
    gc()
    
  }
  
  ####### PROCESS EXTRACTED DATA FOR THRESHOLDS
  ##############################################
  
  # create empty dataframe
  dt <- data.frame()
  
  for (t in 1:length(slow_loss_thresh_list)) {
    
    #for testing
    #t = 1
    
    slowLossThresh <- slow_loss_thresh_list[t]
    
    # preliminary threshold - assign FL / SL / Gain / NA to each px
    d_thresh <- d %>%
      mutate(slowLossThresh = slowLossThresh, 
             maxConf = pmax(FastLoss_Raw_Prob, SlowLoss_Raw_Prob, Gain_Raw_Prob), # get rowwise max probability
             SL_gte = ifelse(SlowLoss_Raw_Prob >= slowLossThresh, 2, 1), # determine if each value exceeds its threshold
             FL_gte = ifelse(FastLoss_Raw_Prob >= fastLossThresh, 3, 1),
             G_gte  = ifelse(Gain_Raw_Prob >= gainThresh, 4, 1),
             SL_gte_mask = ifelse(SL_gte == 2 & SlowLoss_Raw_Prob == maxConf, 2, 1), # determine if each value exceeds threshold and equals maxConf
             FL_gte_mask = ifelse(FL_gte == 3 & FastLoss_Raw_Prob == maxConf, 3, 1),
             G_gte_mask = ifelse(G_gte == 4 & Gain_Raw_Prob == maxConf, 4, 1),
             Change = pmax(SL_gte_mask, FL_gte_mask, G_gte_mask), # set final change value
             Change = ifelse(is.na(Change), 5, Change) # add value for NP Area Mask
             )
    
    #inspect
    #head(d_thresh)
    
    #select only cols of interest
    d_thresh %<>%
      select(ID, year, slowLossThresh, Change) 
    
    #inspect
    #d_thresh %>%
     # head()
    
    # group by ID to approximate 'moving window' and apply:
      #min (any slow loss -> pt gets slow loss, otherwise mode)
      #mode (most common change class)
      #max (all px must be slow loss to get slow loss, otherwise mode)
    
      ##### this configuration prioritizes slow loss 
    
      ##### i could do something more advanced with a tibble? 
    
    # #inspect
    # d_thresh %>%
    #   group_by(ID) %>%
    #   count(Change)
    
    d_movingwindow <- 
      d_thresh %>%
        group_by(ID) %>%
        mutate(Change_mode = mfv1(Change),
               Change_min = ifelse(2 %in% Change, 2, Change_mode),
               Change_max = ifelse(1 %notin% Change & 3 %notin% Change & 4 %notin% Change & 5 %notin% Change, 2, Change_mode),
               Change_mode_F = ifelse(3 %in% Change, 3, mfv1(Change)),
               Change_min_F = ifelse(3 %in% Change, 3, ifelse(2 %in% Change, 2, Change_mode)),
               Change_max_F = ifelse(3 %in% Change, 3, ifelse(1 %notin% Change & 3 %notin% Change & 4 %notin% Change & 5 %notin% Change, 2, Change_mode))
        ) %>% select(-Change) %>%
      unique() %>%
      ungroup()
    
    # #inspect
    # d_movingwindow %>%
    #   head()
    
    
    #bind
    dt <- rbind(dt, d_movingwindow)
  }
  
  ### GROUP BY ID to summarize change over years
  #########################################################
  
  #clear memory
  gc()
  
  #inspect
  dt %>% summary()
  dt %>% head()
  table(dt$Change_mode)
  table(dt$Change_min)
  table(dt$Change_max)
  
  # pivot longer to make var for changeMethod
  dt %<>%
    pivot_longer(4:ncol(dt),
                 names_to = 'changeMethod',
                 values_to = 'changeType') %>%
    group_by(ID, slowLossThresh, changeMethod) %>%
    #mutate(changeMethod = factor(changeMethod, levels = c('Change_min', 'Change_mode', 'Change_max'))) %>%
    mutate(changeMethod = factor(changeMethod, levels = c('Change_min', 'Change_mode', 'Change_max', 'Change_min_F', 'Change_mode_F', 'Change_max_F'))) %>%
    mutate(changeType = factor(changeType, levels = c(seq(1,5,1), 'NA'))) %>%
    ungroup()
  
  #inspect
  dt %>%
    filter(changeMethod %in% c('Change_min_F', 'Change_mode_F', 'Change_max_F')) %>%
    group_by(changeMethod) %>%
    count(changeType)
  
  #inspect
  # dt %>%
  #   head()
  # str(dt)
  # 
  # dt %>%
  #   select(ID) %>%
  #   unique() %>%
  #   count()
  
  # #look at single change method to start
  # dt %<>%
  #   filter(changeMethod == 'Change_min')  
  
  
  # Reduce to single year / obs for each pt - Get Most Recent Year Where Change Occurred
  #############################################
  dt_years <- dt %>%
    group_by(ID, slowLossThresh) %>%
    # to get most recent year where change occurred)
    filter(changeType != 1 & !is.na(changeType)) %>%
    summarize(yearMostRecentChange = max(year)) %>%
    ungroup() %>%
    # join back with dt to get change type for ids with change
    left_join(dt , by = c('ID' = 'ID',
                         'slowLossThresh' = 'slowLossThresh',
                         'yearMostRecentChange' = 'year')) %>%
    #join back with ids that did not experience change
    full_join(dt %>%
                 filter(changeType == 1 ) %>% 
                 slice_min(year),
               by = c('ID' = 'ID',
                      'slowLossThresh' = 'slowLossThresh',
                      'yearMostRecentChange' = 'year',
                      'changeMethod' = 'changeMethod',
                      'changeType' = 'changeType')) %>%
    arrange(ID) %>%
    ungroup()
  
  
 
  #inspect
  #####################################
  # dt_years %>%
  #   head()
  # 
  # dt_years %>%
  #   select(ID) %>%
  #   unique() %>%
  #   count()
  # 
  # dt_years %>%
  #   group_by(changeMethod, changeType) %>%
  #   count()
 
  #################################################
  ######  JOIN WITH FIA DATA AND PROCESS
  ###############################################
  
  #get ids that match CNs
  ids <- dt %>%
    ungroup() %>%
    dplyr::select(ID) %>%
    unique()
  
  #join ids with CNs
  dt_join <- 
  ids %>%
    cbind(fia_sp_aoi_df) %>%
    left_join(dt_years) 
  
  # #inspect
  # str(dt_join)
  # head(dt_join)
  
  #join with FIA data
  dt_fia <- 
    dt_join %>%
    left_join(fia_aoi, by = 'CN')
  
  # #inspect
  # head(dt_fia)
  # str(dt_fia)
  

  
  # questions for eval: 
  
  # did where did both data sets record 
  #the same type of change as the most recent type of change, at the same location? 
  
  # and how does this vary by threshold? 
  # and how does this vary by change method
  
  #here, we're not worried about matching year to year
  
  # Process FIA data to match LCMS change Classes
  #####################################
  
  # #inspect
  # dt_fia %>%
  #   select(ShannonClass) %>%
  #   unique()
  # 
  # #inspect
  # dt_fia %>%
  #   group_by(ShannonClass) %>%
  #   summarize(n = n())
  
  dt_fia %<>%
    mutate(
      # make NAs explicit
      ShannonClass = tidyr::replace_na(ShannonClass, 0),
      MortStatClass = tidyr::replace_na(MortStatClass, 0),
      ## make change classes with FIA data match those from LCMS data
      ShannonClassNum = case_match(ShannonClass, '' ~ 'NA', 'N' ~ '1', 'B' ~ '1', 'S' ~ '2', 'F' ~ '3'),
      MortStatClassNum = case_match(MortStatClass, '' ~ 'NA', 'N' ~ '1', 'B' ~ '1', 'S' ~ '2', 'F' ~ '3'),
      # convert change classes to factors
      ShannonClassNum = factor(ShannonClassNum, levels = levels(changeType)),
      MortStatClassNum = factor(MortStatClassNum, levels = levels(changeType))) 
  
  # export this data frame 
  write.csv(dt_fia, 
            paste0(eval_dir, "01_csvs/03_extracts/", 
                   start_year, "_", end_year, "_", zone_name, "_dt_fia.csv"))
    
  #inspect
  head(dt_fia)
  str(dt_fia)
  # 
  # #inspect
  # dt_fia %>%
  #   group_by(ShannonClassNum) %>%
  #   summarize(n = n())
  # 
  # dt_fia %>%
  #   filter(is.na(ShannonClassNum)) %>%
  #   select(ID) %>%
  #   unique()
  # 
  # dt_fia %>%
  #   group_by(MortStatClassNum) %>%
  #   summarize(n = n())
  # 
  # 
  # 
  # #inspect
  # dt_fia %>%
  #   group_by(ShannonClassNum) %>%
  #   summarize(n = n())
  
  # remove NAs
  # dt_fia %<>%
  #   filter(!is.na(ShannonClassNum)) %>%
  #   filter(!is.na(MortStatClassNum))
  
    
  ## GENERATE CONFUSION MATRIX
  ############################################
  
  # loop over change methods
  
  # loop over thresholds
  
  # save confusion matrices
  
  # report out classwise accuracy for slow loss, fast loss, stable x method in separate table
  
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
  
  # inspect
  str(eval_t)
  summary(eval_t)
  unique(eval_t$changeClass)
  unique(eval_t$metric)
  unique(eval_t$FIAIndex)
  
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
         width = 12, 
         height = 10,
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
  
  # clear memory
  gc()
  
}





  