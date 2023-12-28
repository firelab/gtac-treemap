# Extract reference data from FIA database

# Fields to obtain: 
# Slope
# Aspect
# Elevation
# Disturbance Type
# Disturbance Code

# if available: 
# EVC
# EVH

# TO Do: 
# - get List of single condition plots; filter by single-condition plots
# - Obtain actual plot coordinates and convert to meters / LF projection here

# load libraries
library(tidyverse)
library(magrittr)

# set location of input csvs
data_path <- "//166.2.126.25/TreeMap/01_Data/04_FIA/06_FIA_DataMart/CSV/"

# set location to export ref data to
ref_export <- "//166.2.126.25/TreeMap/03_Outputs/06_Reference_Data/"

# list states - lower 48 states by abbreviation
states <- c("ID", "UT", "WY")
# states <- c("AL", "AR", "AZ", "CA", "CO", "CT", 
#             "DE", "FL", "GA", "ID", "IL", "IN", 
#             "IA", "KS", "KY", "LA", "ME", "MD",
#             "MA", "MI", "MN", "MS", "MO", "MT",   
#             "NE", "NV", "NH", "NJ", "NM", "NY", 
#             "NC", "ND", "OH", "OK", "OR", "PA", 
#             "RI", "SD", "SC", "TN", "TX", "UT", 
#             "VA", "VT", "WV", "WI", "WY", 'WA')

# iterate over state - save by state

for(i in 1:length(states)) {
  
  state_name <- states[i]
  
  print(glue("working on {state_name}"))
  
  # load input tables
  plot <- read.csv(glue('{data_path}{state_name}_PLOT.csv'))
  cond <- read.csv(glue('{data_path}{state_name}_COND.csv'))
  
  # get fields from plot table
  plot %<>% 
    dplyr::select(CN, PLOT, STATECD, UNITCD, COUNTYCD, LAT, LON, ELEV) %>%
    dplyr::arrange(PLOT) 
  
  # get fields from condition table
  cond %<>%
    dplyr::select(CN, PLT_CN, PLOT, ASPECT, SLOPE, DSTRBCD1, DSTRBYR1, # initial fields
                  LIVE_CANOPY_CVR_PCT) %>% # canopy cover and canopy height
    dplyr::rename("COND_CN" = CN) %>%
    arrange(PLOT) 
  
  # filter to single-condition plots
  
  # join
  t <- inner_join(plot, cond, by = c("CN" = "PLT_CN", "PLOT" = "PLOT")) %>%
    # calculate aspect as northing and easting
    dplyr::mutate(radians = (pi/180)*ASPECT,
           NORTHING = cos(radians),
           EASTING = sin(radians)) %>%
    dplyr::select(-radians) %>%
    dplyr::arrange(PLOT, STATECD, UNITCD, COUNTYCD) 
  
  # calculate aspect into northing and easting
  
  # save
  write.csv(t, glue('{ref_export}{state_name}_FIAextract.csv'))
  
}


