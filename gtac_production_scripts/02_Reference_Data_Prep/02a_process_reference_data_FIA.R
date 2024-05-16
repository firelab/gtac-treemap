# Extract reference data from FIA database
# Run for each state at a time
# FIA data must already be dowloaded

# Written by Lila Leatherman (Lila.leatherman@usda.gov)

# Last Updated: 2/21/2024

#######################################################

# Fields to obtain from COND table 
# - DSTRBCD1 DSTRBYR1 # initial fields
# LIVE_CANOPY_CVR_PCT # canopy cover 
# FORTYPCD
# FLDTYPCD
# STDSZCD
# FLDSZCD
# BALIVE
# ALSTK
# GSSTK,
# CARBON_DOWN_DEAD
# QMD_RMRS
# SDIPCT_RMRS

# Fields to calculate based on data in TREE table: 
# - "CARBON_D", 
# - "CARBON_L",
# - "DRYBIO_D",
# - "DRYBIO_L", 
# - TPA_DEAD"
# - "TPA_LIVE"
# - "VOLCFNET_L", 
# - "VOLCFNET_D", 

  

# To obtain in order to calculate derived attributes and prepare Tree List:
# - Cond table: 
#   - COND_STATUS_CODE
# - Tree table: 
#   - DRYBIO_BOLE
#   - DRYBIO_STUMP
#   - DRYBIO_BRANCH
#   - TPA_UNADJ
#   - STATUS_CD
#   - DIA
#   - STANDING_DEAD_CD
#   - VOLBFNET
#   - VOLCFNET
#   - DRYBIO_TOP
#   - DRYBIO_SAPLING
#   - DRYBIO_WDLD_SPP


# - Not in Tree table but in TreeMap 2016 equations: 
#   - DRYBIO_TOP
#   - DRYBIO_SAPLING
#   - DRYBIO_WDLD_SPP


# TO Do: 
# - get List of single condition plots; filter by single-condition plots
# - Obtain actual plot coordinates and convert to meters / LF projection here

# Set inputs
#-----------------------------------------------------#

home_dir <- "//166.2.126.25/TreeMap/"

# set start and end year - obtain data for this year range
start_year <- 1999
end_year <- 2016

# set location of input csvs
fia_path <- glue::glue('{home_dir}01_Data/04_FIA/05_FIA_DataMart/CSV/')

# set location to export ref data to
ref_path <- glue::glue('{home_dir}03_Outputs/06_Reference_Data/')

# list states - lower 48 states by abbreviation
states <- c("ID", "UT", "WY",
            "NV", "CO", "MT")
# states <- c("AL", "AR", "AZ", "CA", "CO", "CT", 
#             "DE", "FL", "GA", "ID", "IL", "IN", 
#             "IA", "KS", "KY", "LA", "ME", "MD",
#             "MA", "MI", "MN", "MS", "MO", "MT",   
#             "NE", "NV", "NH", "NJ", "NM", "NY", 
#             "NC", "ND", "OH", "OK", "OR", "PA", 
#             "RI", "SD", "SC", "TN", "TX", "UT", 
#             "VA", "VT", "WV", "WI", "WY", 'WA')

# Set script options
#-------------------------------------------------------#

# load libraries
library(glue)
library(tidyverse)
library(magrittr)

# Allow for sufficient digits to differentiate plot cn numbers
options("scipen"=100, "digits"=8)

# Create directory
# if(!file.exists(glue::glue('{ref_path}/01_ByState/'))) {
#   dir.create(glue::glue('{ref_path}/01_ByState/'), recursive = TRUE)
# }


# Run - by state
#-------------------------------------------------------#

# iterate over state - save by state

for(i in 1:length(states)) {
  
# for testing  
  #i <- 3
  state_name <- states[i]
  
  print(glue("working on {state_name}"))
  
  # load input tables
  plot <- read.csv(glue('{fia_path}{state_name}_PLOT.csv'))
  cond <- read.csv(glue('{fia_path}{state_name}_COND.csv'))
  tree <- read.csv(glue('{fia_path}{state_name}_TREE.csv'))
  
  # get fields from plot table
  plot_in <- plot %>% 
    dplyr::select(CN, PLOT, STATECD, UNITCD, COUNTYCD, LAT, LON, ELEV, INVYR) %>%
    dplyr::arrange(PLOT) %>%
    dplyr::rename("PLT_CN" = CN)
  
  # get fields from condition table
  cond_in <- cond %>%
    dplyr::select(CN, PLT_CN, CONDID, PLOT, STATECD, UNITCD, COUNTYCD, # id fields
                  ASPECT, SLOPE, DSTRBCD1, DSTRBYR1, # initial fields
                  LIVE_CANOPY_CVR_PCT,# canopy cover 
                  FORTYPCD, FLDTYPCD, STDSZCD, FLDSZCD, BALIVE, ALSTK, GSSTK,
                  CARBON_DOWN_DEAD, QMD_RMRS, SDIPCT_RMRS, # variables that will be derived after imputation
                  COND_STATUS_CD) %>% #variables that will be queried to create end variables 
    dplyr::rename("COND_CN" = CN) %>%
    arrange(PLOT) 
  
  # Join plot and cond data
  t <- inner_join(plot_in, cond_in, by = c("PLT_CN", "PLOT", "STATECD", "UNITCD", "COUNTYCD")) %>%
    # filter to years of interest
    filter(INVYR >= start_year & INVYR <= end_year) %>%
    # filter to forested plots
    filter(COND_STATUS_CD == 1) %>%
    # filter to single-condition plots
    filter(CONDID == 1) %>%
    # calculate aspect as northing and easting
    dplyr::mutate(radians = (pi/180)*ASPECT,
           NORTHING = round(cos(radians), 6),
           EASTING = round(sin(radians), 6)) %>%
    dplyr::select(-radians) %>%
    dplyr::arrange(PLOT, STATECD, UNITCD, COUNTYCD) 
  
  
  # apply any other filters
  
  # join with coordinates
  
  # check for duplicates
  t %>%
    group_by(PLT_CN) %>%
    filter(n()> 1)
  
  # save
  #write.csv(t, glue('{ref_path}{state_name}_FIAextract_allVars.csv'))
  
  # BUILD TREE TABLE
  # ------------------------------------------------------------#
  
  # build tree table
  tree_in <- tree %>%
    # filter to years of interest
    filter(INVYR > start_year) %>%
    dplyr::select(CN, PLT_CN, PLOT, STATECD, UNITCD, COUNTYCD,
                  #DRYBIO_TOP, DRYBIO_SAPLING, DRYBIO_WDLD_SPP, # not in tree table for some reason?
                  DRYBIO_STEM, DRYBIO_BRANCH, DRYBIO_FOLIAGE,
                  DRYBIO_AG, DRYBIO_BG, 
                  DRYBIO_BOLE,  DRYBIO_STUMP,  
                  TPA_UNADJ, STATUSCD, DIA, STANDING_DEAD_CD, VOLBFNET, VOLCFNET,
                  TREECLCD) 
    
  
  # join with raw vars table
  treelist <- 
    tree_in %>%
    right_join(t, by = c("PLT_CN", "PLOT", "STATECD", "UNITCD", "COUNTYCD" )) %>%
      group_by(PLT_CN) 
      
  # Calculate vars from FIA
  # -------------------------------------------#
  
  # CARBON_D
  # Carbon, standing dead (tons per acre). Calculated via the following FIA query:
  #   Query; Sum (DRYBIO_BOLE, DRYBIO_TOP, DRYBIO_STUMP, DRYBIO_SAPLING, DRYBIO_WDLD_SPP) / 2 /2000*TPA_UNADJ WHERE (((COND.COND_STATUS_CD)=1) AND ((TREE.STATUSCD)=2) AND ((TREE.DIA)>=5) AND ((TREE.STANDING_DEAD_CD)=1))
  
  CARBON_D <- 
    treelist %>%
    dplyr::filter(COND_STATUS_CD == 1 &
                    STATUSCD == 2 &
                    DIA >= 5 &
                    STANDING_DEAD_CD == 1) %>%
    dplyr::reframe(CARBON_D = sum(DRYBIO_BOLE, DRYBIO_STUMP, DRYBIO_BRANCH )/2/2000 * TPA_UNADJ) %>%
    distinct()
  
  # CARBON_L
  # Carbon, live above ground (tons per acre). Calculated via the following FIA query:
  #   Query; Sum (DRYBIO_BOLE, DRYBIO_TOP, DRYBIO_STUMP, DRYBIO_SAPLING, DRYBIO_WDLD_SPP) / 2 /2000*TPA_UNADJ WHERE (((COND.COND_STATUS_CD)=1) AND ((TREE.STATUSCD)=1))
  
  CARBON_L <- 
    treelist %>%
    dplyr::filter(COND_STATUS_CD == 1 &
                    STATUSCD == 1 ) %>%
    dplyr::reframe(CARBON_L = sum(DRYBIO_BOLE, DRYBIO_STUMP, DRYBIO_BRANCH  )/2/2000*TPA_UNADJ)%>%
    distinct()
  
  #DRYBIO_D
  # Dry standing dead tree biomass, above ground (tons per acre). Calculated via the following FIA query:
  #   Query; Sum (DRYBIO_BOLE, DRYBIO_TOP, DRYBIO_STUMP, DRYBIO_SAPLING, DRYBIO_WDLD_SPP) /2000*TPA_UNADJ WHERE (((COND.COND_STATUS_CD)=1) AND ((TREE.STATUSCD)=2) AND ((TREE.DIA)>=5) AND ((TREE.STANDING_DEAD_CD)=1))
  
  DRYBIO_D <- treelist %>%
    dplyr::filter(COND_STATUS_CD == 1 &
                    STATUSCD == 2 &
                    DIA >= 5 &
                    STANDING_DEAD_CD == 1) %>%
    dplyr::reframe(DRYBIO_D = sum(DRYBIO_BOLE, DRYBIO_STUMP, DRYBIO_BRANCH )/2000*TPA_UNADJ)%>%
    distinct()
  
  # DRYBIO_L 
  # Dry live tree biomass, above ground (tons per acre). Calculated via the following FIA query:
  #   Query; Sum (DRYBIO_BOLE, DRYBIO_TOP, DRYBIO_STUMP, DRYBIO_SAPLING, DRYBIO_WDLD_SPP) /2000*TPA_UNADJ WHERE (((COND.COND_STATUS_CD)=1) AND ((TREE.STATUSCD)=1))
  
  DRYBIO_L <- treelist %>%
    dplyr::filter(COND_STATUS_CD == 1 &
                    STATUSCD == 1 ) %>%
    dplyr::reframe(DRYBIO_L = sum(DRYBIO_BOLE, DRYBIO_STUMP, DRYBIO_BRANCH )/2000*TPA_UNADJ)%>%
    distinct()
 
    #TPA_DEAD
    #Number of standing dead trees per acre (DIA greater than 5"). Calculated via the following FIA query:
    #Query; Sum TREE.TPA_UNADJ WHERE (((COND.COND_STATUS_CD)=1) AND ((TREE.STATUSCD)=2) AND ((TREE.DIA)>=5) AND ((TREE.STANDING_DEAD_CD)=1))
  
  TPA_DEAD <- treelist %>%
    dplyr::filter(COND_STATUS_CD == 1 &
                    STATUSCD == 2 &
                    DIA >= 5 &
                    STANDING_DEAD_CD == 1) %>%
    dplyr::reframe(TPA_DEAD = sum(TPA_UNADJ)) %>%
    distinct()
    
    # TPA_LIVE: Live Trees Per Acre
    # Number of live trees per acre. Calculated via the following FIA query:
    #   Query; Sum TREE.TPA_UNADJ WHERE (((COND.COND_STATUS_CD)=1) AND ((TREE.STATUSCD)=1) AND ((TREE.DIA)>=1))
  
  TPA_LIVE <- treelist %>%
    dplyr::filter(COND_STATUS_CD == 1 &
                    STATUSCD == 1 &
                    DIA >= 1 ) %>%
    dplyr::reframe(TPA_LIVE = sum(TPA_UNADJ)) %>%
    distinct()
    
#     VOLBFNET_L: Volume, Live (sawlog-board-ft/acre)
#     Volume, live, sawlog board feet per acre (log rule: Int'l ¼ inch). Calculated via the following FIA query:
# Query; Sum VOLBFNET * TPA_UNADJ WHERE (((TREE.TREECLCD)=2) AND ((COND.COND_STATUS_CD)=1) AND ((TREE.STATUSCD)=1))
  
  VOLBFNET_L <- treelist %>%
    dplyr::filter(TREECLCD == 2 &
                    COND_STATUS_CD == 1 &
                    STATUSCD == 1) %>%
    dplyr::reframe(VOLBFNET_L = sum(VOLBFNET*TPA_UNADJ)) %>%
    distinct() 
    
    # VOLCFNET_D: Volume, Standing Dead (ft3/acre)
    # Volume, standing dead, cubic feet per acre. Calculated via the following FIA query:
    #   Query; Sum VOLCFNET*TPA_UNADJ WHERE (((COND.COND_STATUS_CD)=1) AND ((TREE.STATUSCD)=2) AND ((TREE.DIA)>=5) AND ((TREE.STANDING_DEAD_CD)=1))
    
  VOLCFNET_D <- treelist %>%
    dplyr::filter(COND_STATUS_CD == 1 &
                    STATUSCD == 2 &
                    DIA >= 5 &
                    STANDING_DEAD_CD == 1) %>%
    dplyr::reframe(VOLCFNET_D = sum(VOLCFNET*TPA_UNADJ)) %>%
    distinct()
  
    # VOLCFNET_L: Volume, Live (ft3/acre)
    # Volume, live, cubic feet per acre. Calculated via the following FIA query:
    #   Query; Sum VOLCFNET*TPA_UNADJ WHERE (((COND.COND_STATUS_CD)=1) AND ((TREE.STATUSCD)=1))
  
  VOLCFNET_L <- treelist %>%
    dplyr::filter(COND_STATUS_CD == 1 &
                    STATUSCD == 1 ) %>%
    dplyr::reframe(VOLCFNET_L = sum(VOLCFNET*TPA_UNADJ)) %>%
    distinct()

  
  # Join all vars together
  #--------------------------------------------#
  d_list <- list(CARBON_D, CARBON_L, DRYBIO_D, DRYBIO_L, TPA_DEAD, TPA_LIVE, VOLBFNET_L, VOLCFNET_D, VOLCFNET_L)
  
  attributes_out <- purrr::reduce(d_list,
                full_join,
                by = "PLT_CN") %>%
    dplyr::full_join(t, by = "PLT_CN") %>%
    # Select variables of interest
    dplyr::select(PLT_CN, PLOT, LAT, LON, ELEV, SLOPE, ASPECT, NORTHING, EASTING, 
           # attributes
           ALSTK, BALIVE, CARBON_D, CARBON_L, CARBON_DOWN_DEAD,
           DSTRBCD1, DSTRBYR1, DRYBIO_D, DRYBIO_L,FORTYPCD, FLDTYPCD, FLDSZCD, 
           GSSTK, LIVE_CANOPY_CVR_PCT,QMD_RMRS, STDSZCD,  SDIPCT_RMRS, TPA_DEAD,
           TPA_LIVE, VOLBFNET_L, VOLCFNET_D, VOLCFNET_L)
  
  dupes <- attributes_out %>%
    group_by(PLT_CN) %>%
    filter(n() > 1)
  
  # check for duplicates
  print("duplicate CNs - first 6")
  print(
    attributes_out %>%
      group_by(PLT_CN) %>%
      filter(n() > 1) %>% 
      head()
    )
  
  # save
  write.csv(attributes_out, glue('{ref_path}/01_ByState/{state_name}_{start_year}_{end_year}_attributes.csv'), row.names = FALSE )

  }
  