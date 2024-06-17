# Evaluate reference data - GTAC-calculated against Raster Attribute table

# Written by Lila Leatherman (lila.leatherman@usda.gov)

# Last updated: 5/10/2024

##################################################

# Set inputs
#-----------------------------------------------------#

# home dir

# Id where script is located
this.path <- this.path::this.path()

# get path to input script
spl <- stringr::str_split(this.path, "/")[[1]]
setup_dirs.path <- paste( c(spl[c(1:(length(spl)-2))],
                              "00_Library/setup_dirs.R" ),
                            collapse = "/")

source(setup_dirs.path)

# data directory - where source data are located
data_dir <- glue::glue('{home_dir}/01_Data/')

# set start year - obtain data for this year and all years after
start_year <- 1999
end_year <- 2016

# set location of prepared reference data
ref_path <- glue::glue('{home_dir}03_Outputs/06_Reference_Data/')

# set location of raster attribute table
rast_path <- glue::glue("{home_dir}01_Data/01_TreeMap2016_RDA/RDS-2021-0074_Data/Data/")

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
library(terra)

# Allow for sufficient digits to differentiate plot cn numbers
options("scipen"=100, "digits"=8)

# Run - by state
#-------------------------------------------------------#

# iterate over state - save by state

#for(i in 1:length(states)) {

# for testing  
i <- 1
state_name <- states[i]

# Load calculated data
#-------------------------------------------#
ref_dat <- read.csv(glue::glue('{ref_path}/v2016_GTAC/01_ByState/{state_name}_{start_year}_{end_year}_attributes.csv'))
ref_dat %<>%
  select(-c(PLOT, LAT, LON, ELEV, SLOPE, ASPECT, NORTHING, EASTING)) 


# Load raster attribute table
#-------------------------------------------------#
rat <- terra::rast(glue::glue('{rast_path}TreeMap2016.tif'))
t <- data.frame(cats(rat))

t %<>% 
  rename("SDIPCT_RMRS" = SDIPCT_RMR,
         "CARBON_DOWN_DEAD" = CARBON_DWN) %>%
  mutate(CN = as.numeric(CN)) %>%
  select(-Value)

names(t)

# Join
# -------------------------------------------------#
table <- left_join(ref_dat, t, by = c("PLT_CN" = "CN"))


# Initial Inspect 
# --------------------------------------------------#

# plot(table$FORTYPCD.y, table$FORTYPCD.x)
# plot(table$CARBON_D.y, table$CARBON_D.x)
# plot(table$CARBON_L.y, table$CARBON_L.x)

# Better Join
# ------------------------------------------------------#

#fields we want to evaluate
fields_val <- c(#"FORTYPCD",   "FLDTYPCD",   "STDSZCD",    "FLDSZCD",   
                #"BALIVE",     "CANOPYPCT",  "STANDHT",    "ALSTK",      
                #"GSSTK" ,     "QMD_RMRS",   "SDIPCT_RMRS", 
                "TPA_LIVE",   
                "TPA_DEAD",   "VOLCFNET_L", "VOLCFNET_D", "VOLBFNET_L",
                "DRYBIO_L",   "DRYBIO_D", "CARBON_L",   "CARBON_D")

refs <- t %>%
  filter(CN %in% ref_dat$PLT_CN) %>%
  select(-c(ForTypName, FldTypName)) %>%
  select(CN, all_of(fields_val)) %>%
  pivot_longer(any_of(fields_val), names_to = "var", values_to = "ref") %>%
                 mutate(var = factor(var))


preds <- ref_dat %>%
  filter(PLT_CN %in% t$CN) %>% 
  select(PLT_CN, fields_val) %>%
  pivot_longer(any_of(fields_val), names_to = "var", values_to = "pred") %>%
  mutate(var = factor(var))

p_r <- left_join(preds, refs, by = c("PLT_CN" = "CN", "var"))


# Plot vars
#######################################

p <- 
p_r %>%
  #filter(var == fields_val[i]) %>%
  ggplot() +
  geom_abline(intercept = 0, color = "red", linewidth = 0.5 ) + 
  geom_point(aes(x = ref, y = pred), alpha = 0.1) + 
  facet_wrap(~var, scales = "free")+
  labs(x = "RAT values", y = "GTAC values")

print(p)

# Calculate r squared
#######################################

p_r %>%
  split(p_r$var) %>%
  map(\(df) lm(pred~ref, data = df)) %>%
  map(summary) %>%
  map_dbl("r.squared")

# calculate N of each var
p_r %>%
  drop_na() %>%
  group_by(var) %>%
  summarize(n())

