# Assemble reference data into one file for all states

# Written by Lila Leatherman (Lila.Leatherman@usda.gov)

# Last updated: 2/21/24

# Set inputs
#-----------------------------------------------------#

home_dir <- "//166.2.126.25/TreeMap/"

# set start year - obtain data for this year and all years after
start_year <- 1999
end_year <- 2016

# set location to export ref data to
ref_path <- glue::glue('{home_dir}03_Outputs/06_Reference_Data/')

# set location of raster attribute table
#rast_path <- glue::glue("{home_dir}01_Data/01_TreeMap2016_RDA/RDS-2021-0074_Data/Data/")

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

# Set script options
#-------------------------------------------------------#

# load libraries
library(glue)
library(tidyverse)
library(magrittr)
library(terra)

# Allow for sufficient digits to differentiate plot cn numbers
options("scipen"=100, "digits"=8)

# Create directory
if(!file.exists(glue::glue('{ref_path}/02_Final/'))) {
  dir.create(glue::glue('{ref_path}/02_Final/'), recursive = TRUE)
}

# Run
#------------------------------------------------------1

# List files
flist <- list.files(glue('{ref_path}/01_ByState'), pattern = glue('{start_year}_{end_year}_attributes.csv'), full.names = TRUE)

# Read in files and bind
ref <- do.call(rbind, 
               lapply(flist, read.csv))

# Identify any any duplicate CNs
ref %>% group_by(PLT_CN) %>%
  filter(n()>1)

# Do any other filtering

# assign any other reference variables that might be useful

# Assign a TreeMap ID
ref$ID <- seq(1:nrow(ref))

# Export
write.csv(ref, glue('{ref_path}/02_Final/{start_year}_{end_year}_Attributes.csv'))
