# Currently, the RATs for 2020, 2022, and 2023 only have categorical values for stand height and canopy height, because they were binned for the imputation. 
# This script takes a crosswalk of continuous canopy height and stand height values, derived from FVS, and joins it to the existing RAT. 
# Subsequent steps will require: 
# - re-make attribute tables attached to rasters (.dbf) with updated RAT
# - re-make attribute layers
# update attribute layers on RDG and Google Data Catalog

library(tidyverse)

options("scipen" = 100, "digits" = 8)

years <- c(2020, 2022, 2023)
new_dat_path <- "//166.2.126.25/TreeMap/01_Data/08_CanopyHeight/TM_FVS_Outputs.csv"

for(year in years){
    
    #year = 2020

    # Set Input Data Paths
    rat_path <- glue::glue("//166.2.126.25/TreeMap/03_Outputs/06_Reference_Data/v{year}/03_Raster_attributes/TM{year}_RAT_tmid.csv")
    out_rat_path <- gsub(".csv", "_contValues.csv", rat_path)

    # Load data
    new_dat <- read.csv(new_dat_path) %>%
        dplyr::rename(STANDHT = StandHt, 
                      CANOPYPCT = CANOPYPCT)
    rat <- read.csv(rat_path) %>%
        dplyr::select(-c(STANDHT, CANOPYPCT))

    # DEBUG: inspect
    str(new_dat)
    str(rat)

    # join on TMID + CN
    rat_out <- left_join(rat, new_dat %>% select(-TM_ID), by = c("PLT_CN"))

    # inspect
    str(rat_out)

    # Export
    write.csv(rat_out, out_rat_path, row.names = FALSE)
    message("Exported {out_rat_path}")
}