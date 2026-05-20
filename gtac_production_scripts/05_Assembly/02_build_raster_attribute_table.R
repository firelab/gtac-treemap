library(terra)
library(foreign)
library(glue)
library(tidyverse)

# Set Inputs
# --------------------------------------------------#
home_dir <- "//166.2.126.25/TreeMap/"
year <- 2023
study_area <- "CONUS"
project_name <- glue::glue("{year}_Production_rerun_final_zone_imputations")
fallback_project_name <- glue::glue("{year}_Production")

# set paths to where output data will live
mosaic_dir <- glue::glue("{home_dir}03_Outputs/07_Projects/{year}/{project_name}/04_Mosaic_assembled_model_outputs")

vrt_path <- glue::glue("{mosaic_dir}/TreeMap{year}_{study_area}_vrt.vrt")
tif_path <- glue::glue("{mosaic_dir}/TreeMap{year}_{study_area}.tif")
dbf_path <- glue::glue("{tif_path}.vat.dbf")
overwrite_dbf <- TRUE # set to TRUE to overwrite existing dbf if it exists, FALSE to error if dbf already exists at output path

# set path to RAT csv - with remaining attributes
attribute_table_path <- glue::glue("{home_dir}03_Outputs/06_Reference_Data/v{year}/03_Raster_attributes/TM{year}_RAT_tmid_contValues.csv") 

# Build and export raster attribute table
#------------------------------------------------#
message("Building raster attribute table...")
# Load imputation from the tif output
imputation <- terra::rast(tif_path)

# Build and save raster attribute table
f<- terra::freq(imputation)[,c(2:3)]
names(f)<-c("Value","Count")

# load attribute table csv - with remaining attributes
rat <- read.csv(attribute_table_path)
#rat$X <- NULL

# replace NAs in RAT with 0 
rat[is.na(rat)] <- 0

# join csv to frequency table
out <- left_join(f, rat, by = c("Value" = "TM_ID")) %>%
  dplyr::mutate(TM_ID = Value) %>%
  dplyr::relocate(TM_ID, .after = Value) %>%
  dplyr::relocate(Count, .after = PLT_CN) %>%
  dplyr::rename(CARBON_DWN = CARBON_DOWN_DEAD) # manually rename this field to be 10 characters to fit requirements for .dbf 

str(out)

# assign rat to raster explicitly
levels(imputation) <- out

#Write as dbf / attribute table - overwrite existing if it exists
if (overwrite_dbf && file.exists(dbf_path)) {
    message(glue::glue("Warning: dbf already exists: {dbf_path}"))
    message("deleting existing dbf file")
    file.remove(dbf_path)
}


foreign::write.dbf(out, dbf_path)

message("Raster attribute table saved to: ", dbf_path)
