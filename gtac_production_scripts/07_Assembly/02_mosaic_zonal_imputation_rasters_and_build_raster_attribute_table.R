# Mosaic Zonal Imputation Rasters and Append Attribute Table
# Written by Scott Zimmer (scott.zimmer@usda.gov) 
# and Lila Leatherman (Lila.Leatherman@usda.gov)

# This script takes the raster attribute table data, derived from FIA for each of the imputed ids in the
# model run, and joins it to the .dbf file

<<<<<<< Updated upstream
=======
# NOTE: Will need to be updated for AK and HI production

# Last Updated: 2/6/2025

#########################################################
# Set Inputs
#########################################################

# project inputs
>>>>>>> Stashed changes
year <- 2020
project_name <- glue::glue("{year}_Production_newXtable")

#set path to assembled rasters - relative to home_dir
assembled_dir <- glue::glue("03_Outputs/07_Projects/{project_name}/02_Assembled_model_outputs/")

<<<<<<< Updated upstream
# set path to attribute table 
rat_path <- glue::glue("{home_dir}03_Outputs/06_Reference_Data/v{year}/03_Raster_attributes/TM{year}RAT_tmid.csv")

# Make folder for mosaicked model outputs
dir.create("{home_dir}03_Outputs/07_Projects/{project_name}/04_Mosaic_assembled_model_outputs")
=======
# folder for mosaicked model outputs - relative to home_dir
mosaic_dir <- glue::glue("03_Outputs/07_Projects/{project_name}/04_Mosaic_assembled_model_outputs")
>>>>>>> Stashed changes

# paths to data
dbf_table_path <- glue::glue("{mosaic_dir}/TreeMap{year}.tif.vat.dbf") #relative to home_dir
attribute_table_path <- glue::glue("03_Outputs/06_Reference_Data/v{year}/03_Raster_attributes/TM{year}RAT_tmid.csv") # relative to home_dir


# Load TreeMap script library
#--------------------------------------------------#

# necessary to load this before calling any of the home_dir, fia_dir, or data_dir objects

# load library 
this_proj = this.path::this.proj()
lib_path = glue::glue("{this_proj}/gtac_production_scripts/00_Library/treeMapLib.R")
source(lib_path)

library(foreign)

# Update folder  and data paths
#-----------------------------------------------------#
assembled_dir <-  glue::glue("{home_dir}/{assembled_dir}")
mosaic_dir <- glue::glue("{home_dir}/{mosaic_dir}")
dbf_table_path <- glue::glue("{home_dir}/{dbf_table_path}")
attribute_table_path <- glue::glue("{home_dir}/{attribute_table_path}")

# conditionally create mosaic_dir
if (!file.exists(mosaic_dir)){
  dir.create(mosaic_dir, recursive = TRUE)
  
}


# Mosaic Imputation Rasters 
#--------------------------------------------------#
#Direct to all zonal imputation rasters
imputation_rasters<- list.files(assembled_dir,
                                pattern = "Production_Imputation.tif$", full.names = T, recursive = T)


# Make a VRT and assemble a complete, mosaicked tif
terra::vrt(imputation_rasters, glue::glue("{mosaic_dir}/imputation_vrt.vrt"),
           overwrite = TRUE)

# Load the VRT
imputation <- rast(glue::glue("{mosaic_dir}/imputation_vrt.vrt"))

# Save the VRT as a tif
tif_out_path = glue::glue("{mosaic_dir}/TreeMap{year}.tif")

if(file.exists(tif_out_path)) {
  message(glue::glue("Warning: overwriting existing tif: {tif_out_path}"))
}

writeRaster(imputation, tif_out_path,
            gdal=c("COMPRESS=LZW"),
            overwrite = TRUE)

# Delete the VRT
file.remove(glue::glue("{mosaic_dir}/imputation_vrt.vrt"))

# Reload imputation from the tif output
rm(imputation)
if(file.exists(glue::glue("{tif_out_path}.vat.dbf"))){
  message(glue::glue("Warning: dbf already exists: {tif_out_path}.vat.dbf"))
}

imputation<- rast(tif_out_path)

# Build and save raster attribute table
f<- freq(imputation)[,c(2:3)]
names(f)<-c("Value","Count")

<<<<<<< Updated upstream
# load additional raster attributes
rat <- read.csv(rat_path)

# join with frequency table

=======
# load attribute table csv
rat <- read.csv(attribute_table_path)
rat$X <- NULL

# join csv to dbf
out <- left_join(f, rat, by = c("Value" = "TM_ID")) %>%
  dplyr::rename("QMD" = QMDAll) %>%
  dplyr::mutate(TM_ID = Value)

# inspect
str(out)
>>>>>>> Stashed changes

#Write as dbf / attribute table
foreign::write.dbf(out,glue::glue("{tif_out_path}.vat.dbf"))
