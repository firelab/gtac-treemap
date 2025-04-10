# Mosaic Zonal Imputation Rasters and Append Attribute Table
# Written by Scott Zimmer (scott.zimmer@usda.gov) 
# and Lila Leatherman (Lila.Leatherman@usda.gov)

# This script takes the raster attribute table data, derived from FIA for each of the imputed ids in the
# model run, and joins it to the .dbf file

# NOTE: Will need to be updated for AK and HI production

# Last Updated: 2/12/2025

#########################################################
# Set Inputs
#########################################################

# project inputs
year <- 2022
studyArea <- 'CONUS'
project_name <- glue::glue("{year}_Production_newXtable")

#set path to assembled rasters - relative to home_dir
assembled_dir <- glue::glue("03_Outputs/07_Projects/{project_name}/02_Assembled_model_outputs/")

# folder for mosaicked model outputs - relative to home_dir
mosaic_dir <- glue::glue("03_Outputs/07_Projects/{project_name}/04_Mosaic_assembled_model_outputs")

# paths to data - relative to home_dir
dbf_table_path <- glue::glue("{mosaic_dir}/TreeMap{year}.tif.vat.dbf") 
attribute_table_path <- glue::glue("03_Outputs/06_Reference_Data/v{year}/03_Raster_attributes/TM{year}_RAT_tmid.csv") 


# Load TreeMap script library
#--------------------------------------------------#

# necessary to load this before calling any of the home_dir, fia_dir, or data_dir objects

# load library 
this_proj = this.path::this.proj()
lib_path = glue::glue("{this_proj}/gtac_production_scripts/00_Library/treeMapLib.R")
source(lib_path)

library(foreign)

# Update folder and data paths
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
tif_out_path = glue::glue("{mosaic_dir}/TreeMap{year}_{studyArea}.tif")

if(file.exists(tif_out_path)) {
  message(glue::glue("Warning: overwriting existing tif: {tif_out_path}"))
}

terra::writeRaster(imputation, tif_out_path,
            gdal=c("COMPRESS=LZW"),
            overwrite = TRUE
            )

# Delete the VRT
file.remove(glue::glue("{mosaic_dir}/imputation_vrt.vrt"))

# Reload imputation from the tif output
rm(imputation)

# Delete any pre-existing dbf file so that we can overwrite and create a new one
if(file.exists(glue::glue("{tif_out_path}.vat.dbf"))){
  message(glue::glue("Warning: dbf already exists: {tif_out_path}.vat.dbf"))
  message("deleting existing dbf file")
  file.remove(glue::glue("{tif_out_path}.vat.dbf"))
}

imputation <- rast(tif_out_path)

# Build raster attribute table - calculate frequencies
f<- terra::freq(imputation)[,c(2:3)]
names(f)<-c("TM_ID","Count")

# load attribute table csv - with remaining attributs
rat <- read.csv(attribute_table_path)
#rat$X <- NULL

# join csv to frequency table
out <- left_join(f, rat, by = "TM_ID") #%>%
  #dplyr::rename("QMD" = QMDAll) %>%
  #dplyr::mutate(TM_ID = Value)

# inspect
str(out)

#Write as dbf / attribute table
foreign::write.dbf(out,
                   glue::glue("{tif_out_path}.vat.dbf"))
