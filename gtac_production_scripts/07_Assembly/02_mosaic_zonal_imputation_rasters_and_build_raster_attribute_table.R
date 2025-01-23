library(terra)
library(foreign)

home_dir<- "//166.2.126.25/TreeMap/"

year <- 2022
project_name <- glue::glue("{year}_Production_newXtable")

#set path to assembled rasters
assembled_dir <- glue::glue("{home_dir}03_Outputs/07_Projects/{project_name}/02_Assembled_model_outputs/")

# Make folder for mosaicked model outputs
dir.create("{home_dir}03_Outputs/07_Projects/{project_name}/04_Mosaic_assembled_model_outputs")

#Direct to all zonal imputation rasters
imputation_rasters<- list.files(assembled_dir,
                                pattern = "Production_Imputation.tif$", full.names = T, recursive = T)



# Make a VRT and assemble a complete, mosaicked tif
terra::vrt(imputation_rasters, "{home_dir}03_Outputs/07_Projects/{project_name}/04_Mosaic_assembled_model_outputs/imputation_vrt.vrt")
# Load the VRT
imputation<- rast("{home_dir}03_Outputs/07_Projects/{project_name}/04_Mosaic_assembled_model_outputs/imputation_vrt.vrt")
# Save the VRT as a tif
writeRaster(imputation, "{home_dir}03_Outputs/07_Projects/{project_name}/04_Mosaic_assembled_model_outputs/imputation.tif",
            gdal=c("COMPRESS=LZW"))

# Delete the VRT
file.remove("{home_dir}03_Outputs/07_Projects/{project_name}/04_Mosaic_assembled_model_outputs/imputation_vrt.vrt")


# Reload imputation from the tif output
rm(imputation)
imputation<- rast("{home_dir}03_Outputs/07_Projects/{project_name}/04_Mosaic_assembled_model_outputs/imputation.tif")

# Build and save raster attribute table
f<- freq(imputation)[,c(2:3)]
names(f)<-c("Value","Count")

#Write as dbf / attribute table
foreign::write.dbf(f,"{home_dir}03_Outputs/07_Projects/{project_name}/04_Mosaic_assembled_model_outputs/imputation.tif.vat.dbf")
