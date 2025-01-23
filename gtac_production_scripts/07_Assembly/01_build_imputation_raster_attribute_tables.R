library(terra)
library(foreign)

home_dir<- "//166.2.126.25/TreeMap/"

year <- 2022
project_name <- glue::glue("{year}_Production_newXtable")

#set path to assembled rasters
assembled_dir <- glue::glue("{home_dir}03_Outputs/07_Projects/{project_name}/02_Assembled_model_outputs/")

#Direct to the imputation rasters needing attribute tables
imputation_rasters<- list.files(assembled_dir, pattern = "Production_Imputation.tif$", full.names = T, recursive = T)


# Loop through imputation rasters, load each one, calculate frequency table, and build and save attribute table
for (i in seq_along(imputation_rasters)){
  # Load imputation raster
  r<- rast(imputation_rasters[i])
  
  # Calculate frequency table
  f<- freq(r)[,c(2:3)]
  names(f)<-c("Value","Count")
  
  #Write as dbf / attribute table
  foreign::write.dbf(f,gsub(".tif",".tif.vat.dbf",imputation_rasters[i]))
  #
  print(paste0("Finished writing attribute table ",i))
  
}
