library(terra)
library(foreign)
setwd(this.path::here())

#Direct to rasters needing attribute tables
imputation_rasters<- list.files("../03_Outputs/07_Projects/2020_Production/02_Assembled_model_outputs/",
                                pattern = "Production_Imputation.tif$", full.names = T, recursive = T)


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
