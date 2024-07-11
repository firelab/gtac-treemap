# Set up
home_dir  <<- "//166.2.126.25/TreeMap/"
setwd(home_dir)


# Get the valid zone numbers
lf_zones<- vect("./01_Data/02_Landfire/LF_zones/Landfire_zones/refreshGeoAreas_041210.shp")
lf_zone_nums<- sort(lf_zones$ZONE_NUM)


# 2020 ----

# Loop through zones----
for (i in lf_zone_nums){
  
  # Get Daymet in directory path
  ifelse(i<10, 
         daymet_in_dir<- paste0("./03_Outputs/05_Target_Rasters/pre_mask_climate_2020/z0",i),
         daymet_in_dir<- paste0("./03_Outputs/05_Target_Rasters/pre_mask_climate_2020/z",i))
  
  # Get outpath where you want to move files
  ifelse(i<10, 
         out_dir<- paste0("./03_Outputs/05_Target_Rasters/v2020/pre_mask/z0",i),
         out_dir<- paste0("./03_Outputs/05_Target_Rasters/v2020/pre_mask/z",i))

  # Get all file names within the in path
  in_file_names <- list.files(daymet_in_dir, pattern = ".tif",full.names = T) 
  
  # Convert name to the outpath
  out_file_names<- gsub(daymet_in_dir, out_dir, in_file_names)
  
  # And move (aka rename) the files
  file.rename(from = in_file_names, to = out_file_names)
  
}
  
# 2022 ----

# Loop through zones----
for (i in lf_zone_nums){
  
  # Get Daymet in directory path
  ifelse(i<10, 
         daymet_in_dir<- paste0("./03_Outputs/05_Target_Rasters/pre_mask_climate_2022/z0",i),
         daymet_in_dir<- paste0("./03_Outputs/05_Target_Rasters/pre_mask_climate_2022/z",i))
  
  # Get outpath where you want to move files
  ifelse(i<10, 
         out_dir<- paste0("./03_Outputs/05_Target_Rasters/v2022/pre_mask/z0",i),
         out_dir<- paste0("./03_Outputs/05_Target_Rasters/v2022/pre_mask/z",i))
  
  # Get all file names within the in path
  in_file_names <- list.files(daymet_in_dir, pattern = ".tif",full.names = T) 
  
  # Convert name to the outpath
  out_file_names<- gsub(daymet_in_dir, out_dir, in_file_names)
  
  # And move (aka rename) the files
  file.rename(from = in_file_names, to = out_file_names)
  
}