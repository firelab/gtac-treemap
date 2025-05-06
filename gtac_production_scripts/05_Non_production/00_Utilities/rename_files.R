# Rename folders


# shell(paste('rename', 
#             sprintf("content/%s-content", pu_name),
#             sprintf("content/%s", other_name)))


files <- list.files("//166.2.126.25/TreeMap/08_Data_Delivery/01_Separated_Attribute_Rasters/2022", full.names = TRUE)

files.rename = gsub(pattern = "TreeMap_CONUS_2022", 
                    replacement = "TreeMap2022_CONUS",
                    x = files)

file.rename(files, files.rename)

