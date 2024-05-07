# Download and unzip all Landfire disturbance files
# Available for manual download from this location: 
# https://www.landfire.gov/version_download.php

# Written by Lila Leatherman (Lila.Leatherman@usda.gov) and Abhinav Shrestha (abhinav.shrestha@usda.gov) 
# Last Updated: 05/06/2024 (Abhinav Shrestha)



# This script downloads and organizes Landfire Veg (EVC, EVT, EVH) and Topographic (SLOPE_D, ELEV, Asp) layers



#####################################
# Set Inputs
#######################################

# set file destination
#dir <- "//166.2.126.25/TreeMap/01_Data/02_Landfire/LF_220/Disturbance/"

# For testing
dir <- "C:/Users/abhinavshrestha/OneDrive - USDA/Documents/02_TreeMap/temp_dir/"


# list years
years <- c(2001, 2014, 2016, 2020, 2022)

# list years

vegetation_datasets <- c("EVC", "EVT", "EVH")

# set maximum tries to download
maxDownload_count <- 5

################################################
# Run
################################################

ptm.start <- Sys.time() # processing time (ptm): start

# create directory if necessary 
if(!file.exists(dir)) {
  dir.create(dir, recursive = TRUE)
}

# File name format, by years: 
# ---------------------------------------#
# 2001: US_105_<veg_dataset>
# 2014: US_140_<veg_dataset>
# 2016, 2020*, 2022: LF<year>_<veg_dataset>_<LF_dataset_version>
# - LF_dataset_version (year): 200 (2016), 220 (2020), 230 (2022)
# - * exception for 2020 EVC and EVH veg datasets which are projected to 2022 and named LF2022. 
# - * NOT the case for 2022 EVC and EVH datasets that are projected to 2023 but still named LF2022

# sample download url pre 2020 "https://landfire.gov/bulk/downloadfile.php?FNAME=US_Disturbance-US_DIST1999.zip&TYPE=landfire
# sample download url 2015 "https://landfire.gov/bulk/downloadfile.php?FNAME=US_Disturbance-LF2020_Dist_220_CONUS.zip&TYPE=landfire"

url_base <-  "https://www.landfire.gov/bulk/downloadfile.php?FNAME=US_"

url_base_pre2016 <- ".zip&TYPE=landfire"
url_base_200 <- "_200_CONUS.zip&TYPE=landfire"
url_base_220 <- "_220_CONUS.zip&TYPE=landfire" 
url_base_230 <- "_230_CONUS.zip&TYPE=landfire"


ptm_VegStart <- Sys.time()

for (i in 1:length(vegetation_datasets)){
  
  veg_dataset <- vegetation_datasets[i]
  
  print("======================= === ================================================")
  print(paste0("Download and extracting ", veg_dataset, " data for years: ", list(years)))
  print("======================= === ================================================")
  
  ptm.vegDataStart <- Sys.time()
  
  for(j in 1:length(years)){
    
    #j = 1
    
    year_name <- years[j]
    
    print(paste0("downloading ", year_name))
    
    # For every loop, dir resets to main LF directory
    dir <- "C:/Users/abhinavshrestha/OneDrive - USDA/Documents/02_TreeMap/temp_dir/02_Landfire/"
    
    ##
    ##### Set appropriate file name
    ##
    
    if (year_name >= 2016){
      
      if (year_name == 2016) {
        
        url <- paste0(url_base, "200_mosaic-LF", year_name, "_", veg_dataset, url_base_200)
        
        dir <- paste0(dir, "LF_200/Vegetation/", veg_dataset, "/")
        
        # create file name
        zipfilename <- paste0(dir, "LF", year_name,"_200_", veg_dataset, ".zip")
        
      } else if (year_name == 2020) {
        
        url <- paste0(url_base, "220_mosaic-LF2022_", veg_dataset, url_base_220)
        
        dir <- paste0(dir, "LF_220/Vegetation/", veg_dataset, "/")
        
        # create file name
        zipfilename <- paste0(dir, "LF", year_name,"_220_", veg_dataset, ".zip")
        
      } else if (year_name == 2022) {
        
        url <- paste0(url_base, "230_mosaic-LF", year_name, "_", veg_dataset, url_base_230)
        
        dir <- paste0(dir, "LF_220/Vegetation/", veg_dataset, "/")
        
        # create file name
        zipfilename <- paste0(dir, "LF", year_name,"_220_", veg_dataset, ".zip")
        
      }
    } else if (year_name == 2001) {
      
      url <- paste0(url_base, "105-US_105_", veg_dataset, url_base_pre2016)
      
      dir <- paste0(dir, "US_105/Vegetation/", veg_dataset, "/")
      
      # create file name
      zipfilename <- paste0(dir, "US_105_", year_name,"_", veg_dataset, ".zip")
      
    } else if (year_name == 2014) {
      
      url <- paste0(url_base, "140-US_140_", veg_dataset, url_base_pre2016)
      
      dir <- paste0(dir, "US_140/Vegetation/", veg_dataset, "/")
      
      # create file name
      zipfilename <- paste0(dir, "US_105_", year_name,"_", veg_dataset, ".zip")
      
    }
      
    
    ##
    ##### Download and extract
    ##
    
    # create file name for out folder
    # filename <- gsub(".zip", "", zipfilename)
    
    # create directory if necessary 
    if(!file.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
    
    #what url are you working on?
    print(url)
    
    #download files
    options(timeout=7200) # set high number for connection timeout (default = 60 s)
    
    print("- downloading...")
    
    # repeat-tryCatch loop to catch connection errors (https://stackoverflow.com/questions/63340463/download-files-until-it-works; https://stackoverflow.com/questions/50624864/skipping-error-files-when-downloading-using-download-file-in-r)
    
    error_count = 0
    downloadcount = 0
    
    repeat{
      tryCatch({download.file(url, zipfilename, mode = "wb", quite = FALSE)
        downloadcount <<- downloadcount + 1}, # successful download
        error = function(e){error_count <<- error_count + 1 # unsuccessful download, add to error count  
        print("Downloading did not work.")
        return(e)})
      if (downloadcount > 0){ 
        break # stop repeat loop due to successful download
      }
      
      if (error_count == maxDownload_count){
        print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
        print(paste0("Tried downloading ", maxDownload_count, " times. Please check URL, internet connection, and system."))
        print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
        break # stop repeat loop due to maximum download tries reached
      }
      print("Retrying...") # error count still less than max download count, repeat loop to retry
      Sys.sleep(0.5)
    }
    
    print(paste0("successful download (Y = 1, N = 0): ", downloadcount))
    print(paste("number of download errors: ", error_count))
    
    print("-- download complete")
    
    
    print(paste0("- extracting ", year_name, " ", veg_dataset, "..."))
    print(paste0("-- location: ", dir))
    
    #unzip
    unzip(zipfilename, exdir = dir, overwrite = TRUE)
    
    print("-- extraction complete")
    
    #remove zipped files
    print(paste0("- removing compressed (.zip) folder: ", gsub(dir, "", zipfilename)))
    file.remove(zipfilename)
    print(paste0("-- removed ", gsub(dir, "", zipfilename)))
    
    message("moving to next year...")
    print("---------------------------------------------------------------------")  
    
    
    
  }
  
  message("Download and extraction complete for ", veg_dataset)
  print(Sys.time()-ptm.vegDataStart)
  message("Moving to next vegetation data...")
  
}

message("Total time taken for Vegetation datasets download:")
print(Sys.time() - ptm_VegStart)


ptm_TopoStart <- Sys.time()

url_baseTopo <- "https://www.landfire.gov/bulk/downloadfile.php?FNAME=US_Topo_"

url_base_220 <- "_220_CONUS.zip&TYPE=landfire"

years_topo <- c("2020")

topoDatasets <- c("Asp", "Elev", "SlpD")


for (year_name in years_topo){
  
  for (topo_type in topoDatasets){
    
    ptm_topoTypeStart <- Sys.time()
    
    print("======================= === ================================================")
    print(paste0("Download and extracting ", topo_type, " data for years: ", list(years_topo)))
    print("======================= === ================================================")
    
    # For every loop, dir resets to main LF directory
    dir <- "C:/Users/abhinavshrestha/OneDrive - USDA/Documents/02_TreeMap/temp_dir/02_Landfire/"
    
    ##
    ##### Set appropriate file name
    ##
    
    if (year_name >= 2020){
      
        url <- paste0(url_baseTopo, year_name, "-LF", year_name, "_", topo_type, url_base_220)
        
        dir <- paste0(dir, "LF_220/Topo/", topo_type, "/")
        
        # create file name
        zipfilename <- paste0(dir, "LF", year_name,"_220_", topo_type, ".zip")
        
      }
    
      ##
      ##### Download and extract
      ##
      
      # create file name for out folder
      # filename <- gsub(".zip", "", zipfilename)
      
      # create directory if necessary 
      if(!file.exists(dir)) {
        dir.create(dir, recursive = TRUE)
      }
      
      #what url are you working on?
      print(url)
      
      #download files
      options(timeout=7200) # set high number for connection timeout (default = 60 s)
      
      print("- downloading...")
      
      # repeat-tryCatch loop to catch connection errors (https://stackoverflow.com/questions/63340463/download-files-until-it-works; https://stackoverflow.com/questions/50624864/skipping-error-files-when-downloading-using-download-file-in-r)
      
      error_count = 0
      downloadcount = 0
      
      repeat{
        tryCatch({download.file(url, zipfilename, mode = "wb", quite = FALSE)
          downloadcount <<- downloadcount + 1}, # successful download
          error = function(e){error_count <<- error_count + 1 # unsuccessful download, add to error count
          print("Downloading did not work.")
          return(e)})
        if (downloadcount > 0){
          break # stop repeat loop due to successful download
        }

        if (error_count == maxDownload_count){
          print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
          print(paste0("Tried downloading ", maxDownload_count, " times. Please check URL, internet connection, and system."))
          print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
          break # stop repeat loop due to maximum download tries reached
        }
        print("Retrying...") # error count still less than max download count, repeat loop to retry
        Sys.sleep(0.5)
      }
      
      print(paste0("successful download (Y = 1, N = 0): ", downloadcount))
      print(paste("number of download errors: ", error_count))
      
      print("-- download complete")
      
      
      print(paste0("- extracting ", year_name, " ", topo_type, "..."))
      print(paste0("-- location: ", dir))
      
      #unzip
      unzip(zipfilename, exdir = dir, overwrite = TRUE)
      
      print("-- extraction complete")
      
      #remove zipped files
      print(paste0("- removing compressed (.zip) folder: ", gsub(dir, "", zipfilename)))
      file.remove(zipfilename)
      print(paste0("-- removed ", gsub(dir, "", zipfilename)))
      
      message("moving to next dataset...")
      print(Sys.time() - ptm_topoTypeStart)
      print("---------------------------------------------------------------------")  
  
  
        
  }
  
  
}

Sys.time() - ptm_TopoStart


