### PLACEHOLDER script
# will be used to download and organize Landfire Veg (EVC, EVT, EVH) and Topographic (SLOPE, ELEV) layers

# Download and unzip all Landfire disturbance files
# Available for manual download from this location: 
# https://www.landfire.gov/version_download.php

# Written by Lila Leatherman (Lila.Leatherman@usda.gov) and Abhinav Shrestha (abhinav.shrestha@usda.gov) 
# Last Updated: 01/04/2024

# TO DO: 
# - 

#####################################
# Set Inputs
#######################################

# set file destination
#dir <- "//166.2.126.25/TreeMap/01_Data/02_Landfire/LF_220/Disturbance/"

# For testing
dir <- "C:/Users/abhinavshrestha/OneDrive - USDA/Documents/02_TreeMap/temp_dir/"

# list years
years <- 2018:2020

# list years

years_datasetDict <- c("2016" = "200",
                       "2020" = "220", 
                       "2022" = "230")

vegetation_datasets <- c("EVC", "EVT", "EVH")

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
# 1999-2014: US_DIST1999
# 2015-2016: LF2015_Dist_200
# 2017-2020: LF2020_Dist_220

# sample download url pre 2020 "https://landfire.gov/bulk/downloadfile.php?FNAME=US_Disturbance-US_DIST1999.zip&TYPE=landfire
# sample download url 2015 "https://landfire.gov/bulk/downloadfile.php?FNAME=US_Disturbance-LF2020_Dist_220_CONUS.zip&TYPE=landfire"
url_base <-  "https://www.landfire.gov/bulk/downloadfile.php?FNAME=US_"

for (i in 1:length(vegetation_datasets)){
  
  print(paste0("Download and extracting data for ", vegetation_datasets[i], " dataset."))
  ptm.vegDataStart <- Sys.time()
  
  for(j in 1:length(years_datasetDict)){
    
    #j = 1
    
    year_name <- c(names(years_datasetDict))[j]
    
    print(paste0("downloading ", year_name))
    
    ##
    ##### Set appropriate file name
    ##
    
    #create url
    url <- paste0(url_base, years_datasetDict[[year_name]], "_mosaic-LF", year_name, "_", vegetation_datasets[i], "_", years_datasetDict[[year_name]], "_CONUS.zip&TYPE=landfire")
      
    # create file name
    zipfilename <- paste0(dir, gsub("https://www.landfire.gov/bulk/downloadfile.php|\\?|FNAME=US_200_mosaic-|&TYPE=landfire", "", url)) # "?" in URLs create issue while using `gsub`. Use "\\" to escape and "|" to pipe different string segments
    
      
    
    ##
    ##### Download and extract
    ##
    
    # create file name for out folder
    # filename <- gsub(".zip", "", zipfilename)
    
    #what url are you working on? 
    print(url)
    
    #download files
    options(timeout=7200) # set high number for connection timeout (default = 60 s)
    
    download.file(url, zipfilename, mode = "wb")
    
    print(paste0("extracting ", year_name))
    
    #unzip
    unzip(zipfilename, exdir = dir, overwrite = TRUE)
    
    print(paste("Extracted! File name:", gsub(".zip", "", zipfilename)))
    
    #remove zipped files
    print("removing compressed (.zip) folder")
    file.remove(zipfilename)
    
    
  }
  
  message("Download and extraction complete!")
  print(Sys.time()-ptm.vegDataStart)
  
}

message("Total time taken for Vegetation datasets download:")
print(Sys.time() - ptm.start)