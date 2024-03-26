# Download and unzip all Landfire disturbance files
# Available for manual download from this location: 
# https://landfire.gov/disturbance_grids.php

# Written by Lila Leatherman (Lila.Leatherman@usda.gov)
# Last Updated: 

# TO DO: 
# - remove zipped files
# - copy unzipped folders to next folder up, with the same name 

#####################################
# Set Inputs
#######################################

# set file destination
dir <- "//166.2.126.25/TreeMap/01_Data/02_Landfire/LF_220/Disturbance/"

# list years
years <- 2018:2020



################################################
# Run
################################################

# create directory if necessary 
if(!file_exists(dir)) {
  dir.create(dir, recursive = TRUE)
}

# File name format, by years: 
# ---------------------------------------#
# 1999-2014: US_DIST1999
# 2015-2016: LF2015_Dist_200
# 2017-2020: LF2020_Dist_220

# sample download url pre 2020 "https://landfire.gov/bulk/downloadfile.php?FNAME=US_Disturbance-US_DIST1999.zip&TYPE=landfire
# sample download url 2015 "https://landfire.gov/bulk/downloadfile.php?FNAME=US_Disturbance-LF2020_Dist_220_CONUS.zip&TYPE=landfire"
url_base_1 <- "https://landfire.gov/bulk/downloadfile.php?FNAME=US_Disturbance-US_DIST"
url_base_2 <- "https://landfire.gov/bulk/downloadfile.php?FNAME=US_Disturbance-LF"
url_base_2_2016 <- "_Dist_200_CONUS.zip&TYPE=landfire"
url_base_2_2020 <- "_Dist_220_CONUS.zip&TYPE=landfire"

  for(j in 1:length(years)){
    
    #j = 1
    
    year_name <- years[j]
    
    print(paste0("downloading ", year_name))
    
    ##
    ##### Set appropriate file name
    ##
    
    if(year_name < 2015){
      
      #create url
      url <- paste0(url_base_1, year_name, ".zip&TYPE=landfire")
      
      # create file name
      zipfilename <- paste0(dir, "US_DIST", year_name, ".zip")
      
    } else if(year_name >= 2015 & year_name <= 2016) {
      
      #create url
      url <- paste0(url_base_2, year_name, url_base_2_2016)
      
      # create file name
      zipfilename <- paste0(dir, "LF", year_name, gsub("&TYPE=landfire", "", url_base_2_2016))
      
    } else if(year_name > 2016) {
      
      #create url
      url <- paste0(url_base_2, year_name, url_base_2_2020)
      
      # create file name
      zipfilename <- paste0(dir, "LF", gsub("&TYPE=landfire", "", url_base_2_2020))
      
    }
    
    ##
    ##### Download and extract
    ##
    
    # create file name for out folder
    filename <- gsub(".zip", "", zipfilename)
    
    #what url are you working on? 
    print(url)
    
    #download files
    download.file(url, zipfilename, mode = "wb")
    
    print(paste0("extracting ", year_name))
    
    #unzip
    unzip(zipfilename, exdir = dir, overwrite = TRUE)
    
    #remove zipped files
    
      
  }
  

