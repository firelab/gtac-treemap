# Download and unzip all Landfire disturbance files
# Available for manual download from this location: 
# https://landfire.gov/disturbance_grids.php

# Written by Lila Leatherman (Lila.Leatherman@usda.gov)
# Last Updated: 01/04/2024 (Abhinav Shrestha; abhinav.shrestha@usda.gov; AS)

# NEW (05/08/2024; AS):
# - Scripts download data from 1999:2022 (previously 2016:2022). Automatically creates directory for each Landfire version of the dataset. Where:
# -- 1999-2014: US_DIST<year>
# -- 2015-2016: LF<year>_Dist_200
# -- 2017-2020: LF<year>_Dist_220
# -- 2021-2022: LF<year>_Dist_230
# - added a repeat-tryCatch loop that allows for multiple download tries if there is a connection or download error from the host or the local machine. This number of download tries is controlled by the `maxDownload_count` variable. 

#####################################
# Set Inputs
#######################################

# set file destination
dir <- glue::glue("{data_dir}02_Landfire") # `00_inputs_for_targetdata.R` required to run

# dir <- "your-data-path-here" # for testing

# list years
years <- start_year:end_year # LF-200 version onwards

# set maximum tries to download
maxDownload_count <- 5

################################################
# Run
################################################

ptm.start <- Sys.time() # processing time (ptm): start

# create "02_Landfire" directory if necessary 
if(!file.exists(dir)) {
  dir.create(dir, recursive = TRUE)
}

# File name format, by years: 
# ---------------------------------------#
# 1999-2014: US_DIST<year>
# 2015-2016: LF<year>_Dist_200
# 2017-2020: LF<year>_Dist_220
# 2021-2022: LF<year>_Dist_230

# sample download url pre 2015 (1999-2014): for 1999 - "https://landfire.gov/bulk/downloadfile.php?FNAME=US_Disturbance-US_DIST1999.zip&TYPE=landfire
# for 2014 - https://www.landfire.gov/bulk/downloadfile.php?FNAME=US_Disturbance-US_DIST2014.zip&TYPE=landfire
# sample download url 2015 "https://landfire.gov/bulk/downloadfile.php?FNAME=US_Disturbance-LF2020_Dist_220_CONUS.zip&TYPE=landfire"

url_base_1 <- "https://landfire.gov/bulk/downloadfile.php?FNAME=US_Disturbance-US_DIST" # 2019-2014
url_base_2 <- "https://landfire.gov/bulk/downloadfile.php?FNAME=US_Disturbance-LF" # 2014-present
url_base_2_200 <- "_Dist_200_CONUS.zip&TYPE=landfire"
url_base_2_220 <- "_Dist_220_CONUS.zip&TYPE=landfire"
url_base_2_230 <- "_Dist_230_CONUS.zip&TYPE=landfire"

for(j in 1:length(years)){
    
  #j = 1
  
  year_name <- years[j]
  
  print(paste0("downloading ", year_name))
  
  # For every loop, dir resets to main LF directory "02_Landfire"
  
  dir <- glue::glue("{data_dir}02_Landfire") #`00_inputs_for_targetdata.R` required to run
  # dir <- "your-data-path-here" # for testing
  
  ##
  ##### Set appropriate directory according to dataset
  ##### Set appropriate file name according to year and LF dataset
  ##
  
  if(year_name < 2015){
    
    #create url
    url <- paste0(url_base_1, year_name, ".zip&TYPE=landfire")
    
    dir <- paste0(dir, "LF_DIST1999_DIST2014/Disturbance/")
    
    # create file name
    zipfilename <- paste0(dir, "US_DIST", year_name, ".zip")
    
  } else if(year_name >= 2015 & year_name <= 2016) {
    
    #create url
    url <- paste0(url_base_2, year_name, url_base_2_200)
    
    dir <- paste0(dir, "LF_200/Disturbance/")
    
    # create file name
    zipfilename <- paste0(dir, "LF", year_name, gsub("&TYPE=landfire", "", url_base_2_200))
    
  } else if(year_name > 2016 & year_name <= 2020) {
    
    #create url
    url <- paste0(url_base_2, year_name, url_base_2_220)
    
    dir <- paste0(dir, "LF_220/Disturbance/")
    
    # create file name
    zipfilename <- paste0(dir, "LF", year_name, gsub("&TYPE=landfire", "", url_base_2_220))
    
    
  } else if(year_name > 2020){
    
    #create url
    url <- paste0(url_base_2, year_name, url_base_2_230)
    
    dir <- paste0(dir, "LF_230/Disturbance/")
    
    # create file name
    zipfilename <- paste0(dir, "LF", year_name, gsub("&TYPE=landfire", "", url_base_2_230))
    
    
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
    
    
    print(paste0("- extracting ", year_name, "..."))
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

message(paste0("Process complete! Years downloaded: ", years[1], "–", years[length(years)]))
message(print(Sys.time() - ptm.start))
