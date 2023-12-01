# download FIA data
library(glue)

# Download and unzip all Landfire disturbance files
# Available for manual download from this location: 
# https://landfire.gov/disturbance_grids.php

# set longer time to permit download - in seconds
options(timeout=120)

# set file destination
dir <- "//166.2.126.25/TreeMap/01_Data/04_FIA/06_FIA_DataMart/"

# list states - by abbreviation
states <- c("UT")
# states <- c("AL", "AR", "AZ", "CA", "CO", "CT", 
#             "DE", "FL", "GA", "ID", "IL", "IN", 
#             "IA", "KS", "KY", "LA", "ME", "MD",
#             "MA", "MI", "MN", "MS", "MO", "MT",   
#             "NE", "NV", "NH", "NJ", "NM", "NY", 
#             "NC", "ND", "OH", "OK", "OR", "PA", 
#             "RI", "SD", "SC", "TN", "TX", "UT", 
#             "VA", "VT", "WV", "WI", "WY", 'WA')

# sample download url https://apps.fs.usda.gov/fia/datamart/Databases/SQLite_FIADB_CA.zip

url_base <- "https://apps.fs.usda.gov/fia/datamart/Databases/SQLite_FIADB_"


for(j in 1:length(states)){
  
  j = 1
  
  state_name <- states[j]
  
  print(paste0("downloading ", state_name))
  
  ##
  ##### Set appropriate file name
  ##
  
  #create url
  url <- glue('{url_base}{state_name}.zip')
    
  # create file name
  zipfilename <- glue('{dir}{strsplit(url, "/")[[1]][7]}')
    
  
  ##
  ##### Download and extract
  ##
  
  # create file name for out folder
  filename <- gsub(".zip", "", zipfilename)
  
  #what url are you working on? 
  print(url)
  
  #download files
  download.file(url, zipfilename, mode = "wb")
  
  print(paste0("extracting ", state_name))
  
  #unzip
  unzip(zipfilename, exdir = dir, overwrite = TRUE)
  
  #remove zipped files
  file.remove(zipfilename)
  
}
