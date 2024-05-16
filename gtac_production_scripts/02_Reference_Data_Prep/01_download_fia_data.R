# Download PLOT, COND, and TREE tables from FIA database, by State
# Save tables to specified drive as .csv

# Written by Lila Leatherman (lila.leatherman @usda.gov)

# Last updated: 05/08/24

##########################################################

ptm_Start <- Sys.time() # processing time (ptm) calc start point

# alternately, is there a remote sqlite instance I can connect to?

# set home dir
home_dir <- "//166.2.126.25/TreeMap/"


# set file destination - will be created if it does not eists
dir <- glue::glue("{home_dir}TreeMap/01_Data/04_FIA/05_FIA_DataMart/CSV/")

# For testing:
# dir <- "C:/Users/abhinavshrestha/OneDrive - USDA/Documents/02_TreeMap/temp_dir/04_FIA/05_FIA_DataMart/CSV/"


# list states - lower 48 states by abbreviation

# For testing
# states <- c("ID", "UT", "WY") 
# states <- c("NV", "CO", "MT") 


states <- c("AL", "AR", "AZ", "CA", "CO", "CT",
            "DE", "FL", "GA", "ID", "IL", "IN",
            "IA", "KS", "KY", "LA", "ME", "MD",
            "MA", "MI", "MN", "MS", "MO", "MT",
            "NE", "NV", "NH", "NJ", "NM", "NY",
            "NC", "ND", "OH", "OK", "OR", "PA",
            "RI", "SD", "SC", "TN", "TX", "UT",
            "VA", "VT", "WV", "WI", "WY", 'WA') # CONUS

# list tables to download
tables <- c("COND", "PLOT", "TREE")

# Set maximum download tries
maxDownload_count <- 5

# sample url https://apps.fs.usda.gov/fia/datamart/Databases/SQLite_FIADB_CA.zip
url_base <- "https://apps.fs.usda.gov/fia/datamart/CSV/"

# create export dir
if (!file.exists(dir)) {
  dir.create(dir)
}

# set longer time to permit download - in seconds
options(timeout = 7200)

# Download FIA data by state
###########################################################

for (j in seq_along(states)){

  #j = 1 # for testing
  
  ptm_State <- Sys.time()

  state_name <- states[j]

  for(k in seq_along(tables)){

    #k = 1 # for testing

    tbl <- tables[k]

    #create url
    url <- glue::glue("{url_base}{state_name}_{tbl}.csv")

    # create file name
    filename <- glue::glue('{dir}{strsplit(url, "/")[[1]][7]}')
    
    print("---------------------------------------------------------------------")
    print(paste0("Downloading ", state_name, "-", tbl))
    print("---------------------------------------------------------------------")

    #what url are you working on? 
    print(url)
    print(filename)

    #download files
    # download.file(url, filename, mode = "wb") # TODO: old code, delete after testing
    
    # repeat-tryCatch loop to catch connection errors (https://stackoverflow.com/questions/63340463/download-files-until-it-works; https://stackoverflow.com/questions/50624864/skipping-error-files-when-downloading-using-download-file-in-r)
    
    error_count = 0
    downloadcount = 0
    
    repeat{
      tryCatch({download.file(url, filename, mode = "wb", quite = FALSE)
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
    
    print("Download complete!")
    
  }
  
  print(Sys.time()-ptm_State)
  message(paste0("Done with ", state_name, ". Moving to next..."))
  
 

}

print("Done with downloading all FIA data.")
print(Sys.time()-ptm_Start)