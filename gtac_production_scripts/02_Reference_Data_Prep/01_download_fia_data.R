# Download PLOT, COND, and TREE tables from FIA database, by State
# Save tables to specified drive as .csv
# Download SQLite databases also

# Written by Lila Leatherman (lila.leatherman @usda.gov)

# Last updated: 4/15/25

##########################################################


# inputs
# data format - options are "CSV" and "SQLite"
data_format = "SQLite"

# states to download - either 'all', or list of state abbreviations, eg c("CA")

states = "all"
#states = c("WY")


####################################################################
#ptm_Start <- Sys.time() # processing time (ptm) calc start point

# Id where script is located
this.path <- this.path::this.path()

# load library 
this_proj = this.path::this.proj()
lib_path = glue::glue("{this_proj}/gtac_production_scripts/00_Library/treeMapLib.R")
source(lib_path)

# data directory - where source data are located
data_dir <- glue::glue('{home_dir}/01_Data/')

# set file destination - will be created if it does not exist
dir <- glue::glue("{home_dir}/01_Data/04_FIA/05_FIA_DataMart/")


# list states - lower 48 states by abbreviation

# For testing
# states <- c("ID", "UT", "WY") 
# states <- c("NV", "CO", "MT") 

if(states == "all") {
  states <- c("AL", "AR", "AZ", "CA", "CO", "CT",
            "DE", "FL", "GA", "ID", "IL", "IN",
            "IA", "KS", "KY", "LA", "ME", "MD",
            "MA", "MI", "MN", "MS", "MO", "MT",
            "NE", "NV", "NH", "NJ", "NM", "NY",
            "NC", "ND", "OH", "OK", "OR", "PA",
            "RI", "SD", "SC", "TN", "TX", "UT",
            "VA", "VT", "WV", "WI", "WY", 'WA') # CONUS
} else(states = states)

# list tables to download - for downloading csvs
tables <- c("COND", "PLOT", "TREE")

# Set maximum download tries
maxDownload_count <- 5

if(data_format == "CSV"){
  
  # sample url https://apps.fs.usda.gov/fia/datamart/Databases/SQLite_FIADB_CA.zip
  url_base <- "https://apps.fs.usda.gov/fia/datamart/CSV/"
  dir <- glue::glue('{dir}/CSV/') 
} else if(data_format == "SQLite") {
  
  
  url_base <- "https://apps.fs.usda.gov/fia/datamart/Databases/"
  dir <- glue::glue('{dir}/SQLite/') 
} else {
  message("enter a valid data format! valid formats are 'CSV' and 'SQLite'")
}


# create export dir
if (!file.exists(dir)) {
  dir.create(dir)
}

# set longer time to permit download - in seconds
options(timeout = 7200)

# Download FIA data by state
###########################################################

for (state_name in states){

  #j = 1 # for testing
  
  ptm_State <- Sys.time()

  #state_name <- states[j]
  
  if(data_format == 'CSV'){
    

    for(k in seq_along(tables)){
  
      #k = 1 # for testing
  
      tbl <- tables[k]
  
      #create url
      url <- glue::glue("{url_base}{state_name}_{tbl}.csv")
  
      # create file name
      #filename <- glue::glue('{dir}{strsplit(url, "/")[[1]][7]}')
      
      print("---------------------------------------------------------------------")
      print(paste0("Downloading ", state_name, "-", tbl))
      print("---------------------------------------------------------------------")
  
      
      #what url are you working on? 
      #print(url)
      #print(filename)
      
      # download attempt
      download_attempt(url, dir, maxDownload_count)
      }
    } else if(data_format == "SQLite") {
     
    #sample url https://apps.fs.usda.gov/fia/datamart/Databases/SQLite_FIADB_CA.zip
      
    url <- glue::glue("{url_base}/SQLite_FIADB_{state_name}.zip")
      
    # create file name
    #filename <- glue::glue('{dir}{strsplit(url, "/")[[1]][8]}')
    
    #what url are you working on? 
    #print(url)
    #print(filename)
    
    print("---------------------------------------------------------------------")
    print(paste0("Downloading ", state_name, " SQLite Database"))
    print("---------------------------------------------------------------------")
    
    # download attempt 
    download_attempt(url, 
                     dir, 
                     maxDownload_count,
                     unzip = TRUE)
      
      
      
    }
  
  print(Sys.time()-ptm_State)
  message(paste0("Done with ", state_name, ". Moving to next..."))


}

print("Done with downloading all FIA data.")
print(Sys.time()-ptm_Start)