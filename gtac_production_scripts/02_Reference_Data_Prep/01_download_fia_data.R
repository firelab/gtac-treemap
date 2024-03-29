# Download PLOT, COND, and TREE tables from FIA database, by State
# Save tables to specified drive as .csv

# Written by Lila Leatherman (lila.leatherman @usda.gov)

# Last updated: 2/21/24

##########################################################

# alternately, is there a remote sqlite instance I can connect to?

# set home dir
home_dir <- "//166.2.126.25/TreeMap/"

# set file destination - will be created if it does not eists
dir <- glue::glue("{home_dir}TreeMap/01_Data/04_FIA/05_FIA_DataMart/CSV/")

# list states - lower 48 states by abbreviation
# states <- c("ID", "UT", "WY")
states <- c("NV", "CO", "MT")
# states <- c("AL", "AR", "AZ", "CA", "CO", "CT", 
#             "DE", "FL", "GA", "ID", "IL", "IN", 
#             "IA", "KS", "KY", "LA", "ME", "MD",
#             "MA", "MI", "MN", "MS", "MO", "MT",   
#             "NE", "NV", "NH", "NJ", "NM", "NY", 
#             "NC", "ND", "OH", "OK", "OR", "PA", 
#             "RI", "SD", "SC", "TN", "TX", "UT", 
#             "VA", "VT", "WV", "WI", "WY", 'WA')

# list tables to download
tables <- c("COND", "PLOT", "TREE")

# sample url https://apps.fs.usda.gov/fia/datamart/Databases/SQLite_FIADB_CA.zip
url_base <- "https://apps.fs.usda.gov/fia/datamart/CSV/"

# create export dir
if (!exists(dir)) {
  dir.create(dir)
}

# set longer time to permit download - in seconds
options(timeout = 120)

# Download FIA data by state
###########################################################

for (j in seq_along(states)){

  #j = 1 # for testing

  state_name <- states[j]

  print(paste0("downloading ", state_name))

  for(k in seq_along(tables)){

    #k = 1 # for testing

    tbl <- tables[k]

    #create url
    url <- glue("{url_base}{state_name}_{tbl}.csv")

    # create file name
    filename <- glue('{dir}{strsplit(url, "/")[[1]][7]}')

    #what url are you working on? 
    print(url)

    #download files
    download.file(url, filename, mode = "wb")

  }


}
