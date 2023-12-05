# Extract reference data from FIA database

# Fields to obtain: 
# Slope
# Aspect
# Elevation
# Disturbance Type
# Disturbance Code

# if available: 
# EVC
# EVH

# TO Do: 
# - Obtain actual plot coordinates and convert to meters / LF projection here

# load libraries
library(DBI)
library(tidyverse)
library(dbplyr)
library(RSQLite)


# set location of SQLite dbs
db_path <- "//166.2.126.25/TreeMap/01_Data/04_FIA/06_FIA_DataMart/"

# set location to export ref data to
ref_export <- "//166.2.126.25/TreeMap/03_Outputs/06_Reference_Data/"

# iterate over state - save by state
# then join all together


state_name <- "UT"

# Connect to SQLite Database
con <- dbConnect(drv = RSQLite::SQLite(),
                 dbname = glue('{db_path}SQLite_FIADB_{state_name}.db') )

#inspect
summary(con)

DBI::dbListTables(con)
dbListFields(con, "PLOT")
dbListFields(con, "COND")

# get fields from plot table
plot <- tbl(con, "PLOT") %>%
  select(CN, PLOT, STATECD, UNITCD, COUNTYCD, LAT, LON, ELEV) %>%
  arrange(PLOT) %>%
  data.frame()

# get fields from condition table
cond <- tbl(con, "COND") %>%
  select(CN, PLT_CN, PLOT, ASPECT, SLOPE, DSTRBCD1, DSTRBYR1, # initial fields
         LIVE_CANOPY_CVR_PCT) %>% # canopy cover and canopy height
  rename("COND_CN" = CN) %>%
  arrange(PLOT) %>%
  data.frame()

# filter to single-condition plots

# join
t <- inner_join(plot, cond, by = c("CN" = "PLT_CN", "PLOT" = "PLOT")) %>%
  arrange(PLOT, STATECD, UNITCD, COUNTYCD)

dbDisconnect(con) # closes our DB connection



# save
write.csv(t, glue('{ref_export}{state_name}_FIAextract.csv'))
