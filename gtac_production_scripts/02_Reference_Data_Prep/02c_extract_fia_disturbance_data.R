library(RSQLite)
library(sqldf)
library(DBI)
library(data.table)
library(dplyr)


options(scipen = 999)
## Set working directory as the master folder with all FIA SQLite databases within their 
## individual folders. Named: FIADBs.
setwd("C:/Users/User/FIA/SQLite_DBs/State_Databases/")

# This file lists all unique stand identifiers (Plot_CN) values for the plots,
# filter stand list for stands that have single condition plots first.
### ENTER CORRECT STAND TABLE PATHWAY HERE ###
target_stands <- as.data.table(read.table("F:/LANDFIRE_Autokey/FIA_DataTables/LF2020_EVT_GP_byZone_Workaround2_Final.csv", sep = ",", header = TRUE))

target_stands <- unique(target_stands[,"PLT_CN"])
target_stands[] <- lapply(target_stands, as.character)

#Identify stands with multiple conditions here.
#con = dbConnect(RSQLite::SQLite(), dbname="C:/Users/rachelhoutman/FIA/SQLite_FIADB_ENTIRE/SQLite_FIADB_ENTIRE.db")
#cond <- as.data.table(dbGetQuery(con, 'select * from COND'))


states <- list.files()
states <- grep("SQLite", states, value = TRUE)


# This loop iterates through every state database and extracts the tree data from FIA
reference_data <- NULL
for(db in 1:length(states)){
  # connect to the sqlite file
  
  con = dbConnect(RSQLite::SQLite(), dbname=states[db])
  # get a list of all tables
  alltables = dbListTables(con)
  alltables
  
  #cond_all <- as.data.table(dbGetQuery(con, 'select * from COND'))
  cond <- as.data.table(dbGetQuery(con, 'select PLT_CN, SLOPE, ASPECT, DSTRBCD1, DSTRBYR1 , DSTRBCD2, DSTRBYR2, DSTRBCD3, DSTRBYR3, INVYR from COND'))
  cond <- cond[PLT_CN %in% target_stands$PLT_CN,]
  plot <- as.data.table(dbGetQuery(con, 'select CN, ELEV, MEASYEAR from PLOT'))
  plot <- plot[CN %in% target_stands$PLT_CN,]
  
  state_data <- left_join(cond, plot, join_by(PLT_CN == CN) )
  
  reference_data <- rbind(reference_data, state_data)
  print(paste0("Completed ", states[db], " "))
  dbDisconnect(con)
}

plots <- length(unique(reference_data$PLT_CN))

# For disturbance, include only fire and insect/disease. Fire wins over insect and disease, and the most recent fire year
# is recorded as the disturbance year

disturbance_data <- reference_data %>% 
  mutate(
    disturb_code = case_when(DSTRBCD1 %in% c(29, 30, 31) |
                          DSTRBCD2 %in% c(29, 30, 31) | 
                          DSTRBCD3 %in% c(29, 30, 31) ~ 1, ## Fire disturbance
                          !DSTRBCD1 %in% c(29, 30, 31) &
                          !DSTRBCD2 %in% c(29, 30, 31) & 
                          !DSTRBCD3 %in% c(29, 30, 31) & 
                          DSTRBCD1 %in% c(11, 12, 20, 21, 22) |
                          DSTRBCD2 %in% c(11, 12, 20, 21, 22) | 
                          DSTRBCD3 %in% c(11, 12, 20, 21, 22) ~ 2,
                          TRUE ~ 0), ## Insect and disease disturbance
    fire_time1 = case_when(DSTRBCD1 %in% c(29, 30, 31) ~ MEASYEAR - DSTRBYR1,
                           TRUE ~ 99),
    fire_time2 = case_when(DSTRBCD2 %in% c(29, 30, 31) ~ MEASYEAR - DSTRBYR2,
                           TRUE ~ 99),
    fire_time3 = case_when(DSTRBCD3 %in% c(29, 30, 31) ~ MEASYEAR - DSTRBYR3,
                           TRUE ~ 99),
    insect_time1 = case_when(DSTRBCD1 %in% c(11, 12, 20, 21, 22) ~ MEASYEAR - DSTRBYR1,
                           TRUE ~ 99),
    insect_time2 = case_when(DSTRBCD2 %in% c(11, 12, 20, 21, 22) ~ MEASYEAR - DSTRBYR2,
                           TRUE ~ 99),
    insect_time3 = case_when(DSTRBCD3 %in% c(11, 12, 20, 21, 22) ~ MEASYEAR - DSTRBYR3,
                           TRUE ~ 99),
    disturb_year = case_when(disturb_code == 1 ~ pmax(pmin(fire_time1, fire_time2, fire_time3, rm.na = TRUE),0, rm.na = TRUE),
                                 disturb_code == 2 ~ pmax(pmin(insect_time1, insect_time2, insect_time3, rm.na = TRUE),0, rm.na = TRUE),
                                 disturb_code == 0 ~ 99)) %>%
  select(-c(DSTRBCD1, DSTRBCD2, DSTRBCD3, DSTRBYR1, DSTRBYR2, DSTRBYR3, fire_time1, fire_time2, fire_time3,
            insect_time1, insect_time2, insect_time3, INVYR, MEASYEAR))

disturbance_data

write.csv(disturbance_data,"F:/TreeMap2020/XTable/FIA2020_Disturbance.csv")

