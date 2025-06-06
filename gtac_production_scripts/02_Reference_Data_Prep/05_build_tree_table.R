library(RSQLite)
library(sqldf)
library(DBI)
library(data.table)
library(dplyr)

setwd("F:/TreeMap2022/")
options(scipen = 999)

# This file lists all unique stand identifiers (Plot_CN) values for the study area
### ENTER CORRECT STAND TABLE PATHWAY HERE ###
target_stands <- as.data.table(read.table("F:/TreeMap2022/TM_2022_Plot_Table2.csv", sep = ",", header = TRUE))

#tree_header <- c("TM_ID", "TM_PLT_CN", "INVYR", "STATUSCD", "Tree_count", "SPCD", "DBH", "HT", "ACTUALHT", "CR", "SUBP",
#                 "TREE", "AGENTCD", "Species", "History", "CrRatio")

#tree_header <- c("tm_id","CN","STATUSCD","TPA_UNADJ","SPCD","COMMON_NAME","SCIENTIFIC_NAME","SPECIES_SYMBOL","DIA","HT","ACTUALHT","CR","SUBP","TREE","AGENTCD")

tree_header <- c("TM_ID","PLT_CN","STATUSCD","TPA_UNADJ","SPCD","COMMON_NAME","SCIENTIFIC_NAME","SPECIES_SYMBOL","DIA","HT","ACTUALHT","CR","SUBP","TREE","AGENTCD")

## CREATE THE TREE TABLE ##
# Extract the tree data from the FIA database

con = dbConnect(RSQLite::SQLite(), dbname="C:/Users/User/FIA/SQLite_DBs/SQLite_FIADB_ENTIRE.db")
# get a list of all tables
alltables = dbListTables(con)
alltables

tree <- as.data.table(dbGetQuery(con, 'select PLT_CN, STATUSCD, TPA_UNADJ, SPCD, DIA, HT, ACTUALHT, CR, SUBP, TREE, AGENTCD, STANDING_DEAD_CD from TREE'))
target_stands$TM_PLT_CN <- as.character(target_stands$PLT_CN)
tree_filtered <- left_join(target_stands, tree, join_by(TM_PLT_CN == PLT_CN), relationship = "many-to-many")
# Remove trees with STATUSCD==0, not used in sample.
tree_filtered <- tree_filtered[STATUSCD != 0 & !is.na(TPA_UNADJ),]

# This creates an FVS variable, but also identifies trees that are no longer present on the plot and removes them from the treelist
tree_filtered <- tree_filtered %>% 
  mutate(History = case_when(
    STATUSCD == 1 ~ 1,
    STATUSCD == 2 & AGENTCD > 0 & (STANDING_DEAD_CD == 0 | STANDING_DEAD_CD == 1 | is.na(STANDING_DEAD_CD)) ~ 6,
    STATUSCD == 3 | (STATUSCD ==2 & is.na(AGENTCD) & (STANDING_DEAD_CD == 1 | is.na(STANDING_DEAD_CD))) ~ 8,
    .default = 0))

## Confirm that all trees with History == 0 are for records with STANDING_DEAD_CD == 0 (does not qualify as standing dead) and 
## STATUSCD == 2 and AGENTCD is not recorded.
trees_to_remove <- tree_filtered[is.na(AGENTCD) & STATUSCD == 2 & STANDING_DEAD_CD == 0,]
trees_history0 <- tree_filtered[History==0,]

tree_filtered <- tree_filtered[History != 0,] %>%
  select(-STANDING_DEAD_CD)

tree_characteristics = c("TM_ID", "PLT_CN", "STATUSCD", "TPA_UNADJ", "SPCD", "DIA", "HT", "ACTUALHT", "CR", "SUBP", "TREE", "AGENTCD")
tree_table <- tree_filtered[, tree_characteristics, with=FALSE] #%>% 
  #rename(tm_id = TM_ID, CN = PLT_CN)

species_names <- as.data.table(dbGetQuery(con, 'select * from REF_SPECIES'))
species_filtered <- left_join(tree_table, species_names[,c("SPCD", "COMMON_NAME","SCIENTIFIC_NAME","SPECIES_SYMBOL"),], by = "SPCD")

tree_table <- species_filtered %>%
  setcolorder(tree_header) 

## End Standard Code ##
# Remove NA values when writing files (FVS does NOT like them)
### UPDATE YOUR OUTPUT FILE NAMES HERE ###
write.csv(tree_table, file = "TreeMap2022_Tree_Table.csv", na = "", row.names = FALSE)
dbDisconnect(con)

# Find trees in 2016
tm2016 <- as.data.table(read.csv("F:/TreeMap2016/Data/TreeMap2016_tree_table.csv"))

like_trees <- tree_table[TM_PLT_CN %in% tm2016$CN,]
