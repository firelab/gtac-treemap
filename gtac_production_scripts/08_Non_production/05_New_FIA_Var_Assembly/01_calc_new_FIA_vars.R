# Calculate attributes from FIA variables that were not included in the original TreeMap outputs

# This script does the following: 
# Load TreeMap Raster Attribute Table (RAT) with TM_ID and PLT_CN
# Load FIA data for plots corresponding to TreeMap PLT_CN
# Calculate sound volume variables 
# Join calculated variables back to RAT
# Export updated RAT with new variables
# (Use updated RAT in subsequent processing steps to create rasters of new attributes)


######################################################
# SET INPUTS
######################################################

# Specific inputs
#-----------------------------------------------------#

# pick a TreeMap vintage to use for assembly 
tm_year = 2022

# FIA years to include in calculations
start_year = 1999
end_year = tm_year

# set input paths. all these are relative to home_dir set in setup_dirs.R
input_paths = c(
  # export location
  export_dir = "02_Outputs/01_Attribute_calcs/",
  rat_orig_path = "01_Inputs/01_TreeMap_Data/2022/TM2022_RAT_tmid.csv"

)

overwrite_rat = TRUE

#fia_path = "05_FIA_DataMart/SQLite/SQLite_FIADB_States/SQLite_FIADB_WA.db"
fia_path = "05_FIA_DataMart/SQLite/SQLite_FIADB_ENTIRE/SQLite_FIADB_ENTIRE.db"

# write sql query for initial data pull
# make sure you get all FIA tables and fields necessary here
# can also check syntax by writing as a .sql file
sql_query_init = 'select TREE.PLT_CN, TREE.DIA, TREE.CN as "TREE_CN", TPA_UNADJ, STATUSCD, TREECLCD, STANDING_DEAD_CD, VOLCFSND, VOLCFNET, FORTYPCD, COND.INVYR, COND.COND_STATUS_CD
from COND
INNER JOIN TREE ON COND.PLT_CN = TREE.PLT_CN'

# placeholders for variables to be filled in via glue_sql
sql_query_where = 'WHERE COND.INVYR >= {start_year} AND COND.INVYR <= {end_year} AND TREE.PLT_CN in ({id_list*})'

# csv of forest types for species and groups, with codes
forest_types_species_to_group = read.csv("data/forest_types_species_to_group.csv")

# Run set up 
#-------------------------------------------------------#
# Allow for sufficient digits to differentiate plot cn numbers
options("scipen"=100, "digits"=8)

library_list = c(
"glue",
"RSQLite",
"DBI",
"tidyverse",
"magrittr"
)

# load libraries
invisible(lapply(library_list, function(x) library(x, character.only = TRUE)))

## load setup functions
this_proj <- this.path::this.proj()
this_dir <- this.path::this.dir()
setup_path = glue::glue('{this_proj}/functions/setup_functions.R')
source(setup_path)

# append home_dir to input paths
input_paths = lapply(input_paths, function(x) glue::glue("{home_dir}{x}"))

# append fia_dir to fia_path
fia_path = glue::glue("{FIA_dir}{fia_path}")

# Load and prep Raster Attribute Table
#-----------------------------------------------------------------#

rat <- load_RAT(input_paths[['rat_orig_path']],
                CN_column = "PLT_CN",
                ID_column = "TM_ID"
)

# get list of ids to limit sql query of database
id_list = rat$CN

# inspect
#str(rat)

# connect to FIA db
db_path <- fia_path
#print(file.exists(db_path)) # Should return TRUE if the file exists

con = dbConnect(RSQLite::SQLite(), db_path)

# get a list of all tables
# alltables = dbListTables(con)
# alltables

# # list fields in table 
# dbListFields(con, "PLOT")
# dbListFields(con, "TREE")
# dbListFields(con, "COND")


sql_query = glue::glue('{sql_query_init} {sql_query_where}')

# write to query.sql file for inspection
writeLines(sql_query, con = "query.sql")

query <- glue::glue_sql(  sql_query,  .con = con )

message("final sql query, before glue_sql: ")
print(sql_query)


# Execute the query, passing R variables in the 'params' argument
message("running query (this may take a while on the national FIADB)....")
result_df <- dbGetQuery(con, query) 

result_df %<>%   
  replace(is.na(.), 0) %>%
  dplyr::group_by(PLT_CN) %>%
  arrange(PLT_CN)

# message("result df looks like this:")
# print(head(result_df))
# print(str(result_df))

# disconnect from db
dbDisconnect(con)

# Calculate variables
#------------------------------------------------------------------------------------#

message("Calculating variables...")

# VOLCFNET_D: Volume, Standing Dead (ft3/acre)
# Volume, standing dead, cubic feet per acre. Calculated via the following FIA query:
#   Query; Sum VOLCFNET*TPA_UNADJ WHERE (((COND.COND_STATUS_CD)=1) AND ((TREE.STATUSCD)=2) AND ((TREE.DIA)>=5) AND ((TREE.STANDING_DEAD_CD)=1))

VOLCFNET_D <- result_df %>%
  dplyr::filter(COND_STATUS_CD == 1 &
                  STATUSCD == 2 &
                  DIA >= 5 &
                  STANDING_DEAD_CD == 1) %>%
  dplyr::reframe(VOLCFNET_D = sum(VOLCFNET*TPA_UNADJ)) %>%
  distinct()

# VOLCFNET_L: Volume, Live (ft3/acre)
# Volume, live, cubic feet per acre. Calculated via the following FIA query:
#   Query; Sum VOLCFNET*TPA_UNADJ WHERE (((COND.COND_STATUS_CD)=1) AND ((TREE.STATUSCD)=1))

VOLCFNET_L <- result_df %>%
  dplyr::filter(COND_STATUS_CD == 1 &
                  STATUSCD == 1 ) %>%
  dplyr::reframe(VOLCFNET_L = sum(VOLCFNET*TPA_UNADJ)) %>%
  distinct()

# VOLCFSND_L: Volume Live Sound stem wood volume (ft3/acre)
# Query: sum VOLCFSND*TPAUNADJ WHERE COND.COND_STATUS_CD = 1 and TREE.STATUSCD = 1
  
VOLCFSND_L <- result_df %>%
  dplyr::filter(COND_STATUS_CD == 1 &
                STATUSCD == 1 ) %>%
  dplyr::reframe(VOLCFSND_L= sum(VOLCFSND*TPA_UNADJ)) %>%
  distinct() 

# VOLCFSND_D: Volume, Dead Sound stem wood volume (ft3/acre)
# Volume, live, cubic feet per acre. Calculated via the following FIA query:
# Query: sum VOLCFSND*TPA_UNADJ WHERE COND.CON_STATUS_CD = 1 and TREE.STATUS_CD = 2 and STANDING_DEAD_CD = 2


VOLCFSND_D <- result_df %>%
  dplyr::filter(COND_STATUS_CD == 1 &
                  STATUSCD == 2 &
                  STANDING_DEAD_CD == 1) %>%
  dplyr::reframe(VOLCFSND_D = sum(VOLCFSND*TPA_UNADJ)) %>%
  distinct()



# Join all vars together
#--------------------------------------------#
d_list <- list(VOLCFNET_L, VOLCFNET_D, VOLCFSND_L, VOLCFSND_D)

attributes_out <- purrr::reduce(d_list,
              full_join,
              by = "PLT_CN") %>%
  dplyr::rename("CN" = PLT_CN) %>%
  dplyr::mutate(CN = as.numeric(CN),
                # if VOLCFNET_L = 0 and VOLCFSND_L > 0, set VOLCFSND_L to 0
                VOLCFSND_L = ifelse(VOLCFNET_L == 0 & VOLCFSND_L > 0, 0, VOLCFSND_L),
                VOLCFSND_D = ifelse(VOLCFNET_D == 0 & VOLCFSND_D > 0, 0, VOLCFSND_D),
                # calculate pixel values
                VOLCFSND_L_PX = VOLCFSND_L * 900 * 0.000247104393046628 , # sqm/pixel * acres/sqm
                VOLCFSND_D_PX = VOLCFSND_D * 900 * 0.000247104393046628
)

#str(attributes_out)

# check for duplicates
message("checking for duplicate CNs in attributes dataset...")
dupes <- attributes_out %>%
  group_by(CN) %>%
  filter(n() > 1)

if(nrow(dupes) > 0) {
  stop("duplicate CNs found in attributes dataset. Please investigate.")
  } else {
    message("... no duplicate CNs found.")
  }


# message("duplicate CNs - first 6")
# print(head(dupes))

# message("head of attributes dataset:")
# print(head(attributes_out))

# Finalize output RAT
#--------------------------------------------#

# Join to existing RAT to get TMID, FORTYPCD, any other vars of interest
rat_out = rat %>%
  dplyr::select(-c(VOLCFNET_D, VOLCFNET_L)) %>% # remove these fields since i'm recalculating them 
  dplyr::right_join(attributes_out, by = "CN")  %>%
  replace(is.na(.), 0) %>%
  # Select variables of interest
  dplyr::select(CN, TM_ID, FORTYPCD, VOLCFNET_L, VOLCFSND_L, VOLCFSND_D, VOLCFNET_D, VOLCFSND_D_PX, VOLCFSND_L_PX) %>%
  arrange(CN)

# Determine forest type group code from species code
rat_out %<>%
  left_join(forest_types_species_to_group,
       by = c("FORTYPCD" = "Species_Code")) 

# Export
#----------------------------------------------------------------#

rat_export_path = glue::glue("{input_paths[['export_dir']]}01_Raster_attribute_table/TreeMap{tm_year}_rat_volumeCalcs.csv")

# write out to table
if(overwrite_rat | !file.exists(rat_export_path)) {
  message("writing out updated RAT with new variables...")
  write.csv(rat_out, rat_export_path, row.names = FALSE)
  message(glue::glue("...RAT written to {rat_export_path}"))

} else {
  stop("output RAT file already exists and overwrite_rat is set to FALSE. Set overwrite_rat to TRUE to overwrite existing file.")
}


# inspect
#----------------------------------------------------------------#

# hist(rat_out$VOLCFSND_L)
# hist(rat_out$VOLCFNET_L)
# hist(rat_out$VOLCFSND_D)

# plot(rat_out$VOLCFNET_L, rat_out$VOLCFSND_L)
# plot(rat_out$VOLCFNET_L, rat_out$VOLCFSND_L, xlim = c(0, 10000), ylim = c(0, 10000))
# abline(a = 0, b = 1, col = "red", lty = 2, lwd = 2)
# plot(rat_out$VOLCFNET_D, rat_out$VOLCFSND_D)
# abline(a = 0, b = 1, col = "red", lty = 2, lwd = 2)

# plot(rat_out$VOLCFNET_D, rat_out$VOLCFSND_D)
# plot(rat_out$VOLCFSND_L, rat_out$VOLCFSND_D)
# plot(rat_out$VOLCFNET_L, rat_out$VOLCFNET_D)


# rat_out %>%
#   filter(VOLCFNET_L== 0) %>%
#   ggplot() +
#   geom_point(aes(VOLCFNET_L, VOLCFSND_L))

# rat_out %>%
#   filter(VOLCFNET_L== 0,
#          VOLCFSND_L > 0)
