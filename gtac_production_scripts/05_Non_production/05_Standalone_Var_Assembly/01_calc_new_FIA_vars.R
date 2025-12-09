# Calculate attributes from FIA variables that were not included in the original TreeMap outputs

# This script does the following: 


######################################################
# SET INPUTS
######################################################

# Specific inputs
#-----------------------------------------------------#

# pick a TreeMap vintage to use for assembly 
tm_version = "2022_Production"
tm_project_name = "2022_Production_newXtable"

# export location
output_dir = "//afssxgtacnas311/ForestMAP/80_Workspace/40_TreeMap_Volume/Attribute_assembly/"

# name for output products
output_project_name = "TM_Volume_Calcs"

# # list of desired output fields
# output_fields = c("VOLCFSND_Live_perAcre",
#                   "VOLCFSND_Live_perPixel")

# # FIA tables needed
# fia_tables = c("TREE", "COND")

# write sql query for initial data pull
# make sure you get all FIA tables and fields necessary here
sql_query_init = 'select TREE.PLT_CN, TREE.DIA, TREE.CN as "TREE_CN", TPA_UNADJ, STATUSCD, TREECLCD, STANDING_DEAD_CD, VOLCFSND, VOLCFNET, COND.INVYR, COND.COND_STATUS_CD
from COND
INNER JOIN TREE ON COND.PLT_CN = TREE.PLT_CN'

sql_query_where = 'WHERE COND.INVYR >= {start_year} AND COND.INVYR <= {end_year} AND TREE.PLT_CN in ({id_list*})'

# list zones of interest. options = "all" or a list of specific zones
#zones_desired = "all"
zones_desired = c(1, 2, 3)

fia_path = "01_Data/04_FIA/05_FIA_DataMart/SQLite/SQLite_FIADB_ENTIRE/SQLite_FIADB_ENTIRE.db"
#fia_path = "01_Data/04_FIA/05_FIA_DataMart/SQLite/SQLite_FIADB_States/SQLite_FIADB_WA.db"

# rat path 
rat_path = "//166.2.126.25/TreeMap/03_Outputs/06_Reference_Data/v2022/03_Raster_attributes/TM2022_RAT_tmid.csv"


start_year = 1999
end_year = 2022

# Load data
#---------------------------------------------------------------------
# Allow for sufficient digits to differentiate plot cn numbers
options("scipen"=100, "digits"=8)

library(RSQLite)
library(DBI)

this_proj <- this.path::this.proj()
this_dir <- this.path::this.dir()

## load treemap library
lib_path = glue::glue('{this_proj}/gtac_production_scripts/00_Library/treeMapLib.R')
source(lib_path)

# Load project params
#project_params_path = glue::glue('{this_proj}/gtac_production_scripts/03_Imputation/params/{tm_version}_imputation_inputs.RDS')


#load(project_params_path)

fia_path = glue::glue("{home_dir}{fia_path}")


# Load and prep Raster Attribute Table
#-----------------------------------------------------------------#

rat <- load_RAT(rat_path, 
        CN_column = "PLT_CN", 
        ID_column = "TM_ID")

# get list of ids to limit sql query of database
id_list = rat$CN

# inspect
#str(rat)

# connect to FIA db
db_path <- fia_path
print(file.exists(db_path)) # Should return TRUE if the file exists

con = dbConnect(RSQLite::SQLite(), db_path)

# get a list of all tables
# alltables = dbListTables(con)
# alltables

# # list fields in table 
dbListFields(con, "PLOT")
dbListFields(con, "TREE")
dbListFields(con, "COND")


sql_query = glue::glue('{sql_query_init} {sql_query_where}')

query <- glue_sql(  sql_query,  .con = con )

print("final sql query, before glue_sql: ")
print(sql_query)


# Execute the query, passing R variables in the 'params' argument
print("running query....")
result_df <- dbGetQuery(con, query) 

result_df %<>%   
  replace(is.na(.), 0) %>%
  dplyr::group_by(PLT_CN) %>%
  arrange(PLT_CN)

print("result df looks like this:")
print(head(result_df))
print(str(result_df))

# Calculate variables
#------------------------------------------------------------------------------------#

print("Calculating variables")

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
                VOLCFSND_L_PX = VOLCFSND_L * 900 * 0.000247104393046628 , # sqm/pixel * acres/sqm
                VOLCFSND_D_PX = VOLCFSND_D * 900 * 0.000247104393046628
)

dupes <- attributes_out %>%
  group_by(CN) %>%
  filter(n() > 1)


# check for duplicates
print("duplicate CNs - first 6")
print(head(dupes))

print("head of attributes dataset:")
print(head(attributes_out))

# remove CNs with no info available 


# Join to existing RAT to get TMID
rat_out = rat %>%
  dplyr::select(-c(VOLCFNET_D, VOLCFNET_L)) %>% # remove these fields since i'm recalculating them 
  dplyr::right_join(attributes_out, by = "CN")  %>%
  replace(is.na(.), 0) %>%
  # Select variables of interest
  dplyr::select(CN, TM_ID, VOLCFNET_L, VOLCFSND_L, VOLCFSND_D, VOLCFNET_D, VOLCFSND_D_PX, VOLCFSND_L_PX) %>%
  arrange(CN)

print(head(rat_out))

# exclude any records where VOLCFNET_L = 0 and VOLCFSND > 0
rat_out %<>%
  filter(!(VOLCFNET_L == 0 & VOLCFSND_L > 0))

# write out to table
write.csv(rat_out, glue::glue("{output_dir}00_Raster_attribute_table/TreeMap_RAT_SOUNDVOLUME.csv"))

# inspect
#----------------------------------------------------------------#

hist(rat_out$VOLCFSND_L)
hist(rat_out$VOLCFNET_L)
hist(rat_out$VOLCFSND_D)

plot(rat_out$VOLCFNET_L, rat_out$VOLCFSND_L)
plot(rat_out$VOLCFNET_L, rat_out$VOLCFSND_L, xlim = c(0, 10000), ylim = c(0, 10000))
abline(a = 0, b = 1, col = "red", lty = 2, lwd = 2)
plot(rat_out$VOLCFNET_D, rat_out$VOLCFSND_D)
abline(a = 0, b = 1, col = "red", lty = 2, lwd = 2)

plot(rat_out$VOLCFNET_D, rat_out$VOLCFSND_D)
plot(rat_out$VOLCFSND_L, rat_out$VOLCFSND_D)
plot(rat_out$VOLCFNET_L, rat_out$VOLCFNET_D)
plot()

attributes_export = c("VOLCFSND_L_PX", "VOLCFSND_D_PX")

rat_out %>%
  filter(VOLCFNET_L== 0) %>%
  ggplot() +
  geom_point(aes(VOLCFNET_L, VOLCFSND_L))

rat_out %>%
  filter(VOLCFNET_L== 0,
         VOLCFSND_L > 0)
