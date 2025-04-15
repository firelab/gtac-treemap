## Ride along problem prep work
###################################################

# Identify adjacent zones

# Identify which plots fall in which zone


# Setup
#######################################################

year_input <- 2023
study_area <- "CONUS"
#--------------------------------------------------#

# necessary to load this before calling any of the home_dir, fia_dir, or data_dir objects

# load library 
this_proj = this.path::this.proj()
lib_path = glue::glue("{this_proj}/gtac_production_scripts/00_Library/treeMapLib.R")
source(lib_path)

##############################################

# Load project inputs for target data
project_input_script = glue::glue("{this_proj}/gtac_production_scripts/01_Target_Data_Prep/02_Disturbance_data_prep/00a_project_inputs_for_targetdata.R")

zone_input_script = glue::glue("{this_proj}/gtac_production_scripts/01_Target_Data_Prep/02_Disturbance_data_prep/00b_zone_inputs_for_targetdata.R")

# run project input script now; save zone input script for later
source(project_input_script)


# Create list of zones with adjacencies
#####################################################

# load zones shp
# iterate for each zone: list zones which border
# write to data file

lf_zones <- terra::vect(lf_zones_path)

adjacencies <- data.frame(terra::adjacent(lf_zones))
names(adjacencies) <- c("src_ZONE_NUM", "nbr_ZONE_NUM")

# List plots by zone
#########################################################
# intersect coords with zone
# write to data file

# load coords
coords <- read.csv(glue::glue('{FIA_dir}06_Coordinates/select_TREEMAP2022_2send/select_TREEMAP2022_2send.csv'))

# project coords into same coordinate system 
coords <- terra::vect(coords, geom = c("ACTUAL_LON", "ACTUAL_LAT"), crs = "epsg:4269") %>%
  terra::project(crs(lf_zones))

plots_by_zone <- data.frame(terra::intersect(coords, lf_zones))

#####################################################################

# Read in the current EVT table provided by Brenda Lundberg 
evt_table <- as.data.table(read.csv(glue::glue("{home_dir}/01_Data/10_AutoKey/TREEMAP2022_LANDFIRE_EVT Assignments.txt")))
evt_available <- evt_table[!is.na(EcoSysCd) & EcoSysCd > 0,]
evt_available <- left_join(evt_available, plots_by_zone[,c("PLT_CN", "ZONE_NUM"),])

# Crosswalk the EVT and EVG from LANDFIRE

#LF_EVT_crosswalk_2022 <- as.data.table(read.csv("F:/TreeMap2022/LC23_EVT_EVG_Crosswalk.csv"))
LF_EVT_crosswalk <- read.csv("{home_dir}\01_Data\02_Landfire\LF_240\Vegetation\EVT\LF2023_EVT_240_CONUS\CSV_Data\LF23_EVT_240.csv")
LF_EVT_crosswalk_2022 <- unique(LF_EVT_crosswalk_2022[,c("LFRDB", "EVT_GP"),])
evt_available <- unique(as.data.table(left_join(evt_available, LF_EVT_crosswalk_2022, by = c("EcoSysCd" = "LFRDB"))))
evg_available <- unique(as.data.table(evt_available[!is.na(EVT_GP), c("ZONE_NUM", "PLT_CN", "EcoSysCd", "EVT_GP")]))

# Read in the EVT/EVG data from LF 2020 by zone. These are the EVT/EVG records that require plots
# in the FIA data. Any that are not covered should be either excluded from the forest mask,
# or assigned plots with the most similar option.

LF2022_EVT_Zones <- as.data.table(read.csv("F:/LANDFIRE_Autokey/FIA_DataTables/LF2022_Zones_EVT_ForestMask.csv"))
LF2022_EVT_Zones <- unique(LF2022_EVT_Zones[,c("LFZones", "Count", "LFRDB", "EVT_GP")])
LF2022_EVT_Zones <- LF2022_EVT_Zones[EVT_GP > 599,]

# Identify missing EVGs at the national scope:
national_evg_list <- unique(LF2022_EVT_Zones[,c("EVT_GP")])
brendas_evgs <- unique(evt_available[,c("EVT_GP")])
missing_national_evgs <- national_evg_list[!EVT_GP %in% brendas_evgs$EVT_GP,]
