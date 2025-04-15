## Ride along problem prep work
###################################################

# Identify which zones are adjacent to each other
# Identify which plots fall in which zone
# Identify which evgs are represented in EVT_GP layers initially present


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

##########################################################
# Where will these data be saved? 

# ridealong dir
ra_dir <- glue::glue("{home_dir}03_Outputs/06_Reference_Data/v{year}/00_Ridealong_supporting_info/")

# create dir if it does not exists
if(!file.exists(ra_dir){
  dir.create(ra_dir, recursive = TRUE)
})

# Load supporting data
#####################################
zone_metadata <- read.csv(zone_metadata_path)

zones_list = zone_metadata %>% filter(STUDY_AREA == study_area) %>%
  select(ZONE_NUM)

# Create list of zones with adjacencies
#####################################################

# load zones shp
# iterate for each zone: list zones which border
# write to data file

lf_zones <- terra::vect(lf_zones_path)

adjacencies <- data.frame(terra::adjacent(lf_zones))
names(adjacencies) <- c("src_ZONE_NUM", "nbr_ZONE_NUM")

#export
write.csv(adjacencies, 
          glue::glue('{ra_dir}/LANDFIRE_Zone_Adjacencies.csv'),
          row.names = F)

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

# export
write.csv(plots_by_zone, 
          glue::glue('{ra_dir}/Plots_LFZone.csv'))

######################################################################################

# List EVT/EVG records that require plots in the FIA data. Any that are not covered should be either excluded from the forest mask, or assigned plots with the most similar option.
# AKA - EVT / EVG records in current version of data, that are included in the forest mask

# -> from 12_mosaic_raw_EVT_name script

# EVT_Zones <- as.data.table(read.csv("F:/LANDFIRE_Autokey/FIA_DataTables/LF2022_Zones_EVT_ForestMask.csv"))

# Direct to files
evt_files<- list.files(glue::glue("{home_dir}/03_Outputs/05_Target_Rasters/v{year}/pre_mask/"),
                       recursive = T,
                       pattern = "evt_name.tif$",
                       full.names = T)
evt_vrt<- vrt(evt_files)

###

# Which EVTs are present in which zone? 


###

# Get the valid categories from each EVT

for(i in seq_along(evt_files)){
  evt<- rast(evt_files[i])
  cats<-  cats(evt)[[1]]
  
  
  zone = str_extract(evt_files[i], paste0("/z[0-9][0-9]/"))
  zone = gsub("/", "", zone)
  zone = gsub("z", "", zone)
  zone = as.numeric(zone)
  cats$zone_num = zone
  
  assign(paste0("cats_",i), cats)

}

# Bind them all into one
complete_cats<- do.call(rbind, (mget(ls(pattern="cats_"))))
rm(list = ls(pattern = "cats_"))

# filter to cats in study area
complete_cats <- 
  complete_cats %>% filter(zone_num %in% zones_list$ZONE_NUM) 


# Remove duplicates
unique_cats <- complete_cats
unique_cats$zone_num = NULL
unique_cats<- unique_cats[-which(duplicated(unique_cats)),]

complete_cats %>%
  dplyr::select(LFRDB, EVT_GP, zone_num) %>%
  group_by(EVT_GP,zone_num) %>%
  summarize(n())


# How many EVT names are there?
print("unique EVT names:")
length(unique(unique_cats$EVT_NAME))

#How many EVT Groups?
print("unique EVT groups:")
length(unique(unique_cats$EVT_GP))

# How many EVT's are there in non-tree EVT-Lifeforms?
print("EVTs in non-tree EVT Life forms:")
length(which(unique_cats$EVT_LF != "Tree"))

# How many EVT's are there in non-tree-dominated EVT-Orders?
print("EVTs in non-tree-dominated EVT-Orders:")
length(which(unique_cats$EVT_ORDER != "Tree-dominated"))

write.csv(complete_cats, 
          glue::glue('{ra_dir}//LF{year}_Zones_EVT_ForestMask.csv'),
          row.names = FALSE)

#####################################################################

# Read in the current EVT table provided by Brenda Lundberg
evt_table <- as.data.table(read.csv(glue::glue("{home_dir}/01_Data/10_AutoKey/TREEMAP2022_LANDFIRE_EVT Assignments.txt")))
evt_available <- evt_table[!is.na(EcoSysCd) & EcoSysCd > 0,]
evt_available <- left_join(evt_available, plots_by_zone[,c("PLT_CN", "ZONE_NUM"),])

# Crosswalk the EVT and EVG from LANDFIRE
LF_EVT_crosswalk <- read.csv(glue::glue("{home_dir}/01_Data/02_Landfire/LF_240/Vegetation/EVT/LF2023_EVT_240_CONUS/CSV_Data/LF23_EVT_240.csv"))

LF_EVT_crosswalk <- unique(LF_EVT_crosswalk[,c("LFRDB", "EVT_GP"),])
evt_available <- unique(as.data.table(left_join(evt_available, LF_EVT_crosswalk, by = c("EcoSysCd" = "LFRDB"))))
evg_available <- unique(as.data.table(evt_available[!is.na(EVT_GP), c("ZONE_NUM", "PLT_CN", "EcoSysCd", "EVT_GP")]))

# Read in the EVT/EVG data from LF 2020 by zone. These are the EVT/EVG records that require plots
# in the FIA data. Any that are not covered should be either excluded from the forest mask,
# or assigned plots with the most similar option.

#EVT_Zones <- as.data.table(read.csv("F:/LANDFIRE_Autokey/FIA_DataTables/LF2022_Zones_EVT_ForestMask.csv"))
EVT_Zones <- as.data.table(read.csv(glue::glue('{ra_dir}//LF{year}_Zones_EVT_ForestMask.csv')))
#EVT_Zones <- unique(EVT_Zones[,c("LFZones", "Count", "LFRDB", "EVT_GP")])
EVT_Zones <- unique(EVT_Zones[,c("LFRDB", "EVT_GP")])
EVT_Zones <- EVT_Zones[EVT_GP > 599,]

# Identify missing EVGs at the national scope:
national_evg_list <- unique(EVT_Zones[,c("EVT_GP")])
brendas_evgs <- unique(evt_available[,c("EVT_GP")])
missing_national_evgs <- national_evg_list[!EVT_GP %in% brendas_evgs$EVT_GP,]
