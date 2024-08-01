#############################
#                           #
#   Ride along plot filter  #
#   Workarounds 1 and 2     #
#                           #
#############################

library(data.table)
library(dplyr)
library(RSQLite)
library(sqldf)

options("scipen"=9999)
setwd("C:/Users/User/FIA/SQLite_DBs/State_Databases/")

# Read in the list of zones with adjacencies
adjacencies <- as.data.table(read.csv("F:/TreeMap2020/RideAlong/LANDFIRE_Zone_Adjacencies.csv"))

# Read in the plots by zone
plots_by_zone <- as.data.table(read.csv("F:/TreeMap2020/RideAlong/Plots_LFZone.csv"))

# Read in the current EVT table provided by Brenda Lundberg 
evt_table <- as.data.table(read.csv("F:/LANDFIRE_Autokey/FIA_DataTables/TREEMAP2022_LANDFIRE_EVT Assignments.txt"))
evt_available <- evt_table[!is.na(EcoSysCd) & EcoSysCd > 0,]
evt_available <- left_join(evt_available, plots_by_zone[,c("PLT_CN", "ZONE_NUM"),])

# Crosswalk the EVT and EVG from LANDFIRE
LF_EVT_crosswalk_2022 <- as.data.table(read.csv("F:/TreeMap2020/LC22_EVT_EVG_Crosswalk.csv"))
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

#####################
## Original filter ##
#####################

# In this step, we select those plots with EVGs that exist anywhere within LANDFIRE and count how many there are
write.csv(evg_available, "F:/LANDFIRE_Autokey/FIA_DataTables/LF2022_EVT_GP_National.csv", row.names = FALSE)

##################
## Workaround 1 ##
##################

## In this process, we limit the plots available to a given zone to those plots
## which fall within the zone and zones adjacent to the zone.
## This method is more restrictive than workaround 2.
plots_by_zone_adjacent <- NULL
current_zone = 55
for(current_zone in unique(adjacencies$src_ZONE_NUM)){
  zone_list <- adjacencies[src_ZONE_NUM == current_zone, nbr_ZONE_NUM,]
  plots_by_zones_filtered <- plots_by_zone[ZONE_NUM %in% zone_list | ZONE_NUM == current_zone,] %>%
    mutate(Zone = current_zone,
           zone_adj_count = n(),
           ZONE_NUM = NULL,
           ZONE_NAME = NULL) 
  
  plots_by_zone_adjacent <- rbind(plots_by_zone_adjacent, plots_by_zones_filtered)
}

plots_by_zone_adjacent_evt <- left_join(plots_by_zone_adjacent, evg_available[,c("PLT_CN", "EcoSysCd", "EVT_GP")], by = c("PLT_CN" = "PLT_CN"))
# Remove plots that do not have an EVT/EVG assigned:
plots_by_zone_adjacent_evt <- plots_by_zone_adjacent_evt[!is.na(EVT_GP),]

zone_available_plots <- left_join(LF2022_EVT_Zones, plots_by_zone_adjacent_evt[,c("PLT_CN", "Zone", "EVT_GP")], by = c("LFZones" = "Zone", "EVT_GP" = "EVT_GP"))

zone_available_plots <- left_join(zone_available_plots, unique(evt_available[,c("EcoSys", "EVT_GP")]), by = c("EVT_GP"))
write.csv(zone_available_plots, "F:/LANDFIRE_Autokey/FIA_DataTables/LF2020_EVT_GP_byZone_Workaround1.csv", row.names = FALSE)
zones_EVT_missing <- unique(zone_available_plots[is.na(PLT_CN), c("LFZones", "EVT_GP")])
write.csv(zones_EVT_missing, "F:/LANDFIRE_Autokey/FIA_DataTables/LF2020_EVT_GP_Missing_byZone_Workaround1.csv", row.names = FALSE)

##################
## Workaround 2 ##
##################

## In this process, we extend the plots available to those within zones that are
## not adjacent, but are restricted to plots that have species found within the 
## zone and adjacent zones. This method extends plots to those further away.

# Read in the tree table for all plot by state to create a list of species by plot #
# This list is used to limit the plots available within each zone to those with #
# species found within that zone and neighboring zones #

states <- list.files()
states <- grep("SQLite", states, value = TRUE)

species_table <- NULL
db <- 1
for(db in 1:length(states)){
  # connect to the sqlite file
  con = dbConnect(RSQLite::SQLite(), dbname=states[db])
  
  # get a list of all tables
  alltables = dbListTables(con)
  alltables
  tree_table <- as.data.table(dbGetQuery(con, 'select PLT_CN, SPCD, INVYR  from TREE'))
  
  plots_by_zone$PLT_CN <- as.character(plots_by_zone$PLT_CN)
  tree_filtered <- tree_table[(PLT_CN %in% plots_by_zone$PLT_CN),]
  tree_filtered <- as.data.table(left_join(tree_filtered, plots_by_zone, by = c("PLT_CN" = "PLT_CN")))
  
  species_table <- rbind(species_table, tree_filtered)
  print(db)
  dbDisconnect(con)
}

# Filter for plots in the reference data.
TM_2020_plots <- as.data.table(read.csv("F:/TreeMap2020/TM_ReferenceData.csv"))
TM_2020_Species <- species_table[PLT_CN %in% TM_2020_plots$StandID,]

# Get count of each species by zone and collapse to unique values. The results
# in a table of the count of each species by zone.
species_by_zone <- TM_2020_Species %>% 
  group_by(ZONE_NUM, SPCD) %>%
  mutate(count = n()) %>%
  mutate(zone_species_count = sum(count)) %>% 
  mutate(count = NULL,
         PLT_CN = NULL,
         OID_ = NULL,
         INVYR = NULL) %>%
  ungroup() %>%
  unique()
species_by_zone <- as.data.table(species_by_zone)

# Create table of species by zone and adjacent zones. This list is used to 
# filter the plots available to each zone.
species_by_zones_adjacent <- NULL
for(a in unique(adjacencies$src_ZONE_NUM)){
  zone_list <- adjacencies[src_ZONE_NUM == a, nbr_ZONE_NUM,]
  species_by_zones_filtered <- species_by_zone[ZONE_NUM %in% zone_list | ZONE_NUM == a,] %>%
    group_by(SPCD) %>%
    mutate(Zone = a,
           zone_adj_species_count = sum(zone_species_count),
           ZONE_NUM = NULL,
           ZONE_NAME = NULL,
           zone_species_count = NULL) %>%
    unique()
  species_by_zones_adjacent <- rbind(species_by_zones_adjacent, species_by_zones_filtered)
  print(paste0("Zone ", a, " complete."))
}

# Filter stands available to each zone using species by zone + adjacent and species by plot
# For each zone, we want only the plots that contain species within that zone and adjacent zones,
# so we need a list of species for each stand, and the list of species by zone

species_by_zones_adjacent <- as.data.table(species_by_zones_adjacent) # contains the species allowed in stands within a zone.
species_table # contains the species found within each plot.
species_table_simple <- unique(species_table[,c("PLT_CN", "SPCD"),])

plots_available_by_zone <- NULL
for(a in unique(species_by_zones_adjacent$Zone)){
  zonal_species <- species_by_zones_adjacent[Zone == a,]
  species_plot_lists <- as.data.table(species_table_simple %>% 
                                        group_by(PLT_CN) %>%
                                        mutate(plot_species_count = n(),
                                               list_plot_species = list(unique(SPCD))))
  excluded_plots <- species_plot_lists[!SPCD %in% zonal_species$SPCD,]
  excluded_plots <- unique(excluded_plots$PLT_CN)
  included_plots <- species_table_simple[!PLT_CN %in% excluded_plots,]
  included_plots$Zone <- a
  plots_available_by_zone <- rbind(plots_available_by_zone, included_plots)
  print(paste0("Zone ", a, " complete."))
}

plots_available_by_zone_short <- unique(plots_available_by_zone[,c("PLT_CN", "Zone"),]) %>%
  group_by(Zone) %>%
  mutate(available_stands = n()) %>%
  ungroup() 

plots_available_by_zone_short$PLT_CN <- as.double(plots_available_by_zone_short$PLT_CN)
plots_by_zone_EVT <- left_join(plots_available_by_zone_short, evt_available[,c("PLT_CN", "EcoSysCd"),], by = c("PLT_CN" = "PLT_CN"))
plots_by_zone_EVG <- as.data.table(left_join(plots_by_zone_EVT, LF_EVT_crosswalk_2022, by = c("EcoSysCd" = "LFRDB")) )

LF2022_EVT_Zones <- as.data.table(read.csv("F:/LANDFIRE_Autokey/FIA_DataTables/LF2022_Zones_EVT_ForestMask.csv"))
LF2022_EVT_Zones <- unique(LF2022_EVT_Zones[,c("LFZones", "LFRDB", "EVT_GP")])
LF2022_EVT_Zones <- LF2022_EVT_Zones[EVT_GP > 599,]

EVG_List <- NULL
for(i in unique(LF2022_EVT_Zones$LFZones)){
  # List the EVTs by zone, each zone requires at least one plot for each EVG
  LF2022_EVT_Zones_byzone <- LF2022_EVT_Zones[LFZones == i,]
  plots_EVG_2022_byzone <- plots_by_zone_EVG[Zone == i,]
  
  # Count how many instances of each EVT and EVG exist in the plots
  plot_count <- as.data.table(group_by(plots_EVG_2022_byzone, EVT_GP) %>%
                                mutate(count_EVG = n()) %>%
                                ungroup())
  unique_plots_w_evg <- as.data.table(unique(plot_count[,c("Zone", "EVT_GP", "count_EVG"),]))
  
  LF2022_EVG_plotnum <- left_join(LF2022_EVT_Zones_byzone, unique_plots_w_evg)
  EVG_List <- rbind(EVG_List, LF2022_EVG_plotnum)
}



plots_by_zone_EVG_unique <- unique(plots_by_zone_EVG[,c("PLT_CN", "Zone", "EVT_GP")])
plots_by_zone_EVG_available <- left_join(unique(EVG_List), plots_by_zone_EVG_unique, by = c("LFZones" = "Zone", "EVT_GP" = "EVT_GP"))

# Identify EVGs missing plots
zones_EVT_missing <- unique(plots_by_zone_EVG_available[is.na(PLT_CN), c("LFZones", "EVT_GP")])
write.csv(zones_EVT_missing, "F:/LANDFIRE_Autokey/FIA_DataTables/LF2022_EVT_GP_Missing_byZone_Workaround2.csv", row.names = FALSE)

# Remove the zones missing EVGs and remove the EVT column (LRFDB) to filter for unique plots.
plots_by_zone_EVG_available <- plots_by_zone_EVG_available[!is.na(PLT_CN),] %>%
  select(-LFRDB) %>%
  unique()
write.csv(plots_by_zone_EVG_available, "F:/LANDFIRE_Autokey/FIA_DataTables/LF2022_EVT_GP_byZone_Workaround2.csv", row.names = FALSE)




###################################
## Troubleshooting missing plots ##
###################################

# In workaround 1, there are too many missing EVGs to consider. In Workaround 2,
# we have a total of 10 zones with EVGs that have no plots, but only 3 unique 
# EVGs without plots assigned. 

# EVG 650 is missing plots in Zones 26, 35, and 36. This can be recoded to EVG 624 in the
# target data. These zones contain plots for EVG 624.

# EVG 675 is missing plots in Zones 55, 56, and 99. This can be recoded to EVG 684 in the
# target data. These zones contain plots for EVG 684.

# For EVG 708 in Zone 57, we have a very small number of pixels across one edge. There
# are no plots for EVG 708 in zone 57. 
# I filter the species for those plots here to explore what species are present in the 
# plots keyed to EVG 708 but not in the zone.

# Create the reclassification table for EVTs by zone.

zones <- unique(adjacencies$src_ZONE_NUM)
extra_zones <- c(26, 35, 36, 55, 56, 99)

zonal_evt_gp_reclass <- data.table("zone" = append(zones, extra_zones),
                                   "original_evt_gp" = append(rep(692, length(zones)), append(rep(650, 3),rep(75, 3))),
                                   "new_evt_gp" = append(rep(693, length(zones)), append(rep(624, 3),rep(684, 3))))

write.csv(zonal_evt_gp_reclass, "F:/LANDFIRE_Autokey/FIA_DataTables/zonal_evt_gp_reclass_LF2020.csv")


# Identify species that are present in stands keyed to EVT 9332 (EVG 708)

# Set PLT_CN to same data type to join.
evg_available$PLT_CN <- as.character(evg_available$PLT_CN)

# Select all plots keyed to EVG 708
plots_EVG708 <- plots_by_zone_EVG_available[EVT_GP == 708,]

#Filter those plots to those assigned to the zones adjacent to 57
plots_EVG708_adjacent_zone57 <- unique(plots_EVG708[LFZones %in% c(48, 53, 54, 59, 61)]$PLT_CN)

# Filter the full plot list, join with species table to identify the species present in the filtered 708 plots
plots_708 <- left_join(evg_available[PLT_CN %in% plots_EVG708_adjacent_zone57,],TM_2020_Species[,c("PLT_CN", "SPCD", "ZONE_NUM")] )

plots_708 <- unique(plots_708[!is.na(SPCD),2:5])

species_zone57 <- as.data.table(unique(species_by_zone[ZONE_NUM == 57,]))

species_absent_zone57 <- plots_708[!SPCD %in% species_zone57$SPCD,]
species_absent_zone57 <- unique(species_absent_zone57[,c("SPCD", "EVT_GP")])

# Write the list of species missing from zone 57 that are present in the EVG 708 plots.
write.csv(species_absent_zone57, "F:/LANDFIRE_Autokey/FIA_DataTables/FIA_Species_Missing_Zone57_EVG708_2022.csv", row.names = FALSE)

# Add these plots to the workaround 2 plots by zone list.
additional_plots <- unique(plots_EVG708[LFZones %in% c(48, 53, 54, 59, 61)])
additional_plots$LFZones <- 57

workaround2_plots <- rbind(plots_by_zone_EVG_available, unique(plots_EVG708[LFZones %in% c(48, 53, 54, 59, 61)]))

write.csv(workaround2_plots, "F:/LANDFIRE_Autokey/FIA_DataTables/LF2022_EVT_GP_byZone_Workaround2_Final.csv", row.names = FALSE)                    

# Create unique list of plots for spatial overlays

unique_plots <- as.data.table(unique(workaround2_plots$PLT_CN))
unique_plots <- rename(unique_plots, PLT_CN = V1)
write.csv(unique_plots, "F:/LANDFIRE_Autokey/FIA_DataTables/TM_2022_X_table_Plots.csv", row.names = FALSE)
