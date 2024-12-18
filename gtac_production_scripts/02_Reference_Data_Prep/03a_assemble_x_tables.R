library(data.table)
library(dplyr)

options("scipen"=9999)
## Combine X table variables into the x table:
setwd("F:/TreeMap2022/XTable/")

biophysical <- as.data.table(read.csv("F:/TreeMap2020/XTable/TM_2020_X_table_Plots_with_biophys_extracted_nad83_09_12_2024.csv"))
vegetation <- as.data.table(read.csv("F:/TreeMap2020/XTable/FIA2020_FVS_cover_and_height.csv"))
evg_zone <- as.data.table(read.csv("F:/TreeMap2020/XTable/LF2020_EVT_GP_byZone_Workaround2_Final_Target_Data.csv"))
disturbance_data <- as.data.table(read.csv("F:/TreeMap2020/XTable/FIA2020_Disturbance.csv"))
disturbance_data$PLT_CN <- as.numeric(disturbance_data$PLT_CN)
elevation <- as.data.table(read.csv("LF_Plots_Elevation.csv"))
aspect <- as.data.table(read.csv("LF_Plots_Aspect.csv"))
slope <- as.data.table(read.csv("LF_Plots_Slope.csv"))

# Convert DEM elevation from meters to feet
elevation$ELEV <- elevation$ELEV * 3.28084

# Create topography table
topography <- left_join(elevation[,2:3], aspect[,2:3]) %>% 
  left_join(slope[,2:3])

# Create the final topography table. This selects the topographical values from FIA
# data first, then fills in missing data with the DEM-plot overlay data. We implicitely 
# assume FIA plot data is more accurate than the DEM data.
topography <- topography[ASPECT == -1, ':='(ASPECT = 0, SLOPE = 0),]
topography <- topography[PLOT_CN %in% unique(disturbance_data$PLT_CN),]
# In order to address missing topographic data those plots with slope and aspect missing were
# filled in by using the intersect of the original FIA plots and the LANDFIRE layers.
topography_filled_in <- left_join(topography, disturbance_data[,c("PLT_CN", "SLOPE", "ASPECT", "ELEV")], by = c("PLOT_CN" = "PLT_CN"))
topography_filled_in <- topography_filled_in %>% 
  mutate(SLOPE = case_when(is.na(SLOPE.y) ~ SLOPE.x,
                           !is.na(SLOPE.y) ~ SLOPE.y),
         ASPECT = case_when(is.na(ASPECT.y)  ~ ASPECT.x,
                            !is.na(ASPECT.y) ~ ASPECT.y)) %>%
  select(-c(ASPECT.x, ASPECT.y, SLOPE.x, SLOPE.y, ELEV.y)) %>%
  rename(ELEV = ELEV.x)

# Join all fields for the x table EXCLUDING the coordinate fields. These must be
# added later as they are CUI. 
x_table <- left_join(evg_zone[,c("PLT_CN", "Zone", "EVT_GP"),], 
                     disturbance_data[,c("PLT_CN", "disturb_code", "disturb_year"),], by = "PLT_CN" ) %>%
  left_join(biophysical[,c("PLT_CN", "prcp", "srad", "swe", "tmax", "tmin", "vp", "vpd"),]) %>%
  left_join(vegetation[,c("StandID", "canopy_cover", "canopy_height"),], by = c("PLT_CN" = "StandID")) %>%
  left_join(topography_filled_in, by = c("PLT_CN" = "PLOT_CN")) 

# This excludes plots that are missing values for disturbance years. 
x_table_complete <- na.omit(x_table)

# Add in the unique TreeMap ID for each CN
x_table_complete <- as.data.table(x_table_complete %>% 
  arrange(PLT_CN) %>%
  group_by(PLT_CN) %>%
  mutate(TM_ID = cur_group_id()) %>%
  ungroup())

# Order the columns similarly to previous x tables.
x_table_complete <- setcolorder(x_table_complete, c("TM_ID", "Zone", "PLT_CN", "SLOPE", "ASPECT", "ELEV", "prcp", "srad", "swe", 
                                  "tmax", "tmin", "vp", "vpd", "disturb_code", "disturb_year", "canopy_cover",
                                  "canopy_height", "EVT_GP"))

# write out x tables by zone.
for(x in unique(x_table_complete$Zone)){
  x_table_zone <- x_table_complete[Zone == x,]
  write.csv(x_table_zone, paste0("X_tables_by_zone/x_table_", x, ".csv"), row.names = FALSE)
}

# Create the x table with unique national values.
x_table_unique <- x_table_complete %>%
  select(-Zone) %>%
  unique()

write.csv(x_table_unique, "x_table_unique_2020_2022.csv")
