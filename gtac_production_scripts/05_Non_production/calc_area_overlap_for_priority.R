# Calculate area of LF zone overlap with priority zones
# Rank LF zones by the area of overlap with priority zone
# Include name of priority zone with majority overlap of zone

#load libraries
#----------------------------#
library(tidyverse)
library(terra)
library(magrittr)
library(sf)

# Initialize home dir
#-----------------------------------------------#
# Id where THIS script is located
this.path <- this.path::this.path()

# get path to input script
spl <- stringr::str_split(this.path, "/")[[1]]
setup_dirs.path <- paste( c(spl[c(1:(length(spl)-2))],
                              "00_Library/setup_dirs.R" ),
                            collapse = "/")

source(setup_dirs.path)

# set inputs
#---------------------------#

lf_zone_path <- glue::glue("{home_dir}/01_Data/02_Landfire/LF_zones/Landfire_zones/refreshGeoAreas_041210.shp")

priority_zone_path <- glue::glue("{home_dir}/01_Data/06_WCS/WCSLandscapePerims/Wildfire_Crisis_Strategy_Landscapes_(Feature_Layer).shp")

export_dir <- glue::glue("{home_dir}/03_Outputs/07_Projects/2022_Production/00_Prioritization/")

# Load data
#------------------------------#

lf_zones <- terra::vect(lf_zone_path)

priority_zones <- terra::vect(priority_zone_path)

# make sure crs match
priority_zones %<>% terra::project(lf_zones)

#inspect
plot(lf_zones)
plot(priority_zones, add = TRUE)

# Do the math
#----------------------------#

#intersect
y = terra::intersect(lf_zones, priority_zones)

#inspect
plot(y)

y_df <- data.frame(y)


y_df %>%
  group_by(ZONE_NUM) %>%
  summarize(first(NAME))

# convert to sf (data frame), mutate an area, and sum area per name group
overlap_area <- st_as_sf(y) %>% 
  mutate(area = st_area(geometry)) %>% 
  group_by(ZONE_NUM, ZONE_NAME) %>% 
  summarise(wcs_area_sum_m2 = sum(area)) %>% 
  ungroup() %>%
  mutate(wcs_area_acres = wcs_area_sum_m2 * 0.000247105) %>%
  arrange(desc(wcs_area_acres)) %>%
  as.data.frame() %>%
  select(ZONE_NUM, ZONE_NAME, wcs_area_sum_m2, wcs_area_acres)

# Export csv of area overlaps
#-----------------------------#
write.csv(overlap_area, glue::glue("{export_dir}/LF_zone_overlap_WCS_zones.csv"), row.names = FALSE)
