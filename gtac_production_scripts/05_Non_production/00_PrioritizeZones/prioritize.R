# prioritize

library(tidyverse)
library(magrittr)

# Initialize home dir
#-----------------------------------------------#

this_proj <- this.path::this.proj()
lib_path <- glue::glue("{this_proj}/gtac_production_scripts/00_Library/setup_dirs.R" )
source(lib_path)

# Set Inputs
#----------------------------------------------------#
# load csvs of rankings and acres
forest_acres <- read.csv(glue::glue("{home_dir}/01_Data/02_Landfire/metadata/forest_area_by_zone_2016.csv"))
wcs <- read.csv(glue::glue("{home_dir}/03_Outputs/07_Projects/2022_Production/00_Prioritization/LF_zone_overlap_WCS_zones.csv"))

# load shp with full metadata
lf_zone_path <- glue::glue("{home_dir}/01_Data/02_Landfire/LF_zones/Landfire_zones/refreshGeoAreas_041210.shp")
lf_zones <- terra::vect(lf_zone_path)

# convert to data frame
lf_zones <- data.frame(lf_zones) 

# Join
#-----------------------------------------------------#

wcs %<>%
  select(-ZONE_NAME)

# join 
df <- left_join(forest_acres, wcs, 
                by = "ZONE_NUM") %>%
  left_join(lf_zones, by = "ZONE_NUM")

# sort
df %<>% dplyr::arrange(desc(wcs_area_acres),
                       desc(acres_forest)) %>%
  mutate(priority = row_number()) %>%
  select(priority, ZONE_NUM, ZONE_NAME, pct_forest, acres_forest, wcs_area_acres)

# export
write.csv(df, glue::glue("{home_dir}/03_Outputs/07_Projects/2022_Production/00_Prioritization/priority_forest_wcs.csv"),
          row.names = FALSE)
