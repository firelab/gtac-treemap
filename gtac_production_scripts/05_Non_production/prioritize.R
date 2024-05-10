# prioritize

library(tidyverse)
library(magrittr)

# load csvs of rankings and acres

forest_acres <- read.csv("//166.2.126.25/TreeMap/01_Data/02_Landfire/metadata/forest_area_by_zone_2016.csv")

wcs <- read.csv("//166.2.126.25/TreeMap/03_Outputs/07_Projects/2022_Production/00_Prioritization/LF_zone_overlap_WCS_zones.csv")

# join 
df <- left_join(forest_acres, wcs, 
                by = "ZONE_NUM")
# sort
df %<>% dplyr::arrange(desc(wcs_area_acres),
                       desc(acres_forest)) %>%
  mutate(priority = row_number()) %>%
  select(priority, ZONE_NUM, ZONE_NAME, pct_forest, acres_forest, wcs_area_acres)

# export
write.csv(df, "//166.2.126.25/TreeMap/03_Outputs/07_Projects/2022_Production/00_Prioritization/priority_forest_wcs.csv",
          row.names = FALSE)
