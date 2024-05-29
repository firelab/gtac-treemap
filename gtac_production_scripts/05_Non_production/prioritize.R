# prioritize

library(tidyverse)
library(magrittr)

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

# load csvs of rankings and acres

forest_acres <- read.csv(glue::glue("{home_dir}/01_Data/02_Landfire/metadata/forest_area_by_zone_2016.csv"))

wcs <- read.csv(glue::glue("{home_dir}/03_Outputs/07_Projects/2022_Production/00_Prioritization/LF_zone_overlap_WCS_zones.csv"))

# join 
df <- left_join(forest_acres, wcs, 
                by = "ZONE_NUM")
# sort
df %<>% dplyr::arrange(desc(wcs_area_acres),
                       desc(acres_forest)) %>%
  mutate(priority = row_number()) %>%
  select(priority, ZONE_NUM, ZONE_NAME, pct_forest, acres_forest, wcs_area_acres)

# export
write.csv(df, glue::glue("{home_dir}/03_Outputs/07_Projects/2022_Production/00_Prioritization/priority_forest_wcs.csv"),
          row.names = FALSE)
