library(tidyverse)

# Allow for sufficient digits to differentiate plot cn numbers
options("scipen" = 100, "digits" = 8)

dir <- "//166.2.126.25/TreeMap/03_Outputs/06_Reference_Data/"
year <- 2020

# load list of treemap ids
ids <- read.csv(glue::glue('{dir}v{year}/02_X_table_CONUS/x_table_complete_CONUS_2020.csv'))

# load raster attribute table
rat <- read.delim(glue::glue('{dir}v{year}/03_Raster_attributes/TM2020_RAT_021025.txt'), header = TRUE, sep = ",")

# join
out <- left_join(ids, rat, by = c("PLT_CN" = "CN")) %>%
  # rename any cols desired
  dplyr::rename("QMD" = QMDAll)

#export
write.csv(out, glue::glue('{dir}v{year}//03_Raster_attributes/TM{year}_RAT_tmid.csv'), row.names = FALSE)
