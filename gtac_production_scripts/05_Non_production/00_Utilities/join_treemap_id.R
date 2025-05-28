library(tidyverse)

# Allow for sufficient digits to differentiate plot cn numbers
options("scipen" = 100, "digits" = 8)

dir <- "//166.2.126.25/TreeMap/03_Outputs/06_Reference_Data/"
year <- 2022

# load list of treemap ids
ids <- read.csv(glue::glue('{dir}v{year}/02_X_table_CONUS/x_table_complete_CONUS_{year}.csv')) %>%
  dplyr::select(PLT_CN, TM_ID)

# load raster attribute table
rat <- read.delim(glue::glue('{dir}v{year}/03_Raster_attributes/TM{year}_RAT_052825.txt'), header = TRUE, sep = ",")

# join
out <- left_join(ids, rat, by = c("PLT_CN" = "CN")) %>%
  # rename any cols desired
  dplyr::rename("QMD" = QMDAll) 

#inspect
str(out)

#export
write.csv(out, glue::glue('{dir}v{year}//03_Raster_attributes/TM{year}_RAT_tmid.csv'), row.names = FALSE)
