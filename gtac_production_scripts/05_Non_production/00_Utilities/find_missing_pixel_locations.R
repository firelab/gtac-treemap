library(terra)
library(dplyr)

year_in = 2023
zone_in = 19


zone_in_zero = ifelse(zone_in <10, glue::glue("0{zone_in}"), zone_in)

# Direct to the rasters for the zone you want to inspect
files<- list.files(glue::glue("//166.2.126.25/TreeMap/03_Outputs/05_Target_Rasters/v{year_in}/one_mask/z{zone_in_zero}"), pattern=".tif$", full.names = T)

# Load the rasters
stack<-terra::rast(files)
names(stack)<- gsub(".tif","",gsub(glue::glue("//166.2.126.25/TreeMap/03_Outputs/05_Target_Rasters/v{year_in}/one_mask/z{zone_in_zero}/"),"", files))

crs(stack, describe = TRUE)

#
# Generate y/x (lat/lon)
stack$actual_lat <- terra::init(stack, "y")
names(stack$actual_lat) <- "actual_lat"
stack$actual_lon<- terra::init(stack, "x")
names(stack$actual_lon) <- "actual_lon"
#
# Assemble a dataframe
stack_df<- as.matrix(stack)
stack_df <- data.frame(stack_df)

# Find where there are NAs in some layers
  # This may be all you need to do, but below you can find no-NA and some-NA instances
stack_df_some_nas <- stack_df %>% filter(rowSums(is.na(stack_df)) > 0 & rowSums(is.na(stack_df)) < ncol(stack_df)-2)

# #
# stack_df_no_nas <- stack_df %>% filter(rowSums(is.na(stack_df)) == 0)
# stack_df_all_nas <- stack_df %>% filter(rowSums(is.na(stack_df)) == ncol(stack_df)-2)
# 
# 
# ###
# nrow(stack_df_no_nas) + nrow(stack_df_all_nas) + nrow(stack_df_some_nas)
# nrow(stack_df)

#

# load landfire zones
lf_zones <- terra::vect("//166.2.126.25/TreeMap/01_Data/02_Landfire/LF_zones/Landfire_zones/refreshGeoAreas_041210.shp")
lf_zone <- lf_zones[lf_zones$ZONE_NUM == zone_in]

pts <- terra::vect(stack_df_some_nas, geom = c("actual_lon", "actual_lat"), crs = crs(stack))
#pts <- terra::project(pts, "EPSG:4326")

plot(lf_zone)
plot(pts, add = TRUE)

