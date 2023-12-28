# explore landfire evt_grp, remapped evg - in order to make a plan for evg remap

# load libraries
library(raster)
library(terra)
library(tidyverse)
library(magrittr)
library(glue)

# Set the zone
cur.zone.num <- 16

# path to data
data_dir <- "//166.2.126.25/TreeMap/01_Data/"

points_path <- glue('{data_dir}04_FIA/03_FullShp/FIA_US.shp')

xtable_path <- glue('{data_dir}01_TreeMap2016_RDA/01_Input/03_XTables/X_table_all_singlecondition.txt')

target_data_path <- glue('{data_dir}01_TreeMap2016_RDA/02_Target/')

lf_evt_path <- glue('{data_dir}02_Landfire/LF_200/EVT/LF2016_EVT_200_CONUS/Tif/LC16_EVT_200.tif')

###### Load data
###########################

# load point data
fia_pts <- terra::vect(points_path)
allplot <- read.csv(xtable_path)

#set zone identifiers
cur.zone <- glue('z{cur.zone.num}')
cur.zone.zero <- if(cur.zone.num < 10) {
  glue('z0{cur.zone.num}') } else {
    cur.zone
  }

# load raster target data
flist.tif.full <- list.files(glue('{target_data_path}{cur.zone.zero}'), pattern = "*.tif$", recursive = TRUE, full.names = TRUE)
flist.tif <-list.files(glue('{target_data_path}{cur.zone.zero}'), pattern = "*.tif$", recursive = TRUE) 

# faster option for future - make this a .vrt with option = "-separate"
raster.stack <- raster::stack(flist.tif.full)
p4s.albers <- proj4string(raster.stack)
#raster.list <- vector("list", length(flist.tif))
nrasters <- length(flist.tif)
for(i in 1:length(flist.tif))  
{
  raster.list[[i]] <- raster()  
}

#add names to raster list
#names(raster.list) <- gsub(".tif", "",flist.tif)
names(raster.stack) <- gsub(".tif", "", flist.tif)

#convert raster stack to terra object
raster_rast <- terra::rast(raster.stack)

# load Landfire remap
lf_evt <- terra::rast(lf_evt_path)

# load LF zone data
LF_zones <- terra::vect(glue('{data_dir}02_Landfire/LF_zones/Landfire_zones/refreshGeoAreas_041210.shp'))

# crop to single zone for testing
##############################################

zone <- subset(LF_zones, LF_zones$ZONE_NUM == cur.zone.num) #z16 = Utah High Plateaus

# get evt_gp
lf_evt_gp <- subset(lf_evt, "EVT_GP")

lf_evt_gp <- 
  terra::crop(lf_evt_gp, 
              terra::project(zone, crs(lf_evt)),
              mask = TRUE)


# extract all values to points 
############################################

# extract zone num to pts 
zone_extract <- terra::extract(LF_zones,
                               terra::project(fia_pts, crs(LF_zones))) %>%
  dplyr::rename("ID" = id.y) %>%
  dplyr::select(ID, ZONE_NUM)

# extract EVT_GPs from available rasters, to points
evt_gp_df1 <- terra::extract(raster_rast$EVT_GP, 
                             terra::project(fia_pts, crs(raster_rast))) %>%
  rename("EVT_GP_reclass" = EVT_GP)
evt_gp_df2 <- terra::extract(lf_evt_gp$EVT_GP, 
                             terra::project(fia_pts, crs(lf_evt))) %>%
  rename("EVT_GP_orig" = EVT_GP)

evt_gp_df <- 
  left_join(evt_gp_df1, evt_gp_df2, by = "ID") %>%
  filter(!is.na(EVT_GP_reclass) & !is.na(EVT_GP_orig)) %>%
  dplyr::select(-ID) %>%
  distinct() %>%
  arrange(EVT_GP_reclass)

# convert to data frame of points and vars
fia_pts_xy <- data.frame(terra::geom(fia_pts))
fia_pts_xy %<>% cbind(data.frame(fia_pts)) %>%
  cbind(evt_gp_df1) %>%
  left_join(evt_gp_df2) %>%
  left_join(zone_extract) %>%
  rename("CN_xy" = CN) %>%
  dplyr::select(CN_xy, PREV_PLT_C, x, y, PLOT, ZONE_NUM,
                EVT_GP_reclass, EVT_GP_orig) %>%
  arrange(PLOT)

# inspect
# fia_pts_xy %>%
#   filter(!is.na(EVT_GP_reclass)) %>%
#   nrow()

# join coords with plot data
# and zone data 
plot.df <- left_join(allplot, fia_pts_xy,  by = c("ID" = "PLOT") )

#inspect
plot.df %>%
  filter(!is.na(EVT_GP_orig)) %>%
  head()

# and compare - confusion matrix / qualitative 



# likely path forward - re-make evg remap files. and/or do this in the flow of the modeling script. 