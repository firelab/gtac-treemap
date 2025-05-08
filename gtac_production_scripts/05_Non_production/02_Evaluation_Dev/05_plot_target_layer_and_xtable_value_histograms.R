library(tidyverse)
library(magrittr)

setwd("//166.2.126.25/Treemap")


# Assemble complete target layer vectors by directing to target layer vectors of a given type, reading them, and concatenating
dc_vec<- do.call(c, lapply(list.files("./03_Outputs/09_Value_Comparison_X_table_Target_Layers/v2020/target_layer_vectors/", 
                                      recursive=T, pattern="dc.rds", full.names = T), readRDS))
dy_vec<- do.call(c, lapply(list.files("./03_Outputs/09_Value_Comparison_X_table_Target_Layers/v2020/target_layer_vectors/", 
                                      recursive=T, pattern="dy.rds", full.names = T), readRDS))
easting_vec<- do.call(c, lapply(list.files("./03_Outputs/09_Value_Comparison_X_table_Target_Layers/v2020/target_layer_vectors/", 
                                      recursive=T, pattern="easting.rds", full.names = T), readRDS))
elevation_vec<- do.call(c, lapply(list.files("./03_Outputs/09_Value_Comparison_X_table_Target_Layers/v2020/target_layer_vectors/", 
                                      recursive=T, pattern="elevation.rds", full.names = T), readRDS))
evc_vec<- do.call(c, lapply(list.files("./03_Outputs/09_Value_Comparison_X_table_Target_Layers/v2020/target_layer_vectors/", 
                                      recursive=T, pattern="evc.rds", full.names = T), readRDS))
evh_vec<- do.call(c, lapply(list.files("./03_Outputs/09_Value_Comparison_X_table_Target_Layers/v2020/target_layer_vectors/", 
                                      recursive=T, pattern="evh.rds", full.names = T), readRDS))
northing_vec<- do.call(c, lapply(list.files("./03_Outputs/09_Value_Comparison_X_table_Target_Layers/v2020/target_layer_vectors/", 
                                      recursive=T, pattern="northing.rds", full.names = T), readRDS))
prcp_vec<- do.call(c, lapply(list.files("./03_Outputs/09_Value_Comparison_X_table_Target_Layers/v2020/target_layer_vectors/", 
                                      recursive=T, pattern="prcp.rds", full.names = T), readRDS))
slope_vec<- do.call(c, lapply(list.files("./03_Outputs/09_Value_Comparison_X_table_Target_Layers/v2020/target_layer_vectors/", 
                                      recursive=T, pattern="slope.rds", full.names = T), readRDS))
srad_vec<- do.call(c, lapply(list.files("./03_Outputs/09_Value_Comparison_X_table_Target_Layers/v2020/target_layer_vectors/", 
                                      recursive=T, pattern="srad.rds", full.names = T), readRDS))
swe_vec<- do.call(c, lapply(list.files("./03_Outputs/09_Value_Comparison_X_table_Target_Layers/v2020/target_layer_vectors/", 
                                      recursive=T, pattern="swe.rds", full.names = T), readRDS))
tmax_vec<- do.call(c, lapply(list.files("./03_Outputs/09_Value_Comparison_X_table_Target_Layers/v2020/target_layer_vectors/", 
                                      recursive=T, pattern="tmax.rds", full.names = T), readRDS))
tmin_vec<- do.call(c, lapply(list.files("./03_Outputs/09_Value_Comparison_X_table_Target_Layers/v2020/target_layer_vectors/", 
                                      recursive=T, pattern="tmin.rds", full.names = T), readRDS))
vp_vec<- do.call(c, lapply(list.files("./03_Outputs/09_Value_Comparison_X_table_Target_Layers/v2020/target_layer_vectors/", 
                                      recursive=T, pattern="vp.rds", full.names = T), readRDS))
vpd_vec<- do.call(c, lapply(list.files("./03_Outputs/09_Value_Comparison_X_table_Target_Layers/v2020/target_layer_vectors/", 
                                      recursive=T, pattern="vpd.rds", full.names = T), readRDS))


# Load the complete x-table
xtable<- read.csv("./03_Outputs/06_Reference_Data/v2020/02_X_table_CONUS/x_table_complete_CONUS_2020.csv")

#make easting and northing / work with slope
xtable %<>%
  # calculate northing and easting from aspect
  dplyr::mutate(radians = (pi / 180) * ASPECT,
                NORTHING = cos(radians),
                EASTING = sin(radians)) %>%
  # convert slope from percent to degrees, to match target layer
  dplyr::mutate(SLOPE = atan(SLOPE / 100) * 180 / pi)

#Address no aspect issue by setting easting and northing to 0 anywhere with 0 slope and 0 aspect
xtable$EASTING[xtable$SLOPE == 0 & xtable$ASPECT == 0]<- 0
xtable$NORTHING[xtable$SLOPE == 0 & xtable$ASPECT == 0]<- 0




# Go through each variable and make density plot comparing x-table and target layer values
  # Because the target layer vectors have more values than the x-table, the x-table values get repeated. I don't think this is an issue

# dc----
dc_df<- as.data.frame(cbind(dc_vec, xtable$disturb_code))
names(dc_df)<- c("Target Layer", "X-Table")
#
xtable_min <- annotation_custom(grid::textGrob(label = paste0("X-Table Min = ",round(min(xtable$disturb_code),4)),
                                               x = unit(0.65, "npc"), y = unit(0.95, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
target_min <- annotation_custom(grid::textGrob(label = paste0("Target Min = ",round(min(dc_vec),4)),
                                               x = unit(0.65, "npc"), y = unit(0.9, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
#
xtable_max <- annotation_custom(grid::textGrob(label = paste0("X-Table Max = ",round(max(xtable$disturb_code),4)),
                                               x = unit(0.65, "npc"), y = unit(0.85, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
target_max <- annotation_custom(grid::textGrob(label = paste0("Target Max = ",round(max(dc_vec),4)),
                                               x = unit(0.65, "npc"), y = unit(0.8, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))


plot<- dc_df %>%
  pivot_longer(everything(), values_to="Value", names_to="Variable") %>% 
  ggplot() +
  geom_density(aes(group=Variable,x=Value, color=Variable), alpha=0.2, fill="grey75")+
  theme_bw(base_size=16)+
  labs(x="Disturbance Code",y="Density")+
  xtable_min+
  target_min+
  xtable_max+
  target_max


ggsave(paste0("./03_Outputs/09_Value_Comparison_X_table_Target_Layers/v2020/xtable_target_layer_comparison_plots/", "dc.jpg"), plot = plot, width = 10, height = 6, dpi=800)


# dy----
dy_df<- as.data.frame(cbind(dy_vec, xtable$disturb_year))
names(dy_df)<- c("Target Layer", "X-Table")
#
xtable_min <- annotation_custom(grid::textGrob(label = paste0("X-Table Min = ",round(min(xtable$disturb_year),4)),
                                               x = unit(0.65, "npc"), y = unit(0.95, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
target_min <- annotation_custom(grid::textGrob(label = paste0("Target Min = ",round(min(dy_vec),4)),
                                               x = unit(0.65, "npc"), y = unit(0.9, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
#
xtable_max <- annotation_custom(grid::textGrob(label = paste0("X-Table Max = ",round(max(xtable$disturb_year),4)),
                                               x = unit(0.65, "npc"), y = unit(0.85, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
target_max <- annotation_custom(grid::textGrob(label = paste0("Target Max = ",round(max(dy_vec),4)),
                                               x = unit(0.65, "npc"), y = unit(0.8, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))


plot<- dy_df %>%
  pivot_longer(everything(), values_to="Value", names_to="Variable") %>% 
  ggplot() +
  geom_density(aes(group=Variable,x=Value, color=Variable), alpha=0.2, fill="grey75")+
  theme_bw(base_size=16)+
  labs(x="Disturbance Year",y="Density")+
  xtable_min+
  target_min+
  xtable_max+
  target_max

ggsave(paste0("./03_Outputs/09_Value_Comparison_X_table_Target_Layers/v2020/xtable_target_layer_comparison_plots/", "dy.jpg"), plot = plot, width = 10, height = 6, dpi=800)


# easting----
easting_df<- as.data.frame(cbind(easting_vec, xtable$EASTING))
names(easting_df)<- c("Target Layer", "X-Table")
#
xtable_min <- annotation_custom(grid::textGrob(label = paste0("X-Table Min = ",round(min(xtable$EASTING),4)),
                                               x = unit(0.65, "npc"), y = unit(0.95, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
target_min <- annotation_custom(grid::textGrob(label = paste0("Target Min = ",round(min(easting_vec),4)),
                                               x = unit(0.65, "npc"), y = unit(0.9, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
#
xtable_max <- annotation_custom(grid::textGrob(label = paste0("X-Table Max = ",round(max(xtable$EASTING),4)),
                                               x = unit(0.65, "npc"), y = unit(0.85, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
target_max <- annotation_custom(grid::textGrob(label = paste0("Target Max = ",round(max(easting_vec),4)),
                                               x = unit(0.65, "npc"), y = unit(0.8, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))


plot<- easting_df %>%
  pivot_longer(everything(), values_to="Value", names_to="Variable") %>% 
  ggplot() +
  geom_density(aes(group=Variable,x=Value, color=Variable), alpha=0.2, fill="grey75")+
  theme_bw(base_size=16)+
  labs(x="Easting",y="Density")+
  xtable_min+
  target_min+
  xtable_max+
  target_max


ggsave(paste0("./03_Outputs/09_Value_Comparison_X_table_Target_Layers/v2020/xtable_target_layer_comparison_plots/", "easting.jpg"), plot = plot, width = 10, height = 6, dpi=800)

# elevation----
elevation_df<- as.data.frame(cbind(elevation_vec, xtable$ELEV))
names(elevation_df)<- c("Target Layer", "X-Table")
#
xtable_min <- annotation_custom(grid::textGrob(label = paste0("X-Table Min = ",round(min(xtable$ELEV),4)),
                                               x = unit(0.65, "npc"), y = unit(0.95, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
target_min <- annotation_custom(grid::textGrob(label = paste0("Target Min = ",round(min(elevation_vec),4)),
                                               x = unit(0.65, "npc"), y = unit(0.9, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
#
xtable_max <- annotation_custom(grid::textGrob(label = paste0("X-Table Max = ",round(max(xtable$ELEV),4)),
                                               x = unit(0.65, "npc"), y = unit(0.85, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
target_max <- annotation_custom(grid::textGrob(label = paste0("Target Max = ",round(max(elevation_vec),4)),
                                               x = unit(0.65, "npc"), y = unit(0.8, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))


plot<- elevation_df %>%
  pivot_longer(everything(), values_to="Value", names_to="Variable") %>% 
  ggplot() +
  geom_density(aes(group=Variable,x=Value, color=Variable), alpha=0.2, fill="grey75")+
  theme_bw(base_size=16)+
  labs(x="Elevation",y="Density")+
  xtable_min+
  target_min+
  xtable_max+
  target_max


ggsave(paste0("./03_Outputs/09_Value_Comparison_X_table_Target_Layers/v2020/xtable_target_layer_comparison_plots/", "elevation.jpg"), plot = plot, width = 10, height = 6, dpi=800)

# evc----
evc_df<- as.data.frame(cbind(evc_vec, xtable$canopy_cover))
names(evc_df)<- c("Target Layer", "X-Table")
#
xtable_min <- annotation_custom(grid::textGrob(label = paste0("X-Table Min = ",round(min(xtable$canopy_cover),4)),
                                               x = unit(0.65, "npc"), y = unit(0.95, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
target_min <- annotation_custom(grid::textGrob(label = paste0("Target Min = ",round(min(evc_vec),4)),
                                               x = unit(0.65, "npc"), y = unit(0.9, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
#
xtable_max <- annotation_custom(grid::textGrob(label = paste0("X-Table Max = ",round(max(xtable$canopy_cover),4)),
                                               x = unit(0.65, "npc"), y = unit(0.85, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
target_max <- annotation_custom(grid::textGrob(label = paste0("Target Max = ",round(max(evc_vec),4)),
                                               x = unit(0.65, "npc"), y = unit(0.8, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))


plot<- evc_df %>%
  pivot_longer(everything(), values_to="Value", names_to="Variable") %>% 
  ggplot() +
  geom_density(aes(group=Variable,x=Value, color=Variable), alpha=0.2, fill="grey75")+
  theme_bw(base_size=16)+
  labs(x="EVC",y="Density")+
  xtable_min+
  target_min+
  xtable_max+
  target_max

ggsave(paste0("./03_Outputs/09_Value_Comparison_X_table_Target_Layers/v2020/xtable_target_layer_comparison_plots/", "evc.jpg"), plot = plot, width = 10, height = 6, dpi=800)

# evh----
evh_df<- as.data.frame(cbind(evh_vec, xtable$canopy_height))
names(evh_df)<- c("Target Layer", "X-Table")
#
xtable_min <- annotation_custom(grid::textGrob(label = paste0("X-Table Min = ",round(min(xtable$canopy_height),4)),
                                               x = unit(0.65, "npc"), y = unit(0.95, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
target_min <- annotation_custom(grid::textGrob(label = paste0("Target Min = ",round(min(evh_vec),4)),
                                               x = unit(0.65, "npc"), y = unit(0.9, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
#
xtable_max <- annotation_custom(grid::textGrob(label = paste0("X-Table Max = ",round(max(xtable$canopy_height),4)),
                                               x = unit(0.65, "npc"), y = unit(0.85, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
target_max <- annotation_custom(grid::textGrob(label = paste0("Target Max = ",round(max(evh_vec),4)),
                                               x = unit(0.65, "npc"), y = unit(0.8, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))


plot<- evh_df %>%
  pivot_longer(everything(), values_to="Value", names_to="Variable") %>% 
  ggplot() +
  geom_density(aes(group=Variable,x=Value, color=Variable), alpha=0.2, fill="grey75")+
  theme_bw(base_size=16)+
  labs(x="EVH",y="Density")+
  xtable_min+
  target_min+
  xtable_max+
  target_max

ggsave(paste0("./03_Outputs/09_Value_Comparison_X_table_Target_Layers/v2020/xtable_target_layer_comparison_plots/", "evh.jpg"), plot = plot, width = 10, height = 6, dpi=800)

# northing----
northing_df<- as.data.frame(cbind(northing_vec, xtable$NORTHING))
names(northing_df)<- c("Target Layer", "X-Table")
#
xtable_min <- annotation_custom(grid::textGrob(label = paste0("X-Table Min = ",round(min(xtable$NORTHING),4)),
                                               x = unit(0.65, "npc"), y = unit(0.95, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
target_min <- annotation_custom(grid::textGrob(label = paste0("Target Min = ",round(min(northing_vec),4)),
                                               x = unit(0.65, "npc"), y = unit(0.9, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
#
xtable_max <- annotation_custom(grid::textGrob(label = paste0("X-Table Max = ",round(max(xtable$NORTHING),4)),
                                               x = unit(0.65, "npc"), y = unit(0.85, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
target_max <- annotation_custom(grid::textGrob(label = paste0("Target Max = ",round(max(northing_vec),4)),
                                               x = unit(0.65, "npc"), y = unit(0.8, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))


plot<- northing_df %>%
  pivot_longer(everything(), values_to="Value", names_to="Variable") %>% 
  ggplot() +
  geom_density(aes(group=Variable,x=Value, color=Variable), alpha=0.2, fill="grey75")+
  theme_bw(base_size=16)+
  labs(x="Northing",y="Density")+
  xtable_min+
  target_min+
  xtable_max+
  target_max


ggsave(paste0("./03_Outputs/09_Value_Comparison_X_table_Target_Layers/v2020/xtable_target_layer_comparison_plots/", "northing.jpg"), plot = plot, width = 10, height = 6, dpi=800)

# prcp----
prcp_df<- as.data.frame(cbind(prcp_vec, xtable$prcp))
names(prcp_df)<- c("Target Layer", "X-Table")
#
xtable_min <- annotation_custom(grid::textGrob(label = paste0("X-Table Min = ",round(min(xtable$prcp),4)),
                                               x = unit(0.65, "npc"), y = unit(0.95, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
target_min <- annotation_custom(grid::textGrob(label = paste0("Target Min = ",round(min(prcp_vec),4)),
                                               x = unit(0.65, "npc"), y = unit(0.9, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
#
xtable_max <- annotation_custom(grid::textGrob(label = paste0("X-Table Max = ",round(max(xtable$prcp),4)),
                                               x = unit(0.65, "npc"), y = unit(0.85, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
target_max <- annotation_custom(grid::textGrob(label = paste0("Target Max = ",round(max(prcp_vec),4)),
                                               x = unit(0.65, "npc"), y = unit(0.8, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))


plot<- prcp_df %>%
  pivot_longer(everything(), values_to="Value", names_to="Variable") %>% 
  ggplot() +
  geom_density(aes(group=Variable,x=Value, color=Variable), alpha=0.2, fill="grey75")+
  theme_bw(base_size=16)+
  labs(x="Precipitation",y="Density")+
  xtable_min+
  target_min+
  xtable_max+
  target_max


ggsave(paste0("./03_Outputs/09_Value_Comparison_X_table_Target_Layers/v2020/xtable_target_layer_comparison_plots/", "prcp.jpg"), plot = plot, width = 10, height = 6, dpi=800)


# slope----
slope_df<- as.data.frame(cbind(slope_vec, xtable$SLOPE))
names(slope_df)<- c("Target Layer", "X-Table")
#
xtable_min <- annotation_custom(grid::textGrob(label = paste0("X-Table Min = ",round(min(xtable$SLOPE),4)),
                                               x = unit(0.65, "npc"), y = unit(0.95, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
target_min <- annotation_custom(grid::textGrob(label = paste0("Target Min = ",round(min(slope_vec),4)),
                                               x = unit(0.65, "npc"), y = unit(0.9, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
#
xtable_max <- annotation_custom(grid::textGrob(label = paste0("X-Table Max = ",round(max(xtable$SLOPE),4)),
                                               x = unit(0.65, "npc"), y = unit(0.85, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
target_max <- annotation_custom(grid::textGrob(label = paste0("Target Max = ",round(max(slope_vec),4)),
                                               x = unit(0.65, "npc"), y = unit(0.8, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))


plot<- slope_df %>%
  pivot_longer(everything(), values_to="Value", names_to="Variable") %>% 
  ggplot() +
  geom_density(aes(group=Variable,x=Value, color=Variable), alpha=0.2, fill="grey75")+
  theme_bw(base_size=16)+
  labs(x="Slope",y="Density")+
  xtable_min+
  target_min+
  xtable_max+
  target_max

ggsave(paste0("./03_Outputs/09_Value_Comparison_X_table_Target_Layers/v2020/xtable_target_layer_comparison_plots/", "slope.jpg"), plot = plot, width = 10, height = 6, dpi=800)

# srad----
srad_df<- as.data.frame(cbind(srad_vec, xtable$srad))
names(srad_df)<- c("Target Layer", "X-Table")
#
xtable_min <- annotation_custom(grid::textGrob(label = paste0("X-Table Min = ",round(min(xtable$srad),4)),
                                               x = unit(0.65, "npc"), y = unit(0.95, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
target_min <- annotation_custom(grid::textGrob(label = paste0("Target Min = ",round(min(srad_vec),4)),
                                               x = unit(0.65, "npc"), y = unit(0.9, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
#
xtable_max <- annotation_custom(grid::textGrob(label = paste0("X-Table Max = ",round(max(xtable$srad),4)),
                                               x = unit(0.65, "npc"), y = unit(0.85, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
target_max <- annotation_custom(grid::textGrob(label = paste0("Target Max = ",round(max(srad_vec),4)),
                                               x = unit(0.65, "npc"), y = unit(0.8, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))


plot<- srad_df %>%
  pivot_longer(everything(), values_to="Value", names_to="Variable") %>% 
  ggplot() +
  geom_density(aes(group=Variable,x=Value, color=Variable), alpha=0.2, fill="grey75")+
  theme_bw(base_size=16)+
  labs(x="Solar Radiation",y="Density")+
  xtable_min+
  target_min+
  xtable_max+
  target_max


ggsave(paste0("./03_Outputs/09_Value_Comparison_X_table_Target_Layers/v2020/xtable_target_layer_comparison_plots/", "srad.jpg"), plot = plot, width = 10, height = 6, dpi=800)

# swe----
swe_df<- as.data.frame(cbind(swe_vec, xtable$swe))
names(swe_df)<- c("Target Layer", "X-Table")
#
xtable_min <- annotation_custom(grid::textGrob(label = paste0("X-Table Min = ",round(min(xtable$swe),4)),
                                               x = unit(0.65, "npc"), y = unit(0.95, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
target_min <- annotation_custom(grid::textGrob(label = paste0("Target Min = ",round(min(swe_vec),4)),
                                               x = unit(0.65, "npc"), y = unit(0.9, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
#
xtable_max <- annotation_custom(grid::textGrob(label = paste0("X-Table Max = ",round(max(xtable$swe),4)),
                                               x = unit(0.65, "npc"), y = unit(0.85, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
target_max <- annotation_custom(grid::textGrob(label = paste0("Target Max = ",round(max(swe_vec),4)),
                                               x = unit(0.65, "npc"), y = unit(0.8, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))


plot<- swe_df %>%
  pivot_longer(everything(), values_to="Value", names_to="Variable") %>% 
  ggplot() +
  geom_density(aes(group=Variable,x=Value, color=Variable), alpha=0.2, fill="grey75")+
  theme_bw(base_size=16)+
  labs(x="Snow Water Equivalent",y="Density")+
  xtable_min+
  target_min+
  xtable_max+
  target_max



ggsave(paste0("./03_Outputs/09_Value_Comparison_X_table_Target_Layers/v2020/xtable_target_layer_comparison_plots/", "swe.jpg"), plot = plot, width = 10, height = 6, dpi=800)

# tmax----
tmax_df<- as.data.frame(cbind(tmax_vec, xtable$tmax))
names(tmax_df)<- c("Target Layer", "X-Table")
#
xtable_min <- annotation_custom(grid::textGrob(label = paste0("X-Table Min = ",round(min(xtable$tmax),4)),
                                               x = unit(0.65, "npc"), y = unit(0.95, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
target_min <- annotation_custom(grid::textGrob(label = paste0("Target Min = ",round(min(tmax_vec),4)),
                                               x = unit(0.65, "npc"), y = unit(0.9, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
#
xtable_max <- annotation_custom(grid::textGrob(label = paste0("X-Table Max = ",round(max(xtable$tmax),4)),
                                               x = unit(0.65, "npc"), y = unit(0.85, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
target_max <- annotation_custom(grid::textGrob(label = paste0("Target Max = ",round(max(tmax_vec),4)),
                                               x = unit(0.65, "npc"), y = unit(0.8, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))


plot<- tmax_df %>%
  pivot_longer(everything(), values_to="Value", names_to="Variable") %>% 
  ggplot() +
  geom_density(aes(group=Variable,x=Value, color=Variable), alpha=0.2, fill="grey75")+
  theme_bw(base_size=16)+
  labs(x="Maximum Temperature",y="Density")+
  xtable_min+
  target_min+
  xtable_max+
  target_max


ggsave(paste0("./03_Outputs/09_Value_Comparison_X_table_Target_Layers/v2020/xtable_target_layer_comparison_plots/", "tmax.jpg"), plot = plot, width = 10, height = 6, dpi=800)


# tmin----
tmin_df<- as.data.frame(cbind(tmin_vec, xtable$tmin))
names(tmin_df)<- c("Target Layer", "X-Table")
#
xtable_min <- annotation_custom(grid::textGrob(label = paste0("X-Table Min = ",round(min(xtable$tmin),4)),
                                               x = unit(0.65, "npc"), y = unit(0.95, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
target_min <- annotation_custom(grid::textGrob(label = paste0("Target Min = ",round(min(tmin_vec),4)),
                                               x = unit(0.65, "npc"), y = unit(0.9, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
#
xtable_max <- annotation_custom(grid::textGrob(label = paste0("X-Table Max = ",round(max(xtable$tmin),4)),
                                               x = unit(0.65, "npc"), y = unit(0.85, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
target_max <- annotation_custom(grid::textGrob(label = paste0("Target Max = ",round(max(tmin_vec),4)),
                                               x = unit(0.65, "npc"), y = unit(0.8, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))


plot<- tmin_df %>%
  pivot_longer(everything(), values_to="Value", names_to="Variable") %>% 
  ggplot() +
  geom_density(aes(group=Variable,x=Value, color=Variable), alpha=0.2, fill="grey75")+
  theme_bw(base_size=16)+
  labs(x="Minimum Temperature",y="Density")+
  xtable_min+
  target_min+
  xtable_max+
  target_max


ggsave(paste0("./03_Outputs/09_Value_Comparison_X_table_Target_Layers/v2020/xtable_target_layer_comparison_plots/", "tmin.jpg"), plot = plot, width = 10, height = 6, dpi=800)

# vp----
vp_df<- as.data.frame(cbind(vp_vec, xtable$vp))
names(vp_df)<- c("Target Layer", "X-Table")
#
xtable_min <- annotation_custom(grid::textGrob(label = paste0("X-Table Min = ",round(min(xtable$vp),4)),
                                               x = unit(0.65, "npc"), y = unit(0.95, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
target_min <- annotation_custom(grid::textGrob(label = paste0("Target Min = ",round(min(vp_vec),4)),
                                               x = unit(0.65, "npc"), y = unit(0.9, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
#
xtable_max <- annotation_custom(grid::textGrob(label = paste0("X-Table Max = ",round(max(xtable$vp),4)),
                                               x = unit(0.65, "npc"), y = unit(0.85, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
target_max <- annotation_custom(grid::textGrob(label = paste0("Target Max = ",round(max(vp_vec),4)),
                                               x = unit(0.65, "npc"), y = unit(0.8, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))


plot<- vp_df %>%
  pivot_longer(everything(), values_to="Value", names_to="Variable") %>% 
  ggplot() +
  geom_density(aes(group=Variable,x=Value, color=Variable), alpha=0.2, fill="grey75")+
  theme_bw(base_size=16)+
  labs(x="Vapor Pressure",y="Density")+
  xtable_min+
  target_min+
  xtable_max+
  target_max

ggsave(paste0("./03_Outputs/09_Value_Comparison_X_table_Target_Layers/v2020/xtable_target_layer_comparison_plots/", "vp.jpg"), plot = plot, width = 10, height = 6, dpi=800)

# vpd----
vpd_df<- as.data.frame(cbind(vpd_vec, xtable$vpd))
names(vpd_df)<- c("Target Layer", "X-Table")
#
xtable_min <- annotation_custom(grid::textGrob(label = paste0("X-Table Min = ",round(min(xtable$vpd),4)),
                                               x = unit(0.65, "npc"), y = unit(0.95, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
target_min <- annotation_custom(grid::textGrob(label = paste0("Target Min = ",round(min(vpd_vec),4)),
                                               x = unit(0.65, "npc"), y = unit(0.9, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
#
xtable_max <- annotation_custom(grid::textGrob(label = paste0("X-Table Max = ",round(max(xtable$vpd),4)),
                                               x = unit(0.65, "npc"), y = unit(0.85, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))
target_max <- annotation_custom(grid::textGrob(label = paste0("Target Max = ",round(max(vpd_vec),4)),
                                               x = unit(0.65, "npc"), y = unit(0.8, "npc"), hjust=0,
                                               gp = grid::gpar(cex = 1)))


plot<- vpd_df %>%
  pivot_longer(everything(), values_to="Value", names_to="Variable") %>% 
  ggplot() +
  geom_density(aes(group=Variable,x=Value, color=Variable), alpha=0.2, fill="grey75")+
  theme_bw(base_size=16)+
  labs(x="Vapor Pressure Deficit",y="Density")+
  xtable_min+
  target_min+
  xtable_max+
  target_max



ggsave(paste0("./03_Outputs/09_Value_Comparison_X_table_Target_Layers/v2020/xtable_target_layer_comparison_plots/", "vpd.jpg"), plot = plot, width = 10, height = 6, dpi=800)


