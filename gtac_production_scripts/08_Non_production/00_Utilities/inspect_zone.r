library(terra)
library(tidyverse)
library(magrittr)
library(tidyterra)
library(glue)
library(ggplot2)

# load library 
this_proj = this.path::this.proj()
lib_path = glue::glue("{this_proj}/gtac_production_scripts/00_Library/treeMapLib.R")
source(lib_path)

zone = 8
zone_zero = sprintf("%02d", zone)
project_name = "2023_Production_rerun_w2022model"
home_dir = "//166.2.126.25/TreeMap/03_Outputs/"
target_data_version = "v2023"

# set input data paths
#----------------------------------------#
# rasters
raster_path = glue::glue("{home_dir}07_Projects/{year}/{project_name}/02_Assembled_model_outputs/z{zone_zero}/01_Imputation/z{zone_zero}_{project_name}_Imputation.tif")
#raster_path2 = "//166.2.126.25/TreeMap/03_Outputs/07_Projects/2023_Production_rerun_w2022model/02_Assembled_model_outputs/z08/01_Imputation/z08_2023_Production_rerun_w2022model_Imputation.tif"

# target data
target_dir = glue::glue("{home_dir}05_Target_Rasters/{target_data_version}/masked/z{zone_zero}")

# load data
#-------------------------------------------#
ras <- terra::rast(raster_path)

# list raster files
flist_tif <- list.files(path = target_dir, pattern = "*.tif$", recursive = TRUE, full.names = TRUE)

# vars to check: 
vars_to_check <- c("evc", "evh", "evt_name", "evt_gp", "disturb_code", "disturb_year")

# filter list to those that contain the vars of interest
flist_tif_check <- flist_tif[str_detect(flist_tif, paste(vars_to_check, collapse = "|"))]

# load rasters using custom function
rs2 <- load_and_name_rasters(flist_tif_check)

# what are target data values where imputed raster is NA? get a freq table of target data values where imputed raster is NA

# check that imputed output has the same number of data pixels as the target layers
#-------------------------------------------------------------------#
# only checking one layer in the target data stack because at this point, they are ostensibly the same
px_imputed <- global(ras, fun = "notNA")[[1]]
px_target <- global(rs2[[1]], fun = "notNA")[[1]]

na_imputed <- global(ras, fun = "isNA")[[1]]
na_target <- global(rs2[[1]], fun = "isNA")[[1]]

print(glue::glue("# of Valid pixels: 
                  Imputed: {px_imputed}
                  Target: {px_target}"))

print(glue::glue("# of NA pixels: 
                  Imputed: {na_imputed}
                  Target: {na_target}"))

# Create a mask that is 1 where the imputed raster is NA, and 0 otherwise
na_mask <- is.na(ras)

# Mask the target data (rs2) keeping only the pixels where ras is NA
target_data_where_na <- terra::mask(rs2, na_mask, maskvalues = -9999)

plot(ras)
plot(target_data_where_na)

# Generate the frequency table for the masked target data
na_freq_table <- terra::freq(target_data_where_na)

layer_names <- data.frame(cbind(names(target_data_where_na), 1:nlyr(target_data_where_na)))
colnames(layer_names) <- c("layer_name", "code")
layer_names$code <- as.numeric(layer_names$code)

# Join layer names to the frequency table
na_freq_table_out <- na_freq_table %>%
  left_join(layer_names, by = c("layer" = "code")) %>%
  select(layer_name, value, count)

print(head(na_freq_table_out, n = 20))

na_freq_table_out %>%
filter(layer_name != "evt_name") %>%
  ggplot() +
  geom_bar(aes(x = value, y = count), stat = "identity") +
  facet_wrap(~ layer_name, scales = "free") +
  ggtitle(glue::glue("z{zone_zero} Frequency of target values where imputed raster is NA")) +
  xlab("Value") +
  theme_minimal() -> p 

path_out <- glue::glue("D:/tmp/TreeMap/z{zone_zero}_inspect_whereImputationNA_faceted.png")
ggsave(path_out, plot = p, width = 12, height = 8)

na_freq_table_out %>%
  filter(layer_name == "evt_name") %>%
  ggplot() +
  geom_bar(aes(x = value, y = count), stat = "identity") +
  ggtitle(glue::glue("z{zone_zero} Frequency of evt_name values where imputed raster is NA")) +
  xlab("Value") +
  theme_minimal() +
  # turn x axis labels to vertical 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) -> p_evt_name
path_out <- glue::glue("D:/tmp/TreeMap/z{zone_zero}_inspect_whereImputationNA_evt_name.png")
ggsave(path_out, plot = p_evt_name, width = 12, height = 8)
