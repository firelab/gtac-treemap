library(tidyverse)
tile_dir = "//166.2.126.25/TreeMap/03_Outputs/07_Projects/2023_Production_rerun_w2022model_k2/01_Raw_model_outputs//z08/raster/tiles/"


tile_n = 1

# list files in tile dir
tile_list <- list.files(path = tile_dir, pattern = "*.tif$", recursive = TRUE, full.names = TRUE)

t1k1 = terra::rast(tile_list[str_detect(tile_list, "tile1_k1")])
t1k2 = terra::rast(tile_list[str_detect(tile_list, "tile1_k2")])

diff = t1k1 - t1k2

plot(diff)
head(freq(diff))

