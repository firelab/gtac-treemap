# Build EVT_GP rasters from EVT_GP_remap
# 
# This script takes the evt_gp_remap.tif file and evt_gp_remap_table.csv file
# and creates an evt_gp.tif file where the pixel values are the original EVT_GP codes
# 
# Written by: Lila Leatherman (lila.leatherman@usda.gov)
# Last Updated: 1/22/2026

#####################################################################################
# Script inputs
#####################################################################################

# define year
year_input <- "2023"

# define project area
study_area <- "CONUS"

# List of zones to process (or "all" for all zones)
zones_to_process <- "all"
#zones_to_process <- c(seq(38,99, 1))

################################################################
# Load Library
################################################################

# Load TreeMap script library
this_proj = this.path::this.proj()
lib_path = glue::glue("{this_proj}/gtac_production_scripts/00_Library/treeMapLib.R")
source(lib_path)

# Load project inputs for target data
targetDataProjectInputs(year_input = year_input,
                        study_area = study_area)

# Get list of zones
lf_zones <- terra::vect(lf_zones_path)
lf_zone_nums <- sort(lf_zones$ZONE_NUM)

# Determine which zones to process
if (length(zones_to_process) == 1 && zones_to_process == "all") {
  zones_list <- lf_zone_nums
} else {
  zones_list <- zones_to_process
}

message(glue::glue("Processing {length(zones_list)} zones"))

################################################################
# Process each zone
################################################################

for (zone_input in zones_list) {
  
  message(glue::glue("\n========================================"))
  message(glue::glue("Processing zone {zone_input}"))
  message(glue::glue("========================================"))
  
  # Load zone-specific inputs
  targetDataZonalInputs(zone_input)
  
  # Define file paths
  evt_gp_remap_tif <- glue::glue("{target_dir_mask_z}/evt_gp_remap.tif")
  evt_gp_remap_csv <- glue::glue("{evt_gp_remap_table_path}/evt_gp_remap.csv")
  evt_gp_output <- glue::glue("{target_dir_mask_z}/evt_gp.tif")
  
  # Check if files exist
  if (!file.exists(evt_gp_remap_tif)) {
    warning(glue::glue("Zone {zone_input}: evt_gp_remap.tif not found at {evt_gp_remap_tif}. Skipping..."))
    next
  }
  
  if (!file.exists(evt_gp_remap_csv)) {
    warning(glue::glue("Zone {zone_input}: evt_gp_remap_table.csv not found at {evt_gp_remap_csv}. Skipping..."))
    next
  }
  
  # Load the remap raster
  message("  Loading evt_gp_remap.tif...")
  evt_gp_remap_rast <- terra::rast(evt_gp_remap_tif)
  
  # Load the remap table
  message("  Loading evt_gp_remap_table.csv...")
  remap_table <- read.csv(evt_gp_remap_csv)
  
  # Create reclassification matrix: from EVT_GP_remap back to EVT_GP
  # Format: [from_value, to_value]
  reclass_matrix <- remap_table[, c("EVT_GP_remap", "EVT_GP")]
  
  message(glue::glue("  Reclassifying {nrow(reclass_matrix)} values..."))
  message("  First few remaps:")
  print(head(reclass_matrix))
  
  # Reclassify the raster
  evt_gp_rast <- terra::classify(evt_gp_remap_rast, reclass_matrix)
  
  # Write the output
  message(glue::glue("  Writing evt_gp.tif to {evt_gp_output}..."))
  terra::writeRaster(evt_gp_rast, evt_gp_output, datatype = "INT2U", overwrite = TRUE)
  
  message(glue::glue("  ✓ Zone {zone_input} complete"))
  
  # Clear memory
  rm(evt_gp_remap_rast, evt_gp_rast)
  gc()
}

message("\n========================================")
message("All zones processed successfully!")
message("========================================")
