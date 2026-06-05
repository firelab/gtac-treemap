# Run zonal accuracy aggregation for a single year/model run.
# This script reads zonal evaluation RDS files and writes single-year CSV outputs.

# Inputs
######################################################
year <- 2023
study_area <- "CONUS"

# Project names are tried in order until a matching eval file is found.
# Fallback names are used only for locating evaluation files.
# The primary project is used for the main raster/zonal output paths.
primary_project_name_suffix <- "Production_rerun_final_zone_imputations"
fallback_project_name_suffixes <- c(
  "Production_rerun_w2022model_optimzed_take2",
  "Production"
)

zones <- "all" # options: "all" or numeric vector, e.g., c(1, 2, 3)
response_vars <- c("evc", "evh", "evt_gp", "disturb_code_bin")
eval_vars <- c(response_vars, "disturb_code", "disturb_year")
eval_types <- c("TargetLayerComparison")

# Load libraries and shared functions
######################################################
lib_path <- glue::glue("{this.path::this.proj()}/gtac_production_scripts/00_Library/treeMapLib.R")
source(lib_path)

assembly_lib_path <- glue::glue("{this.path::this.proj()}/gtac_production_scripts/00_Library/zonal_accuracy_assembly_lib.R")
source(assembly_lib_path)

# r_path = glue::glue("{home_dir}03_Outputs/07_Projects/2023/2023_Production_rerun_final_zone_imputations/04_Mosaic_assembled_model_outputs/TreeMap2023_CONUS.tif")

# r <- terra::rast(r_path)

# dbf_path <- gsub(".tif$", ".tif.vat.dbf", r_path)
# rat <- foreign::read.dbf(dbf_path)
# str(rat)
# Run single-year export
######################################################
result <- run_single_year_accuracy(
  year = year,
  primary_project_name_suffix = primary_project_name_suffix,
  fallback_project_name_suffixes = fallback_project_name_suffixes,
  study_area = study_area,
  zones = zones,
  response_vars = response_vars,
  eval_vars = eval_vars,
  eval_types = eval_types,
  home_dir = home_dir
)

message("Wrote single-year accuracy outputs:")
print(result$output_paths)
