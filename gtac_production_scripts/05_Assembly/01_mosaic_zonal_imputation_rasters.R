library(terra)
library(foreign)
library(glue)

# Set Inputs
# --------------------------------------------------#

home_dir <- "//166.2.126.25/TreeMap/"
year <- 2023
study_area <- "CONUS"
project_name <- glue::glue("{year}_Production_rerun_final_zone_imputations")
fallback_project_name <- glue::glue("{year}_Production")

#set path to assembled rasters
assembled_dir <- glue::glue("{home_dir}03_Outputs/07_Projects/{year}/{project_name}/02_Assembled_model_outputs/")
fallback_assembled_dir <- glue::glue("{home_dir}03_Outputs/07_Projects/{fallback_project_name}/02_Assembled_model_outputs/")

# set paths to where output data will live
mosaic_dir <- glue::glue("{home_dir}03_Outputs/07_Projects/{year}/{project_name}/04_Mosaic_assembled_model_outputs")

vrt_path <- glue::glue("{mosaic_dir}/TreeMap{year}_{study_area}_vrt.vrt")
tif_path <- glue::glue("{mosaic_dir}/TreeMap{year}_{study_area}.tif")
dbf_path <- glue::glue("{tif_path}.vat.dbf")
overwrite_dbf <- TRUE # set to TRUE to overwrite existing dbf if it exists, FALSE to error if dbf already exists at output path

# set path to RAT csv - with remaining attributes
attribute_table_path <- glue::glue("{home_dir}03_Outputs/06_Reference_Data/v{year}/03_Raster_attributes/TM{year}_RAT_tmid_contValues.csv") 

# all CONUS zones-- does not include zone 11 which does not exist
zones_list <- c(seq(from = 1, to = 10, by = 1), 
               seq(from = 12, to = 66, by = 1),
               98, 99)

# Make folder for mosaicked model outputs
dir.create(mosaic_dir, recursive = TRUE, showWarnings = FALSE)

output_file_pattern <- "(Production(_final)?_Imputation|z[0-9]{1,2}_[0-9]{4}_Production_rerun_w2022model_hybrid_imputation)(\\.tif)?$"

# Write helper functions
#-----------------------------------------------#

# Proactively filter out unreadable/corrupt inputs to prevent vrt/write failures.
raster_is_readable <- function(path) {
    tryCatch({
        r <- terra::rast(path)
        if (terra::nlyr(r) == 0) {
            return(FALSE)
        }
        # Force an actual disk read; some corrupt TIFFs pass rast() but fail on value access.
        terra::readStart(r)
        on.exit(terra::readStop(r), add = TRUE)
        v <- terra::readValues(r, row = 1, nrows = 1, col = 1, ncols = 1, mat = FALSE)
        is.atomic(v)
    }, error = function(e) {
        message(glue::glue("Skipping unreadable raster: {path}"))
        message(glue::glue("  GDAL/terra error: {e$message}"))
        FALSE
    })
}

find_imputation_rasters <- function(directory, pattern) {
    list.files(directory,
               pattern = pattern,
               full.names = TRUE,
               recursive = TRUE,
               ignore.case = TRUE)
}

select_zone_raster <- function(zone_num, primary_rasters, fallback_rasters) {
    zone_id <- sprintf("z%02d", zone_num)
    zone_pattern <- glue::glue("z0*{zone_num}([^0-9]|$)")

    zone_primary <- primary_rasters[grepl(zone_pattern, basename(primary_rasters), ignore.case = TRUE, perl = TRUE)]
    zone_fallback <- fallback_rasters[grepl(zone_pattern, basename(fallback_rasters), ignore.case = TRUE, perl = TRUE)]

    selected_path <- NA_character_
    selected_source <- "missing"

    # Select exactly one readable primary raster per zone when available.
    if (length(zone_primary) > 0) {
        for (p in zone_primary) {
            if (raster_is_readable(p)) {
                selected_path <- p
                selected_source <- "primary"
                if (length(zone_primary) > 1) {
                    message(glue::glue("Zone {zone_id}: selected primary raster: {p}"))
                }
                break
            }
        }
    }

    # Only use fallback rasters if no readable primary raster exists for this zone.
    if (is.na(selected_path) && length(zone_fallback) > 0) {
        for (fp in zone_fallback) {
            if (raster_is_readable(fp)) {
                selected_path <- fp
                selected_source <- "fallback"
                message(glue::glue("Using fallback raster for {zone_id}: {fp}"))
                break
            }
        }
    }

    data.frame(
        zone = zone_id,
        source = selected_source,
        selected_file = selected_path,
        stringsAsFactors = FALSE
    )
}

build_zone_selection_table <- function(zones, primary_rasters, fallback_rasters) {
    selections <- lapply(zones, function(z) select_zone_raster(z, primary_rasters, fallback_rasters))
    do.call(rbind, selections)
}

# Load rasters
#-----------------------------------------------#

# Direct to all zonal imputation rasters
imputation_rasters <- find_imputation_rasters(assembled_dir, output_file_pattern)
fallback_rasters <- find_imputation_rasters(fallback_assembled_dir, output_file_pattern)

if (length(imputation_rasters) == 0) {
    message(glue::glue("No primary imputation rasters found under: {assembled_dir}. Will attempt fallback rasters by zone."))
}

if (length(imputation_rasters) == 0 && length(fallback_rasters) == 0) {
    stop(glue::glue("No imputation rasters found under either {assembled_dir} or {fallback_assembled_dir}"))
}

zone_selection_table <- build_zone_selection_table(zones_list, imputation_rasters, fallback_rasters)

message("Zone-to-file selection table:")
print(zone_selection_table, row.names = FALSE, right = FALSE)

valid_imputation_rasters <- zone_selection_table$selected_file[!is.na(zone_selection_table$selected_file)]
skipped_zones <- zone_selection_table$zone[is.na(zone_selection_table$selected_file)]

if (length(skipped_zones) > 0) {
   stop(glue::glue("No readable raster found for {length(skipped_zones)} zone(s) after fallback attempts. Cannot build mosaic. Zones: {paste(skipped_zones, collapse = ', ')}"))
}

if (length(valid_imputation_rasters) == 0) {
    stop("No readable imputation rasters were found. Cannot build mosaic.")
}


message(glue::glue("Successfully validated {length(valid_imputation_rasters)} imputation rasters for mosaicking."))


# Build and export mosaic
#-----------------------------------------------#
message("Proceeding with mosaic creation...")
# Make a VRT and assemble a complete, mosaicked tif
terra::vrt(valid_imputation_rasters, vrt_path, overwrite = TRUE)

# Load the VRT
imputation <- rast(vrt_path)
# Save the VRT as a tif
writeRaster(imputation, tif_path, datatype = "INT4S", overwrite = TRUE,
            gdal=c("COMPRESS=LZW"))

message("Mosaicked imputation raster saved to: ", tif_path)

# Delete the VRT
file.remove(vrt_path)


