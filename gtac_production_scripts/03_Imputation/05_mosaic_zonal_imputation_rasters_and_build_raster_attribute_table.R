library(terra)
library(foreign)
library(glue)

home_dir <- "//166.2.126.25/TreeMap/"

year <- 2023
project_name <- glue::glue("{year}_Production_rerun_final_zone_imputations")
fallback_project_name <- glue::glue("{year}_Production")
#set path to assembled rasters
assembled_dir <- glue::glue("{home_dir}03_Outputs/07_Projects/{project_name}/02_Assembled_model_outputs/")
mosaic_dir <- glue::glue("{home_dir}03_Outputs/07_Projects/{project_name}/04_Mosaic_assembled_model_outputs")
fallback_assembled_dir <- glue::glue("{home_dir}03_Outputs/07_Projects/{fallback_project_name}/02_Assembled_model_outputs/")
vrt_path <- glue::glue("{mosaic_dir}/imputation_vrt.vrt")
tif_out_path <- glue::glue("{mosaic_dir}/imputation.tif")
dbf_out_path <- glue::glue("{tif_out_path}.vat.dbf")

# Make folder for mosaicked model outputs
dir.create(mosaic_dir, recursive = TRUE, showWarnings = FALSE)

output_file_pattern <- "Production(_final)?_Imputation\\.tif$"

#Direct to all zonal imputation rasters
imputation_rasters<- list.files(assembled_dir,
                                pattern = "Production_final_Imputation.tif$|Production_Imputation.tif$", full.names = TRUE, recursive = TRUE)

fallback_rasters <- list.files(fallback_assembled_dir,
                               pattern = "Production_Imputation.tif$|Production_final_Imputation.tif$", full.names = TRUE, recursive = TRUE)

if (length(imputation_rasters) == 0) {
    stop(glue::glue("No imputation rasters found under: {assembled_dir}"))
}

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

extract_zone_id <- function(path) {
    m <- regexpr("z[0-9]{1,2}", path, ignore.case = TRUE)
    if (m[1] == -1) return(NA_character_)
    tolower(regmatches(path, m))
}

valid_imputation_rasters <- character(0)
skipped_rasters <- character(0)

for (p in imputation_rasters) {
    if (raster_is_readable(p)) {
        valid_imputation_rasters <- c(valid_imputation_rasters, p)
    } else {
        zone_id <- extract_zone_id(p)
        fallback_match <- character(0)

        if (!is.na(zone_id) && length(fallback_rasters) > 0) {
            fallback_match <- fallback_rasters[grepl(zone_id, fallback_rasters, ignore.case = TRUE)]
        }

        fallback_used <- FALSE
        if (length(fallback_match) > 0) {
            for (fp in fallback_match) {
                if (raster_is_readable(fp)) {
                    valid_imputation_rasters <- c(valid_imputation_rasters, fp)
                    message(glue::glue("Using fallback raster for {zone_id}: {fp}"))
                    fallback_used <- TRUE
                    break
                }
            }
        }

        if (!fallback_used) {
            skipped_rasters <- c(skipped_rasters, p)
        }
    }
}

if (length(skipped_rasters) > 0) {
   stop(glue::glue("Skipped {length(skipped_rasters)} raster(s) after fallback attempts. Cannot build mosaic. Skipped rasters: {paste(skipped_rasters, collapse = ', ')}"))
}

if (length(valid_imputation_rasters) == 0) {
    stop("No readable imputation rasters were found. Cannot build mosaic.")
}


message(glue::glue("Successfully validated {length(valid_imputation_rasters)} imputation rasters for mosaicking."))
message("Proceeding with mosaic creation...")
# Make a VRT and assemble a complete, mosaicked tif
terra::vrt(valid_imputation_rasters, vrt_path, overwrite = TRUE)

# Load the VRT
imputation <- rast(vrt_path)
# Save the VRT as a tif
writeRaster(imputation, tif_out_path, datatype = "INT4S", overwrite = TRUE,
            gdal=c("COMPRESS=LZW"))

message("Mosaicked imputation raster saved to: ", tif_out_path)

# Delete the VRT
file.remove(vrt_path)


# Reload imputation from the tif output
rm(imputation)
imputation <- rast(tif_out_path)

# Build and save raster attribute table
f<- freq(imputation)[,c(2:3)]
names(f)<-c("Value","Count")

#Write as dbf / attribute table
if (file.exists(dbf_out_path)) {
    file.remove(dbf_out_path)
}

foreign::write.dbf(f, dbf_out_path)

message("Raster attribute table saved to: ", dbf_out_path)

