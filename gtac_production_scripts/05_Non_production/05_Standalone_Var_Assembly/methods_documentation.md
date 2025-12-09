# TreeMap Volume Attribute Assembly Workflow

This document outlines the workflow for calculating, rasterizing, and mosaicking new Forest Inventory and Analysis (FIA) volume attributes for the TreeMap project. The process is divided into three stages, each handled by a specific R script.

## Workflow Overview

1.  **Attribute Calculation:** Calculate new volume metrics from raw FIA database tables and link them to TreeMap IDs.
2.  **Zonal Rasterization:** Create raster layers for these attributes for each TreeMap processing zone.
3.  **National Assembly:** Mosaic the zonal rasters into a seamless CONUS-wide product.

---

## Detailed Methods

### 1. Attribute Calculation
**Script:** `01_calc_new_FIA_vars.R`

**Purpose:** 
To query the FIA database for specific tree-level volume data, aggregate it to the plot level, and merge these new attributes with the existing TreeMap Raster Attribute Table (RAT).

**Key Steps:**
*   **Data Loading:** Connects to the FIA SQLite database (`SQLite_FIADB_ENTIRE.db`) and loads the existing TreeMap RAT.
*   **Querying:** Executes SQL queries to join `COND` and `TREE` tables, filtering by inventory years (1999-2022) and specific plot CNs present in the RAT.
*   **Variable Calculation:** Calculates four primary volume metrics by summing `VOLCFNET` (Net cubic-foot volume) or `VOLCFSND` (Sound cubic-foot volume) multiplied by `TPA_UNADJ` (Trees per acre unadjusted).
    *   Filters are applied based on `COND_STATUS_CD`, `STATUSCD` (live/dead), and `STANDING_DEAD_CD`.
*   **Unit Conversion:** Converts per-acre volume estimates to per-pixel estimates using a conversion factor (900 sqm/pixel * 0.000247104 acres/sqm).
*   **RAT Update:** Joins the calculated attributes back to the original RAT based on the Plot CN (`PLT_CN`), ensuring every TreeMap ID (`TM_ID`) has associated volume data (filling missing values with 0).
*   **Export:** Saves the updated RAT as `TreeMap_RAT_SOUNDVOLUME.csv`.

### 2. Zonal Rasterization
**Script:** `02_new_fia_vars_to_raster.R`

**Purpose:** 
To generate spatial raster layers for the new attributes by mapping the values from the updated RAT to the imputed TreeMap rasters for each zone.

**Key Steps:**
*   **Setup:** Defines the zones to process and the attributes to export (e.g., `VOLCFSND_L_PX`, `VOLCFSND_D_PX`).
*   **Iteration:** Loops through each specified zone (e.g., `z01`, `z02`...).
*   **Raster Loading:** Loads the raw imputed raster (containing `TM_ID` values) for the current zone.
*   **Attribute Mapping:** Uses the `assembleExport` function (from `treeMapLib.R`) to replace `TM_ID` values in the raster with the corresponding attribute values from the `TreeMap_RAT_SOUNDVOLUME.csv` lookup table.
*   **Export:** Writes out individual GeoTIFF files for each attribute within the zone's output directory.

### 3. National Assembly
**Script:** `03_assemble_regions_to_CONUS.R`

**Purpose:** 
To combine the individual zonal rasters into a single, continuous mosaic covering the Conterminous United States (CONUS).

**Key Steps:**
*   **File Discovery:** Scans the zonal output directories to find all TIFF files corresponding to a specific attribute (e.g., all `VOLCFSND_L_PX.tif` files).
*   **Virtual Mosaic:** Creates a Virtual Raster (VRT) file that references all zonal files, effectively stitching them together without duplicating data.
*   **Raster Writing:** Exports the VRT to a physical GeoTIFF file (`_CONUS.tif`), applying LZW compression to reduce file size.
*   **Cleanup:** Removes the temporary VRT file.
