# Known Bugs and Fixes

This document tracks known bugs, gotchas, tricky edge cases, and their corresponding fixes across the GTAC Treemap repository to retain knowledge across sessions.

## 2026-05-15 - Zonal Mosaic Fails on Corrupt TIFF + INT2S Overflow Warning

- Symptoms:
	- `TIFFFetchDirectory` / `TIFFReadDirectory` errors for a zonal imputation TIFF (example: `z37_2023_Production_final_Imputation.tif`).
	- `[vrt] vrt did not use 1 of the 66 files` during mosaic build.
	- `[writeRaster] detected values outside of the limits of datatype INT2S` when exporting mosaic.
	- `dir.create(... already exists)` warning on reruns.
- Root cause:
	- At least one zonal TIFF was unreadable/corrupt on disk.
	- Mosaic export used `INT2S`, but imputation IDs exceeded signed 16-bit range.
	- Output directory creation did not suppress existing-directory warnings.
- Fix implemented:
	- In `gtac_production_scripts/03_Imputation/05_mosaic_zonal_imputation_rasters_and_build_raster_attribute_table.R`:
		- Added pre-validation of raster readability and skipped unreadable files with messages.
		- Switched mosaic output datatype to `INT4S`.
		- Set `dir.create(..., showWarnings = FALSE)` for idempotent reruns.
		- Added explicit checks for zero inputs / zero valid inputs.
- Follow-up:
	- Recreate any skipped zonal TIFF(s) from `03_Imputation` assembly outputs before final production delivery if full-zone coverage is required.
