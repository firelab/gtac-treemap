# Agent Context: `gtac_production_scripts`

This document serves as deep-dive contextual memory for AI assistants operating on the GTAC Treemap project, primarily detailing the data flow, architecture, and quirks of the `gtac_production_scripts` R pipeline.

## Pipeline Overview
The `gtac_production_scripts` pipeline is responsible for constructing "Treemap," a national spatial surface predicting detailed forest attributes by matching pixel-level environmental variables (targets) to field plot data (references) using statistical imputation. 

The pipeline runs sequentially:

### 1. 00_Library
- Contains sourceable scripts with helper functions.
- `treemapLib.R`, `impute_row.R`: Heavy lifting for the imputation looping logic.
- `targetDataProjectInputs.R`, `targetDataZonalInputs.R`: Functions to ingest structured parameters (`.RDS` files).

### 2. 01_Target_Data_Prep
- **Inputs**: Raw topographic, disturbance (LCMS/LF), climatic (Daymet), and vegetation spatial layers.
- **Actions**: Masks, crops, resamples, and standardizes these to the target mapping zones. 
- Notable Subfolders: `/Climate_data_prep/`, `/Disturbance_task_scripts/`.
- Param files (`v2016_GTAC_target_data_inputs.RDS`, etc.) are held in `/params/`.

### 3. 02_Reference_Data_Prep
- **Inputs**: FIA (Forest Inventory and Analysis) databases.
- **Actions**: Calculates and compiles species, age, basal area, and structural variables into 'X-tables' (predictor tables) and reference response sets for modeling.

### 4. 03_Imputation
- This is the heavy computation stage.
- **Params**: Reads imputation settings from `.RDS` parameter files in the `/params/` subfolder (e.g., `2023_Production_imputation_inputs.RDS`).
- **Core Scripts**:
  - `01_build_imputation.R`: Initializes the models (using Random Forests/k-NN via packages like `yaImpute`).
  - `02_run_imputation.R`: Runs the models spatially across rasters.
  - `03_assemble_imputation_rasters.R`: Aggregates the predicted tiles into coherent maps.

### 5. 04_Evaluation
- Conducts automated model assessments.
- `01_target_layer_comparison.R`, `02_oob_evaluation.R`, `03_cross_validation.R`.
- Contains Rmd scripts to autogenerate zonal evaluation reports.

### 6. 05_Non_production & 06_Alternate_Imputations
- `05_Non_production`: For ad-hoc querying, formatting (RDS to Word), investigating specific errors (CV/OOB investigations), and checking edge cases.
- `06_Alternate_Imputations`: Used for running side-by-side comparisons of algorithmic tweaks, such as "ridealong" variables vs standard variables, or analyzing stochastic variance across model iterations.

### 7. 07_Assembly
- Finalizes mapping zones into contiguous national outputs.
- Adds categorical descriptive fields to the final `.tif` files by generating Raster Attribute Tables (RATs) in script `01_build_imputation_raster_attribute_tables.R`.
- Merges broad cross-validation stats up to a national scale.

## Pre-existing Documentation Note
If you need specifics on historical naming conventions, actual path targets for the HPC or drives, consult files in `gtac_production_scripts/Documentation/`:
- `Folder Structure - Input Data.md`
- `Folder Structure - Outputs from Imputation.md`
- `Methods.md`