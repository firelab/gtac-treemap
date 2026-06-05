# GTAC Treemap - Overview & Agent Guidance
*Last Updated: 2026-05-11*

This is the centralized agent instruction document for the `gtac-treemap` repository. 

## About this Repository
The `gtac-treemap` repository contains scripts and pipelines used to generate and deliver the "Treemap" geospatial forest layer produced by the Geospatial Technology and Applications Center (GTAC) and Rocky Mountain Research Station (RMRS). It covers data preparation, large-scale spatial imputation, statistical evaluation, and final data delivery tools.

*Note: For now, the agent guidance is focused strictly on the `gtac_production_scripts` pipeline. Instructions for other folders will be built up over time.*

## Structure Highlight: `gtac_production_scripts`
The `gtac_production_scripts` folder uses an R-based machine learning pipeline (likely using *k*-NN or Random Forest frameworks via the `yaImpute` package or similar). It executes sequentially through numerically prefixed folders:
- **00_Library**: Core R functions and reusable libraries.
- **01_Target_Data_Prep**: Geo-processing of predictor variables (LANDFIRE, Daymet climatic data, Disturbance layers).
- **02_Reference_Data_Prep**: Scraping, cleaning, and formatting FIA (Forest Inventory and Analysis) field plot data as training reference sets.
- **03_Imputation**: Execution of the imputation workflows across geographic zones.
- **04_Evaluation**: Statistical and spatial evaluations (Out-of-Bag stats, cross-validation, plot comparison).
- **05_Non_production**: Scratch scripts, utilities, and focused exploratory scripts.
- **06_Alternate_Imputations**: Testing iterations (e.g., stochasticity impacts, ridealong fixes).
- **07_Assembly**: Mosaicing outputs, building Raster Attribute Tables, and assembling national map products.
- **Documentation**: Includes further details on directory mappings and technical methods.

## Agent Guidelines
1. **geeViz**: When working with geospatial data, utilize the geeViz MCP server. See additional instructions in `docs/geeviz-mcp-instructions.md`.
1. **Tooling Prefixing**: When modifying production logic in `gtac_production_scripts`, remember these are primarily `R` datasets (`.RDS`, `.RData`) and geospatial raster flows (`.tif`). 
2. **Follow Sequence**: When debugging imputation bugs, agents must track data flow sequentially from `00_Library` → `01_Target_Data_Prep` → `02_Reference_Data_Prep` → `03_Imputation` → `04_Evaluation` → `07_Assembly`.
3. **Deep Context Location**: For detailed architectural steps, inputs/outputs structure, and exact script purposes, load and read `docs/agent-context.md`.
4. **Documentation Sync**: Any changes made to the pipeline should consider updates to `gtac_production_scripts/Documentation` and inline `.R` script comments.
5. **Changelog Updates**: Automatically update the `CHANGELOG.md` file in the repository root whenever you make significant methodological changes to the pipeline or logic. Do not log standard bug fixes, formatting, or minor script adjustments—focus strictly on meaningful modifications to the methodology.
6. **Known Bugs Tracker**: Keep a list of known bugs and fixes in `docs/known-bugs-and-fixes.md` to retain knowledge across sessions.
7. **Safe Editing**: Never overwrite content blindly. Always read existing content, understand the context, and carefully apply changes.
8. **Recency Tracking**: Whenever you modify this `copilot-instructions.md` file, update the '*Last Updated: [Date]*' line at the top of the file to the current date.

