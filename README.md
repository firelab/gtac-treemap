# gtac-treemap
<<<<<<< Updated upstream
Repository for TreeMap work performed in collaboration with GTAC
=======

Repository for TreeMap work performed in collaboration with GTAC (Geospatial Technology and Applications Center) and the Rocky Mountain Research Station (RMRS).

## Overview

TreeMap provides detailed spatial information on forest characteristics including the number of live and dead trees, biomass, and carbon across forested extents of the United States. TreeMap data products use LANDFIRE data as key input layers and are constrained to years when LANDFIRE data is available. 

TreeMap contains 22-band 30 x 30m resolution gridded map images for each year and study area, with each band representing an attribute derived from select Forest Inventory Analysis (FIA) data and one band representing the TreeMap ID (TM_ID). TreeMap products are the output of a Random Forest machine learning algorithm that assigns the most similar FIA plot to each pixel of gridded LANDFIRE input data. The objective is to combine the complementary strengths of detailed-but-spatially-sparse FIA data with less-detailed-but-spatially-comprehensive LANDFIRE data to produce better estimations of forest characteristics at a variety of scales.



## Vintages and Versions

The following TreeMap datasets are maintained or produced by this repository:

| Year | Methods Ver | LANDFIRE Ver | FIADB Ver | Extent     | DOI |
|------|-------------|--------------|-----------|------------|-----|
| 2009 | 1.0         | LF2008       | 1.5.1     | Western US | https://doi.org/10.2737/RDS-2018-0003 |
| 2014 | 2.0         | LF2014       | 1.7.0.01  | CONUS      | https://doi.org/10.2737/RDS-2019-0026 |
| 2016 | 2.1         | LF2016       | 1.9       | CONUS      | https://doi.org/10.2737/RDS-2021-0074 |
| 2020 | 2.2         | LF2020       | 1.9.1     | CONUS      | https://doi.org/10.2737/RDS-2025-0031 |
| 2022 | 2.2         | LF2022       | 1.9.1     | CONUS      | https://doi.org/10.2737/RDS-2025-0032 |
| 2023 | 2.2         | LF2023       | 1.9.1     | CONUS      | TBD |

## Publications and Methodological Notes

* **Methods 1.0 (2009):** Original methodology, only for western US. Riley et al. 2016. Ecosphere. https://doi.org/10.1002/ecs2.1472
* **Methods 2.0 (2014):** Extended to all CONUS, included more source plots, and inclusion of disturbance as a predictor variable. Riley et al. 2021. Nature Scientific Data. https://doi.org/10.1038/s41597-020-00782-x
* **Methods 2.1 (2016):** Includes disturbance as a response variable. Riley et al. 2022. Journal of Forestry. https://doi.org/10.1093/jofore/fvac022
* **Methods 2.2 (2020, 2022, 2023):** Introduces ride-along problem fix, new suite of Daymet climate variables instead of Landfire climate variables. Earth Systems Science Data, in prep.

## Additional Resources

* **TreeMap Data Explorer:** [https://apps.fs.usda.gov/lcms-viewer/treemap.html](https://apps.fs.usda.gov/lcms-viewer/treemap.html)
* **TreeMap Raster Data Gateway:** [https://data.fs.usda.gov/geodata/rastergateway/treemap/](https://data.fs.usda.gov/geodata/rastergateway/treemap/)
* Contact `sm.fs.treemaphelp@usda.gov` with any questions or specific data requests.
>>>>>>> Stashed changes
