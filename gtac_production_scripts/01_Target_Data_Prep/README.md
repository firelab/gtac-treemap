# Target data download and preparation scripts

## Information on Landfire (LF) dataset used as target data

> The datasets downloaded and prepared with the scripts in this directory adhrere closely to the naming conventions described below.

### Naming convention of [LF datasets](https://landfire.gov/lf_naming_convention.php)  

| LF version  | Description |
| ------------- | ------------- |
| 200  | 1st version of 2016 remap (new base map) |
| 220  | 2nd version of 2016 remap  |
| 230  | 3rd version of 2016 remap |
<br>

#### Disturbance datasets (available from 1999 to 2022):

| Years   | Dataset name |
| ------------- | ------------- |
| 1999-2014 | US_DIST\<year> |
| 2015-2016  | LF\<year>_Dist_200  |
| 2017-2020  | LF\<year>_Dist_220 |
| 2021-2022  | LF\<year>_Dist_230 |
<br>

#### Vegetation datasets (available years: 2001, 2014, 2016, 2020, 2022):

> * Existing vegetation cover (EVC)
> * Existing vegetation height (EVH)
> * Existing vegetation type (EVT)
> * Available? - Y\N (year to which dataset is project to)


| Year   | EVC | EVH   | EVT | Dataset name |
| ------------- | ------------- |------------- | ------------- | ------------- |
| 2001 | Y | Y  | Y  | US_105-US_105_\<VegDataset> |
| 2014 | Y | Y  | Y  | US_140-US_140_\<VegDataset> |
| 2016 | Y | Y  | Y  | US_200_mosaic-LF\<year>_\<VegDataset>_200 |
| 2020 | Y (2022) | Y (2022)  | Y  | US_220_mosaic-LF\<year>_\<VegDataset>_220 *** |
| 2022 | Y (2023) | Y (2023)  | Y  | US_230_mosaic-LF\<year>_\<VegDataset>_230 |



*** *Discrepancy in naming convention with the EVC and EVH datasets for 2020 LF*:
* The datasets projected to a future year for the US_220 LF version are named based on their year of projection. This does not apply to the 2022 dataset (also projected), which retains its original year name.
* For example: 
    + EVC 2020 projected to 2022 is named US_220_mosaic-LF2022. 
    + EVC 2022 dataset projected to 2023 named US_230_mosaic-LF2022.
    + **ISSUE:** There are two files with "...LF-2022..." in their names. 
    + **SOLUTION:** For now, determine which year by the …"US_220/230..." portion of the file name, where 220 = 2020 and 230 = 2022.
<br>

#### Topography datasets (available year: 2020):

| Year   | Aspect | Elevation | Slope (degrees) | 
| ------------- | ------------- |------------- | ------------- | 
| 2020 | US_Topo_LF\<year>_Asp_220_CONUS | US_Topo_LF\<year>_Elev_220_CONUS  | US_Topo_LF\<year>_SlpD_220_CONUS  | 
<br>

#### Biophysical variables

Available zipped by zone on FigShare: https://springernature.figshare.com/collections/LANDFIRE_Biophysical_Gradient_Raster_Datasets/5142572
