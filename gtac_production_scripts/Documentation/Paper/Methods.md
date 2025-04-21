# TreeMap Methods

## Reference Data Prep

Reference data refers to the FIA-derived attributes that are used as reference/predictive variables in the imputation. These include: 

## Map Attribute Data Prep

Map attribute data refers to those FIA-derived attributes that are not modeled directly, but are reported as attribute layers in the final map outputs. 

## Target Data Prep

Target layers include

### Topo layer prep 
- Minimal changes from 2016 Method
- For 2020/2022: Used topographic layers from Landfire: Elevation, Slope, and Aspect from Landfire2020
    - Elevation: m
    - Slope: degrees
    - Aspect: [units]
- Converted aspect to northing and easting using equations:
    - northing = cos((pi/180)*aspect)
    - easting = sin((pi/180)*aspect)
- Correct an issue with northing and easting to ensure that where aspect = -1, northing and easting = 0
  northing_zone <- mask(northing_zone, aspect_zone, maskvalues=-1, updatevalue=0)
  easting_zone <- mask(easting_zone, aspect_zone, maskvalues=-1, updatevalue=0)
  
### Vegetation Layer initial Prep
- Minimal changes from 2016 method
- For 2020/2022: Used EVT, EVC, and EVH layers from Landfire version corresponding to appropriate year
- Vegetation layer prep happens on a zone-wise basis, following Landfire zones

#### Existing Vegetation Cover (EVC)
- Reclass raw EVC values into classes of Tree Cover that match the middle value in each range of tree cover values (Table). The original values in the Attribute table for EVC can be inspected [here](https://landfire.gov/sites/default/files/DataDictionary/2024/LF24_EVCADD.pdf)

            | Raw EVC Value    | EVC Reclass|
            | ---------------- | -------    |
            | -9999            | NA         |
            | 1-109            | NA         |
            | 110-119          | 15         |
            | 120-129          | 25         |
            | 130-139          | 35         |
            | 140-149          | 45         |
            | 150-159          | 55         |
            | 160-169          | 65         |
            | 170-179          | 75         |
            | 180-189          | 85         |
            | 190-199          | 95         |
            | 200-399          | NA         |

This EVC layer is used as a preliminary forest mask. 

#### Existing Vegetation Height (EVH)

- Reclass raw EVH values into classes of Existing Vegetation height. [Original EVH Attribute Table](https://landfire.gov/sites/default/files/DataDictionary/2024/LF24_EVHADD.pdf)

            | Raw EVH Value    | EVH Reclass| Class Description |
            | ---------------- | -------    | ------- |
            | -9999            | NA         | NA value      |
            | 0-100            | NA         | Other vegetation types     |
            | 101-105          | 3          | Tree height: 1-5m   |
            | 105-110          | 8          | Tree height: 6-10m  |
            | 111-125          | 18         | Tree height: 11-25m |
            | 126-150          | 38         | Tree height: 25-50m |
            | 151-199          | NA         | Tree height: >50m (nonexistent in CONUS) |
            | 200-399          | NA         | Other vegetation types |
        
#### Existing Vegetation Type (EVT)

- We start with the Landfire Existing Vegetation Type (EVT) layer. We reclassify EVT to the corresponding Existing Vegetation Type Group (EVT_GP), which is a broader category that we use for modeling. There are several EVT_GPs that we convert to NA. 

| EVT GP   | Reclass Value| Class Description |
|----------|--------------|-------------------|
|  13      |              |                   |
|  14      |              |                   |
|  15      |              |                   |
|  26      |              |                   |
|  60      |              |                   |
|  730     |              |                   |


#### 'Ride-along problem' and fix

- Describe the ride-along problem
- Fix: Limit plots available for imputation in each zone to those plots with species that are present in the zone being imputed, or any of the landfire zones that border the zone being imputed. 
- Insert a map of landfire zones for context
- Example: If we are imputing Landfire zone 1, (PNW, North Cascades). Within zone 1 and its bordering zones, say 18 EVT groups are present. Only plots that contain one of those 18 EVT groups will be included in the zone-specific x-table for that zone.
- This also requires that every EVT group in a zone is present in both the Landfire EVT layer and the X table. If there are any EVT gps that re not present in either dataset, the unmatched EVT groups are reclassified to nearest-matching appropriate groups based on expert opinion. 

- Table of reclassed evt groups, for final mask and reclass stage

### Climate Layer Prep
- List of climate vars: 
  Maximum temperature
  Minimum temperature
  Precipitation
  Vapour Pressure Deficit
  Vapour Pressure Deficit
  Soil Water Equivalent (SWE)
  Solar Radiation
  

- Download data from Daymet

### Disturbance Layer Prep

- Changes from 2016 (or 2014) method: NA
- FSIC GO refactored code to make it more modular

### Final masking
- Each zone is applied a tree mask der

## Imputation

### Build imputation model 

Changes from 2016 method:  
- refactored code base for efficiency and modularity

### Apply imputation model and assemble tiles
Changes from 2016 method:


### Assemble imputation output to CONUS-wide

## Evaluation

### Model evaluation
- 

### Target Layer Comparison

- Changes from 2016 method:

### Cross-validation

- All-new method for 2020/2022
- k-fold cross-validation with 10 folds
- to evaluate accuracy / stability of imputed attribute variables

#### CV Methods
- Drop any plots with that have EVT_GPs that are present in <0.3% of plots: these EVT_GPs are too rare to be represented in cross-validation
- 