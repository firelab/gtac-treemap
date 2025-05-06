#   Target Layer Preparation: How to

## Summary

This document provides an overview of the steps required to prepare target data for a new version of TreeMap, and WILL provide detailed instructions on the intermediary or confusing steps in the process. 

## Steps in target layer preparation - outline

1. Topo data prep		
    a. Download slope and elevation layers from Landfire - 	Does not need to be done every year
	b.	Calculate northing and easting from slope -	Does not need to be done every year 
    c. Crop and mask topo data to each Landfire zone - Does not need to be done every year

2. Climate Data Prep
    a. Download biophysical vars from DayMet and calculate 30-year climate normals 1980-2010 - Does not need to be done every year
	b. Download solar radiation from GEE - Does not need to be done every year
	c. Calculate VPD -	Does not need to be done every year
	d. Crop, mask, and resample to each landfire zone - 	Does not need to be done every year
			
3. Disturbance data prep	
    a. Download updated disturbance data from Landfire
    b. Create layers with binary disturbance type and year of disturbance, for each zone

4. Vegetation data prep
    a. Download updated EVT, EVH, EVC layers from Landfire
    b. Crop and mask vegetation data to zone	
	c. Create forest mask from EVT x EVC	
	d. Reclass EVT to EVT_GP and create evt_gp remap table	

5. Final masking 
    a. Mask topo and veg layers with new forest mask from step 4c
	b. Mask climate data with new forest mask from step 4c
	c. Mask disturbance data with new forest mask from step 4c
	d. Create CONUS-wide VRTs from zonal target layers	

## Walk through of each step

### Step 1: Topo data prep

You most likely won't need to re-run this 

### Step 2: Climate data prep

### Step 3: Disturbance data prep

#### 3a. Download updated disturbance data from Landfire

- Check for when new disturbance data is available - final data available in Oct of the following year (e.g., 2023 data was out in Fall 2024)
- Ensure disturbance data formats stay the same; make a contingency plan if not
- Download for all study areas (CONUS, AK, HI)
- Access Landfire data here: https://landfire.gov/disturbance/annualdisturbance
- Download DIST or HDIST for recent years (Lila check that HDIST has the current year disturbance in it...)
- Currently we just download manually; we have draft scripts for downloading that haven't been updated. Probably we'll keep downloading manually bc Landfire keeps changing, so it would be hard to script the download
- Make downloads match existing file and folder format (see Folder Structures)
	- Specifically, extract zips, then move folder contents up one level so that folder names aren't duplicated
- Extract crs from Landfire raster and save it separately

#### 3b. Create layers with i) binary disturbance and ii) year of disturbance, for each zone

- Uses a control script 

### Step 4: Vegetation data prep

### Step 5: Final masking