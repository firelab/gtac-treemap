"""
This script separates columns/attributes from the raster attribute table of TreeMap2016.tif
into separate images, builds pyramids for those images, and creates an xml metadata file based on the full dataset's xml.
Please update use considerations and this description with any changes.

"""

# Print use considerations
print('\n**************************************************************\n**************************************************************')
print('USE CONSIDERATIONS: ')
print('* This script makes use of gdal, numpy, simpledbf, and pandas python libraries. Please insure these are installed in the environment before running.')
print('* File paths for TreeMap2016.tif, the associated .dbf, and the output folder are assigned within the script. If these need to be changed, it must done in the script.')
print('* Columns/attributes to be separated and their data types are defined within the script. If these need to change, it must be done in the script.')
print('* Chunk size can be adjusted in the script for higher or lower RAM availability.')
print('**************************************************************\n**************************************************************')

######################################################################
# Setup
######################################################################

import os, numpy as np
from osgeo import gdal
from simpledbf import Dbf5
import pandas as pd
import xml.etree.ElementTree as ET
from datetime import datetime

# Specify chunk size, 29060 SHOULD run on machines with >= 32gb RAM depending on other RAM usage
chunk_size = 29060

# Specify year of original image
year = '2016'

# Specify file path to .tif (image), .dbf (attribute table), and xml (metadata)
treeMapTif = r"\\166.2.126.25\TreeMap\01_Data\01_TreeMap2016_RDA\RDS-2021-0074_Data\Data\TreeMap2016.tif"
treeMapDbf = r"\\166.2.126.25\TreeMap\01_Data\01_TreeMap2016_RDA\RDS-2021-0074_Data\Data\TreeMap2016.tif.vat.dbf"
treeMapXml = r"\\166.2.126.25\TreeMap\01_Data\01_TreeMap2016_RDA\RDS-2021-0074_Supplements\_metadata_RDS-2021-0074.xml"

# Specify output folder
outputFolder = r"C:\Users\NicholasStorey\OneDrive - USDA\Documents\TestFolder"

# Specify no data value in RDS dataset
rawTreeMapNoDataValue = 2147483647
treeMapDatasetNoDataValue = -99.00000000000

# Column names to create individual attribute images of (and their data types)
    # Columns whose full precision can only be contained within Float64: VOLCFNET_L, VOLCFNET_D, VOLBFNET_L, DRYBIO_L, DRYBIO_D, CARBON_L, CARBON_D 
cols = [('FORTYPCD', gdal.GDT_UInt16), ('FLDTYPCD', gdal.GDT_UInt16), ('STDSZCD', gdal.GDT_Byte),
        ('FLDSZCD', gdal.GDT_Byte), ('BALIVE', gdal.GDT_Float32), ('CANOPYPCT', gdal.GDT_Byte),
        ('STANDHT', gdal.GDT_UInt16), ('ALSTK', gdal.GDT_Float32), ('GSSTK', gdal.GDT_Float32),
        ('QMD_RMRS', gdal.GDT_Float32) , ('SDIPCT_RMR', gdal.GDT_Float32), ('TPA_LIVE', gdal.GDT_Float32),
        ('TPA_DEAD', gdal.GDT_Float32), ('VOLCFNET_L', gdal.GDT_Float32), ('VOLCFNET_D', gdal.GDT_Float32),
        ('VOLBFNET_L', gdal.GDT_Float32), ('DRYBIO_L', gdal.GDT_Float32), ('DRYBIO_D', gdal.GDT_Float32),
        ('CARBON_L', gdal.GDT_Float32), ('CARBON_D', gdal.GDT_Float32), ('CARBON_DWN', gdal.GDT_Float32)]

# Column names with their associated descriptions
col_descriptions = {
    'FORTYPCD': 'forest type code (assigned by FIA algorithm)',
    'FLDTYPCD': ' forest type code (assigned in field by FIA crew)',
    'STDSZCD': 'stand size code (assigned by FIA algorithm)',
    'FLDSZCD': 'stand size code (assigned in field by FIA crew)',
    'BALIVE': 'live tree basal area (square feet [ft.])',
    'CANOPYPCT': 'live canopy cover (percent) estimated by FVS routine',
    'STANDHT': 'height of dominant trees (ft.) estimated by FVS routine',
    'ALSTK': 'all live tree stocking (percent)',
    'GSSTK': 'growing-stock stocking (percent)',
    'QMD_RMRS': 'stand quadratic mean diameter (collected in RMRS only)',
    'SDIPCT_RMR': 'stand density index (percent of maximum) (collected in RMRS only)',
    'TPA_LIVE': 'number of live trees per acre',
    'TPA_DEAD': 'number of standing dead trees per acre (DIA ≥ 5 inches)',
    'VOLCFNET_L': 'live volume (cubic ft. per acre [ac.])',
    'VOLCFNET_D': 'standing dead volume (cubic ft. per ac.)',
    'VOLBFNET_L': 'volume, live, sawlog board feet per ac. (log rule: International 1/4 inch) ',
    'DRYBIO_L': 'aboveground dry live tree biomass (tons per ac.)',
    'DRYBIO_D': 'aboveground dry standing dead tree biomass (tons per ac.)',
    'CARBON_L': 'live aboveground carbon (tons per ac.)',
    'CARBON_D': 'standing dead carbon (tons per ac.)',
    'CARBON_DWN': 'down dead carbon > 3 inches diameter (tons per ac.); estimated by FIA based on forest type, geographic area, and live tree carbon density.'
    }

# Import dbf, convert to pandas DataFrame, isolate values
dbf = Dbf5(treeMapDbf)
df = dbf.to_dataframe()
ctrlValues = df.Value

# Load original tif, specify band
treeMapImage = gdal.Open(treeMapTif, gdal.GA_ReadOnly)
og_band = treeMapImage.GetRasterBand(1)

# Get GeoTIFF GDAL driver
driver = gdal.GetDriverByName('GTiff')


######################################################################
# Functions
######################################################################

# Takes a column name and datatype and creates a new gtiff
def attributeToImage(columnName, gdal_dtype):
    # Print the column being processed
    print('\n******************************************')
    print('CREATING IMAGE FOR ' + columnName)
    print('******************************************\n')
    
    # Get values of specified attribute/column
    attValues = df[columnName].values

    # Create a dictionary that maps original values to new values
    value_map = pd.Series(attValues, index=ctrlValues)

    # Set creation options for LZW compression, tiling, and sparse file format
    creation_options = ["COMPRESS=LZW", "TILED=YES", "SPARSE_OK=TRUE"]

    # Convert gdal dtype to numpy dtype
    np_dtype = gdal_to_numpy_dtype(gdal_dtype)

    # Get maximum value for datatype (used as NoDataValue)
    newNoDataValue = np.iinfo(np_dtype).max if np.issubdtype(np_dtype, np.integer) else np.finfo(np_dtype).max

    # Create the new image
    newImage = driver.Create(outputFolder + f'\TreeMap{year}_{columnName}.tif', treeMapImage.RasterXSize, treeMapImage.RasterYSize, 1, gdal_dtype, options = creation_options)
    newImage.SetGeoTransform(treeMapImage.GetGeoTransform())
    newImage.SetProjection(treeMapImage.GetProjection())

    # Get the new raster's band and set the no data value
    newImageBand = newImage.GetRasterBand(1)
    newImageBand.SetNoDataValue(float(newNoDataValue))

    # Get the X + Y size of the original image's band for chunking purposes
    xsize = og_band.XSize
    ysize = og_band.YSize

    # Process the image in chunks
    for i in range(0, ysize, chunk_size):
        if i + chunk_size < ysize:
            rows = chunk_size
        else:
            rows = ysize - i
        for j in range(0, xsize, chunk_size):
            if j + chunk_size < xsize:
                cols = chunk_size
            else:
                cols = xsize - j

            # Print current chunk
            print(f"Processing chunk ({i}, {j})")

            # Read out original band data as an array for current chunk
            band_data = og_band.ReadAsArray(j, i, cols, rows)
            
            # Create a boolean mask of the same size as the band_data where True values represent valid data 
            # and False values represent NoData values in the original image
            no_data_mask = band_data != rawTreeMapNoDataValue

            # Create a new array of the same size as the band_data filled with the NoData value
            # The dtype is set to be the same as the desired output data type
            new_band_data = np.full(band_data.shape, newNoDataValue, dtype=np_dtype)

            # Using the boolean mask, replace the corresponding positions in the new_band_data with the mapped values from value_map
            new_band_data[no_data_mask] = value_map[band_data[no_data_mask]].values

            # Replace all -99 values with the newNoDataValue
            new_band_data[np.isclose(new_band_data, treeMapDatasetNoDataValue, atol=1e-8)] = newNoDataValue

            # Write the processed data (new_band_data) into the output image at the same position as the original chunk
            newImageBand.WriteArray(new_band_data, j, i)

            # Flush the data to disk. This is necessary because GDAL uses a cache to improve performance by writing data in chunks
            # If you do not call FlushCache, not all data may be written to disk if the cache is not full
            newImageBand.FlushCache()

    # Compute statistics
    print('Computing statistics...')
    newImageBand.ComputeStatistics(False)

    # Build pyramids
    print('Building pyramids...')
    newImage.BuildOverviews("NEAREST", [2, 4, 8, 16, 32, 64])

    # Create xml metadata
    print('Building xml metadata...')
    create_xml_metadata(treeMapXml, f'{outputFolder}/TreeMap{year}_{columnName}.xml', columnName, col_descriptions[columnName])

    # Close the image (forces it to write to disk)
    newImage = None

# Takes a gdal datatype and returns the corresponding numpy datatype
def gdal_to_numpy_dtype(gdal_dtype):
    if gdal_dtype == gdal.GDT_Byte:
        return np.uint8
    elif gdal_dtype == gdal.GDT_UInt16:
        return np.uint16
    elif gdal_dtype == gdal.GDT_Int16:
        return np.int16
    elif gdal_dtype == gdal.GDT_UInt32:
        return np.uint32
    elif gdal_dtype == gdal.GDT_Int32:
        return np.int32
    elif gdal_dtype == gdal.GDT_Float32:
        return np.float32
    elif gdal_dtype == gdal.GDT_Float64:
        return np.float64
    else:
        raise ValueError(f"Unsupported GDAL data type: {gdal_dtype}")
    

def create_xml_metadata(original_xml_file, new_xml_file, col_name, col_description):
    # Parse the original XML file
    tree = ET.parse(original_xml_file)
    root = tree.getroot()

    # Find the elements to change
    publisher = root.find('.//publish')
    abstract = root.find('.//abstract')
    eaover = root.find('.//eaover')

    # Specify that this is the publisher for the source data
    publisher.text += ' (source data)'

    # Remove non-relevant text from the abstract and append information regarding the specific attribute
    abstract.text = abstract.text.replace(' (the GeoTIFF included in this data publication)', '')
    abst_additional_text = f'\n \n This GeoTIFF is a subset of the main TreeMap{year}.tif, in which a single attribute, {col_name}, has been written to the raster band.'.format(year=year,col_name=col_name)
    abstract.text += abst_additional_text

    # Define the replacement text with placeholders for dynamic content
    eaover_replacement_text = """
    Below is a description of the file included in this publication and its relationship to the FIA DataMart.\n

    IMPORTANT INFORMATION \n\n

    TreeMap{year}_{col_name}.tif is a subset of the full TreeMap{year}.tif. Band values for TreeMap{year}_{col_name}.tif are those found in the attribute table for the full TreeMap{year}.tif.
    \n\n

    DATA FILE DESCRIPTIONS (1)\n\n

    (1) TreeMap{year}_{col_name}.tif: 
    Raster dataset (GeoTIFF file) representing a single attribute, {col_name}, of the full model output generated by random forests imputation of forest inventory plot data measured by Forest Inventory and Analysis (FIA) to unsampled (and sampled) spatial locations on the landscape for circa 2016 conditions. {col_name} is a measure of {col_description}. Predictor variables in the random forests imputation were chosen to optimize the prediction of aboveground forest carbon.
    These include topographic variables (slope, aspect, and elevation from the FIA PLOT and COND tables), true plot location, vegetation (forest cover, height, and vegetation group assigned to each plot via Forest Vegetation Simulator [FVS, https://www.fs.fed.us/fvs/, Dixon 2002] and LANDFIRE methods), disturbance (years since disturbance and disturbance type as derived from LANDFIRE disturbance rasters), and biophysical variables (maximum and minimum temperature, relative humidity, precipitation, photosynthetically active radiation, and vapor pressure deficit derived by overlay of the plot coordinates with LANDFIRE biophysical rasters). Variables and methods for the full model output are defined in more completeness in Riley et al. (2016) and Riley et al. (2021), the accompanying Data Dictionary file (“TreeMap2016_Data_Dictionary.pdf”), and the FIA documentation (Burrill et al. 2018).

    """.format(year=year, col_name=col_name, col_description=col_description)

    # Replace the contents of the <eaover> tag
    eaover.text = eaover_replacement_text

    # Change the metadata creation date
    metd = root.find('.//metd')
    date = datetime.now()
    metd.text = date.strftime('%Y%m%d')

    # Save the modified XML to a new file
    tree.write(new_xml_file)

######################################################################
# Main Function Call
######################################################################

attributeToImage('QMD_RMRS', gdal.GDT_Float32)
quit()

for col_name, gdal_dtype in cols:
    attributeToImage(col_name, gdal_dtype)
    quit()
    
