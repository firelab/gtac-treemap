"""
This script:
1. separates columns (attributes) from the raster attribute table of TreeMap tifs into separate images, 
2. builds pyramids for those images, 
3. calculates statistics, 
4. converts to COG format, 
5. builds attribute tables (for discrete attributes), 
6. creates an xml + html metadata file based on a template,
7. creates arc compatable metadata (tif.xml), 
8. creates arc compatable statistics (aux.xml), 
9. creates a readme, 
10. zips all the files together (including manually made symbology files (lyrx + qml)).

The user is prompted upon start which products they want processed.
Please update use considerations and this description with any changes.

"""


# Print use considerations
print('\n**************************************************************\n**************************************************************')
print('USE CONSIDERATIONS: ')
print('* This script makes use of gdal, numpy, simpledbf, bs4, and pandas python libraries. Please insure these are installed in the environment before running.')
print('* Filepaths for the main TreeMap tif, dbf and the output folder are assigned within the script. If these need to be changed, it must done in the script.')
print('* Attributes to be separated and their data types are defined within the script. If these need to change, it must be done in the script.')
print('* Existing attribute files in the output folder will NOT be reprocessed. Please delete their tif files if you wish to reprocess')
print('* Chunk size can be adjusted in the script for higher or lower RAM availability. 29060 is typically appropriate for systems with 32gb of RAM, depending on other RAM usage. Greater chunk sizes will decrease processing time on systems that can afford it.')
print('**************************************************************\n**************************************************************\n')

######################################################################
# Import
######################################################################
#%%
import os
import numpy as np
from osgeo import gdal
from simpledbf import Dbf5
import pandas as pd
import json
import re
import xml.etree.ElementTree as ET
from bs4 import BeautifulSoup
from datetime import datetime
from zipfile import ZipFile
import msvcrt
import time
import traceback

gdal.UseExceptions()
#%%
############################################################################
# User Variables (Edit these values between versions + computing environments)
############################################################################

# Specify chunk size, 29060 SHOULD run on machines with >= 32gb RAM depending on other RAM usage
chunk_size = 48000 * 2

# Specify filepath to .tif (image), .dbf (attribute table)
treeMapTif = r"\\166.2.126.25\TreeMap\03_Outputs\07_Projects\2020_Production_newXtable\04_Mosaic_assembled_model_outputs\TreeMap2020_CONUS.tif"
treeMapDbf = r"\\166.2.126.25\TreeMap\03_Outputs\07_Projects\2020_Production_newXtable\04_Mosaic_assembled_model_outputs\TreeMap2020_CONUS.tif.vat.dbf"

# Specify the character limit for column names in the DBF (DBFs typically have a 10 character limit, so CARBON_DOWN_DEAD would appear as CARBON_DOW in the DBF)
dbfColumnCharLimit = 10

# Specify project area
projectArea = "CONUS"

# specify project year
projectYear = 2022

# Specify no data value in main dataset dbf
treeMapDatasetNoDataValue = np.nan # np.nan = NaN

# Set creation options for GeoTIFF compression, tiling, and sparse file format
creation_options = ["COMPRESS=DEFLATE", "BIGTIFF=YES", "SPARSE_OK=TRUE"]

# Specify data access link, used in metadata
data_gateway_link = 'https://data.fs.usda.gov/geodata/rastergateway/treemap/index.php'

# Specify output folder - will be created if it doesn't already exist
outputFolder = "//166.2.126.25/TreeMap/08_Data_Delivery/01_Separated_Attribute_Rasters/"+str(projectYear)

# Name of TreeMap ID column in Raster Attribute Table
tmid_col_name = "TM_ID"

# Column names to create individual attribute images of, their full names, and their data type
    # Columns whose full precision can only be contained within Float64: VOLCFNET_L, VOLCFNET_D, VOLBFNET_L, DRYBIO_L, DRYBIO_D, CARBON_L, CARBON_D
        # It was decided that 32 bit precision was sufficient for these attributes
cols = [('FORTYPCD', gdal.GDT_UInt16), 
        ('FLDTYPCD', gdal.GDT_UInt16),
        ('STDSZCD', gdal.GDT_Byte), 
        ('FLDSZCD', gdal.GDT_Byte), 
        ('BALIVE', gdal.GDT_Float32), 
        ('CANOPYPCT', gdal.GDT_Byte),
        ('STANDHT', gdal.GDT_UInt16), 
        ('ALSTK', gdal.GDT_Float32), 
        ('GSSTK', gdal.GDT_Float32), 
        ('QMD', gdal.GDT_Float32),
        ('SDIsum', gdal.GDT_Float32), 
        ('TPA_LIVE', gdal.GDT_Float32),
        ('TPA_DEAD', gdal.GDT_Float32), 
        ('VOLCFNET_L', gdal.GDT_Float32), 
        ('VOLCFNET_D', gdal.GDT_Float32), 
        ('VOLBFNET_L', gdal.GDT_Float32),
        ('DRYBIO_L', gdal.GDT_Float32), 
        ('DRYBIO_D', gdal.GDT_Float32),
        ('CARBON_L', gdal.GDT_Float32), 
        ('CARBON_D', gdal.GDT_Float32), 
        ('CARBON_DWN', gdal.GDT_Float32),
        ('TM_ID', gdal.GDT_UInt16)]

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
    'QMD': 'stand quadratic mean diameter (in)',
    'SDIsum': 'sum of stand density index',
    'TPA_LIVE': 'number of live trees per acre',
    'TPA_DEAD': 'number of standing dead trees per acre (DIA ≥ 5 inches)',
    'VOLCFNET_L': 'live volume (cubic ft. per acre)',
    'VOLCFNET_D': 'standing dead volume (cubic ft. per acre)',
    'VOLBFNET_L': 'volume, live, (sawlog board feet per acre) (log rule: International 1/4 inch)',
    'DRYBIO_L': 'aboveground dry live tree biomass (tons per acre)',
    'DRYBIO_D': 'aboveground dry standing dead tree biomass (tons per acre)',
    'CARBON_L': 'live aboveground carbon (tons per acre)',
    'CARBON_D': 'standing dead carbon (tons per acre)',
    'CARBON_DWN': 'down dead carbon > 3 inches diameter (tons per acre); estimated by FIA based on forest type, geographic area, and live tree carbon density.',
    'TM_ID': 'unique identifier assigned to each plot; corresponds to FIA PLT_CN'
    }

# Discrete (thematic + ordinal) columns with their associated color information. FORTYPCD + FLDTYPCD do not get assigned colors from here and are thus listed as NA
discrete_cols= {
    'FORTYPCD': ['NA', 'NA', 'NA'],
    'FLDTYPCD': ['NA', 'NA', 'NA'],
    'STDSZCD': ['custom', 'standsize', '4'],
    'FLDSZCD': ['custom', 'fieldsize', '6']
    }

# Column units
col_units = {
    'FORTYPCD': 'NA',
    'FLDTYPCD': 'NA',
    'STDSZCD': 'NA',
    'FLDSZCD': 'NA',
    'BALIVE': 'ft^2',
    'CANOPYPCT': 'percent',
    'STANDHT': 'ft',
    'ALSTK': 'percent',
    'GSSTK': 'percent',
    'QMD': 'in',
    'SDIsum': 'NA',
    'TPA_LIVE': 'trees/acre',
    'TPA_DEAD': 'trees/acre',
    'VOLCFNET_L': 'ft^3/acre',
    'VOLCFNET_D': 'ft^3/acre',
    'VOLBFNET_L': 'sawlog-board-ft/acre',
    'DRYBIO_L': 'tons/acre',
    'DRYBIO_D': 'tons/acre',
    'CARBON_L': 'tons/acre',
    'CARBON_D': 'tons/acre',
    'CARBON_DWN': 'tons/acre',
    'TM_ID': 'NA'
}

#%%
######################################################################
# Script Setup
######################################################################

# Import dbf, convert to pandas DataFrame, isolate id values
dbf = Dbf5(treeMapDbf)
df = dbf.to_dataframe()
ctrlValues = df[tmid_col_name]
#%%
# Load original tif, specify band, get raw no data value
treeMapImage = gdal.Open(treeMapTif, gdal.GA_ReadOnly)
og_band = treeMapImage.GetRasterBand(1)
rawTreeMapNoDataValue = og_band.GetNoDataValue()

# Get GeoTIFF GDAL driver
driver = gdal.GetDriverByName('GTiff')

# Set output name
outputFileName = 'TreeMap'+str(projectYear)+"_"+projectArea

# Make sure output folder exists
if not os.path.exists(outputFolder):
    os.makedirs(outputFolder)

#%%
######################################################################
# Functions
######################################################################


def attributeToImage(columnName, gdal_dtype, processing_mode):
    '''
    Creates a new COG formatted geotiff from the main TreeMap tif with pyramids, statistics, and attribute tables (if applicable).
    
    Args: 
        columnName (str): Name of attribute to be processed (e.g. CARBON_D).
        gdal_dtype (gdal datatype): Desired datatype of the new geotiff. 
        
    Returns:
        None
    '''
    

    # If the image mode is 
    if processing_mode == 'all':

        # Check if attribute image already exists in the output folder and skip if so
        if os.path.isfile(os.path.join(outputFolder, f'{outputFileName}_{columnName}.tif')):
            print(f'\n File for {columnName} already exists. Skipping...')
            return
    
    # Print the column being processed
    print('\n******************************************')
    print('CREATING IMAGE FOR ' + columnName)
    print('******************************************\n')

    # Account for DBF column names vs official attribute names
    if columnName == 'TM_ID':
        df_column = df[tmid_col_name]
    elif columnName == 'CARBON_DOWN_DEAD':
        df_column = df['CARBON_DWN']
    else:
        df_column = df[columnName[:dbfColumnCharLimit]]
    
    # Convert gdal dtype to numpy dtype
    np_dtype = gdal_to_numpy_dtype(gdal_dtype)

    # Get maximum value for datatype (used as NoDataValue)
    newNoDataValue = np.iinfo(np_dtype).max if np.issubdtype(np_dtype, np.integer) else np.finfo(np_dtype).max

    # Replace NoData values with the new value
    df_column = df_column.fillna(newNoDataValue)
    df_column = df_column.replace({treeMapDatasetNoDataValue: newNoDataValue})

    # Get values of specified attribute/column.
    attValues = df_column.values

    # Create a dictionary that maps original values to new values
    value_map = pd.Series(attValues, index=ctrlValues)

    # Create a temporary file to store the attribute image while building it
    tmp_file = '/vsimem/tmp.tif'

    # Create the new image
    newImage = driver.Create(tmp_file, treeMapImage.RasterXSize, treeMapImage.RasterYSize, 1, gdal_dtype, options = creation_options)
    newImage.SetGeoTransform(treeMapImage.GetGeoTransform())
    newImage.SetProjection(treeMapImage.GetProjection())

    # Get the new raster's band and set the no data value
    newImageBand = newImage.GetRasterBand(1)
    newImageBand.SetNoDataValue(float(newNoDataValue))
    newImageBand.SetDefaultRAT(None)

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

            # Write the processed data (new_band_data) to the output image at the same position as the original chunk
            newImageBand.WriteArray(new_band_data, j, i)

            # Flush the data to disk. 
            newImageBand.FlushCache()

    # Compute statistics
    print('Computing statistics...')
    newImageBand.ComputeStatistics(False)

    # Build pyramids
    print('Building pyramids...')
    newImage.BuildOverviews('NEAREST', [2, 4, 8, 16, 32, 64])

    # Change the name of the raster band to the attribute's name
    change_band_name(tmp_file, columnName)

    # Build the attribute table, if the column is discrete
    if columnName in discrete_cols.keys():
        print('Building attribute table...')
        create_attribute_table(columnName, tmp_file, attValues)
        save_attribute_table(os.path.join(outputFolder, f'{outputFileName}_{columnName}.tif.aux.xml'))
    
    # Translate the temporary GeoTIFF to COG format and save to output folder
    print('Translating to COG format and saving tif...')
    output_file = os.path.join(outputFolder, f'{outputFileName}_{columnName}.tif')
    gdal.Translate(output_file, newImage, format = 'COG', creationOptions = creation_options)

    # Remove temporary files
    newImage = None
    gdal.Unlink(tmp_file)

def save_attribute_table(output_file):
    rat_file = "/vsimem/tmp.tif.aux.xml"
    rat_data = gdal.VSIFOpenL(rat_file, "rb")
    if rat_data:
        content = gdal.VSIFReadL(1, gdal.VSIStatL(rat_file).size, rat_data)
        gdal.VSIFCloseL(rat_data)
    with open(output_file, "wb") as f:
        f.write(content)

def create_basic_metadata(col_name):
    '''
    Creates XML and HTML metadata file for specified attribute based on the main TreeMap dataset XML/HTML files.
    
    Args: 
        col_name (str): Name of attribute being processed (e.g. CARBON_D)
        
    Returns:
        None
    '''

    # XML
    with open(os.path.join(metd_template_dir, f'{projectYear}_metadata_template.xml'), 'r', encoding='utf-8') as file:
        content = file.read()
   
    content = content.replace('{col_name}', col_name)
    content = content.replace('{col_description}', col_descriptions[col_name])
    content = content.replace('{date}', datetime.now().strftime('%Y%m%d'))
    content = content.replace('{data_gateway_link}', data_gateway_link)

    with open(os.path.join(outputFolder, f'{outputFileName}_{col_name}.xml'), 'w', encoding='utf-8') as f:
        f.write(content)

    # HTML
    with open(os.path.join(metd_template_dir,  f'{projectYear}_metadata_template.html'), 'r', encoding='utf-8') as file:
        content = file.read()
   
    content = content.replace('{col_name}', col_name)
    content = content.replace('{col_description}', col_descriptions[col_name])
    content = content.replace('{date}', datetime.now().strftime('%Y%m%d'))
    content = content.replace('{data_gateway_link}', data_gateway_link)

    with open(os.path.join(outputFolder, f'{outputFileName}_{col_name}.html'), 'w', encoding='utf-8') as f:
        f.write(BeautifulSoup(content, 'html.parser').prettify())


def create_arc_metadata(col_name, tif_path):
    '''
    Creates *.tif.xml metadata file that is compatible with ArcGIS Pro. Uses the treemap arcmeta template stored in the supp_files folder in the repo, as well as the existing .xml file for the attribute.

    Args:
        col_name (str): Name of attribute being processed (e.g. CARBON_D)

    Returns:
        None
    '''
    
    # Filepath for attribute's base xml file
    att_xml_metadata = os.path.join(outputFolder, f'{outputFileName}_{col_name}.xml')
    
    # Construct filepath for new xml based on the provided attribute xml 
    new_meta_file = os.path.join(outputFolder, f'{outputFileName}_{col_name}.tif.xml')
    
    # Open existing attribute xml
    source_tree = ET.parse(att_xml_metadata)
    source_root = source_tree.getroot()
    
    # Open template xml
    template_file = os.path.join((metd_template_dir), 'TreeMap_ArcMeta_template.xml')
    template_tree = ET.parse(template_file)
    template_root = template_tree.getroot()
    
    # Open raster (for using image metadata)
    raster = gdal.Open(tif_path)
    band = raster.GetRasterBand(1)
    
    # Mapping dictionary TEMPLATE_TAG: SOURCE_TAG.
        # Note: only for tags in template that have a corresponding source tag
        # Note: measDesc (template tag) does not work well with function and so has been defined elsewhere
    tag_mapping = {
        'CreaDate': 'citation//pubdate',
        'nativeExtBox//westBL': 'westbc',
        'nativeExtBox//eastBL': 'eastbc',
        'nativeExtBox//southBL': 'southbc',
        'nativeExtBox//northBL': 'northbc',
        'SyncDate': 'citeinfo//pubdate',
        'ModDate': 'citeinfo//pubdate',
        'mdContact//rpIndName': 'metc//cntper',
        'mdContact//rpOrgName': 'metc//cntorg',
        'mdContact//rpPosName': 'metc//cntpos',
        'mdContact//voiceNum': 'metc//cntvoice',
        'mdContact//delPoint': 'metc//address',
        'mdContact//city': 'metc//city',
        'mdContact//adminArea': 'metc//state',
        'mdContact//country': 'metc//country',
        'mdContact//eMailAdd': 'metc//cntemail',
        'mdContact//postCode': 'metc//postal',
        'mdDateSt': 'metainfo//metd',
        'distInfo//rpOrgName': 'distrib//cntorg',
        'distInfo//rpPosName': 'distrib//cntpos',
        'distInfo//delPoint': 'distrib//address',
        'distInfo//city': 'distrib//city',
        'distInfo//adminArea': 'distrib//state',
        'distInfo//country': 'distrib//country',
        'distInfo//postCode': 'distrib//postal',
        'idCitation//resTitle': 'citation//title',
        'idCitation//pubDate': 'citation//pubdate',
        'citRespParty//rpIndName': 'metc//cntper',
        'citRespParty//rpOrgName': 'metc//cntorg',
        'citRespParty//rpPosName': 'metc//cntpos',
        'citRespParty//voiceNum': 'metc//cntvoice',
        'citRespParty//delPoint': 'metc//address',
        'citRespParty//city': 'metc//city',
        'citRespParty//adminArea': 'metc//state',
        'citRespParty//country': 'metc//country',
        'citRespParty//eMailAdd': 'metc//cntemail',
        'citRespParty//postCode': 'metc//postal',
        'citRespParty//displayName': 'metc//cntper',
        'idAbs': 'abstract',
        'idPurp': 'purpose',
        'idCredit': 'datacred',
        'idPoC//rpIndName': 'metc//cntper',
        'idPoC//rpOrgName': 'metc//cntorg',
        'idPoC//rpPosName': 'metc//cntpos',
        'idPoC//voiceNum': 'metc//cntvoice',
        'idPoC//delPoint': 'metc//address',
        'idPoC//city': 'metc//city',
        'idPoC//adminArea': 'metc//state',
        'idPoC//country': 'metc//country',
        'idPoC//eMailAdd': 'metc//cntemail',
        'idPoC//postCode': 'metc//postal',
        'placeKeys': 'place',
        'themeKeys': 'theme',
        'searchKeys': 'theme',
        'Consts//useLimit': 'useconst',
        'envirDesc': 'native',
        'dataExt//exDesc': 'spdom//descgeog',
        'dataExt//westBL': 'westbc',
        'dataExt//eastBL': 'eastbc',
        'dataExt//southBL': 'southbc',
        'dataExt//northBL': 'northbc',
        'TempExtent//tmBegin': 'timeperd//caldate',
        'TempExtent//tmEnd': 'timeperd//caldate',
        'dataLineage': 'procdesc',
        'dqInfo//evalMethDesc': 'attraccr',
        'eainfo//enttypd': 'eaover',
        'onLineSrc': 'stdorder//networka//networkr'
    }
    
    # Iterate through each template->source mapping and transfer information appropriately
    for template_tag, src_tag in tag_mapping.items():
        # Get all elements from template and source xml that match the tags  
        template_elements = template_root.findall('.//' + template_tag)
        src_elements = source_root.findall('.//' + src_tag)
        
        # If there's only 1 element from source and no children tags, simply change the template element's text to the source element's text
        if len(src_elements) == 1 and len(src_elements[0]) == 0:
            template_elements[0].text = src_elements[0].text
            
        # Else if there's multiple source elements and each has children tags (as is the case for theme tags, which hold theme keys), iterate through each one and transfer information appropriately
        elif len(src_elements) > 1 and len(src_elements[0]) > 0:
            # If we are adding searchKeys, add all the keys
            if template_elements[0].tag == 'searchKeys':
                for element in src_elements:
                    for child in element:
                        if child.tag == 'themekey':
                            new_tag = ET.Element('keyword')
                            new_tag.text = child.text
                            template_elements[0].append(new_tag)
            # Else we are likely dealing with themeKeys
            else:
                for template_element, src_element in zip(template_elements, src_elements):
                    # For each child in the source element, if it is the title of the theme, copy the source text to the template element. 
                    # Else if the child is a themekey, create a new tag in the template called <keyword> and transfer the source text
                    for src_child in src_element:
                        if src_child.tag == 'themekt' and template_tag == 'themeKeys':
                            template_element.find('.//resTitle').text = src_child.text
                        elif src_child.tag == 'themekey':
                            new_tag = ET.Element('keyword')
                            new_tag.text = src_child.text
                            template_element.append(new_tag)
        
        # Else it must be a singular element with children tags (as is the case for the place tag, which holds placekeys)                
        else:
            for src_child in src_elements[0]:
                if src_child.tag == 'placekey':
                    new_tag = ET.Element('keyword')
                    new_tag.text = src_child.text
                    template_elements[0].append(new_tag)
                    
    # Set values for tags that don't play well in the loop above
        # Data completeness and accuracy reports
    for element in template_root.findall('.//dqInfo//report'):
        if element.get('type') == "DQCompOm":
            element.find('measDesc').text = source_root.find('.//complete').text
        elif element.get('type') == "DQQuanAttAcc":
            element.find('evalMethDesc').text = source_root.find('.//attraccr').text
            # Raster dimensions + resolution
    for element in template_root.findall('.//axisDimension'):
        if element.get('type') == "001":
            element.find('.//dimSize').text = str(raster.RasterXSize)
            element.find('dimResol//value').text = str(raster.GetGeoTransform()[1])
        else:
            element.find('.//dimSize').text = str(raster.RasterYSize)
            element.find('.//dimResol//value').text = str(raster.GetGeoTransform()[5] * -1) # multiply by -1 because y resolution stored as negative
    
                    
    # Set values not found in the source xml
        # General items
    template_root.find('.//DataProperties//itemName').text = f'{outputFileName}_{col_name}.tif'
    template_root.find('.//DataProperties//itemLocation//linkage').text = str(check_file_in_folder(att_xml_metadata, f'{outputFileName}_{col_name}.tif'))
    template_root.find('.//SyncDate').text = datetime.now().strftime('%Y%m%d')
    template_root.find('.//SyncTime').text = datetime.now().strftime('%M%S%H')
    template_root.find('.//ModDate').text = datetime.now().strftime('%Y%m%d')
    template_root.find('.//ModTime').text = datetime.now().strftime('%M%S%H')

        # Set raster metadata
    template_root.find('.//coordRef').text = raster.GetProjection()
    template_root.find('.//AttributeDescription').text = col_name + ': ' + col_descriptions[col_name]
    template_root.find('.//BandMinValue').text = str(band.GetStatistics(True, False)[0])
    template_root.find('.//BandMaxValue').text = str(band.GetStatistics(False, True)[1])
    template_root.find('.//BandUnits').text = col_units[col_name]
    template_root.find('.//BandBitsPerValue').text = str(gdal.GetDataTypeSize(band.DataType))
    template_root.find('.//HasColormap').text = str(has_color_map(band))
    template_root.find('.//CompressionType').text = str(raster.GetMetadata("IMAGE_STRUCTURE").get("COMPRESSION", "NONE"))
    template_root.find('.//NumBands').text = str(raster.RasterCount)
    template_root.find('.//Format').text = raster.GetDriver().LongName        
    template_root.find('.//PixelType').text = gdal_to_pixel_type(band.DataType)
    template_root.find('.//NoDataValue').text = str(band.GetNoDataValue())
    template_root.find('.//Band//minVal').text = str(band.GetStatistics(True, False)[0])
    template_root.find('.//Band//maxVal').text = str(band.GetStatistics(False, True)[1])
    template_root.find('.//Band//bitsPerVal').text = str(gdal.GetDataTypeSize(band.DataType))
        # Check if attribute is discrete or continuous
    if col_name in discrete_cols:
        template_root.find('.//SourceType').text = 'DISCRETE'
    else:
        template_root.find('.//SourceType').text = 'CONTINUOUS'
        
        # Calculate corner + center coordinates and assign
    ul_x, ul_y, lr_x, lr_y = get_corners(raster)
    center_x, center_y = get_center_coordinate(ul_x, lr_x, lr_y, ul_y)
    template_root.find('.//suppInfo').text = template_root.find('.//suppInfo').text.format(XMIN_ORIG = str(ul_x), YMAX_ORIG = str(ul_y), XMAX_ORIG = str(lr_x), YMIN_ORIG = str(lr_y))
    for element in template_root.findall('.//cornerPts'):
        element.find('pos').text = element.find('pos').text.format(XMIN_ORIG=str(ul_x), XMAX_ORIG=str(lr_x), YMIN_ORIG = str(lr_y), YMAX_ORIG = str(ul_y))
    template_root.find('.//centerPt//pos').text = template_root.find('.//centerPt//pos').text.format(CENTER_X=center_x, CENTER_Y=center_y)
    
        # Constants
    template_root.find('.//scaleRange//minScale').text = '150000000'
    template_root.find('.//scaleRange//maxScale').text = '5000'
    template_root.find('.//idCitation//datasetSeries//seriesName').text = 'TreeMap'
    template_root.find('.//idCitation//collTitle').text = 'TreeMap'
    template_root.find('.//mdConst//othConsts').text = 'None'
    template_root.find('.//onLineSrc').text = data_gateway_link
    template_root.find('.//eainfo//enttypl').text = col_name
    template_root.find('.//eainfo//enttypds').text = col_descriptions[col_name]
        
    # Write the file
    template_tree.write(new_meta_file)


def check_file_in_folder(filepath, target_filename):
    '''This function takes a filepath and checks if the target_filename is in the folder
    
    Args:
        filepath (str): A file in the same folder to check.
        target_filename (str): The file to search for in the folder.
        
    Returns:
        str: The filepath of the found file, or None if not found.
    '''
    
    # Get director of input filepath
    directory = os.path.dirname(filepath)
    
    # Check each item
    for filename in os.listdir(directory):
        if filename == target_filename:
            return os.path.join(directory, filename)


def has_color_map(band):
    '''
    Takes in a raster band and checks if it has a color map.
        
    Args:
        band (gdal.Band): Band of raster to check.

    Returns:
        bool: True if raster band has a color map 
    '''
    
    if band.GetRasterColorTable() != None:
        return True
    else:
        return False
    

def gdal_to_pixel_type(gdal_type):
    '''
    Converts a gdal datatype to an ESRI datatype.

    Args:
        gdal_type (gdal datatype): gdal datatype

    Returns:
        str: Pixel type (e.g. U8, U16, S16, F32, etc.)
    '''
    
    if gdal_type == gdal.GDT_Byte:
        return "U8"
    elif gdal_type == gdal.GDT_UInt16:
        return "U16"
    elif gdal_type == gdal.GDT_UInt32:
        return "U32"
    elif gdal_type == gdal.GDT_Int16:
        return "S16"
    elif gdal_type == gdal.GDT_Int32:
        return "S32"
    elif gdal_type == gdal.GDT_Float32:
        return "F32"
    elif gdal_type == gdal.GDT_Float64:
        return "F64"
    else:
        return None
    

def get_corners(dataset):
    """
    Gets the corner coordinates (center of pixel)

    Args:
        dataset (raster): The raster object of the image in question (output of gdal.Open())

    Returns:
        float: Coorindates of upper left x, upper left y, lower right x, lower right y
    """
    
    width = dataset.RasterXSize
    height = dataset.RasterYSize

    gt = dataset.GetGeoTransform()

    ul_x = gt[0] + 0.5 * gt[1]    # add half the resolution to get to the center of the pixel
    ul_y = gt[3] + 0.5 * gt[5]

    lr_x = gt[0] + (width * gt[1]) - 0.5 * gt[1]   # subtract half the resolution to get to the center of the pixel
    lr_y = gt[3] + (height * gt[5]) - 0.5 * gt[5]

    return ul_x, ul_y, lr_x, lr_y


def get_center_coordinate(xmin, xmax, ymin, ymax):
    """
    Gets the center coordinates (center of pixel)

    Args:
        xmin (flaot): Minimum x-coordinate
        xmax (float): Maximum x-coordinate
        ymin (float): Minimum y-coordinate
        ymax (float): Maximum y-coordinate

    Returns:
        float: x,y coordinates of center
    """
    
    center_x = (xmin + xmax) / 2.0
    center_y = (ymin + ymax) / 2.0
    return center_x, center_y


def create_arc_stats(col_name, tif_path):
    '''
    Creates *.tif.aux.xml metadata stats file that is compatible with ArcGIS Pro. Uses the treemap arcstats template stored in the supp_files folder in the repo, as well as the existing .xml file for the attribute.

    Args:
        col_name (str): Name of attribute being processed (e.g. CARBON_D)

    Returns:
        None
    '''
    
    # Create statistics metadata (*.tif.aux.xml)
        # Open template file
    stats_template_file = os.path.join((metd_template_dir), 'TreeMap_ArcStats_template.xml')
    stats_template_tree = ET.parse(stats_template_file)
    stats_template_root = stats_template_tree.getroot()
    
        # Open image
    raster = gdal.Open(tif_path)
    band = raster.GetRasterBand(1)
    
        # Get band data and remove nodata values
    band_data = band.ReadAsArray()
    no_data_value = band.GetNoDataValue()
    valid_data = band_data[band_data != no_data_value]
    
        # Get min + max value and calculate histogram
    stats = band.GetStatistics(True, True)
    num_buckets, include_out, approx = 256, 0, 0
    bucket_counts = band.GetHistogram(min=stats[0], max=stats[1], buckets=num_buckets, include_out_of_range=include_out, approx_ok=approx)

        # Populate elements
    stats_template_root.find('.//Histograms//HistMin').text = str(stats[0])
    stats_template_root.find('.//Histograms//HistMax').text = str(stats[1])
    stats_template_root.find('.//Histograms//BucketCount').text = str(num_buckets)
    stats_template_root.find('.//Histograms//IncludeOutOfRange').text = str(include_out)
    stats_template_root.find('.//Histograms//Approximate').text = str(approx)
    stats_template_root.find(".//Histograms//HistCounts").text = "|".join(map(str, bucket_counts))
    
        # Find and update the statistics in Metadata
    metadata = stats_template_root.find(".//Metadata")

    for mdi in metadata.findall("MDI"):
        key = mdi.attrib.get("key")
        if key == "STATISTICS_MAXIMUM":
            mdi.text = str(stats[1])
        elif key == "STATISTICS_MEAN":
            mdi.text = str(stats[2])
        elif key == "STATISTICS_MEDIAN":
            mdi.text = str(np.median(valid_data))
        elif key == "STATISTICS_MINIMUM":
            mdi.text = str(stats[0])
        elif key == "STATISTICS_STDDEV":
            mdi.text = str(stats[3])
        elif key == "STATISTICS_VALID_PERCENT":
            mdi.text = str((valid_data.size / band_data.size) * 100)

    # Write the file
    stats_template_tree.write(os.path.join(outputFolder, f'{outputFileName}_{col_name}.tif.aux.xml'))


def change_band_name(file_path, col_name):
    '''
    Changes the band name of the specified tif.

    Args:
        file_path (str): Filepath of the target tif.
        col_fullname (str): The attribute's full name. This gets set as the band name. 
    '''

    # Open the image and band
    image = gdal.Open(file_path, gdal.GA_Update)
    band = image.GetRasterBand(1)

    # Set the band name
    band.SetDescription(col_name)


def create_attribute_table(col_name, file_path, vals):
    '''
    Creates a raster attribute table for the specified image.
    
    Args:
        col_name (str): Name of the attribute being processed (e.g. 'FLDSZCD').
        file_path (str): File path of the file.
        
    Returns:
        None.
    '''

    if col_name == 'FLDTYPCD' or col_name == 'FORTYPCD':
        create_thematic_att_table(file_path, vals)
    elif col_name == 'FLDSZCD' or col_name == 'STDSZCD':
        create_ordinal_att_table(col_name, file_path, vals)
    else:
        print('Attribute table not applicable to ' + col_name)


def create_thematic_att_table(file_path, vals):
    '''
    Creates a raster attribute table for the specified image.
    
    Args:
        col_name (str): Name of the attribute being processed (e.g. 'FLDSZCD').
        file_path (str): File path of the file.
        
    Returns:
        None.
    '''

    # Path to the forest_type_palette_lookup JSON file in the repo
        # This json consists of a list of codes, a list of names, and a list hex colors. Corresponding values from the codes, names, and colors lists share the same indicies.
    palettes_file = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), 'gee_viz_setup_scripts', 'forest_type_palette_lookup_UPDATED.json')
        
    # Open the color maps JSON file
    with open(palettes_file) as json_file:
        lut = json.load(json_file)
        
    codes = lut["code"]
    names = lut["names"]
    colors = lut["palette"]
  
    # Open the raster file for update
    image = gdal.Open(file_path, gdal.GA_Update)
    
    # Get the first band from the raster
    band = image.GetRasterBand(1)
        
    # Get the value assigned for NoData cells in the raster
    nodata_value = band.GetNoDataValue()
        
    # Retrieve the unique values from the raster excluding NoData
    unique_vals = np.unique(vals)
    unique_values_nodata_excluded = unique_vals[unique_vals != nodata_value]
        
    # Create a new Raster Attribute Table (RAT)
    rat = gdal.RasterAttributeTable()
    
    # Set the row count of the RAT to the number of unique values
    rat.SetRowCount(len(unique_values_nodata_excluded))
        
    # Create columns for the pixel value and RGB values
    rat.CreateColumn("Value", gdal.GFT_Integer, gdal.GFU_Generic)
    rat.CreateColumn("Label", gdal.GFT_String, gdal.GFU_Name)
    rat.CreateColumn("Red", gdal.GFT_Integer, gdal.GFU_Red)
    rat.CreateColumn("Green", gdal.GFT_Integer, gdal.GFU_Green)
    rat.CreateColumn("Blue", gdal.GFT_Integer, gdal.GFU_Blue)
        
    # Fill the RAT with the unique values and corresponding RGB values.
    for i, value in enumerate(unique_values_nodata_excluded):
    	# Set pixel value
        rat.SetValueAsInt(i, 0, int(value))
        
        # Get index of the code
        code_index = codes.index(int(value))
        
        # Set label value
        rat.SetValueAsString(i, 1, names[code_index])
    
    	# Set RGB values
        color_map_rgb = hex_to_rgb(colors[code_index])
        rat.SetValueAsInt(i, 2, color_map_rgb[0])  # Red
        rat.SetValueAsInt(i, 3, color_map_rgb[1])  # Green
        rat.SetValueAsInt(i, 4, color_map_rgb[2])  # Blue
       	 
    # Attach the RAT to the raster band
    band.SetDefaultRAT(rat)


def create_ordinal_att_table(col_name, file_path, vals):
    '''
    Creates a raster attribute table for the specified image.
    
    Args:
        col_name (str): Name of the attribute being processed (e.g. 'FLDSZCD').
        file_path (str): File path of the file.
        
    Returns:
        None.
    '''

    # Path to the palettes JSON file
    palettes_file = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'supp_files', 'palettes.json')
    
    # Extracting the label, theme and number for the color palette
    label, theme, number = discrete_cols[col_name]

    # Open the color maps JSON file
    with open(palettes_file) as json_file:
        color_maps = json.load(json_file)

    # Retrieve the specific color map in hex format
    color_map_hex = color_maps[label][theme][number]
       	 
    # Convert each hex color code to its RGB equivalent
    color_map_rgb = [hex_to_rgb(color) for color in color_map_hex]
    
    # Open the raster file for update
    image = gdal.Open(file_path, gdal.GA_Update)
    #image = gdal.OpenEx(file_path, gdal.GA_Update, open_options={"IGNORE_COG_LAYOUT_BREAK": "YES"})
    
    # Get the first band from the raster
    band = image.GetRasterBand(1)
        
    # Get the value assigned for NoData cells in the raster
    nodata_value = band.GetNoDataValue()
        
    # Retrieve the unique values from the raster excluding NoData
    unique_vals = np.unique(vals)
    unique_values_nodata_excluded = unique_vals[unique_vals != nodata_value]
        
    # Create a new Raster Attribute Table (RAT)
    rat = gdal.RasterAttributeTable()
    
    # Set the row count of the RAT to the number of unique values
    rat.SetRowCount(len(unique_values_nodata_excluded))
        
    # Create columns for the pixel value and RGB values
    rat.CreateColumn("Value", gdal.GFT_Integer, gdal.GFU_Generic)
    rat.CreateColumn("Red", gdal.GFT_Integer, gdal.GFU_Red)
    rat.CreateColumn("Green", gdal.GFT_Integer, gdal.GFU_Green)
    rat.CreateColumn("Blue", gdal.GFT_Integer, gdal.GFU_Blue)

    # Fill the RAT with unique values and corresponding RGB values
    for i, value in enumerate(unique_values_nodata_excluded):
        # Set pixel value
        rat.SetValueAsInt(i, 0, int(value))

        # Set RGB values
        rat.SetValueAsInt(i, 1, color_map_rgb[i][0])
        rat.SetValueAsInt(i, 2, color_map_rgb[i][1])
        rat.SetValueAsInt(i, 3, color_map_rgb[i][2])
       	 
    # Attach the RAT to the raster band
    band.SetDefaultRAT(rat)


def hex_to_rgb(hex_code):
    '''
    Converts hex codes to rgb values.
    
    Args:
        hex_code (str): Hex code to be converted.
        
    Returns:
        List of the red, green, and blue values.
    '''
    
    if hex_code.startswith('#'):
        hex_code = hex_code[1:]
    
    return [int(hex_code[i:i+2], 16) for i in range(0, 6, 2)]


def get_readme_text(col_name, zip_only=True):
    '''
    Get the readme text for the attribute using attribute_readme_template.txt in supp_files.

    Args:
        col_name (str): Name of the attribute being processed.
        zip_only (bool): Determines if the readme is only written to the zip file
    
    Returns:
        String of the readme contents.
    '''

    # Define additional text to be added to the readme if the attribute is continuous
    additional_toptext = f'Instructions for applying the official {outputFileName} symbology in ArcGIS Pro and QGIS are also included.'
    additional_filedescriptions = f'''
	{outputFileName}_{col_name}.tif.lyrx - An ArcGIS layer file containing the official symbology for the attribute.
						    	    Please see "Applying Official Symbology" in this document.

	{outputFileName}_{col_name}.tif.qml - A QGIS layer file containing the official symbology for the attribute.
						   	   Please see "Applying Official Symbology" in this document.
    '''
    continuous_symbologyinstructions = f'''\nApplying Official Symbology:

	ArcGIS Pro:
		1. Once the raster has been added to your project, in the "Raster Layer" ribbon at the top of the window select "Symbology"
			Or navigate to the "Symbology" pane another way
		2. In the top right corner of the "Symbology" pane, click on the 3 horizontal lines and select "Import from layer file"
		3. Navigate to and select the layer file, {outputFileName}_{col_name}.tif.lyrx
    
	QGIS:
		1. Once the raster has been added to your project, in the right click on the layer and select "Properties..."
		2. On the left hand side of the "Properties" window, select "Symbology"
		3. Click on "Style" at the bottom of the window and select "Load Style..."
		4. Navigate to and select the layer file, {outputFileName}_{col_name}.tif.qml
    '''
    thematic_symbologyinstructions = f'''\nApplying Official Symbology:

	ArcGIS Pro:
		1. Once the raster has been added to your project, in the "Raster Layer" ribbon at the top of the window select "Symbology"
			Or navigate to the "Symbology" pane another way
		2. Change symbology type to 'Unique Values'
    
	QGIS:
		1. Simply add the raster to your project
    '''

    # Get the filepath to the readme template file
    template_filepath = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'supp_files', 'attribute_readme_template.txt')

    # Open the template file
    with open(template_filepath, 'r') as readme:
        readme_text = readme.read()
    
    # Insert additional text if the attributes are continuous or if they are thematic, else remove the placeholders in the .txt
    if col_name not in discrete_cols.keys():
        readme_text = readme_text.format(
                                        outputFileName = outputFileName,
                                        projectYear = projectYear, 
                                         projectArea = projectArea, col_name=col_name,
                                         additional_toptext=additional_toptext, additional_filedescriptions=additional_filedescriptions,
                                         continuous_symbologyinstructions=continuous_symbologyinstructions,
                                         thematic_symbologyinstructions='')
    elif col_name == 'FLDTYPCD' or col_name == 'FORTYPCD':
        readme_text = readme_text.format(outputFileName = outputFileName,
                                         projectYear = projectYear, 
                                         projectArea = projectArea, col_name=col_name, additional_toptext=additional_toptext, 
                                         additional_filedescriptions='', continuous_symbologyinstructions='',
                                         thematic_symbologyinstructions=thematic_symbologyinstructions)
    else:
        readme_text = readme_text.format(outputFileName = outputFileName,
                                         projectYear = projectYear, 
                                         projectArea = projectArea, col_name=col_name, additional_toptext='', 
                                         additional_filedescriptions='', continuous_symbologyinstructions='',
                                         thematic_symbologyinstructions='')

    # If directed to write the file to the output folder, do so (if zip_only is False, write a file to the output folder and temp memory, if zip_only is True, only write to temp memory)
    if not zip_only:
        new_filepath = os.path.join(outputFolder, f'{outputFileName}_{col_name}_readme.txt')
        with open(new_filepath, 'w') as new_file:
            new_file.write(readme_text)

    return readme_text
    

def zip_files(files, col_name):
    '''
    Zips files together and writes a new zip. Creates a readme based on the attribute type (discrete or continuous)
    
    Args: 
        files: Filepaths of files to be zipped
        zip_file_name: Name of the to-be-created zip file (must include .zip).
        
    Returns:
        None
    '''
    
    # Assign output path for the zip file
    zip_directory = os.path.join(outputFolder, '00_Zipped_Files')
    
    # Check if the zip directory already exists
    if not os.path.exists(zip_directory):
        os.makedirs(zip_directory)
        
    # Append the name of the directory to the zip file name
    full_zip_filepath = os.path.join(zip_directory, f'{outputFileName}_{col_name}.zip')
    
    # Create a ZipFile object in write mode
    with ZipFile(full_zip_filepath, 'w') as zipf:
        # Write the readme file to the zip file
        zipf.writestr('readme.txt', get_readme_text(col_name))

        # Loop through provided files and add them to the zip file
        for file in files:
            zipf.write(file, os.path.basename(file))

    
# def determine_version():
#     '''
#     Determines the version of the TreeMap dataset based on its filename
#     Args:
#         None.
#     Returns:
#         str: The tm_ver of the dataset
#     '''
    
#     # Get name of file
#     filename = os.path.basename(treeMapTif)
    
#     # Grab everything in the filename between 'TreeMap' and '.tif'
#     match = re.search('TreeMap(.+?).tif', filename)
    
#     # If there's a match, return the string
#     if match:
#         return match.group(1)
#     else:
#         raise ValueError('Could not determine TreeMap version. Please update the main dataset filepath or implement changes to the determine_version() function in the script.')


def gdal_to_numpy_dtype(gdal_dtype):
    '''
    Takes a gdal datatype and returns the corresponding numpy datatype

    Args:
        gdal_dtype (gdal datatype): gdal data type to be converted

    Returns:
        numpy datatype: Corresponding numpy datatype 
    '''
    
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


def prompt_user():
    '''
    Determines which functions to run in the script. Informs user of defined inputs.

    Args:
        None.

    Returns:
        None.
    '''

    choosing = True

    while(choosing):
        mode = input('''\nChoose Mode: 
                'images' to generate attribute tifs (.tif)
                'meta' to generate metadata (.xml, .html, .aux.xml, .tif.xml - separated attribute tifs must already exist)
                'package' to package all necessary files for the raster data gateway (tif, metadata, symbology files - if they exist)
                     
                Please type choice: ''')
        
        second_mode = ''
        
        if mode == 'images':
            choosing2 = True
            while(choosing2):
                second_mode = input('''\n*************************************\nChoose Image Mode: 
                    'all' to generate all attributes. Existing attributes in the output folder will not be overwritten.
                    OR
                    Type the name of the attribute you want to process (e.g., 'FORTYPCD').
                                
                    Please type choice: ''')
                
                if second_mode.lower() != 'all' and (not any(col[0].upper() == second_mode.upper() for col in cols)):
                    print(f'\n{second_mode} is not a defined attribute. Please choose a valid option or update the attribute list in the script.')
                else:
                    choosing2 = False
            
            print('\n\n****************************************************************************************************')
            print('Input tif: ' + treeMapTif)
            print('Input dbf: ' + treeMapDbf)
            print('Output folder: ' + outputFolder)
            print('****************************************************************************************************')
            print('Current chunk size:' + str(chunk_size))
            print('****************************************************************************************************')
            print('****************************************************************************************************')
            print('Project Year: ' + str(projectYear))
            print('****************************************************************************************************\n')
            print('Project Area: ' + projectArea)
            print('****************************************************************************************************\n')
            
            # Check with user if inputs are correct
            print("If paths, chunk size, Project Year, and Project Area are correct press 'enter', otherwise press 'q' to quit and correct in script")
            key = msvcrt.getch()
            if key.lower() == b'q':
                quit()

            choosing = False

        elif mode == 'meta':
            choosing2 = True
            acceptable_inputs = ['all', 'arc', 'basic']
            while(choosing2):
                # Query user for which metadata files to generate
                second_mode = input('''\n\nChoose Metadata Mode: 
                    'all' to generate all metadata (.xml, .html, .aux.xml, .tif.xml)
                    'basic' to generate basic metadata (.xml, .html)
                    'arc' to generate arcgis metadata (.aux.html, .tif.xml)
                                
                    Please type choice: ''')
                
                if second_mode not in acceptable_inputs:
                    print(f'\n{second_mode} is not a valid option. Please choose a valid option.')
                else:
                    choosing2 = False
            
            print('\n\n****************************************************************************************************')
            print('Output folder (must contain attribute tifs): ' + outputFolder)
            print('Metadata template folder: ' + metd_template_dir)
            print('****************************************************************************************************')
            print('Project Year: ' + str(projectYear))
            print('****************************************************************************************************\n')
            print('Project Area: ' + projectArea)
            print('****************************************************************************************************\n')

            # Check with user if inputs are correct
            print("If paths, Project Year, and Project Area are correct press 'enter', otherwise press 'q' to quit and correct in script")
            key = msvcrt.getch()
            if key.lower() == b'q':
                quit()

            choosing = False

        elif mode == 'package':
            print('\n\n****************************************************************************************************')
            print('Attribute tifs and metadata folder: ' + outputFolder)
            print('Symbology files: ' + symbology_dir)
            print('****************************************************************************************************')
            print('Project Year: ' + str(projectYear))
            print('****************************************************************************************************\n')
            print('Project Area: ' + projectArea)
            print('****************************************************************************************************\n')

            # Check with user if inputs are correct
            print("If paths, Project Year, and Project Area are correct press 'enter', otherwise press 'q' to quit and correct in script")
            key = msvcrt.getch()
            if key.lower() == b'q':
                quit()

            choosing = False

        else:
            raise ValueError(f'{mode} is not a valid option. Please choose again.')

    return mode, second_mode

def find_folder(start_directory, target_folder_name):
    '''
    Returns the path of a target folder within the specified directory.

    Args:
        start_directory (str): Path of the starting directory
        target_folder_name (str): Name of the target folder
    '''

    for dirpath, dirnames, filenames in os.walk(start_directory):
        for dirname in dirnames:
            if dirname == target_folder_name:
                return os.path.join(dirpath, dirname)
            

def package_for_rdg(col_name):
    '''
    Zips necessary attribute files for the raster data gateway. Files must be 

    Args:
        col_name (str): Name of the attribute to zip

    Returns:
        None.
    '''

    if not os.path.exists(os.path.join(outputFolder, f'{outputFileName}_{col_name}.tif')):
        print(f'\nThe tif for {col_name} was not found. Skipping...')
        return

    # Define files to be zipped
    print(f'\nPackaging {col_name}...')
    tif_file = os.path.join(outputFolder, f'{outputFileName}_{col_name}.tif')
    xml_file = os.path.join(outputFolder, f'{outputFileName}_{col_name}.xml')
    html_file = os.path.join(outputFolder, f'{outputFileName}_{col_name}.html')
    arcxml_file = os.path.join(outputFolder, f'{outputFileName}_{col_name}.tif.xml')
    arcstats_file = os.path.join(outputFolder, f'{outputFileName}_{col_name}.tif.aux.xml')

    files_to_zip = [tif_file, xml_file, html_file, arcxml_file, arcstats_file]
    
    # If the attribute is not TM_ID or discrete, add arcgis + qgis symbology files to the list of files to be zipped
    if col_name == 'TM_ID' or col_name not in discrete_cols.keys():
        arc_lyrx_file = os.path.join(symbology_dir, f'{outputFileName}_{col_name}.tif.lyrx')
        qgis_qml_file = os.path.join(symbology_dir, f'{outputFileName}_{col_name}.tif.qml')

        # Check if the files exist and inform user if not
        if not os.path.exists(arc_lyrx_file):
            print(f'ArcGIS Pro layer file, {outputFileName}_{col_name}.tif.lyrx, does not exist in {symbology_dir} and was not packaged.')
        else:
            files_to_zip.append(arc_lyrx_file)
        
        if not os.path.exists(qgis_qml_file):
            print(f'QGIS layer file, {outputFileName}_{col_name}.tif.qml, does not exist in {symbology_dir} and was not packaged.')
        else:
            files_to_zip.append(qgis_qml_file)
        
    # Zip files
    zip_files(files_to_zip, col_name)

    print(files_to_zip)


def generate_metadata(col_name, meta_mode):
    # Get path to the separated attribute image
    image_path = os.path.join(outputFolder, f'{outputFileName}_{col_name}.tif')

    if os.path.exists(image_path):
        # Print the column being processed
        print('\n******************************************')
        print('CREATING METADATA FOR ' + col_name)
        print('******************************************\n')
        # Create xml and html metadata
        if meta_mode == 'basic' or meta_mode == 'all':
            print('Building metadata...')
            create_basic_metadata(col_name)
        # Create ESRI xml metadata
        if meta_mode == 'arc' or meta_mode == 'all':
            print('Building ESRI compatible metadata and statistics...')
            create_arc_metadata(col_name, image_path)
            create_arc_stats(col_name, image_path)

    else:
        print(f'Image for {col_name} does not exist in the output folder. Skipping...')
        
#%%
######################################################################
# Main Function Calls
######################################################################

# Determine the TreeMap version
#tm_ver = determine_version()

# Determine the general metadata template folder
metd_template_dir = find_folder(os.path.dirname(os.path.abspath(__file__)), 'metadata_templates')
metd_template_dir = os.path.join(metd_template_dir, str(projectYear))

# Determine this TreeMap version's symbology folder
symbology_dir = os.path.join(find_folder(os.path.dirname(os.path.abspath(__file__)), 'symbology_files'), str(projectYear))
#%%

# Inform user of assigned inputs + outputs and get input on modes to run
mode, second_mode = prompt_user()

# Main Function
if mode == 'images':
    # If user specified processing all images
    if second_mode == 'all':
        for col_name, gdal_dtype in cols:
            start_time = time.perf_counter()

            try:
                attributeToImage(col_name, gdal_dtype, second_mode)
            except Exception as e: 
                print(f'ERROR: {e}\n{traceback.format_exc()}\n\nFailed to process {col_name}')
                
            end_time = time.perf_counter()
            elapsed = (end_time - start_time)/60
            print(f'Time to complete: {elapsed} minutes')
    # Else process the specific attribute they provided
    else:
        for col_name, gdal_dtype in cols:
            if col_name.upper() == second_mode.upper():
                start_time = time.perf_counter()

                try:
                    attributeToImage(col_name, gdal_dtype, second_mode)
                except Exception as e:
                    print(f'ERROR: {e}\n{traceback.format_exc()}\n\nFailed to process {col_name}')
                
                end_time = time.perf_counter()
                elapsed = (end_time - start_time)/60
                print(f'Time to complete: {elapsed} minutes')
# Metadata generation mode
elif mode == 'meta':
    for col_name, gdal_dtype in cols:
        start_time = time.perf_counter()

        try:
            generate_metadata(col_name, second_mode)
        except Exception as e:
            print(f'ERROR: {e}\n{traceback.format_exc()}\n\nFailed to process {col_name}')

        end_time = time.perf_counter()
        elapsed = (end_time - start_time)/60
        print(f'Time to complete: {elapsed} minutes')
# Package in ZIP mode
elif mode == 'package':
    for col_name, gdal_dtype in cols:
        start_time = time.perf_counter()

        try:
            package_for_rdg(col_name)
        except Exception as e:
            print(f'ERROR: {e}\n{traceback.format_exc()}\n\nFailed to process {col_name}')
            
        end_time = time.perf_counter()
        elapsed = (end_time - start_time)/60
        print(f'Time to complete: {elapsed} minutes')
# Otherwise they didn't provide a valid mode
else:
    print(f'\n{mode} is not valid. Please restart the script and specify a valid mode.')
# %%
