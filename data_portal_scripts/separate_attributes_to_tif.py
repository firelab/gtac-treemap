"""
This script separates columns/attributes from the raster attribute table of TreeMap tifs into separate images, builds pyramids for those images, 
calculates statistics, converts to COG format, applies color maps (for appropriate attributes), creates an xml + html metadata file based on the full dataset's,
creates arc compatable metadata (tif.xml), creates arc compatable statistics (aux.xml), and zips all the files together.
Please update use considerations and this description with any changes.

"""


# Print use considerations
print('\n**************************************************************\n**************************************************************')
print('USE CONSIDERATIONS: ')
print('* This script makes use of gdal, numpy, simpledbf, bs4, and pandas python libraries. Please insure these are installed in the environment before running.')
print('* Filepaths for the main TreeMap tif, dbf, xml, html, and the output folder are assigned within the script. If these need to be changed, it must done in the script.')
print('* Columns/attributes to be separated and their data types are defined within the script. If these need to change, it must be done in the script.')
print('* Existing columns/attributes will NOT be reprocessed. Please delete their tif files if you wish to reprocess.')
print('* Chunk size can be adjusted in the script for higher or lower RAM availability.')
print('**************************************************************\n**************************************************************\n')

######################################################################
# Setup
######################################################################

import os, numpy as np
from osgeo import gdal
from simpledbf import Dbf5
import pandas as pd
import xml.etree.ElementTree as ET
from bs4 import BeautifulSoup, NavigableString
from datetime import datetime
from zipfile import ZipFile
import msvcrt
import time

# Specify chunk size, 29060 SHOULD run on machines with >= 32gb RAM depending on other RAM usage
chunk_size = 29060 * 2

# Specify file path to .tif (image), .dbf (attribute table), and xml + html (metadata)
treeMapTif = r"\\166.2.126.25\TreeMap\01_Data\01_TreeMap2016_RDA\RDS-2021-0074_Data\Data\TreeMap2016.tif"
treeMapDbf = r"\\166.2.126.25\TreeMap\01_Data\01_TreeMap2016_RDA\RDS-2021-0074_Data\Data\TreeMap2016.tif.vat.dbf"
treeMapXml = r"\\166.2.126.25\TreeMap\01_Data\01_TreeMap2016_RDA\RDS-2021-0074_Supplements\_metadata_RDS-2021-0074.xml"
treeMapHtml = r"\\166.2.126.25\TreeMap\01_Data\01_TreeMap2016_RDA\RDS-2021-0074_Supplements\_metadata_RDS-2021-0074.html"

# Specify output folder
outputFolder = r"\\166.2.126.25\TreeMap\03_Outputs\04_Separated_Attribute_Rasters"

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

col_colors = {
    #'FORTYPCD': ['', '', ''],
    #'FLDTYPCD': ['', '', ''],
    'STDSZCD': ['colorbrewer', 'Set1', '6'],
    'FLDSZCD': ['colorbrewer', 'Set1', '6'],
    'CANOPYPCT': ['crameri', 'bamako', '50'],
    'STANDHT': ['crameri', 'bamako', '50'],
    }

# Which attributes are discrete?
discrete_attributes = ['FORTYPCD', 'FLDTYPCD', 'STDSZCD', 'FLDSZCD', 'TPA_LIVE', 'TPA_DEAD']

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

def attributeToImage(columnName, gdal_dtype):
    '''
    Creates a new COG formatted geotiff from the main TreeMap tif with pyramids, statistics, and metadata.
    
    Args: 
        columnName (str): Name of column or attribute to be processed (e.g. CARBON_D).
        gdal_dtype (gdal datatype): Desired datatype of the new geotiff. 
        
    Returns:
        None
    '''
    
    # Check if image already exists and skip if so
    if os.path.isfile(outputFolder + f'\TreeMap{year}_{columnName}.tif'):
        print(f'\n File for {columnName} already exists. Skipping...')
        return
    
    # Print the column being processed
    print('\n******************************************')
    print('CREATING IMAGE FOR ' + columnName)
    print('******************************************\n')
    
    # Get values of specified attribute/column
    attValues = df[columnName].values

    # Create a dictionary that maps original values to new values
    value_map = pd.Series(attValues, index=ctrlValues)

    # Convert gdal dtype to numpy dtype
    np_dtype = gdal_to_numpy_dtype(gdal_dtype)

    # Get maximum value for datatype (used as NoDataValue)
    newNoDataValue = np.iinfo(np_dtype).max if np.issubdtype(np_dtype, np.integer) else np.finfo(np_dtype).max

    # Create a temporary file
    tmp_file = '/vsimem/tmp.tif'
    
    # Set creation options for compression, tiling, and sparse file format
    creation_options = ["COMPRESS=DEFLATE", "BIGTIFF=YES", "SPARSE_OK=TRUE"]

    # Create the new image
    newImage = driver.Create(tmp_file, treeMapImage.RasterXSize, treeMapImage.RasterYSize, 1, gdal_dtype, options = creation_options)
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

            # Flush the data to disk. 
            newImageBand.FlushCache()

    # Compute statistics
    print('Computing statistics...')
    newImageBand.ComputeStatistics(False)

    # Build pyramids
    print('Building pyramids...')
    newImage.BuildOverviews(determine_resample(columnName), [2, 4, 8, 16, 32, 64])

    # Close the image (forces it to write to disk)
    newImage = None
    
    # Translate the temporary GeoTIFF to COG format and save to output folder
    print('Translating to COG format...')
    output_file = outputFolder + f'\TreeMap{year}_{columnName}.tif'
    gdal.Translate(output_file, '/vsimem/tmp.tif', format = 'COG', creationOptions = creation_options)

    # Remove temporary file
    gdal.Unlink('/vsimem/tmp.tif')

    # Create xml and html metadata
    print('Building metadata...')
    create_xml_metadata(columnName)
    create_html_metadata(columnName)
    print('Building ESRI compatable metadata and statistics...')
    create_arc_metadata(columnName)
    create_arc_stats(columnName)
    
    # Zip files together
    print('Zipping files...')
    tif_file = os.path.join(outputFolder, f'TreeMap{year}_{columnName}.tif')
    xml_file = os.path.join(outputFolder, f'TreeMap{year}_{columnName}.xml')
    html_file = os.path.join(outputFolder, f'TreeMap{year}_{columnName}.html')
    arcxml_file = os.path.join(outputFolder, f'TreeMap{year}_{columnName}.tif.xml')
    arcstats_file = os.path.join(outputFolder, f'TreeMap{year}_{columnName}.tif.aux.xml')
    zip_files([tif_file, xml_file, html_file, arcxml_file, arcstats_file], f'{outputFolder}/00_Zipped_Files/TreeMap{year}_{columnName}.zip')


def create_xml_metadata(col_name):
    '''
    Creates xml metadata file based on an original xml file (scripted to work on TreeMap2016, should work mostly on other versions if metadata format is the same). Small tweaks may be necessary.
    
    Args: 
        col_name (str): Name of column or attribute being processed (e.g. CARBON_D)
        
    Returns:
        None
    '''
    
    # Parse the original XML file
    tree = ET.parse(treeMapXml)
    root = tree.getroot()


    # Publisher
    publisher = root.find('.//publish')
    # Add text to publisher info specifying source data
    publisher.text += ' (source data)'

    # Abstract
    abstract = root.find('.//abstract')
    # Remove irrelevant text from the abstract and append information regarding the specific attribute
    abstract.text = abstract.text.replace(' (the GeoTIFF included in this data publication)', '')
    abstract.text += f'\n\nThis GeoTIFF is a subset of the main TreeMap{year}.tif, in which a single attribute, {col_name}, has been written to the raster band.'

    # Entity and Attribute Overview
    eaover = root.find('.//eaover')
    # Replace text from eaoverview
    eaover.text = """
    Below is a description of the file included in this publication and its relationship to the FIA DataMart.

    IMPORTANT INFORMATION

    TreeMap{year}_{col_name}.tif is a subset of the full TreeMap{year}.tif. Band values for TreeMap{year}_{col_name}.tif are those found in the attribute table of the full TreeMap{year} dataset.
    \n\n

    DATA FILE DESCRIPTIONS (1)

    (1) TreeMap{year}_{col_name}.tif: 
    Raster dataset (GeoTIFF file) representing a single attribute, {col_name}, of the full model output generated by random forests imputation of forest inventory plot data measured by Forest Inventory and Analysis (FIA) to unsampled (and sampled) spatial locations on the landscape for circa 2016 conditions. {col_name} is a measure of {col_descriptions[col_name]}. Predictor variables in the random forests imputation were chosen to optimize the prediction of aboveground forest carbon.
    These include topographic variables (slope, aspect, and elevation from the FIA PLOT and COND tables), true plot location, vegetation (forest cover, height, and vegetation group assigned to each plot via Forest Vegetation Simulator [FVS, https://www.fs.fed.us/fvs/, Dixon 2002] and LANDFIRE methods), disturbance (years since disturbance and disturbance type as derived from LANDFIRE disturbance rasters), and biophysical variables (maximum and minimum temperature, relative humidity, precipitation, photosynthetically active radiation, and vapor pressure deficit derived by overlay of the plot coordinates with LANDFIRE biophysical rasters). Variables and methods for the full model output are defined in more completeness in Riley et al. (2016) and Riley et al. (2021), the accompanying Data Dictionary file (“TreeMap{year}_Data_Dictionary.pdf”), and the FIA documentation (Burrill et al. 2018).

    """
    
    # Process Description
    procdesc = root.find('.//procdesc')
    # Insert text before citations
    insert_text_before_xml(procdesc, 'For complete details see', f'{col_name} was then separated into its own raster by writing values in the raster attribute table to the corresponding pixels in the raster band.' + ' ')

    # Standard Order Process
    stdorder = root.find('.//stdorder')
    # Remove extra digital forms and update the file compression text in the remaining form
    remove_extra_digital_forms_xml(stdorder)
    stdorder.find('.//filedec').text = 'Files zipped with zipfile python library'
    
    # Metadata Date
    metd = root.find('.//metd')
    # Change the metadata creation date
    metd.text = datetime.now().strftime('%Y%m%d')

    # Save the modified XML to a new file
    tree.write(os.path.join(outputFolder, f'TreeMap{year}_{col_name}.xml'))


def create_html_metadata(col_name):
    '''
    Creates html metadata file based on an original html file (scripted to work on TreeMap2016, likely will need some reworking for projects passed TreeMap2016 (e.g. tweaks to specific strings).
    
    Args: 
        col_name (str): Name of column or attribute being processed (e.g. CARBON_D)
        
    Returns:
        None
    ''' 
    
    # Helper function to find specific text in a given tag
    def has_specific_text(tag, tag_name, text):
        return tag.name == tag_name and text in tag.get_text()


    # Open the original HTML file and parse it with BeautifulSoup
    with open(treeMapHtml, 'r', encoding='utf-8') as file:
        soup = BeautifulSoup(file, 'html.parser')
    
    
    # Publisher
        # Find the 'dt' tag that contains this text from the publisher info (gives us the full publisher info)
    publisher = soup.find(lambda tag: has_specific_text(tag, 'dt', 'Forest Service Research Data Archive'))
     
    # If the publisher information is found, modify it
    if publisher:
        # Extract the tag that contains the publisher information
        i_tag = publisher.find('i')
    
        # Create a new tag with the modified publisher text
        new_publisher_text = 'Forest Service Research Data Archive (source data)'
        new_publisher_tag = soup.new_string(' ' + new_publisher_text)
    
        # Remove the original publisher information
        publisher.clear()
    
        # Add the modified publisher information to the parsed HTML
        publisher.append(i_tag)
        publisher.append(new_publisher_tag)
    
    
    # Abstract
        # Find the 'dd' tag containing this text from the abstract (this 'dd' tag contains the abstract)
    abstract_text = soup.find(lambda tag: has_specific_text(tag, 'dd', 'We matched forest plot data from Forest Inventory and Analysis (FIA) to a 30x30 meter (m) grid'))
    
    # If the abstract text is found, remove a specified portion of it
    if abstract_text:
        replace_text_html(abstract_text, '(the GeoTIFF included in this data publication)', '')
        abstract_newtext = f'<br /> <br />This GeoTIFF is a subset of the main TreeMap{year}.tif, in which a single attribute, {col_name}, has been written to the raster band.'
        
        # Add modified content to the parsed HTML
        new_content_soup = BeautifulSoup(abstract_newtext, 'html.parser')
        for element in new_content_soup.contents:
            abstract_text.append(element)
        
        
    # Entity and Attribute Overview
        # Find the 'dd' tag containing this text from the eaoverview (this 'dd' tag contains the eaoverview)
    eaoverview_text = soup.find(lambda tag: has_specific_text(tag, 'dd', 'Below is a description of the files'))

    # If the Entity and Attribute Overview text is found, modify it
    if eaoverview_text:
        # Go two levels deeper to the second 'dd' tag
        inner_dd = eaoverview_text.find('dd').find('dd')
        
        # If the second 'dd' tag is found, clear its contents
        if inner_dd:
            inner_dd.clear()
            eaoverview_newtext = '''<dd><br />
    <br />
    IMPORTANT INFORMATION<br />
    <br />
    TreeMap{year}_{col_name}.tif is a subset of the full TreeMap{year}.tif. Band values for TreeMap{year}_{col_name}.tif are those found in the attribute table of the full TreeMap{year} dataset.<br />
    <br />
    	  	  <br />
    DATA FILE DESCRIPTIONS (1)<br />
    <br />
    (1) TreeMap{year}_{col_name}.tif: <br />
        Raster dataset (GeoTIFF file) representing a single attribute, {col_name}, of the full model output generated by random forests imputation of forest inventory plot data measured by Forest Inventory and Analysis (FIA) to unsampled (and sampled) spatial locations on the landscape for circa 2016 conditions. {col_name} is a measure of {col_description}. Predictor variables in the random forests imputation were chosen to optimize the prediction of aboveground forest carbon.
        These include topographic variables (slope, aspect, and elevation from the FIA PLOT and COND tables), true plot location, vegetation (forest cover, height, and vegetation group assigned to each plot via Forest Vegetation Simulator [FVS, https://www.fs.fed.us/fvs/, Dixon 2002] and LANDFIRE methods), disturbance (years since disturbance and disturbance type as derived from LANDFIRE disturbance rasters), and biophysical variables (maximum and minimum temperature, relative humidity, precipitation, photosynthetically active radiation, and vapor pressure deficit derived by overlay of the plot coordinates with LANDFIRE biophysical rasters). Variables and methods for the full model output are defined in more completeness in Riley et al. (2016) and Riley et al. (2021), the accompanying Data Dictionary file (“TreeMap{year}_Data_Dictionary.pdf”), and the FIA documentation (Burrill et al. 2018).<br />
    <br />'''.format(year=year, col_name=col_name, col_description=col_descriptions[col_name])
            # Add modified content to the parsed HTML
            new_content_soup = BeautifulSoup(eaoverview_newtext, 'html.parser')
            for element in new_content_soup.contents:
                inner_dd.append(element)   
    
    
    # Process Description
        # Find the 'br' tag that directly precedes the target paragraph
    br_before_target = soup.find(lambda tag: tag.name == 'br' and 'For complete details see Riley et al. (2016)' in tag.find_next_sibling(string=True))
    
    if br_before_target:
        # Create a new string with the new paragraph
        new_paragraph = soup.new_string(f'{col_name} was then separated into its own raster by writing values in the raster attribute table to the corresponding pixels in the raster band.')
    
        # Create two new 'br' tags to put after the new paragraph
        new_br_tag1 = soup.new_tag('br')
        new_br_tag2 = soup.new_tag('br')
    
        # Insert the new paragraph and the new 'br' tags after the 'br' tag before the target paragraph
        br_before_target.insert_after(new_paragraph, new_br_tag1, new_br_tag2)
        
        
    # Standard Order Process
        # Find the 'dt' tag containing the 'Digital_Form:'
    dt_tag = soup.find(lambda tag: has_specific_text(tag, 'dt', 'Digital_Form:'))
    
    # If the 'dt' tag is found, get its parent 'dd' tag and modify it
    if dt_tag:
        # Get the parent 'dd' tag
        order_process_text = dt_tag.find_parent('dd')
    
        # If the 'dd' tag is found, clear its contents and insert new text
        if order_process_text:
            # Clear existing content
            order_process_text.clear()
        
        order_process_newtext = '''<dl><dt>
        <i>Digital_Form:</i>
       </dt>
       <dd>
        <dl>
         <dt>
          <i>Digital_Transfer_Information:</i>
         </dt>
         <dd>
          <dl>
           <dt>
            <i>Format_Name: </i>TIF</dt>
           <dt>
            <i>Format_Version_Number: </i>see Format Specification</dt>
           <dt>
            <i>Format_Specification:</i>
           </dt>
           <dd>GeoTIFF file</dd>
           <dt>
            <i>File_Decompression_Technique: </i>Files zipped with zipfile python library</dt>
          </dl>
         </dd>
         <dt>
          <i>Digital_Transfer_Option:</i>
         </dt>
         <dd>
          <dl>
           <dt>
            <i>Online_Option:</i>
           </dt>
           <dd>
            <dl>
             <dt>
              <i>Computer_Contact_Information:</i>
             </dt>
             <dd>
              <dl>
               <dt>
                <i>Network_Address:</i>
               </dt>
               <dd>
                <dl>
                 <dt>
                  <i>Network_Resource_Name:</i>
                  <a
                   TARGET="viewer"
                   href="https://doi.org/10.2737/RDS-2021-0074">https://doi.org/10.2737/RDS-2021-0074</a>
                 </dt>
                </dl>
               </dd>
              </dl>
             </dd>
            </dl>
           </dd>
          </dl>
         </dd>
        </dl>
       </dd>
       <dt>
        <i>Fees: </i>none</dt>
      </dl>'''
    
        # Add modified content to the parsed HTML
        new_content_soup = BeautifulSoup(order_process_newtext, 'html.parser')
        for element in new_content_soup.contents:
            order_process_text.append(element)


    # Metadata Date
    metadata_date_tag = soup.find(lambda tag: has_specific_text(tag, 'dt', 'Metadata_Date:'))

    # If the Metadata Date tag is found, modify it
    if metadata_date_tag:
        # Extract the tag that contains the metadata date
        i_tag = metadata_date_tag.find('i')
    
        # Create a new tag with the current date
        new_date = datetime.now()
        new_date_tag = soup.new_string(' ' + new_date.strftime('%Y%m%d'))
    
        # Remove the original metadata date
        metadata_date_tag.clear()
    
        # Add the modified metadata date to the parsed HTML
        metadata_date_tag.append(i_tag)
        metadata_date_tag.append(new_date_tag)
        
    # Save the modified HTML to a new file
    with open(os.path.join(outputFolder, f'TreeMap{year}_{col_name}.html'), 'w', encoding='utf-8') as f:
        f.write(str(soup))
    

def create_arc_metadata(col_name):
    '''
    Creates *.tif.xml metadata file that is compatible with ArcGIS Pro. Uses the treemap arcmeta template stored in the supp_files folder in the repo, as well as the existing .xml file for the attribute

    Args:
        col_name (str): Name of column or attribute being processed (e.g. CARBON_D)

    Returns:
        None
    '''
    
    # Filepath for attribute's base xml file
    att_xml_metadata = os.path.join(outputFolder, f'TreeMap{year}_{col_name}.xml')
    
    # Construct filepath for new xml based on the provided attribute xml 
    new_meta_file = os.path.join(outputFolder, f'TreeMap{year}_{col_name}.tif.xml')
    
    # Open existing attribute xml
    source_tree = ET.parse(att_xml_metadata)
    source_root = source_tree.getroot()
    
    # Open template xml
    template_file = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'supp_files', 'TreeMap_ArcMeta_template.xml')
    template_tree = ET.parse(template_file)
    template_root = template_tree.getroot()
    
    # Open raster (for using image metadata)
    raster = gdal.Open(f'{outputFolder}\TreeMap{year}_{col_name}.tif')
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
        'eainfo//enttypd': 'eaover'
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
    template_root.find('.//DataProperties//itemName').text = f'TreeMap{year}_{col_name}.tif'
    template_root.find('.//DataProperties//itemLocation//linkage').text = str(check_file_in_folder(att_xml_metadata, f'TreeMap{year}_{col_name}.tif'))
    template_root.find('.//SyncDate').text = datetime.now().strftime('%Y%m%d')
    template_root.find('.//SyncTime').text = datetime.now().strftime('%M%S%H')
    template_root.find('.//ModDate').text = datetime.now().strftime('%Y%m%d')
    template_root.find('.//ModTime').text = datetime.now().strftime('%M%S%H')

        # Set raster metadata
    template_root.find('.//coordRef').text = raster.GetProjection()
    template_root.find('.//AttributeDescription').text = col_name + ': ' + col_descriptions[col_name]
    template_root.find('.//BandMinValue').text = str(band.GetStatistics(True, False)[0])
    template_root.find('.//BandMaxValue').text = str(band.GetStatistics(False, True)[1])
    template_root.find('.//BandUnits').text = col_descriptions[col_name]
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
    if col_name in discrete_attributes:
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
    template_root.find('.//onLineSrc').text = 'https://data.fs.usda.gov/geodata/rastergateway/treemap/'
    template_root.find('.//eainfo//enttypl').text = col_name
    template_root.find('.//eainfo//enttypds').text = col_descriptions[col_name]
        
    # Write the file
    template_tree.write(new_meta_file)

    
def create_arc_stats(col_name):
    '''
    Creates *.tif.aux.xml metadata stats file that is compatible with ArcGIS Pro. Uses the treemap arcstats template stored in the supp_files folder in the repo, as well as the existing .xml file for the attribute

    Args:
        col_name (str): Name of column or attribute being processed (e.g. CARBON_D)

    Returns:
        None
    '''
    
    # Create statistics metadata (*.tif.aux.xml)
        # Open template file
    stats_template_file = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'supp_files', 'TreeMap_ArcStats_template.xml')
    stats_template_tree = ET.parse(stats_template_file)
    stats_template_root = stats_template_tree.getroot()
    
    raster = gdal.Open(f'{outputFolder}\TreeMap{year}_{col_name}.tif')
    band = raster.GetRasterBand(1)
    
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
    band_data = band.ReadAsArray()
    no_data_value = band.GetNoDataValue()
    valid_data = band_data[band_data != no_data_value]
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
    stats_template_tree.write(os.path.join(outputFolder, f'TreeMap{year}_{col_name}.tif.aux.xml'))
    
    
def insert_text_before_xml(element, insert_before, new_text):
    '''
    Inserts new text before the specified text.

    Args:
        element (ElementTree element): Element containing the full text.
        insert_before (str): Text to insert new text before.
        new_text (str): New text to be inserted.

    Returns:
        None.
    '''
    
	# Find the index of the insert_before text in the source text
    index = element.text.find(insert_before)
    
    # If the insert_before text is not found, return the original source text
    if index == -1:
        return

	# Otherwise, insert the new text before the insert_before text
    element.text = element.text[:index] + new_text + element.text[index:]


def remove_extra_digital_forms_xml(element):
    '''
    Removes digital forms from the xml that aren't the form for the TIF

    Args:
        element (ElementTree element): Stadard Order Process element.

    Returns:
        None.
    '''
    
    for digform in element.findall('digform'):
        if digform.find('.//formname').text != 'TIF':
            element.remove(digform)
            
            
def has_color_map(band):
    '''
    Takes in a raster band and checks if it has a color map
        
    Args:
        band (gdal.Band): Band of raster to check.

    Returns:
        bool: True if raster band has a color map 
    '''
    
    if band.GetRasterColorTable() != None:
        return True
    else:
        return False


def get_min_max(filename):
    dataset = gdal.Open(filename)
    band = dataset.GetRasterBand(1)
    min_val, max_val, _, _ = band.GetStatistics(True, True)
    return min_val, max_val
    

def zip_files(file_names, zip_file_name):
    '''
    Zips files together and writes a new .zip
    
    Args: 
        file_names: Filepaths, including new file name, to be zipped.
        zip_file_name: Filepath of the output .zip
        
    Returns:
        None
    '''
    
    # Create a ZipFile object in write mode
    with ZipFile(zip_file_name, 'w') as zipf:
        # Loop through files and add them to the zip file
        for file_name in file_names:
            zipf.write(file_name)
            
            
def determine_resample(col_name):
    '''
    Determines the resample method for pyramids.

    Args:
        col_name (str): Name of column or attribute being processed (e.g. CARBON_D)

    Returns:
        str: A string ('MODE' or 'NEAREST') of the resample method
    '''
    
    if col_name in ('STDSZCD', 'FLDSZCD', 'FLDTYPCD', 'FORTYPCD'):
        return 'MODE'
    else:
        return 'AVERAGE'


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
        
        
def determine_year():
    '''
    Determines the year of the TreeMap dataset based on its filename (the value of treeMapTif)

    Args:
        None.

    Returns:
        str: The year of the dataset
    '''
    
    # Get name of file
    filename = os.path.basename(treeMapTif)
    
    os.path.basename(treeMapTif)
    
    # Split string to exclude 'TreeMap'
    split_filename = filename.split('TreeMap')[1]
    
    # Return the next 4 characters in the string (which should be the year)
    return split_filename[:4]


def inform_input_output():
    '''
    Prints the filepaths of the main dataset and output folder. Prompts the user to confirm they are correct.

    Args:
        None.

    Returns:
        None.
    '''
    
    print('\n\n****************************************************************************************************')
    print('Input tif: ' + treeMapTif)
    print('Input dbf: ' + treeMapDbf)
    print('Input xml: ' + treeMapXml)
    print('Input html: ' + treeMapHtml)
    print('Output folder: ' + outputFolder)
    print('****************************************************************************************************\n')
    
    # Check with user if filepaths are correct
    print("If filepaths are correct press 'enter', otherwise press 'q' to quit and please update filepaths in script:")
    key = msvcrt.getch()
    if key.lower() == b'q':
        quit()
    
    
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
    

def replace_text_html(node, text_to_remove, replacement_text):
    """
    This function recursively traverses a BeautifulSoup node (HTML element or text), and removes the specified text from any text node it encounters.

    Args:
        node (bs4.element.Tag or bs4.element.NavigableString): The BeautifulSoup node (HTML element or text) to traverse.
        text_to_remove (str): The text to be removed from any text nodes encountered during traversal.
        replacement_text (str): The text to replace the removed text with.

    Returns:
        None
    """
    
    # If the node is a text node (NavigableString), check if the specified text is in it
    if isinstance(node, NavigableString):
        # If the specified text is found, replace it with an empty string
        if text_to_remove in node:
            node.replace_with(node.replace(text_to_remove, replacement_text))
    else:
        # If the node is not a text node, it must be an HTML element node
        # In this case, recursively call remove_text on each of its child nodes (content)
        for child_node in node.contents:
            replace_text_html(child_node, text_to_remove, replacement_text)
        
        

######################################################################
# Main Function Calls
######################################################################

# Determine the year from the filepath
year = determine_year()

# Inform user of assigned inputs + outputs
inform_input_output()

# Main Function
for col_name, gdal_dtype in cols:
    start_time = time.perf_counter()
    
    attributeToImage(col_name, gdal_dtype)
    
    end_time = time.perf_counter()
    elapsed = (end_time - start_time)/60
    print(f'\nTime to complete: {elapsed} minutes')
    
