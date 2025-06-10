'''
Uploads TreeMap attributes to gcloud and manifests them as bands of a single Earth Engine image stored in an image collection.

Run for each image you'd like in the image collection (e.g., once for CONUS image, once for ALASKA image).

YOU MUST SET THE START AND END DATE OF THE IMAGE COLLECTION IN EARTH ENGINE MANUALLY (THESE CAN BE SET IN THE SCRIPT FOR THE INDIVIDUAL IMAGES)
'''

import os, ee
from geeViz import assetManagerLib as aml
from glob import glob
from osgeo import gdal

#################################################
# User Variables (edit these)
#################################################

# Year of the dataset and associated landfire version
year = '2022'
study_area = 'CONUS'

if year == '2020':
    landfire_ver = '2.2.0'
else: 
    landfire_ver = '2.3.0'

# The folder in which to look for images, and the format of the file names to select
image_folder = rf'\\166.2.126.25\TreeMap\08_Data_Delivery\01_Separated_Attribute_Rasters\{year}'
name_format = f'TreeMap{year}_{study_area}_*.tif'

# Google Cloud bucket to upload the attributes to and the project id
gcs_bucket = f'gs://separated-attributes/{year}'
project_id = 'treemap-386222'

# Earth Engine paths
    # The folder to save the image collection in
gee_folder = f'projects/treemap-386222/assets/Final_Outputs/{year}'
    # The name of the image collection to create
gee_image_collection_name = f'TreeMap{year}'
    # The name of the image to create
gee_image_name = f'TreeMap{year}_{study_area}'

# pyramidPolicy lookup for attributes
pyramidPolicy_lookup = {
    'FORTYPCD': 'MODE',         #Nominal attribute
    'FLDTYPCD':'MODE',          #Nominal attribute
    'STDSZCD':'MODE',           #Ordinal attribute
    'FLDSZCD':'MODE',           #Ordinal attribute
    'BALIVE':'MEAN',            #Continuous attributes
    'CANOPYPCT':'MEAN',         #Continuous attributes
    'STANDHT':'MEAN',           #Continuous attributes
    'ALSTK':'MEAN',             #Continuous attributes
    'GSSTK':'MEAN',             #Continuous attributes
    'QMD':'MEAN',               #Continuous attributes
    'SDIsum':'MEAN',            #Continuous attributes
    'TPA_LIVE':'MEAN',          #Continuous attributes
    'TPA_DEAD':'MEAN',          #Continuous attributes
    'VOLCFNET_L':'MEAN',        #Continuous attributes
    'VOLCFNET_D':'MEAN',        #Continuous attributes
    'VOLBFNET_L':'MEAN',        #Continuous attributes
    'DRYBIO_L':'MEAN',          #Continuous attributes
    'DRYBIO_D':'MEAN',          #Continuous attributes
    'CARBON_L':'MEAN',          #Continuous attributes
    'CARBON_D':'MEAN',          #Continuous attributes
    'CARBON_DWN':'MEAN',        #Continuous attributes
    'TM_ID' : 'MODE'            #Nominal attribute
}

# Properties for the image collection
image_collection_properties = {
    'year': year
}

# Properties for the image
    # time_start and time_end should be a span of one year (e.g., 2022-01-01, 2023-01-01)
    # Properties can be changed and new ones can be added manually in Earth Engine if needed
image_properties = {
    'study_area': study_area, 
    'year': year,
    'landfire_ver': landfire_ver,
    'system:time_start': ee.Date(f'{year}-01-01'),
    'system:time_end': ee.Date(f'{int(year)+1}-01-01')
}

#################################################
# Script Setup
#################################################

# Initialize Google Cloud and Earth Engine with the project
ee.Authenticate()
ee.Initialize(project=project_id)

# For each image in the directory that matches the name format, add it to a dictionary and save its pyramiding policy, band name, and no data value
image_dict = {}
for image in glob(os.path.join(image_folder, name_format)):
    image_dict[image] = {}
    # Check for the resample type
    for attribute in pyramidPolicy_lookup.keys():
        if attribute in os.path.basename(image):
            image_dict[image]['pyramidingPolicy'] = pyramidPolicy_lookup[attribute]
            image_dict[image]['bandName'] = attribute

    # Get the noDataValue via gdal. Store NoData value for correct masking during upload 
    ds = gdal.Open(image)
    band = ds.GetRasterBand(1)
    no_data_val = band.GetNoDataValue()
    image_dict[image]['noDataValue'] = no_data_val

#################################################
# Main process: create collection and upload
#################################################

# Create the image collection in EE assets
aml.create_image_collection(f'{gee_folder}/{gee_image_collection_name}', image_collection_properties)

# Upload each attribute raster as a band in a single EE image
aml.uploadToGEEAssetImagesAsBands(
    image_dict,
    gcs_bucket,
    f'{gee_folder}/{gee_image_collection_name}/{gee_image_name}',
    overwrite=False,
    properties=image_properties
    )





