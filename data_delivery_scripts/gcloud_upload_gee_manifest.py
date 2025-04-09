'''
This script is a work-in-progress.

It takes all tifs in the format 'TreeMap{year}_*.tif' and uses the assetManagerLib.py from geeViz 
to upload to gcloud and manifest an asset into earth engine.
'''

import os
from glob import glob
from geeViz import assetManagerLib as aml
tm_ver = '2022'
image_folder = r'\\166.2.126.25\TreeMap\03_Outputs\04_Separated_Attribute_Rasters\2022'
gcs_bucket = 'separated-attributes/2022/'
gee_asset_path = 'treemap-386222/assets/Final_Outputs/2022/TreeMap_CONUS_2022_SeparatedAttributes'

# Get all the images
images = glob(os.path.join(image_folder, f'TreeMap_CONUS_{tm_ver}_*.tif'))

# Band names
band_names_pyramids = [
    'FORTYPCD':'MODE',
    'FLDTYPCD':'MODE',
    'STDSZCD':'MODE',
    'FLDSZCD':'MODE',
    'BALIVE':'MEAN',
    'CANOPYPCT':'MEAN',
    'STANDHT':'MEAN',
    'ALSTK':'MEAN',
    'GSSTK':'MEAN',
    'QMD':'MEAN',
    'SDIsum':'MEAN',
    'TPA_LIVE':'MEAN',
    'TPA_DEAD':'MEAN',
    'VOLCFNET_L':'MEAN',
    'VOLCFNET_D':'MEAN',
    'VOLBFNET_L':'MEAN',
    'DRYBIO_L':'MEAN',
    'DRYBIO_D':'MEAN',
    'CARBON_L':'MEAN',
    'CARBON_D':'MEAN',
    'CARBON_DOWN_DEAD':'MEAN'
]

properties = {
    'studyArea': 'CONUS',
    'year': tm_ver,
}

# Take many images and combine into one via geeViz 
aml.uploadToGEEImageAsset(
    images,
    gcs_bucket,
    gee_asset_path,
    overwrite=False,
    bandNames=list(band_names_pyramids.keys()),
    properties=properties,
    pyramidingPolicy=list(band_names_pyramids.values()),
    noDataValues=None,
    )