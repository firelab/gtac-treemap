'''
This script is a work-in-progress.

It takes all tifs in the format 'TreeMap{year}_*.tif' and uses the assetManagerLib.py from geeViz to upload to gcloud
and manifest an asset into earth engine.
'''

import os
from glob import glob
import geeViz

tm_ver = '2016'
image_folder = r'\\166.2.126.25\TreeMap\03_Outputs\04_Separated_Attribute_Rasters\2016'
gcs_bucket = ''
gee_asset_path = ''

# Get all the images
images = glob(os.path.join(image_folder, f'TreeMap{tm_ver}_*.tif'))

# Pseudo-code, take many images and combine into one via geeViz
# geeViz.uploadToGEEImageAsset(images, gcs_bucket, gee_asset_path)