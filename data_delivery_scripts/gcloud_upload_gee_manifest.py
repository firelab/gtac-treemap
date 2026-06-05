'''
Uploads TreeMap attributes to gcloud and manifests them as bands of a single Earth Engine image stored in an image collection.
 
Run for each image you'd like in the image collection (e.g., once for CONUS image, once for ALASKA image).
 
YOU MUST SET THE START AND END DATE OF THE IMAGE COLLECTION IN EARTH ENGINE MANUALLY (THESE CAN BE SET IN THE SCRIPT FOR THE INDIVIDUAL IMAGES)
'''
#%%
import os, ee
from glob import glob
from osgeo import gdal
gdal.UseExceptions()
# Initialize Google Cloud and Earth Engine with the project
ee.Authenticate()
ee.Initialize(project='treemap-386222')
from geeViz import assetManagerLib as aml
#%%
#################################################
# User Variables (edit these)
#################################################
 
# Year of the dataset and associated landfire version
year = '2023'
study_area = 'CONUS'
landfire_ver = '2.4.0'
 
# The folder in which to look for images, and the format of the file names to select
image_folder = rf'\\afssxgtacnas104\TreeMap\08_Data_Delivery\01_Separated_Attribute_Rasters\2023_Production_rerun_final_zone_imputations'
name_format = f'TreeMap{year}_{study_area}_*.tif'
 
# Google Cloud bucket to upload the attributes to and the project id
gcs_bucket = f'gs://separated-attributes/2026_UPDATE/{year}'
project_id = 'treemap-386222'
#%%
# Earth Engine paths
    # The folder to save the image collection in
gee_folder = f'projects/treemap-386222/assets/2026_UPDATE/{year}'
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
    'BALIVE':'MEAN',            #Continuous attributes/
    'CANOPYPCT':'MEAN',         #Continuous attributes/
    'STANDHT':'MEAN',           #Continuous attributes/
    'ALSTK':'MEAN',             #Continuous attributes/
    'GSSTK':'MEAN',             #Continuous attributes/
    'QMD':'MEAN',               #Continuous attributes/
    'SDIsum':'MEAN',            #Continuous attributes/
    'TPA_LIVE':'MEAN',          #Continuous attributes/
    'TPA_DEAD':'MEAN',          #Continuous attributes/
    'VOLCFNET_L':'MEAN',        #Continuous attributes/
    'VOLCFNET_D':'MEAN',        #Continuous attributes/
    'VOLBFNET_L':'MEAN',        #Continuous attributes/
    'DRYBIO_L':'MEAN',          #Continuous attributes/
    'DRYBIO_D':'MEAN',          #Continuous attributes/
    'CARBON_L':'MEAN',          #Continuous attributes/
    'CARBON_D':'MEAN',          #Continuous attributes/
    'CARBON_DWN':'MEAN',        #Continuous attributes/
    'TM_ID' : 'MODE'            #Nominal attribute
}
 
# Properties for the image collection
image_collection_properties = {
    #'year': year
}
 

#%%
#################################################
# Script Setup
#################################################

# dictionary for landfire version / naming info based on year
landfire_version_lookup = {
    '2016': {'year': '2016', 'name': 'LF_2016', 'version': '2.0.0', 'number': '200'},
    '2020': {'year': '2020', 'name': 'LF_2020', 'version': '2.2.0', 'number': '220'},
    '2022': {'year': '2022', 'name': 'LF_2022', 'version': '2.3.0', 'number': '230'},
    '2023': {'year': '2023', 'name': 'LF_2023', 'version': '2.4.0', 'number': '240'},
    '2024': {'year': '2024', 'name': 'LF_2024', 'version': '2.5.0', 'number': '250'}
}

landfire_info = landfire_version_lookup.get(year, {})


# Properties for the image
    # time_start and time_end should be a span of one year (e.g., 2022-01-01, 2023-01-01)
    # Properties can be changed and new ones can be added manually in Earth Engine if needed
image_properties = {
    'study_area': study_area,
    'year': year,
    'landfire_ver': landfire_info.get('version', 'unknown'),
    'landfire_version_name': landfire_info.get('name', 'unknown'),
    'landfire_version_number': landfire_info.get('number', 'unknown'),
    'system:time_start': ee.Date(f'{year}-01-01'),
    'system:time_end': ee.Date(f'{int(year)+1}-01-01')
}



#%%
# For each image in the directory that matches the name format, add it to a dictionary and save its pyramiding policy, band name, and no data value
if not os.path.isdir(image_folder):
    raise FileNotFoundError(
        f"Image folder does not exist or is not accessible: {image_folder}\n"
        "If this is a network path, confirm VPN/share access and permissions before running again."
    )

matched_images = sorted(glob(os.path.join(image_folder, name_format)))
if not matched_images:
    raise ValueError(
        f"No input TIFFs matched pattern '{name_format}' in '{image_folder}'.\n"
        "At least one source raster is required to build the EE import manifest tilesets."
    )

print(f"Found {len(matched_images)} input TIFF(s) matching '{name_format}'.")
image_dict = {}
seen_band_names = {}
for image in matched_images:
    image_dict[image] = {}
    base_name = os.path.basename(image)
    stem = os.path.splitext(base_name)[0]
    expected_prefix = f'TreeMap{year}_{study_area}_'
    attribute_token = stem.removeprefix(expected_prefix)

    # Prefer an exact attribute token match; fall back to longest substring match for legacy names.
    if attribute_token in pyramidPolicy_lookup:
        found_attribute = attribute_token
    else:
        found_attribute = None
        for attribute in sorted(pyramidPolicy_lookup.keys(), key=len, reverse=True):
            if attribute in base_name:
                found_attribute = attribute
                break

    if found_attribute is None:
        raise ValueError(
            f"Could not infer a band name/pyramiding policy from file name: {base_name}\n"
            "Expected one of the attribute keys in pyramidPolicy_lookup to be present in the file name."
        )

    if found_attribute in seen_band_names:
        raise ValueError(
            "Multiple input TIFFs resolved to the same Earth Engine band name.\n"
            f"Band name: {found_attribute}\n"
            f"First file: {seen_band_names[found_attribute]}\n"
            f"Second file: {image}\n"
            "Rename or remove the duplicate source file, or tighten the band-name parsing logic."
        )

    seen_band_names[found_attribute] = image
    image_dict[image]['pyramidingPolicy'] = pyramidPolicy_lookup[found_attribute]
    image_dict[image]['bandName'] = found_attribute
    image_dict[image]['gcsURI'] = f"{gcs_bucket}/{os.path.basename(image)}"
 
    # Get the noDataValue via gdal. Store NoData value for correct masking during upload
    ds = gdal.Open(image)
    band = ds.GetRasterBand(1)
    no_data_val = band.GetNoDataValue()
    image_dict[image]['noDataValue'] = no_data_val
 
#%%
 
#################################################
# Main process: create collection and upload
#################################################
 
# Create the image collection in EE assets
aml.create_image_collection(f'{gee_folder}/{gee_image_collection_name}', image_collection_properties)
 
# Upload each attribute raster as an image in gcs
for image in image_dict.keys():
    aml.uploadTifToGCS(image, gcs_bucket)
    

#%%

# Check to make sure all images have a gcsURI before attempting to ingest
for image, info in image_dict.items():
    if 'gcsURI' not in info:
        raise ValueError(
            f"Image is missing a gcsURI and cannot be ingested: {image}\n"
            "Check that the file was uploaded to GCS successfully and that the gcsURI was stored in the image_dict correctly."
        )
    else: 
        print(f"Image ready for ingestion:\nBand name: {info['bandName']} \nGCS URI: {info['gcsURI']}")

# Combine images in gcs to a single image with multiple bands that exists as an asset
aml.ingestFromGCSImagesAsBands(
    image_dict.values(),
    f'{gee_folder}/{gee_image_collection_name}/{gee_image_name}',
    overwrite=True,
    properties=image_properties
)



 
 
# %%
# 