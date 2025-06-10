'''
Copies TreeMap attributes from an image within an image collection in EE where each attribute is a band, to an image collection where each attribute is a single band image.

Adds properties (version and attribute) to each image. 

The output file is used in the TreeMap viewer. 

Last update: 05/15/2025 (MOB)
'''


import ee
from geeViz import assetManagerLib as aml

# Initialize Google Cloud and Earth Engine with the project
project_id = 'treemap-386222'
ee.Authenticate()
ee.Initialize(project=project_id)


#################################################
# User Variables (edit these)
#################################################
treeMapYear = '2022'
study_area = 'CONUS'

input_path = f'projects/treemap-386222/assets/Final_Outputs/{treeMapYear}/TreeMap{treeMapYear}'   # Path to input image collection
output_path = f'projects/treemap-386222/assets/Final_Outputs/{treeMapYear}/TreeMap_{treeMapYear}'  # Folder where single-band images go




#################################################
# Process each image
#################################################

image_list = aml.listAssets(input_path)
#print(image_list)
image_collection = ee.ImageCollection.fromImages([ee.Image(img) for img in image_list])


for img in image_list:
    ee_img = ee.Image(img)
    img_id = img.split('/')[-1]  # Get image name without path

    band_names = ee_img.bandNames().getInfo()

    for band in band_names:

        attribute_name = band.replace(f"TreeMap{treeMapYear}_{study_area}_", "")

        single_band = ee_img.select([band]).set({
            'version': treeMapYear,
            'attribute': attribute_name
        })

        export_name = f'{img_id}_{band}'
        export_path = f'{output_path}/{export_name}'

        # Check if already exists
        if not aml.ee_asset_exists(export_path):
            print(f'Exporting: {export_path}')

            # Export to asset
            task = ee.batch.Export.image.toAsset(
                image=single_band,
                description=export_name,
                assetId=export_path,
                region=single_band.geometry().bounds(),  
                scale=30,  
                maxPixels=1e13
            )
            task.start()
        else:
            print(f'Skipped (already exists): {export_path}')


