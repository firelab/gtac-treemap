### UPDATED LANDFIRE DISTURBANCE LAYER CREATION
# Objective: have one script that we can run start to finish that will pull in all landfire disturbance layers 1999-2016 and create a slow loss layer for TreeMap
# This script is NOT YET pulling in LCMS slow loss
# Objective is to ensure that we can replicate the TreeMap Workflow and get an identical product

#import packages
import arcpy
from arcpy import env
from arcpy.sa import *
arcpy.CheckOutExtension("Spatial")

# set snap raster
# UPDATE THIS PATH TO TREEMAP NAS
arcpy.env.snapRaster = "G:\\TreeMap2016\\Spatial_data\\Landfire_Remap_2016\\national\\disturbance\\dist2015.tif"

# RECLASS TO FIRE/INSECT
# reclass to unique code for fire and insects/disease for each year (all other disturbances are considered NoData)
# one at a time: 
# pull in landfire raster for YEAR, and reclass to FIRE / INSECT DISEASE only
################################################################################

# ADAPT THIS FOR EACH YEAR
# 2015: fire=20151, insect/disease=20152, neither="NoData"
# update inRaster path
base_path = '\\166.2.126.25\TreeMap\01_Data\02_Landfire\LF_220\Disturbance\LF2016_Dist_200_CONUS\Tif'

inRaster = "G:\\TreeMap2016\\Spatial_data\\Landfire_Remap_2016\\national\\disturbance\\US_200D2015\\Tif\\us_200d2015.tif" 
# don't update this command
outReclass2 = Reclassify(inRaster, "VALUE", RemapRange([[-10000,1,"NODATA"],[10,234,20151], [410,464,"NODATA"], [470,504,20151],[520,534,"NODATA"], [540,564,20152], [570,584,"NODATA"],[600,764,"NODATA"], [770,804,20151], [810,815,"NODATA"],[820,834,"NODATA"], [840,854,20152], [870,884,"NODATA"], [910,962,"NODATA"], [970,1002,20151], [1010,1032,"NODATA"], [1040,1062,20152], [1070,1133,"NODATA"]]))
#update this out path
outReclass2.save("G:\\TreeMap2016\\working_KLR\\disturbance\\us_dist2015_reclass.tif")

# PULL OUT FIRE ONLY
# for each year, for each fire/insect raster, get a raster with ONLY fire
####################################################################################

#adapt for each year

print("Making fire only rasters")

# 2015: fire=20151, other="NoData"
inRaster = "G:\\TreeMap2016\\working_KLR\\disturbance\\us_dist2015_reclass.tif"
outReclass1 = Reclassify(inRaster, "VALUE", RemapRange([[20151,20151,20151],[20152,20152,"NODATA"], [-33000,0,"NODATA"]]))
outReclass1.save("G:\\TreeMap2016\\working_KLR\\disturbance\\us_dist2015_fire.tif")

# PULL OUT INSECT ONLY
# FOR EACH YEAR
####################################################################################



# MOSAIC ALL FIRE RASTERS TO GET MOST RECENT YEAR
####################################################################################

# mosaic all fire rasters, with most recent year taking precedence
print("mosaic all fire rasters with most recent year taking precedence")
#UPDATE THIS TO REFER TO ALL INPUT FIRE RASTERS
# could paste file names w/ path to create this as a list-- OR just list all input rasters
inRasters = "G:\\Tree_List_c2014\\target_data\\working_KLR\\disturbance\\disturbance_fire_most_recent_1999_2014.tif;G:\\TreeMap2016\\working_KLR\\disturbance\\us_dist2015_fire.tif;G:\\TreeMap2016\\working_KLR\\disturbance\\us_dist2016_fire.tif"
outputLocation = "G:\\TreeMap2016\\working_KLR\\disturbance"
outRaster = "disturbance_fire_most_recent_1999_2016.tif"
coordsys = arcpy.Describe("G:\\TreeMap2016\\working_KLR\\disturbance\\us_dist2015_fire.tif").spatialReference
pixelType = "16_BIT_SIGNED"
cellsize = "30"
mosaic_method = "LAST"
arcpy.MosaicToNewRaster_management(inRasters, outputLocation, outRaster, coordsys, pixelType, cellsize, "1", "LAST", "#")
outGrid = outputLocation + "\\" + outRaster
arcpy.BuildRasterAttributeTable_management(outGrid)