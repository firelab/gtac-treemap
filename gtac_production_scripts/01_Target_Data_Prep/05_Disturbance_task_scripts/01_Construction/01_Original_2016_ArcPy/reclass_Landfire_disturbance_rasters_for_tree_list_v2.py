# reclassify Landfire disturbance rasters for tree list application
# written by Karin Riley, 2/13/2018
# updated for c2016 tree list by Karin Riley, 3/9/2020 and 10/19/2020

import arcpy
from arcpy import env
from arcpy.sa import *
arcpy.CheckOutExtension("Spatial")

# set snap raster
arcpy.env.snapRaster = "G:\\TreeMap2016\\Spatial_data\\Landfire_Remap_2016\\national\\disturbance\\dist2015.tif"


# reclass to unique code for fire and insects/disease for each year (all other disturbances are considered NoData)

print("Reclassifying to fire and insect/disease codes")
# 2015: fire=20151, insect/disease=20152, neither="NoData"
inRaster = "G:\\TreeMap2016\\Spatial_data\\Landfire_Remap_2016\\national\\disturbance\\US_200D2015\\Tif\\us_200d2015.tif"
outReclass2 = Reclassify(inRaster, "VALUE", RemapRange([[-10000,1,"NODATA"],[10,234,20151], [410,464,"NODATA"], [470,504,20151],[520,534,"NODATA"], [540,564,20152], [570,584,"NODATA"],[600,764,"NODATA"], [770,804,20151], [810,815,"NODATA"],[820,834,"NODATA"], [840,854,20152], [870,884,"NODATA"], [910,962,"NODATA"], [970,1002,20151], [1010,1032,"NODATA"], [1040,1062,20152], [1070,1133,"NODATA"]]))
outReclass2.save("G:\\TreeMap2016\\working_KLR\\disturbance\\us_dist2015_reclass.tif")

# 2016: fire=20161, insect/disease=20162, neither="NoData"
inRaster = "G:\\TreeMap2016\\Spatial_data\\Landfire_Remap_2016\\national\\disturbance\\US_200D2016\\Tif\\us_200d2016.tif"
outReclass2 = Reclassify(inRaster, "VALUE", RemapRange([[-10000,1,"NODATA"],[10,234,20161], [410,464,"NODATA"], [470,504,20161],[520,534,"NODATA"], [540,564,20162], [570,584,"NODATA"],[600,764,"NODATA"], [770,804,20161], [810,815,"NODATA"],[820,834,"NODATA"], [840,854,20162], [870,884,"NODATA"], [910,962,"NODATA"], [970,1002,20161], [1010,1032,"NODATA"], [1040,1062,20162], [1070,1133,"NODATA"]]))
outReclass2.save("G:\\TreeMap2016\\working_KLR\\disturbance\\us_dist2016_reclass.tif")


# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# to follow the same logic that was used with the FIA plots, if an area burned,
#  it's assigned the most recent burn year. If it hasn't burned, the most recent
# year of insect/disease is assigned (where applicable). In other words,
# burning takes priority over a more recent insect infestation (disturbance data onl
# only goes back about 11 years).

# Therefore, we need rasters with only the burn info (all other pixels "No Data")
# (This has already been done for 1999-2012)

print("Making fire only rasters")

# 2015: fire=20151, other="NoData"
inRaster = "G:\\TreeMap2016\\working_KLR\\disturbance\\us_dist2015_reclass.tif"
outReclass1 = Reclassify(inRaster, "VALUE", RemapRange([[20151,20151,20151],[20152,20152,"NODATA"], [-33000,0,"NODATA"]]))
outReclass1.save("G:\\TreeMap2016\\working_KLR\\disturbance\\us_dist2015_fire.tif")

# 2016: fire=20161, other="NoData"
inRaster = "G:\\TreeMap2016\\working_KLR\\disturbance\\us_dist2016_reclass.tif"
outReclass1 = Reclassify(inRaster, "VALUE", RemapRange([[20161,20161,20161],[20162,20162,"NODATA"], [-33000,0,"NODATA"]]))
outReclass1.save("G:\\TreeMap2016\\working_KLR\\disturbance\\us_dist2016_fire.tif")

# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# we also need rasters with only the insect/diease info (all other pixels "No Data")

print("making insect/disease only rasters")

# 2015: insect/disease=20152, other="NoData"
inRaster = "G:\\TreeMap2016\\working_KLR\\disturbance\\us_dist2015_reclass.tif"
outReclass1 = Reclassify(inRaster, "VALUE", RemapRange([[20151,20151,"NODATA"],[20152,20152,20152], [-33000,0,"NODATA"]]))
outReclass1.save("G:\\TreeMap2016\\working_KLR\\disturbance\\us_dist2015_insectdisease.tif")

# 2016: insect/disease=20162, other="NoData"
inRaster = "G:\\TreeMap2016\\working_KLR\\disturbance\\us_dist2016_reclass.tif"
outReclass1 = Reclassify(inRaster, "VALUE", RemapRange([[20161,20161,"NODATA"],[20162,20162,20162], [-33000,0,"NODATA"]]))
outReclass1.save("G:\\TreeMap2016\\working_KLR\\disturbance\\us_dist2016_insectdisease.tif")

# ---------------------------------------------------------------------------------------------------------------------------------------------------
# mosaic all fire rasters, with most recent year taking precedence
print("mosaic all fire rasters with most recent year taking precedence")
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

# ---------------------------------------------------------------------------------------------------------------------------------------------------
# mosaic all insect and disease rasters, with most recent year taking precedence
print("mosaic all insect/disease rasters with most recent year taking precedence")
inRasters = "G:\\Tree_List_c2014\\target_data\\working_KLR\\disturbance\\disturbance_insectdisease_most_recent_1999_2014.tif;G:\\TreeMap2016\\working_KLR\\disturbance\\us_dist2015_insectdisease.tif;G:\\TreeMap2016\\working_KLR\\disturbance\\us_dist2016_insectdisease.tif"
outputLocation = "G:\\TreeMap2016\\working_KLR\\disturbance"
outRaster = "disturbance_insectdisease_most_recent_1999_2016.tif"
coordsys = arcpy.Describe("G:\\TreeMap2016\\working_KLR\\disturbance\\us_dist2015_insectdisease.tif").spatialReference
pixelType = "16_BIT_SIGNED"
cellsize = "30"
mosaic_method = "LAST"
arcpy.MosaicToNewRaster_management(inRasters, outputLocation, outRaster, coordsys, pixelType, cellsize, "1", "LAST", "#")
outGrid = outputLocation + "\\" + outRaster
arcpy.BuildRasterAttributeTable_management(outGrid)

# ---------------------------------------------------------------------------------------------------------------------------------------------------
# mosaic all fires and all insect/disease rasters, with fire taking priority
print("mosaic all fires and all insect/disease rasters, with fire taking priority")
inRasters = "G:\\TreeMap2016\\working_KLR\\disturbance\\disturbance_insectdisease_most_recent_1999_2016.tif;G:\\TreeMap2016\\working_KLR\\disturbance\\disturbance_fire_most_recent_1999_2016.tif"
outputLocation = "G:\\TreeMap2016\\working_KLR\\disturbance"
outRaster = "disturbance_insect_disease_fire_most_recent_1999_2016.tif"
coordsys = arcpy.Describe("G:\\TreeMap2016\\working_KLR\\disturbance\\us_dist2015_fire.tif").spatialReference
pixelType = "16_BIT_SIGNED"
cellsize = "30"
mosaic_method = "LAST"
arcpy.MosaicToNewRaster_management(inRasters, outputLocation, outRaster, coordsys, pixelType, cellsize, "1", "LAST", "#")

# ---------------------------------------------------------------------------------------------------------------------------------------------------
# reclass to disturbance type
print("reclass to disturbance type")
inRaster = "G:\\TreeMap2016\\working_KLR\\disturbance\\disturbance_insect_disease_fire_most_recent_1999_2016.tif"
outReclass1 = Reclassify(inRaster, "VALUE", RemapValue([[32767,0], [19991,1], [19992,2], [20001,1], [20002,2], [20011,1], [20012,2], [20021,1], [20022,2], [20031,1], [20032,2], [20041,1], [20042,2], [20051,1], [20052,2], [20061,1], [20062,2], [20071,1], [20072,2], [20081,1], [20082,2], [20091,1], [20092,2], [20101,1], [20102,2], [20111,1], [20112,2], [20121,1], [20122,2], [20131,1], [20132,2], [20141,1], [20142,2], [20151,1], [20152,2], [20161,1], [20162,2]]))
outReclass1.save("G:\\TreeMap2016\\working_KLR\\disturbance\\disturbance_code_1999_2016.tif")


# ---------------------------------------------------------------------------------------------------------------------------------------------------
# reclass to disturbance years before measurement
print("reclass to disturbance years before measurement")
inRaster = "G:\\TreeMap2016\\working_KLR\\disturbance\\disturbance_insect_disease_fire_most_recent_1999_2016.tif"
outReclass1 = Reclassify(inRaster, "VALUE", RemapRange([[32767,32768,99], [19991,19993,17], [20001,20003,16], [20011,20013,15], [20021,20023,14], [20031,20033,13], [20041,20043,12], [20051,20053,11], [20061,20063,10], [20071,20073,9], [20081,20083,8], [20091,20093,7], [20101,20103,6], [20111,20113,5], [20121,20123,4], [20131,20133,3], [20141,20143,2], [20151,20153,1], [20161,20163,0]]))
outReclass1.save("G:\\TreeMap2016\\working_KLR\\disturbance\\disturbance_year_1999_2016.tif")


#--------------------------------------------------------------------------------------------------------
# "NoData" values need to have a value (in case they fall on a forested pixel)
print("add NoData values")
# disturbance code
inRaster = "G:\\TreeMap2016\\working_KLR\\disturbance\\disturbance_code_1999_2016.tif"
outReclass1 = Reclassify(inRaster, "VALUE", RemapValue([["NODATA",0],[1,1], [2,2]]))
##outReclass1.save("G:\\TreeMap2016\\working_KLR\\disturbance\\disturbance_code_1999_2016_nodata_wrongextent.tif")
outReclass1.save("G:\\TreeMap2016\\working_KLR\\disturbance\\disturbance_code_1999_2016_nodata.tif")
# disturbance year
inRaster = "G:\\TreeMap2016\\working_KLR\\disturbance\\disturbance_year_1999_2016.tif"
outReclass1 = Reclassify(inRaster, "VALUE", RemapValue([["NODATA",99],[1,1], [2,2], [3,3], [4,4], [5,5], [6,6], [7,7], [8,8], [9,9], [10,10], [11,11], [12,12], [13,13], [14,14], [15,15], [16,16], [17,17]]))
##outReclass1.save("H:\\TreeMap2016\\working_KLR\\disturbance_year_1999_2014_nodata_wrongextent.tif")
outReclass1.save("G:\\TreeMap2016\\working_KLR\\disturbance\\disturbance_year_1999_2014_nodata.tif")
# had trouble getting this to work so did it manually in ArcGIS.
# did similar for disturbance code, setting "NODATA" values to 99


#-----------------------------------------------------------------------------------------------------------------------
# since the whole country isn't completed at this point, we need to mask to the completed extent
print("mask to completed extent")
# disturbance code
##inRaster = "H:\\TreeMap2016\\working_KLR\\disturbance_code_1999_2016_nodata_wrongextent.tif"
##inMaskData = "H:\\TreeMap2016\\Spatial_data\\Landfire_Remap_2016\\national\\disturbance\\dist2015.tif"
##outExtractByMask = ExtractByMask(inRaster, inMaskData)
##outExtractByMask.save("H:\\TreeMap2016\\target_data\\national_recoded\\disturbance_code_1999_2016_nodata.tif")
# disturbance year
##inRaster = "H:\\TreeMap2016\\working_KLR\\disturbance_year_1999_2016_nodata_wrongextent.tif"
##inMaskData = "H:\\TreeMap2016\\Spatial_data\\Landfire_Remap_2016\\national\\disturbance\\dist2015.tif"
##outExtractByMask = ExtractByMask(inRaster, inMaskData)
##outExtractByMask.save("H:\\TreeMap2016\\target_data\\national_recoded\\disturbance_year_1999_2016_nodata.tif")
