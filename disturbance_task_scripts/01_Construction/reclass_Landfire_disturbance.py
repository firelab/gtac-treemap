### UPDATED LANDFIRE DISTURBANCE LAYER CREATION
# Objective: have one script that we can run start to finish that will pull in all landfire disturbance layers 1999-2016 and create a slow loss layer for TreeMap
# This script is NOT YET pulling in LCMS slow loss
# Objective is to ensure that we can replicate the TreeMap Workflow and get an identical product

#import packages
import arcpy, os
from arcpy import env
from arcpy.sa import *
arcpy.CheckOutExtension("Spatial")
arcpy.env.overwriteOutput = True

# set snap raster
arcpy.env.snapRaster = "X:/01_Data/02_Landfire/LF_220/Disturbance/LF2016_Dist_200_CONUS/Tif/LC16_Dist_200.tif"
# set processing extent to zone 16
arcpy.env.extent = "X:/01_Data/02_Landfire/LF_zones/z16_nad83.shp"

# create variable for TreeMap disturbance data directory
inws = "X:/01_Data/02_Landfire/LF_220/Disturbance/test"

# walk through directories in inws and look for tifs
def getRasterList(dir):
    walk = arcpy.da.Walk(dir, topdown=True, datatype="RasterDataset", type = "TIF")
    rasterList = []
    # use os.path.join() to combine paths and filenames into full paths
    for dirpath, dirnames, filenames in walk:
        for filename in filenames:
            inRaster = os.path.join(dirpath, filename)
            rasterList.append(inRaster)

    return rasterList

def Reclass_standard(year1,year2,raster, year):
     
    outReclass = arcpy.sa.Reclassify(raster, "Value", RemapRange([[-10000,1,"NODATA"],[10,234,year1], [410,464,"NODATA"], [470,504,year1],
    [520,534,"NODATA"], [540,564,year2], [570,584,"NODATA"],[600,764,"NODATA"], [770,804,year1], [810,815,"NODATA"],[820,834,"NODATA"], [840,854,year2], [870,884,"NODATA"], [910,962,"NODATA"], [970,1002,year1], [1010,1032,"NODATA"], [1040,1062,year2], [1070,1133,"NODATA"]]))
    outPath = os.path.join(r"X:\01_Data\02_Landfire\LF_220\Disturbance\test", f"LF{str(year)}_Reclass.tif")
    outReclass.save(outPath)

    return outReclass


def Reclass_fire(year1,year2,raster, year):
    outReclass_fire = arcpy.sa.Reclassify(raster, "Value", RemapRange([[year1,year1,year1],[year2,year2,"NODATA"], [-33000,0,"NODATA"]]))
    outPath = os.path.join(r"X:\01_Data\02_Landfire\LF_220\Disturbance\test", f"LF{str(year)}_Reclass_fire.tif")
    outReclass_fire.save(outPath)


def Reclass_disease(year1,year2,raster, year):
    outReclass_disease = arcpy.sa.Reclassify(raster, "Value", RemapRange([[year2,year2,year2],[year1,year1,"NODATA"], [-33000,0,"NODATA"]]))
    outPath = os.path.join(r"X:\01_Data\02_Landfire\LF_220\Disturbance\test", f"LF{str(year)}_Reclass_disease.tif")
    outReclass_disease.save(outPath)

if __name__ == "__main__":
    start_year = 1999
    end_year = 2020
    years = range(start_year, end_year)
    rasterList = getRasterList(inws)
    for raster in rasterList:
        for year in years:
            try:
                year1 = (year*10)+1
                year2 = (year*10)+2
                print(year1, year2)
                print("reclassifying")
                outRaster = Reclass_standard(year1, year2, raster, year)
                print("reclassifying fire only")
                Reclass_fire(year1, year2, outRaster, year)
                print("reclassifying disease only")
                Reclass_disease(year1, year2, outRaster, year)

            except Exception as e:
                print("skipping ",year, e)




#print("Reclassifying to fire and insect/disease codes")

########## RECLASS TO FIRE/INSECT ##########
#reclass to unique code for fire and insects/disease for each year (all other disturbances are considered NoData).
#One at a time, pull in landfire raster for YEAR, and reclass to FIRE / INSECT DISEASE only
#2016: fire=year1, insect/disease=year2, neither="NoData"

'''

inRaster = "X:/01_Data/02_Landfire/LF_220/Disturbance/LF2016_Dist_200_CONUS/Tif/LC16_Dist_200.tif"
print("Input raster set")
# don't update this command
outReclass = arcpy.sa.Reclassify(inRaster, "Value", RemapRange([[-10000,1,"NODATA"],[10,234,year1], [410,464,"NODATA"], [470,504,year1],[520,534,"NODATA"], [540,564,year2], [570,584,"NODATA"],[600,764,"NODATA"], [770,804,year1], [810,815,"NODATA"],[820,834,"NODATA"], [840,854,year2], [870,884,"NODATA"], [910,962,"NODATA"], [970,1002,year1], [1010,1032,"NODATA"], [1040,1062,year2], [1070,1133,"NODATA"]]))
print("Saving reclassified fire/insect disease raster")
outReclass.save("X:/01_Data/02_Landfire/LF_220/Disturbance/LF2016_Dist_200_CONUS/Tif/LF2016_Reclass.tif")

########## PULL OUT FIRE ONLY ##########
# for each year, for each fire/insect raster, get a raster with ONLY fire
# 2016: fire=year1, other="NoData"
print("Making fire only rasters")

inRaster = "//166.2.126.25/TreeMap/01_Data/02_Landfire/LF_220/Disturbance/LF2016_Dist_200_CONUS/Tif/LF2016_Reclass.tif"
outReclass_Fire = arcpy.sa.Reclassify(inRaster, "Value", RemapRange([[year1,year1,year1],[year2,year2,"NODATA"], [-33000,0,"NODATA"]]))
print("Saving fire only raster")
outReclass_Fire.save("X:/01_Data/02_Landfire/LF_220/Disturbance/LF2016_Dist_200_CONUS/Tif/LF2016_Reclass_Fire.tif")

########## PULL OUT INSECT/DISEASE ONLY ##########
#2016: insect/disease=year2, other="NoData"

print("Making insect/disease only rasters")

outReclass_Disease = arcpy.sa.Reclassify(inRaster, "Value", RemapRange([[year2,year2,year2],[year1,year1,"NODATA"], [-33000,0,"NODATA"]]))
outReclass_Disease.save("X:/01_Data/02_Landfire/LF_220/Disturbance/LF2016_Dist_200_CONUS/Tif/LF2016_Reclass_Disease.tif")

########## MOSAIC ALL FIRE RASTERS TO GET MOST RECENT YEAR ##########
# mosaic all fire rasters, with most recent year taking precedence

print("Making mosaic of all fire rasters with most recent year taking precedence")
#UPDATE THIS TO REFER TO ALL INPUT FIRE RASTERS
# could paste file names w/ path to create this as a list-- OR just list all input rasters
inRasters = "X:/01_Data/02_Landfire/LF_220/Disturbance/LF2016_Dist_200_CONUS/Tif/LF2016_Reclass_Disease.tif; X:/01_Data/02_Landfire/LF_220/Disturbance/LF2016_Dist_200_CONUS/Tif/LF2016_Reclass_Fire.tif"

#prep 2015 and 2016 fire rasters
arcpy.env.workspace = "fire raster path"

#inRasters = arcpy.ListRasters("*", "TIF")
outputLocation = "X:/01_Data/02_Landfire/LF_220/Disturbance/LF2016_Dist_200_CONUS/Tif"
#startYear, endYear
outRaster = "LF_Fire_mostRecent_2016.tif"
coordsys = arcpy.Describe("X:/01_Data/02_Landfire/LF_220/Disturbance/LF2016_Dist_200_CONUS/Tif/LC16_Dist_200.tif").spatialReference
pixelType = "16_BIT_SIGNED"
cellsize = "30"
mosaic_method = "LAST"
arcpy.MosaicToNewRaster_management(inRasters, outputLocation, outRaster, coordsys, pixelType, cellsize, "1", "LAST", "#")

# raster attribute table function below producing ERROR 000049: Failed to build attribute table
# mosaic is a single-band integer raster, so it should work...
#outGrid = outputLocation + "//" + outRaster
#arcpy.BuildRasterAttributeTable_management(outGrid)

'''