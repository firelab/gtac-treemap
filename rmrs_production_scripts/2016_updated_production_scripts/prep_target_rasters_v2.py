# prep target rasters for TreeMap 2016
# written by Karin Riley, 2/24/2020

# Note: Landfire Remap 2016 uses different codes than previous versions

import arcpy
from arcpy import env
from arcpy.sa import *
import os

arcpy.CheckOutExtension("Spatial")

# set snap raster
arcpy.env.snapRaster = "G:\\TreeMap2016\\Spatial_data\\Landfire_Remap_2016\\national\\US_200EVC\\Tif\\us_200evc.tif"

# subset EVC raster to only forested pixels
# forested codes are now 110-199
print "reclassifying EVC"
inRaster = "G:\\TreeMap2016\\Spatial_data\\Landfire_Remap_2016\\national\\US_200EVC\\Tif\\us_200evc.tif"
##inRemapFile = "G:\\TreeMap2016\\Landfire 2016\\remap_tables\\EVC_remap.txt"
outRaster = Reclassify(inRaster, "Value", RemapRange([[-9999,100,"NODATA"], [110,119,15], [120,129,25], [130,139,35], [140,149,45], [150,159,55], [160,169,65], [170,179,75], [180,189,85], [190,199,95], [200,399,"NODATA"]]))
outRaster.save("G:\\TreeMap2016\\working_KLR\\national_veg\\EVC_reclass_forest_only.tif")
# this gave me a totally fubared output and I gave up and used the "Reclassify (3D)" GUI

# use this forest mask to mask EVT
inRaster = "G:\\TreeMap2016\\Spatial_data\\Landfire_Remap_2016\\national\\US_200EVT\\Tif\\us_200evt.tif"
inMaskData = "G:\\TreeMap2016\\working_KLR\\national_veg\\EVC_reclass_forest_only.tif"
outExtractByMask = ExtractByMask(inRaster, inMaskData)
outExtractByMask.save("G:\\TreeMap2016\\working_KLR\\national_veg\\EVT_maskedEVCforest.tif")

# need to omit and/or reclassify several of these EVGs
# should just be remapping a few px - according to Karin
# remapping a few pixels in some of the classes because there were only a few px for some of the EVGs ,
# no plots that keyed to these EVGs
inRaster = "G:\\TreeMap2016\\working_KLR\\national_veg\\EVT_maskedEVCforest.tif"
ReclassRaster = Reclassify(inRaster, "EVT_GP", RemapValue([[13,"NODATA"],[14,"NODATA"],[15,"NODATA"],[26,"NODATA"],[60,"NODATA"],[602,602],[603,603],[605,605],[607,607],[610,610],[614,614],[615,615],[620,620],[621,621],[622,622],[624,624],[625,625],[626,626],[627,627],[628,628],[629,629],[630,630],[631,631],[632,632],[633,633],[634,634],[635,635],[638,638],[639,639],[640,640],[642,642],[643,643],[644,644],[645,645],[650,650],[651,651],[652,652],[655,655],[656,656],[657,657],[658,658],[659,659],[660,660],[661,661],[662,662],[664,664],[665,665],[666,666],[667,667],[668,668],[670,670],[672,672],[673,673],[675,675],[676,676],[677,677],[678,678],[679,679],[680,680],[681,681],[682,682],[683,683],[684,684],[685,685],[686,686],[687,687],[688,688],[689,689],[690,690],[691,691],[692,693],[693,693],[694,694],[695,695],[696,696],[701,701],[707,707],[708,708],[730,"NODATA"],[731,731],[740,740]]))
ReclassRaster.save("G:\\TreeMap2016\\working_KLR\\national_veg\\EVT_maskedEVCforest_reclass.tif")

# use EVG raster to mask EVC
inRaster = "G:\\TreeMap2016\\working_KLR\\national_veg\\EVC_reclass_forest_only.tif"
inMaskData = "G:\\TreeMap2016\\working_KLR\\national_veg\\EVT_maskedEVCforest_reclass.tif"
outExtractByMask = ExtractByMask(inRaster, inMaskData)
outExtractByMask.save("G:\\TreeMap2016\\working_KLR\\national_veg\\EVC_maskedEVT_reclass.tif")

# use EVG raster to mask EVH, then reclassify to 2014 conventions
inRaster = "G:\\TreeMap2016\\Spatial_data\\Landfire_Remap_2016\\national\\US_200EVH\\Tif\\us_200evh.tif"
inMaskData = "G:\\TreeMap2016\\working_KLR\\national_veg\\EVT_maskedEVCforest_reclass.tif"
outExtractByMask = ExtractByMask(inRaster, inMaskData)
outExtractByMask.save("G:\\TreeMap2016\\working_KLR\\national_veg\\EVH_maskedEVT.tif")
inRaster = "G:\\TreeMap2016\\working_KLR\\national_veg\\EVH_maskedEVT.tif"
outRaster = Reclassify(inRaster, "Value", RemapRange([[101,105,3],[106,110,8],[111,125,18],[126,150,38]]))
outRaster.save("G:\\TreeMap2016\\working_KLR\\national_veg\\EVH_masked_reclass.tif")

# need to only produce target rasters for zones in SE and NE (other zones are already done)
# z37, 46, 47, 48, 49, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 98, 99
# subset EVG, EVH, and EVC rasters to these zones
##zonenums = [37, 46, 47, 48, 49, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 98, 99]
zonenums = [44, 45]
inrasters = ["EVC_maskedEVT_reclass", "EVH_masked_reclass", "EVT_maskedEVCforest_reclass"]
outrasters2 = ["canopy_cover", "canopy_height", "EVT_GP"]
for j in range(0,len(zonenums)):
    zonefolder = "G:\\TreeMap2016\\target_data\\target_data_reclassified_SE_NE\\" + str(zonenums[j])
    if not os.path.exists(zonefolder):
        os.makedirs(zonefolder)
	##where = ' "ZONE_NUM" = %i ' % (zonenums[j])
	##arcpy.SelectLayerByAttribute_management("zones", "NEW_SELECTION", where)
	for k in range(0,len(outrasters2)):
            inRaster = "G:\\TreeMap2016\\working_KLR\\national_veg\\" + inrasters[k] + ".tif"
	    inMaskData = "G:\\Tree_List_c2014\\target_data\\working_KLR\\Landfire_zone_polygons\\zone" + str(zonenums[j]) + ".shp"
	    outExtractByMask = ExtractByMask(inRaster, inMaskData)
	    outRaster = zonefolder + "\\" + outrasters2[k] + ".tif"
	    outExtractByMask.save(outRaster)

# reclass disturbance year
indy = "G:\\TreeMap2016\\working_KLR\\disturbance\\disturbance_year_1999_2016_nodata.tif"
outReclass3 = Reclassify(indy, "Value", RemapValue([[0,0],[1,1],[2,2],[3,3],[4,4],[5,5],[6,6],[7,7],[8,8],[9,9],[10,10],[11,11],[12,12],[13,13],[14,14],[15,15],[16,15],[17,15],[99,99]]))
outReclass3.save("G:\\TreeMap2016\\working_KLR\\disturbance\\disturbance_year_1999_2016_nodata_reclass.tif")

# use EVG zone rasters to mask topographic and disturbance predictor rasters
outrasters = ["ASPECT", "disturb_code", "disturb_year", "ELEV", "SLOPE"]
inRasters = ["G:\\TreeMap2016\\Spatial_data\\Landfire_Remap_2016\\national\\US_ASP_2016_09142020\\Tif\\us_asp_2016.tif", "G:\\TreeMap2016\\working_KLR\\disturbance\\disturbance_code_1999_2016_nodata.tif", "G:\\TreeMap2016\\working_KLR\\disturbance\\disturbance_year_1999_2016_nodata_reclass.tif", "G:\\TreeMap2016\\Spatial_data\\Landfire_Remap_2016\\national\\US_DEM_2016_09142020\\Tif\\us_dem_2016.tif", "G:\\TreeMap2016\\Spatial_data\\Landfire_Remap_2016\\national\\US_SLP_Deg_2016_09142020\\Tif\\us_slpd_2016.tif"] 
for j in range(0,len(zonenums)):
    zonefolder = "G:\\TreeMap2016\\target_data\\target_data_reclassified_SE_NE\\" + str(zonenums[j])
    for k in range(0,len(outrasters)):
        inRaster = inRasters[k]
	inMaskData = zonefolder + "\\EVT_GP.tif"
	outExtractByMask = ExtractByMask(inRaster, inMaskData)
	outRaster = zonefolder + "\\" + outrasters[k] + ".tif"
	outExtractByMask.save(outRaster)

# use EVG raster to mask biophysical predictor rasters
inRasters = ["par", "ppt", "relhum", "tmax", "tmin", "vpd"]
outrasters = ["PARI", "PPTI", "RELHUMI", "TMAXI", "TMINI", "VPDI"]
##regionlist = ["SE", "SE", "NE", "SE", "NE", "NE", "NE", "NE", "SE", "SE", "SE","NE", "NE", "NE", "NE", "NE", "NE", "NE", "NE", "NE", "NE", "SE", "SE"]
regionlist = ["SC", "SC"]
for j in range(0,len(zonenums)):
    zonefolder = "G:\\TreeMap2016\\target_data\\target_data_reclassified_SE_NE\\" + str(zonenums[j])
    for k in range(0,len(outrasters)):
        inRaster = "G:\\Tree_List_2013\\Spatial_data\\input_grids_Chris_Winne\\d.gradients\\" + regionlist[j] + "\\z" + str(zonenums[j]) + "\\z" + str(zonenums[j]) + inRasters[k] + ".img"
	inMaskData = zonefolder + "\\EVT_GP.tif"
	outExtractByMask = ExtractByMask(inRaster, inMaskData)
	outRaster = zonefolder + "\\" + outrasters[k] + ".tif"
	outExtractByMask.save(outRaster)


# check number of pixels in each raster
##zonenums = [37, 46, 47, 48, 49, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 98, 99]
zonenums = ["01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "38", "39", "40", "41", "42", "43", "50"]
zonenums = [44, 45]

layers = ["ASPECT", "canopy_cover", "canopy_height", "disturb_code", "disturb_year", "ELEV", "EVT_GP", "SLOPE", "PARI", "PPTI", "RELHUMI", "TMAXI", "TMINI", "VPDI"]
for zone in zonenums:
    pixelvec = [-99,-99,-99,-99,-99,-99,-99,-99,-99,-99,-99,-99,-99,-99]
    j=-1
    for layer in layers:
        pixelcount = 0        
        j=j+1
        inRaster = "G:\\TreeMap2016\\target_data\\target_data_reclassified_SE_NE\\z" + str(zone) + "\\" + layer + ".tif"
        ##inRaster = "G:\\TreeMap2016\\target_data\\target_data_reclassified_final2\\z" + zone + "\\" + layer + ".tif"        
        cursor = arcpy.da.SearchCursor(inRaster, ['Count'])
        for row in cursor:
            ##print(row)
            rownum = int(row[0])
            pixelcount = pixelcount + rownum
        pixelvec[j] = pixelcount
    if (pixelvec[0]==pixelvec[1]==pixelvec[2]==pixelvec[3]==pixelvec[4]==pixelvec[5]==pixelvec[6]==pixelvec[7]==pixelvec[8]==pixelvec[9]==pixelvec[10]==pixelvec[11]==pixelvec[12]==pixelvec[13]):
        print(pixelvec)
        print(zone, "SAME")
    else:
        print(pixelvec)
        print (zone, "PROBLEM")

# reclass EVG to 1,2,....
# rename files I initally made and put in different folder
for j in range(0,len(zonenums)):
    zonefolder = "G:\\TreeMap2016\\target_data\\target_data_reclassified_SE_NE\\" + str(zonenums[j]) + "\\EVG_not_reclassified"
    if not os.path.exists(zonefolder):
        os.makedirs(zonefolder)
    inRaster = "G:\\TreeMap2016\\target_data\\target_data_reclassified_SE_NE\\" + str(zonenums[j]) + "\\EVT_GP.tif"
    outRaster = "G:\\TreeMap2016\\target_data\\target_data_reclassified_SE_NE\\" + str(zonenums[j]) + "\\EVG_not_reclassified\\EVT_GP.tif"
    arcpy.Copy_management(inRaster, outRaster)
    arcpy.Delete_management(inRaster)

for zonenum in zonenums:
    inRaster = "G:\\TreeMap2016\\target_data\\target_data_reclassified_SE_NE\\" + str(zonenum) + "\\EVG_not_reclassified\\EVT_GP.tif"
    inRemapFile = "G:\\TreeMap2016\\working_KLR\\EVG_remap\\z" + str(zonenum) + "_EVG_remap.txt"
    print "reclassifying zone ", str(zonenum) 
    outRaster = ReclassByASCIIFile(inRaster, inRemapFile)
    outRaster.save("G:\\TreeMap2016\\target_data\\target_data_reclassified_SE_NE\\" + str(zonenum) + "\\EVT_GP.tif")
    

# check extents in all target rasters 
top = [-99,-99,-99,-99,-99,-99,-99,-99,-99,-99,-99,-99,-99,-99]
bottom = [-99,-99,-99,-99,-99,-99,-99,-99,-99,-99,-99,-99,-99,-99]
left = [-99,-99,-99,-99,-99,-99,-99,-99,-99,-99,-99,-99,-99,-99]
right = [-99,-99,-99,-99,-99,-99,-99,-99,-99,-99,-99,-99,-99,-99]
##zonenum = "z47"
zonenums = [37, 46, 47, 48, 49, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 98, 99]
layers = ["ASPECT", "canopy_cover", "canopy_height", "disturb_code", "disturb_year", "ELEV", "EVT_GP", "SLOPE", "PARI", "PPTI", "RELHUMI", "TMAXI", "TMINI", "VPDI"]

for j in range(0,len(zonenums)):
    print(zonenums[j])
    for k in range(0,len(layers)):
        inRaster = "G:\\TreeMap2016\\target_data\\target_data_reclassified_SE_NE\\z" + str(zonenums[j]) + "\\" + layers[k] + ".tif"
        topresult = arcpy.GetRasterProperties_management(inRaster,"TOP")
        top[k] = topresult.getOutput(0)
        bottomresult = arcpy.GetRasterProperties_management(inRaster,"BOTTOM")
        bottom[k] = bottomresult.getOutput(0)
        leftresult = arcpy.GetRasterProperties_management(inRaster,"LEFT")
        left[k] = leftresult.getOutput(0)
        rightresult = arcpy.GetRasterProperties_management(inRaster,"RIGHT")
        right[k] = rightresult.getOutput(0)
    if (top[0]==top[1]==top[2]==top[3]==top[4]==top[5]==top[6]==top[7]==top[8]==top[9]==top[10]==top[11]==top[12]==top[13]):
        print(top)
        print(zonenums[j], "TOP=SAME")
    else:
        print(top)
        print (zonenums[j], "TOP=PROBLEM")
    if (bottom[0]==bottom[1]==bottom[2]==bottom[3]==bottom[4]==bottom[5]==bottom[6]==bottom[7]==bottom[8]==bottom[9]==bottom[10]==bottom[11]==bottom[12]==bottom[13]):
        print(bottom)
        print(zonenums[j], "BOTTOM=SAME")
    else:
        print(bottom)
        print (zonenums[j], "BOTTOM=PROBLEM")
    if (left[0]==left[1]==left[2]==left[3]==left[4]==left[5]==left[6]==left[7]==left[8]==left[9]==left[10]==left[11]==left[12]==left[13]):
        print(left)
        print(zonenums[j], "LEFT=SAME")
    else:
        print(left)
        print (zonenums[j], "LEFT=PROBLEM")
    if (right[0]==right[1]==right[2]==right[3]==right[4]==right[5]==right[6]==right[7]==right[8]==right[9]==right[10]==right[11]==right[12]==right[13]):
        print(right)
        print(zonenums[j], "RIGHT=SAME")
    else:
        print(right)
        print (zonenums[j], "RIGHT=PROBLEM")


# the bottom of z37 PARI is off by decimal dust but all others are ok
# if we have a problem in z37 I should fix those




    
        
