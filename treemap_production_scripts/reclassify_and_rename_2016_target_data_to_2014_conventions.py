# rename and reclassify Landfire 2016 data to match convention for TreeMap2014
# written by Karin Riley, 5/5/2020

import os
import arcpy
from arcpy import env
from arcpy.sa import *

arcpy.CheckOutExtension("Spatial")

zonenums = ["z14","z15","z24","z25","z27","z28","z13","z03","z05","z04","z06","z07","z01","z02","z08","z09","z10","z12","z16","z17","z18","z19","z20","z21","z22","z23","z26","z29","z30","z31","z32","z33","z34","z35","z36","z38","z39","z40","z41","z42","z43","z50"]
            
# reclassify cover, height, and disturbance year
for j in range(0,len(zonenums)):
    newfolder = "G:\\TreeMap2016\\working_KLR\\target_data_reclassified_final\\" + zonenums[j]
    if not os.path.exists(newfolder):
        os.makedirs(newfolder)
    inevc = "G:\\TreeMap2016\\working_KLR\\target_data_integer_final\\evc-mask-" + zonenums[j] + ".tif"
    outReclass1 = Reclassify(inevc, "Value", RemapRange([[10,19,15],[20,29,25],[30,39,35],[40,49,45],[50,59,55],[60,69,65],[70,79,75],[80,89,85],[90,100,95]]))
    outReclass1.save("G:\\TreeMap2016\\working_KLR\\target_data_reclassified_final\\" + zonenums[j] + "\\canopy_cover.tif")
    inevh = "G:\\TreeMap2016\\working_KLR\\target_data_integer_final\\evh-mask-" + zonenums[j] + ".tif"
    outReclass2 = Reclassify(inevh, "Value", RemapRange([[0,5,3],[6,10,8],[11,25,18],[26,50,38],[51,200,75]]))
    outReclass2.save("G:\\TreeMap2016\\working_KLR\\target_data_reclassified_final\\" + zonenums[j] + "\\canopy_height.tif")
    indy = "G:\\TreeMap2016\\working_KLR\\target_data_integer_final\\dist-year-mask-" + zonenums[j] + ".tif"
    outReclass3 = Reclassify(indy, "Value", RemapValue([[0,0],[1,1],[2,2],[3,3],[4,4],[5,5],[6,6],[7,7],[8,8],[9,9],[10,10],[11,11],[12,12],[13,13],[14,14],[15,15],[16,15],[17,15],[99,99]]))
    outReclass3.save("G:\\TreeMap2016\\working_KLR\\target_data_reclassified_final\\" + zonenums[j] + "\\disturb_year.tif")

# copy and rename remaining layers
outlayers = ["ASPECT", "disturb_code", "ELEV", "EVT_GP", "PARI", "PPTI", "RELHUMI", "SLOPE", "TMAXI", "TMINI", "VPDI"]
inlayers = ["asp", "dist-code", "elev", "evg-reclass", "par", "ppt", "relhum", "slp", "tmax", "tmin", "vpd"]
for j in range(0,len(zonenums)):
    for k in range(0,len(outlayers)):
        inData = "G:\\TreeMap2016\\working_KLR\\target_data_integer_final\\" + inlayers[k] + "-mask-" + zonenums[j] + ".tif"
        outData = "G:\\TreeMap2016\\working_KLR\\target_data_reclassified_final\\" + zonenums[j] + "\\" + outlayers[k] + ".tif"
        arcpy.Copy_management(inData, outData)
