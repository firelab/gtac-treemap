# validate tree list outputs, Step 1
# written by Karin Riley, 4/17/2020

import arcpy
from arcpy.sa import *
arcpy.CheckOutExtension("Spatial")

# outputs will be row number in x table
# set zone number and input data folder
curzone = "z37"
targetfolder = curzone
##outname = curzone + "_2014-xtable-2016-raster-fixppt"
##outfolder = "G:\\TreeMap2016\\outputs\\old_recipe\\" + curzone
outname = curzone + "_2014-xtable-2016-raster-fixpptsc"
outfolder = "G:\\TreeMap2016\\outputs\\fixpptsc\\" + curzone
tlRaster = outfolder + "\\" + outname + ".tif"
targetdatafolder1 = "G:\\TreeMap2016\\target_data\\target_data_reclassified_final\\" + curzone
targetdatafolder2 = "G:\\TreeMap2016\\working_KLR\\target_data_integer_final\\EVG_mask_but_not_reclass"

# define projection (R strips off datum for some reason)
dsc = arcpy.Describe("G:\\Spatial_Data\\Landfire\\c2012\\US_130_FBFM40\\Grid\\us_130fbfm40")
coord_sys = dsc.spatialReference
arcpy.DefineProjection_management(tlRaster, coord_sys)

# convert to integer (if needed)
outInt = Int(tlRaster)
copyname1 = outfolder + "\\" + curzone + "_int"
outInt.save(copyname1)

# build attribute table
arcpy.BuildRasterAttributeTable_management(copyname1)

# join x table to raster
outLocation = "G:\\Tree_List_c2014\\x_table\\x_table_2014.gdb"
outTable = "x_table_final_EVG_Karin_reclass_plus_loblolly_manual"
inField = "Value"
joinTable = outLocation + "\\" + outTable
joinField = "ID"
arcpy.JoinField_management(copyname1, inField, joinTable, joinField)
copyname = outfolder + "\\" + outname + "_int.tif"
arcpy.RasterToOtherFormat_conversion(copyname1, outfolder, "TIFF")
inData = outfolder + "\\" + curzone + "_int.tif"
arcpy.Rename_management(inData, copyname)

# write raster for cover, one for height, and one for EVG
lookupField = "canopy_cov"
outRaster = Lookup(copyname, lookupField)
evcout = outfolder + "\\EVC_out.tif"
outRaster.save(evcout)
lookupField = "canopy_hei"
evhout = outfolder + "\\EVH_out.tif"
outRaster = Lookup(copyname, lookupField)
outRaster.save(evhout)
lookupField = "EVT_GP_12_"
outRaster = Lookup(copyname, lookupField)
evgout = outfolder + "\\EVG_out.tif"
outRaster.save(evgout)

# write rasters for disturbance code and year
lookupField = "disturb_co"
outRaster = Lookup(copyname, lookupField)
dcout = outfolder + "\\disturb_code.tif"
outRaster.save(dcout)
lookupField = "disturb_ye"
outRaster = Lookup(copyname, lookupField)
dyout = outfolder + "\\disturb_year.tif"
outRaster.save(dyout)

# perform combines for Landfire and predicted EVC, EVH, and EVG
outCombine = Combine([evcout, targetdatafolder1 + "\\canopy_cover.tif"])
outCombine.save(outfolder + "\\EVC_combine.tif")
outCombine = Combine([evhout, targetdatafolder1 + "\\canopy_height.tif"])
outCombine.save(outfolder + "\\EVH_combine.tif")
outCombine =  Combine([evgout, targetdatafolder2 + "\\evg-mask-" + curzone + ".tif"])
outCombine.save(outfolder + "\\EVG_combine.tif")
# export attribute tables
arcpy.TableToTable_conversion(outfolder + "\\EVC_combine.tif", outfolder, "EVC_combine.dbf")
arcpy.TableToTable_conversion(outfolder + "\\EVH_combine.tif", outfolder, "EVH_combine.dbf")
arcpy.TableToTable_conversion(outfolder + "\\EVG_combine.tif", outfolder, "EVG_combine.dbf")

# perform combines for disturbance
outCombine = Combine([dcout, targetdatafolder1 + "\\disturb_code.tif"])
outCombine.save(outfolder + "\\disturb_code_combine.tif")
outCombine = Combine([dyout, targetdatafolder1 + "\\disturb_year.tif"])
outCombine.save(outfolder + "\\disturb_year_combine.tif")

# analyze attribute tables in R for prediction accuracy
