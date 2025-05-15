"""
   Copyright 2023 Ian Housman

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
"""

# Script to take raw TreeMap RDS and create all GEE assets for both raw and individual images for each attribute
####################################################################################################
import os,sys,random,numpy,json,subprocess,datetime
sys.path.append(os.getcwd())
import pandas as pd
from simpledbf import Dbf5
#Module imports
import geeViz.getImagesLib as getImagesLib
import geeViz.taskManagerLib as tml
import geeViz.assetManagerLib as aml
ee = getImagesLib.ee
Map = getImagesLib.Map
Map.clearMap()
####################################################################################################

# Specify year of TreeMap product
treeMapYear = '2016'
study_area = 'CONUS'

# Specify local RDS TreeMap tif and attribute table
treeMapTif = r"X:\01_Data\01_TreeMap2016_RDA\RDS-2021-0074_Data\Data\TreeMap2016.tif"
treeMapDbf = r"X:\01_Data\01_TreeMap2016_RDA\RDS-2021-0074_Data\Data\TreeMap2016.tif.vat.dbf"

#treeMapTif = rf"V:\03_Outputs\07_Projects\{treeMapYear}_Production_newXtable\04_Mosaic_assembled_model_outputs\TreeMap{treeMapYear}_{study_area}.tif"
#treeMapDbf = rf"V:\03_Outputs\07_Projects\{treeMapYear}_Production_newXtable\04_Mosaic_assembled_model_outputs\TreeMap{treeMapYear}_{study_area}.tif.vat.dbf"


# Specify json version of dbf that will be created for a less proprietary lookup table format
treeMapJson = os.path.splitext(treeMapDbf)[0]+'.json'

# Specify no data value in RDS dataset
rawTreeMapNoDataValue = 2147483647     #TreeMap2016

# GCS bucket raw RDS dataset will first be uploaded to (must already exist and have write permissions)
gcs_bucket = 'gs://treemap-test'
assetFolder = f'projects/treemap-386222/assets/Final_Outputs/{treeMapYear}/TreeMap_{treeMapYear}'
rawTreeMapAssetPath = assetFolder+'/'+'TreeMap_RDS_2016'

# Collection all individual attribute images will be exported to
# Will automatically be created if it does not exist
treeMapAttributeCollection = assetFolder

# Column names to create individual attribute images of
cols = ['FORTYPCD', 'FLDTYPCD',\
        'STDSZCD', 'FLDSZCD', 'BALIVE', 'CANOPYPCT', 'STANDHT',\
       'ALSTK', 'GSSTK', 'QMD_RMRS', 'SDIPCT_RMR', 'TPA_LIVE', 'TPA_DEAD',\
       'VOLCFNET_L', 'VOLCFNET_D', 'VOLBFNET_L', 'DRYBIO_L', 'DRYBIO_D',\
       'CARBON_L', 'CARBON_D', 'CARBON_DWN']

####################################################################################################
# Create image collection for attributes if it doesn't already exist
aml.create_image_collection(treeMapAttributeCollection)

# First convert the dbf lookup table to json
dbf = Dbf5(treeMapDbf)
df = dbf.to_dataframe()#.head()
allCNs = list(df.Value)
#####################################################
#####################################################
# Functions
# Function to convert a dbf to json
def dfToJSON(df,outJsonFilename):
   columns = df.columns
   rows = df.transpose().to_numpy()
   outJson = {}
   for i,c in enumerate(columns):
      outJson[c]=list(rows[i])

   o = open(outJsonFilename,'w')
   o.write(json.dumps(outJson))
   o.close()
#####################################################
# Function to find the bit depth of integer attributes
# This only handles byte, uint16, and int16 at the moment
def getBitDepth(min,max):
   bitRanges = {'byte':[0,255],'uInt16':[0,(2**16)-1],'Int16':[(2**16)/2*-1,(2**16)/2]}
   for k in bitRanges.keys():
      values = bitRanges[k]
      if min >= values[0] and max <= values[1]:
         return k
##################################################### 
# Find the number of uinque values for a given column    
def getNUnique(columnName):
   codeLookup = dict(zip(df.Value,df[columnName]))
   n = len(set(codeLookup.values()))
   print(columnName,n)
   return [columnName,n]
##################################################### 
# Function to get a random color
def getRandomColor():
   return ''.join([random.choice('0123456789ABCDEF') for j in range(6)])
##################################################### 
# Function to cast a gee image object to a data type based on the type of the field values and the range of values
# All float fields remain float. Int fields try to find the min bit depth
# Also defaults the pyramiding to mode for int and mean for float - this could be changed depending on intended uses
def castType(img,tp,min,max):
   if str(tp).find('int')>-1:
      bitDepth = getBitDepth(min,max)
      print(tp,'Casting to int',bitDepth)
      if bitDepth == 'uInt16':
         print('setting to uint16')
         img = img.uint16()
      elif bitDepth == 'Int16':
         print('setting to int16')
         img = img.int16()
      elif bitDepth == 'byte':
         print('setting to byte')
         img = img.byte()
      return 'mode',img
   else:
      print(tp,'Casting to float')
      return 'mean',img.float()
#####################################################
# Function to get individual attribute image
def simpleGetAttributeImage(columnNameNumbers,sa,crs,transform):
   # Get the values for the given column
   imgValues = list(df[columnNameNumbers])

   # Try to find the type of numbers for the attribute
   tp = type(numpy.min(imgValues))
   
   # Get some stats for the values to help better find what data type is best
   imgValuesForStats = numpy.array(imgValues)

   # Remobve -99 from values (notably for RMRS attributes)
   imgValuesForStats = list(imgValuesForStats[imgValuesForStats!=-99.])
   pctls = numpy.quantile(imgValuesForStats,[0,0.05,0.95,1])
   print(columnNameNumbers,pctls,tp,len(list(set(imgValuesForStats))))

   # Remap from raw values to values for given attribute
   tmAttribute = tmCN.remap(allCNs,list(imgValues))

   # Mask out -99 values
   tmAttribute = tmAttribute.updateMask(tmAttribute.neq(-99))

   # Cast the image type and find the pyramiding method
   pyramidingMethod,tmAttribute = castType(tmAttribute,tp,pctls[0],pctls[-1])

   # Set up image for export
   tmAttribute = tmAttribute.rename([columnNameNumbers]).set({'attribute':columnNameNumbers,'version':treeMapYear,'system:time_start':ee.Date.fromYMD(treeMapYear,6,1).millis()})

   # Set up name and export image
   nm = 'TreeMap{}-{}'.format(treeMapYear,columnNameNumbers)
   getImagesLib.exportToAssetWrapper(tmAttribute,nm,treeMapAttributeCollection+'/'+nm,pyramidingPolicyObject = pyramidingMethod,roi= sa,scale= None,crs = crs,transform = transform)
#####################################################
# Original image service setup method
# This has now been re-written in javaScript in the TreeMap Viewer
def getAttributeImage(columnNameNumbers,columnNameNames=None,randomColors=True,colors=None,min=None,max=None,pctlStretch=None):
   imgValues = list(df[columnNameNumbers])
   nameColumnProvided = columnNameNames != None
   if columnNameNames == None:
      columnNameNames = columnNameNumbers
   imgNames =  list(df[columnNameNames])
   
   # remap = [codeLookup.keys(),codeLookup.values()]
   # viz = {'min':imgValues[0],'max':img

   
   tmAttribute = tmCN.remap(allCNs,imgValues).float()

   viz = {}
   zippedValuesNames = list(set(zip(imgValues,imgNames)))
   if nameColumnProvided:
      viz['queryDict'] = dict(zippedValuesNames)
   
   zippedValuesNames.sort()
   uniqueValues = [i[0] for i in zippedValuesNames]
   uniqueNames = [i[1] for i in zippedValuesNames]
   if pctlStretch != None:
      viz['min'] = int(numpy.quantile(uniqueValues,pctlStretch[0]))
   elif min == None:
      viz['min'] = uniqueValues[0]
   else:
      viz['min'] = min

   if pctlStretch != None:
      viz['max'] = int(numpy.quantile(uniqueValues,pctlStretch[-1]))
   elif max == None:
      viz['max'] = uniqueValues[-1]
   else:
      viz['max'] = max
   # print(uniqueValues)
   if randomColors and colors == None:
      colors = []
      palette = []
      for i in range(viz['min'],viz['max']+1):
         # print(i)
         
         if i in uniqueValues:
            # print(i,'in')
            c = getRandomColor()
            colors.append(c)
            palette.append(c)
         
         else:
            # print(i,'out')
            palette.append('#000')
      print(len(colors),len(uniqueValues))
      viz['palette']=palette
      viz['classLegendDict'] = dict(zip(uniqueNames,colors))
   else:
      viz['palette']=colors
   # namesLookup = 
   Map.addLayer(tmAttribute,viz,'Tree Map {}'.format(columnNameNames),False)
   # print(uniqueValues)
####################################################################################################
# Function calls

# Create json version of dbf
# This is used inside the TreeMap viewer
# dfToJSON(df,treeMapJson)

# # First upload RDS to EE asset
aml.uploadToGEEImageAsset(
   treeMapTif,
   gcs_bucket,
   rawTreeMapAssetPath,
   overwrite = False,
   bandNames = ['Value'],
   properties = {'attribute':'Value',
                 'year':treeMapYear,
                 'version':treeMapYear,
                 'system:time_start':ee.Date.fromYMD(treeMapYear,6,1)},
   pyramidingPolicy='MODE',
   noDataValues=rawTreeMapNoDataValue
   )
tml.trackTasks2()

# Get some info about RDS dataset
tmCN  = ee.Image(rawTreeMapAssetPath)
sa = tmCN.geometry()
proj = tmCN.projection().getInfo()
crs = proj['wkt']
transform = proj['transform']

# View RDS dataset
Map.addLayer(tmCN.randomVisualizer(),{},'TreeMap Raw CN',False)
Map.view()

# Export each attribute
for col in cols:
   simpleGetAttributeImage(col,sa,crs,transform)
tml.trackTasks2()


####################################################################################################
Map.turnOnInspector()
# Map.view()