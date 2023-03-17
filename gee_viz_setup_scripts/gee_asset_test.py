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

#TCC tile moving window visualization
####################################################################################################
import os,sys,random,numpy,json
sys.path.append(os.getcwd())
import pandas as pd
from simpledbf import Dbf5
#Module imports
import geeViz.getImagesLib as getImagesLib
import geeViz.taskManagerLib as tml
ee = getImagesLib.ee
Map = getImagesLib.Map
Map.clearMap()
####################################################################################################
treeMapTif = r"X:\01_Data\01_TreeMap2016_RDA\RDS-2021-0074_Data\Data\TreeMap2016.tif"
treeMapDbf = r"X:\01_Data\01_TreeMap2016_RDA\RDS-2021-0074_Data\Data\TreeMap2016.tif.vat.dbf"
treeMapJson = os.path.splitext(treeMapDbf)[0]+'.json'
tmCN  = ee.Image('projects/lcms-292214/assets/CONUS-Ancillary-Data/TreeMap2016')
treeMapAttributeCollection = 'projects/lcms-292214/assets/CONUS-Ancillary-Data/treeMap2016Attributes'
sa = tmCN.geometry()
proj = tmCN.projection().getInfo()
crs = proj['wkt']
transform = proj['transform']
# tmCN = tmCN.updateMask(tmCN.neq(2147483647));
# Map.addLayer(tmCN.randomVisualizer(),{},'TreeMap Raw CN',False);
####################################################################################################

dbf = Dbf5(treeMapDbf)
df = dbf.to_dataframe()#.head()
allCNs = list(df.Value)
def dfToJSON(df,outJsonFilename):
   columns = df.columns
   rows = df.transpose().to_numpy()
   outJson = {}
   for i,c in enumerate(columns):
      outJson[c]=list(rows[i])

   o = open(outJsonFilename,'w')
   o.write(json.dumps(outJson))
   o.close()


def getNUnique(columnName):
   codeLookup = dict(zip(df.Value,df[columnName]))
   n = len(set(codeLookup.values()))
   print(columnName,n)
   return [columnName,n]
# counts = [getNUnique(f) for f in df.columns]
def getRandomColor():
   return ''.join([random.choice('0123456789ABCDEF') for j in range(6)])


def castType(img,tp):
   if str(tp).find('int')>-1:
      print(tp,'Casting to int')
      return 'mode',img.int16()
   else:
      print(tp,'Casting to float')
      return 'mean',img.float()
def simpleGetAttributeImage(columnNameNumbers):
   imgValues = list(df[columnNameNumbers])
   tp = type(numpy.min(imgValues))
   # print(len(imgValues))
   imgValuesForStats = numpy.array(imgValues)
   imgValuesForStats = list(imgValuesForStats[imgValuesForStats!=-99.])
   pctls = numpy.quantile(imgValuesForStats,[0,0.05,0.95,1])
   print(columnNameNumbers,pctls,tp)
   tmAttribute = tmCN.remap(allCNs,list(imgValues))
   tmAttribute = tmAttribute.updateMask(tmAttribute.neq(-99))
   pyramidingMethod,tmAttribute = castType(tmAttribute,tp)
   tmAttribute = tmAttribute.rename([columnNameNumbers]).set({'attribute':columnNameNumbers,'version':2016,'system:time_start':ee.Date.fromYMD(2016,6,1).millis()})
   nm = 'TreeMap2016-{}'.format(columnNameNumbers)
   getImagesLib.exportToAssetWrapper(tmAttribute,nm,treeMapAttributeCollection+'/'+nm,pyramidingPolicyObject = pyramidingMethod,roi= sa,scale= None,crs = crs,transform = transform)

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

cols = ['FORTYPCD', 'FLDTYPCD',\
        'STDSZCD', 'FLDSZCD', 'BALIVE', 'CANOPYPCT', 'STANDHT',\
       'ALSTK', 'GSSTK', 'QMD_RMRS', 'SDIPCT_RMR', 'TPA_LIVE', 'TPA_DEAD',\
       'VOLCFNET_L', 'VOLCFNET_D', 'VOLBFNET_L', 'DRYBIO_L', 'DRYBIO_D',\
       'CARBON_L', 'CARBON_D', 'CARBON_DWN']
# for col in cols:
   # simpleGetAttributeImage(col)
tml.trackTasks2()
# getAttributeImage('FORTYPCD','ForTypName') 
# getAttributeImage('FLDTYPCD','FldTypName') 
# getAttributeImage('CANOPYPCT',colors=['808','DDD','080'],pctlStretch=[0.05,0.95])#min=0,max=80)
# getAttributeImage('CARBON_L',colors=['D0D','0D0'],pctlStretch=[0.05,0.95])#min = 5,max=25)
# getAttributeImage('CARBON_D',colors=['D0D','DD0'],pctlStretch=[0.05,0.95])#min = 5,max=25)
# getAttributeImage('DRYBIO_L',colors=['D0D','DF0'],pctlStretch=[0.05,0.95])
# getAttributeImage('DRYBIO_D',colors=['D0D','DD0'],pctlStretch=[0.05,0.95])
# getAttributeImage('STANDHT',colors=['808','DDD','080'],pctlStretch=[0.05,0.95])
# dfToJSON(df,treeMapJson)
####################################
Map.turnOnInspector()
# Map.view()