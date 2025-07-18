"""
   Copyright 2023 Ian Housman
   Modified:
    - M.O. Borja for TreeMap 2020 and 2022 

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

# Script that converts a .dbf file into a .json file.
####################################################################################################
import os,json
import pandas as pd
from simpledbf import Dbf5
####################################################################################################

# Specify year and study area of TreeMap product
treeMapYear = '2022'
studyArea = 'CONUS'

# Specify local TreeMap attribute table
treeMapDbf = rf"V:\03_Outputs\07_Projects\{treeMapYear}_Production_newXtable\04_Mosaic_assembled_model_outputs\TreeMap{treeMapYear}_{studyArea}.tif.vat.dbf"

# Specify json version of dbf that will be created for a less proprietary lookup table format
treeMapJson = os.path.splitext(treeMapDbf)[0]+'.json'



#####################################################
#####################################################

# Function to convert a dbf to json
def dfToJSON(df,outJsonFilename):
   """
   Converts a pandas dataframe into a JSON dictionary where each column becaomes a key and values are stored as lists. 

   Parameters:
   - df: pandas dataframe from .dbf
   - outJsonFilename: output path to save the json file
   """

   columns = df.columns
   rows = df.transpose().to_numpy()

   outJson = {}
   for i,c in enumerate(columns):
     outJson[c]=list(rows[i])
    
   with open(outJsonFilename, 'w') as outFile:
    json.dump(outJson, outFile, indent=2)

    print(f"JSON file saved to: {outJsonFilename}")


#####################################################
####################################################################################################
# Function calls

#Read the .dbf file 
print(f"Reading DBF: {treeMapDbf}")
dbf = Dbf5(treeMapDbf)
df = dbf.to_dataframe()#.head()

# Create json version of dbf
# This is used inside the TreeMap viewer
dfToJSON(df,treeMapJson)
# %%
