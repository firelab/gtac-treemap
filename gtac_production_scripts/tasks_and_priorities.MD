### PRIORITIES

This document lists current priorities for updating the GTAC TreeMap workflow, as well as completed tasks. 

Completed tasks are denoted by ~~strikethrough~~. 

### ASK IAN: 

#### General: 
- Provide the option to load in existing parameter set? 
- Estimate total time required for each zone- for data prep, imputation, validation, etc. 
- tryCatch for listing new packages and installing if needed
- Script organization - two numbers, one for each stage - eg 02_00_inputs_for_imp 
- Dictionaries/dfs in library with disturbance codes, evt groups, tree classes to reclassify
	- Currently, these live in the target data prep scripts
	- Make sure projections align between target, reference, and output data 
	- Get a CRS explicitly for desired output projection 
		- Does this need to be an overarching setting? 
	- Prioritization: Calculate tree cover % for each landfire zone to aid progress tracking 
	- ~~Handle relative paths in R to call scripts from other scripts - this.path()~~
	- ~~Add high-level option for selecting target layers-- e.g., different disturb layers~~
	- ~~Folder organization - structure by project; have all files end up in a folder for the same zone~~
	- ~~Make scripts conform to folder organization~~
	- ~~How to make gc() silent - silent when it runs in a loop~~ 
	- ~~Try running off local drive - to compare speed~~
	- ~~Add numbers to reference and target data prep scripts~~
	- ~~At the end of 00_set up imputation, remove objects that don't get used in the future~~
	- ~~Make sure variable names are consistent across scripts: put in set up script if possible~~
	- ~~Library for TreeMap functions~~
		- ~~Evaluation function - compare one raster vs another~~
		- ~~Assemblage functions~~
			- ~~Classify based on lookup table~~
			- ~~Concat based on lookup table~~
	- ~~Maintainable code structures~~
	- ~~Turn operations into functions instead of iterating over lists~~
		- ~~Apply with tidyverse map~~ 
	


#### Target data:
- Control script to run multiple zones in a row 
- Write out file with parameters used to generate target data
- MAKE SURE EXTENTS OF PREPPED TARGET DATA MATCH
	- - holding up imputation on zone 19
- Test on a different LF zone 
- Test prep all vars for zone 16 - put these in v2016_GTAC target data folder 
- Add progress tracking for parallelization 
- How to store disturbance data? So many different years; are they really different landfire versions? 
- Script to download and extract all necessary Landfire data 
	- Disturbance
	- Topo
	- EVC
	- EVH
- Script to download / access LCMS Raw Probability rasters - Python / CMD 
- Script to download Landfire Biophys layers - stored on FigShare
- ~~Combine LCMS / Landfire disturbance prep scripts into analagous process~~
- ~~Chunk target data prep into tiles for disturbance - and parallelize~~
- ~~Make target data prep script more flexible to handle varied path names~~
- ~~Control script for target data prep~~
- ~~Perform raster math on Aspect - convert to Northing and Easting~~

#### Reference data:
- 00 control script for reference data prep?
- Include unabbreviated plot coords in X-table - load from export 
- Calculate FIA derivatives alongside reference data prep 
	- Compare raster attribute table values for FIA vars to what I've calculated
	- Check carbon, biomass, and TPA calculations with John Shaw
- Appropriate file naming and data storage 
	- More organization in Reference Data folders 
- ~~Address multiple records for CNs in reference tables~~
	- ~~All appear to be coming from WY~~
- ~~Extract EVC and EVH from FIA db - to give us an option vs. FVS - Rachel Houtman will do comparison~~
- ~~Calculate end attributes that come from the TREE table:~~
	- ~~Carbon, Live Above Ground~~
	- ~~DryBio_D~~
	- ~~DryBio_L~~
- ~~Automate download of FIA db tables by state~~
- ~~Perform math on Aspect earlier, outside imputation script - convert to Northing and Easting~~
	

#### Imputation: 
- Is it necessary to create dummy rows for non-appearing EVGs? 
- How to check if the yaImpute installed is my version? And install if not
	- Add devtools installation in treeMapLib file
- Join x table with new coords from John - importantly, this new file doesn't have coords for all points in the x table
- ~~Try with dopar~~
- ~~Try to write out rows within future loop then read back in and see time difference~~
- ~~Make a plot that shows valid, non-na tiles plotted over raster extent~~
	- ~~With key for tile number~~
- ~~Increase mem fraction for terra in 02_run_imputation~~
- ~~Have a single setup script run for build imputation and run imputation - to make sure names stay the same~~
	- ~~Make sure inputs and file names etc align with all imputation scripts~~
- ~~Perform data frame processing with Tidyverse~~
	- Tidyverse is definitely an improvement for (Lila's readability. Determine if tidyverse is an improvement for speed? Does it streamline things in memory
- ~~Loop over tiles~~
- ~~Make imputation function run on data frame, rather than raster~~
- ~~Make imputation take northing and easting instead of ASPECT~~
- ~~Make imputation take POINT_X and POINT_Y as inputs instead of calculating from raster~~
- ~~Export plots left out of each tree to use in model validation~~
- ~~Once imputation function is stable, remove it from imputation script -- refer just to Lib~~
- ~~Source Lib and setup script in imputation steps~~
- Progress bar on parallelization of imputation
- ~~Convert to R terra from R raster package -~~
- ~~Explore options other than by row to break up imputation - eg tile~~
- ~~Default / published operation is to treat disturbance code as binary - currently, this is parsed at a couple different instances. Make it parsed one way, once, in build_imputation script~~
- ~~Remove unused objects.~~
	- ~~Using rm()~~
	- ~~Vet script for extra / unused objects~~
- ~~Distinguish legacy comments from important ones~~
	- ~~Remove unused / confusing sections of commented-out code~~
	- ~~Determine most current version of script - based on commented out sections and comparison with literature~~
- ~~Rework imputation function so it's more standalone -- adjust the "scope" so that it takes all the arguments it requires~~
- ~~Split into two scripts for 1) imputation prep 2) imputation zone~~
- ~~Add gc() calls to remove unused memory~~
- ~~Develop option to test model on a smaller, explicit geographic subset of interest -- eg. Shapefile of choice, rather than entire landfire zone~~
- ~~Export model accuracy reports~~
- ~~Remove all  setwd()~~

#### Validation
- 
- Add calculated FIA vars and carbon attributes to validation 
	- ~~Test getting these values from raster attribute table~~
	- For OOB: calc RMSE, use original plot CN vs imputed plot CN
	- For predicted outputs: use original plot CN vs imputed plot CN
		- can't do landfire comparison 
- ~~Standardize names of Target vs Reference data in eval reports:~~
	- ~~Ground (FIA)~~
	- ~~Imputed~~
	- ~~Observed (Target)~~
	- ~~Observed (Out of Bag)~~

~~Increase number of decimal points for reporting accuracy and other metrics~~
- ~~Separate R script to run markdown report generation - more flexibility to change settings etc by using render~~
- ~~Fix R Markdown confusion matrix rendering~~
	-~~output formatted confusion  matrix, scaled appropriately to page~~
		- ~~Try: https://stackoverflow.com/questions/57175351/flextable-autofit-in-a-rmarkdown-to-word-doc-causes-table-to-go-outside-page-mar~~
	- ~~Automated highlighting on the diagonal / correct cells~~
- ~~Plot raw imputed raster at top of validation report - as a visual cue. Or at least a map with the Landfire zone~~
- ~~Save validation reports to dedicated directory~~
- ~~Save validation reports with a specified name~~
-~~ Rename "map validation" to "landfire agreement"~~
- ~~Decide what to do with / how to handle model vs map validation outputs -~~
	- ~~calculate first, then decide what I want to do with them~~
- ~~Automated generation of confusion matrices for OOB observations~~
- ~~Separate out confusion matrix from assembly functions~~
- ~~Build zonal validation report using R markdown~~
	- ~~For EVC, EVH, EVT_GP, dist_code, dist_year:~~
		- ~~Confusion matrix with accuracy by class~~
		- ~~Plot imputed vs. landfire plots of each class~~
