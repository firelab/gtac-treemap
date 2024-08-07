# How to run the TreeMap imputation

## Using the imputation control script!

The imputation control script is designed to run imputation and evaluation for multiple zones in a loop. Ideally, you should only need to update the parameters in one place to change them appropriately for your year and inputs of interest. 

_Parameters to change:_


1. 00_Library/setup_dirs.R
    - ```home_dir  <<- "//Your/Home/Directory" #e.g., "//166.2.126.25/TreeMap"  ```
    - ```FIA_dir  <<-  "//Your/FIA/Directory"```
    - ```tmp_dir <<- "//Your/Temporary/Directory" #e.g., "D:/tmp/"```

Update these paths FIRST. Make sure that the "setup_dirs.R" script a) exists and b) is listed in your .gitignore file. 

2. 03_Imputation/000_imputation_control_script.R : 
    - ``` year_input <- 2020 # change to year of interest ```
    - ``` zones_list <- c(1, 2, 3 ... ) # change to zones of interest. pull this from a csv if desired. will run zones in the order specified```
other parameters in this script should not need to be changed unless the paths to the scripts have changed. 

3. 03_Imputation/00a_project_inputs_for_imp.R : 
    - check parameters under the "Set inputs" block. This is where the bulk of things can get messy. Here we set the "name" of the project that gets used for outputs, and the base file paths that are used to refer to data. 

4. 03_Imputation/00b_zonal_inputs_for_imp.R : 
    - shouldn't need to change anything


