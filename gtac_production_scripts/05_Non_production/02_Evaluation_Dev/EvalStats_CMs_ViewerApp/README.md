# Evaluation stats and confusion matrices (CMs) viewer app

This sub-directory consists files for an R shiny app developed for interactive viewing for evaluation statistics and CMs. This app is designed to help with the quality assessment (QA) of TreeMap products.

* The [RUN_APP.bat](./RUN_APP.bat) file starts the R environment and runs the [run.R](./run.R) script that calls the server and ui portions of the R shiny app and launches the app on a local browser. **This is the recommended
 method of launching the R Shiny App.**  
    * Review the [App Configuration](#app-configuration) section of this README to correctly set up the app before launching.
* The server and UI portions of the R shiny app in the [server.R](./server.R) and [ui.R](./ui.R) scripts (respectively).
* The inputs in the app are supposed to be entered and loaded by section (as numbered in the shiny app; 1, 2, 3).
    + Running a latter section before executing the previous one will return an error message.
    + If variables for a previous section are changed and run, then the remaining sections need to be re-run apply the change and display updated tables and plots. 
* For troubleshooting, please open either the ui.R or server.R in RStudio and use the "Run App" option to run the app and view error messages in the RStudio console.

## App Configuration
1. Please make sure the "setup_dirs.R" file in your local repository is set properly before running the app.
2. Review the errors (if any) displayed in the terminal while it executes the "RUN_APP.bat" file:
    * Open the "RUN_APP.bat" file on a text editor to set the correct path to "Rscript.exe" in the .bat file (inconsistencies in the path might arise from the version of R that is currently installed on the machine).
    * Open the "run.R" script in Rstudio and source the script. This will install all necessary libraries and launch the app. Once sourced, executing the .bat file to run the app should work for all consecutive runs.
3. The [server.R](./server.R) and [ui.R](./ui.R) scripts might need to be updated according to the naming conventions of the specific TreeMap product version (e.g., 2020, 2022, 2023) and folder paths. For the outputs to be evaluated: 
    * Update the project name (folder name) of the outputs by updating the `choices` parameter of the `selectInput()` function for `inputID = "project_name"` in the [ui.R script](/ui.R#L9).  
    * Update the output name (filename prefixes) of the outputs by updating the conditional `if/else if` conditional block in the [server.R script](./server.R#L26). Add a condition for the newly added project name to set the output name.  
        * For example, if the output name is the same as the newly added project name the added code block will look like this:
            ```
            # existing if/else if conditional block...
            } else if (project_name == "new_project_name") {
                output_name = "new_project_name"
            }  
            ```