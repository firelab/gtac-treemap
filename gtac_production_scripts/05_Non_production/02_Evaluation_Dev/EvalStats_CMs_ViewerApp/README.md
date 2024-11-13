# Evaluation stats and confusion matrices (CMs) viewer app

This sub-directory consists files for an R shiny app developed for interactive viewing for evaluation statistics and CMs. This app is designed to help with the quality assessment (QA) of TreeMap products.

* The server and UI portions of the R shiny app in the "server.R" and "ui.R" scripts (respectively).
* The "RUN_APP.bat" file starts the R environment and runs the "run.R" script that calls the server and ui portions of the R shiny app and launches the app on a local browser.
* The inputs in the app are supposed to be entered and loaded by section (as numbered in the shiny app; 1, 2, 3).
    + Running a latter section before executing the previous one will return an error message.
    + If variables for a previous section are changed and run, then the remaining sections need to be re-run apply the change and display updated tables and plots. 
* For troubleshooting, please open either the ui.R or server.R in RStudio and use the "Run App" option to run the app and view error messages in the RStudio console.

**TROUBLESHOOTING NOTES**:
* Please make sure the "setup_dirs.R" file in your local repository is set properly before running the app.
* If terminal automatically exits when executing the "RUN_APP.bat" file:
    * Open the "RUN_APP.bat" file on a text editor to set the correct path to "Rscript.exe" in the .bat file (inconsistencies in the path might arise from the version of R that is currently installed on the machine).
    * Open the "run.R" script in Rstudio and source the script. This will install all necessary libraries and launch the app. Once sourced, executing the .bat file to run the app should work for all consecutive runs.
