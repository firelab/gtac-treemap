# packages required
list.of.packages <- c("terra", "tidyverse", "magrittr", "glue", "tictoc", "foreach", "doParallel", "this.path", "shiny", "rsconnect", "bslib", "data.table", "DT", "plotly")

# #check for packages and install if needed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages)

# load all packages
vapply(list.of.packages, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)

runApp(launch.browser=TRUE)