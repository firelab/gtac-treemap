# Generate zonal report from Rmd file
# Written by Lila Leatherman (Lila.Leatherman@usda.gov)

# Last updated: 3/20/2024


# TO DO: file path error for figures when using rmarkdown::render

### SETUP AND RUN
######################################

this.path <- this.path::this.path()
spl <- stringr::str_split(this.path, "/")[[1]]

# get path to input script with settings for imputation
input_script.path <- paste( c(spl[c(1:(length(spl)-2))],
                              "03_Imputation/00_inputs_for_imp.R" ),
                            collapse = "/")

source(input_script.path)

# Specific inputs
#----------------------------------------------#

# Eval report for OOB or derived vars
# options: "OOB" or "derivedVars"
#eval_vars <- "derivedVars"
eval_vars <- "OOB" 

# name of raster to validate
raster_name <- glue::glue("2016_Orig_Test_keepinbag_ntree250_tilesz2000_nT36")
#raster_name <- glue::glue("2016_GTAC_LCMSDist_tilesz2000_nT36")


# list variables to evaluate
#var_names <- c("canopy_cover", "canopy_height", "EVT_GP")
var_names <- c("canopy_cover", "canopy_height", "EVT_GP", "disturb_code")


# Load data
#----------------------------------------------#

# get path to rmd
rmd_path <- paste( c(spl[c(1:(length(spl)-1))],
                     '03_zonal_eval_report_generator.Rmd' ),
                   collapse = "/")

# get dir for temporary outputs - needs to be a place w write permissions for R
tmpout_dir <- paste( c(spl[c(1:(length(spl)-1))],
                       'tmp/' ),
                     collapse = "/")


#load raw imputation output raster
ras <- terra::rast(glue('{assembled_dir}/01_Imputation/{raster_name}.tif'))

# load RDS of cm files - conditionally point to appropriate dir

if(eval_vars = "OOB") {
  cms_all <- readRDS(glue::glue('{eval_dir}/01_OOB_Evaluation/{output_name}_CMs_{eval_vars}.RDS'))
} else if(eval_vars = "derivedVars") {
  cms_all <- readRDS(glue::glue('{eval_dir}/02_LF_Comparison/{output_name}_CMs_{eval_vars}.RDS'))
}

# TEMP
cms_all <- readRDS(glue::glue('{eval_dir}/01_OOB_Evaluation/2016_Orig_Test_keepinbag_ntree250_CMs_{eval_vars}.RDS'))


# Render report
#-----------------------------------------------------#

rmarkdown::render(rmd_path, 
                  output_format = "word_document",
                  output_file = glue::glue('{cur.zone.zero}_{output_name}_eval_report_{eval_vars}'),
                  output_dir = tmpout_dir,
                  params = list(raster_name = raster_name, 
                                zone_num = zone_num,
                                eval_vars = eval_vars)
)

# File Organization
#------------------------------------------------#

# move report from tmpout dir to desired out dir
file.copy(from=glue::glue('{tmpout_dir}/{cur.zone.zero}_{output_name}_eval_report_{eval_vars}.docx'),
          to= glue::glue('{eval_dir}/03_Eval_Reports/'),
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)

# remove file from tmp location
file.remove(glue::glue('{tmpout_dir}/{cur.zone.zero}_{output_name}_eval_report_{eval_vars}.docx'))
