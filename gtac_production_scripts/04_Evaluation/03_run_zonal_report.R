# Generate zonal report from Rmd file
# Written by Lila Leatherman (Lila.Leatherman@usda.gov)

# Last updated: 3/11/2024


# TO DO: file path error for figures when using rmarkdown::render

### SETUP AND RUN
######################################

this.path <- this.path::this.path()
spl <- stringr::str_split(this.path, "/")[[1]]

# get path to input script with settings for imputation
input_script.path <- paste( c(spl[c(1:(length(spl)-2))],
                              "03_Imputation/00_inputs_for_imp.R" ),
                            collapse = "/")

# get path to rmd
rmd_path <- paste( c(spl[c(1:(length(spl)-1))],
                     '03_zonal_eval_report_generator.Rmd' ),
                   collapse = "/")


source(input_script.path)

# load RDS of cm files
cms_all <- readRDS(glue::glue('{eval_dir}/02_LF_Comparison/{output_name}_CMs_derivedVars.RDS'))
#cms_all <- readRDS(glue::glue('{eval_dir}/01_OOB_Evaluation/{output_name}_CMs_OOB.RDS'))


# render output report
rmarkdown::render(rmd_path, 
                  output_format = "word_document",
                  output_file = glue::glue('{cur.zone.zero}_eval_report_LFComparison'),
                  output_dir = glue::glue('{eval_dir}/03_Eval_Reports/')
                  )
