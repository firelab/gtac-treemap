# Generate zonal report from Rmd file
# Written by Lila Leatherman (Lila.Leatherman@usda.gov)

# Last updated: 4/1/2024


# TO DO:


### SETUP AND RUN
######################################

# Specific inputs
#---------------------------------------------#

# list variables to evaluate
eval_vars <- c("canopy_cover", "canopy_height", "EVT_GP", "disturb_code")
#eval_vars <- c("canopy_cover", "canopy_height", "EVT_GP")


# Eval report for OOB or derived vars
# options: "OOB" or "TargetLayerComparison" 
#eval_type <- "TargetLayerComparison"
eval_type <- "OOB" 


# Standard inputs
#---------------------------------------------#

# Set inputs - from input script
this.path <- this.path::this.path() # Id where THIS script is located

# get path to input script
spl <- stringr::str_split(this.path, "/")[[1]]
input_script_path <- paste(c(spl[c(1:(length(spl) - 1))],
                             "00_inputs_for_evaluation.R"),
                           collapse = "/")

source(input_script_path)



# Other settings
#--------------------------------------------------#

# set number of digits to round to
round_dig <- 4



# Other options
# --------------------------------#

# Allow for sufficient digits to differentiate plot cn numbers

options("scipen" = 100, "digits" = 8)


# Prep constructed paths
#----------------------------------------------#

# get path to rmd
rmd_path <- paste(c(spl[c(1:(length(spl) - 1))],
                    "04b_zonal_eval_report_generator.Rmd"),
                  collapse = "/")

# set dir for temporary outputs - needs to be a place w/ write permissions for R (network drives aren't allowed)
tmpout_dir <- paste(c(spl[c(1:(length(spl) - 1))],
                      "tmp/"),
                    collapse = "/")

plot_labels <- c("Imputed", "Observed")

# Load evaluation data
#------------------------------------------------------#

#load raw imputation output raster
ras <- terra::rast(glue::glue("{assembled_dir}/01_Imputation/{raster_name}.tif"))

# conditional loads and variables based on evaluation type: 

#   - load RDS of cm files
#   - label reference type
#   - make labels for plots

if(eval_type == "OOB") {
  
  cms_path <- glue::glue("{eval_dir}/01_OOB_Evaluation/{output_name}_CMs_{eval_type}.RDS")
  plot_labels <- c("Imputed", "Observed (Out-of-bag)")

} else if(eval_type == "TargetLayerComparison") {
  
  cms_path <- glue::glue("{eval_dir}/02_Target_Layer_Comparison/{output_name}_CMs_{eval_type}.RDS")
  plot_labels <- c("Imputed", "Observed (Target Layers)")
}

cms_all <- readRDS(cms_path)

# Load zone
#----------------------------------------#

# load LF zone data
LF_zones <- terra::vect(lf_zones_path)

# select single LF zone
zone <- subset(LF_zones, LF_zones$ZONE_NUM == zone_num)

#project
zone <- terra::project(zone, crs(ras))

# get name of zone
zone_name <- glue::glue("LFz{zone_num}_{gsub(' ', '', zone$ZONE_NAME)}")

# Load X table
#--------------------------------------------------#

# load X_df
X_df <- read.csv(xtable_path) %>%
rename(PLOTID = ID)

# Load raster attribute table and points
#-------------------------------------------------#

# load rat
rat <- terra::rast(glue::glue("{rat_path}TreeMap2016.tif"))
rat <- data.frame(cats(rat))

rat %<>%
  rename("SDIPCT_RMRS" = SDIPCT_RMR,
         "CARBON_DOWN_DEAD" = CARBON_DWN) %>%
  mutate(CN = as.numeric(CN)) 

# join with X df  
rat %<>%
  right_join(X_df, by = c("CN" = "CN", "tm_id" = "PLOTID")) 

# convert to spatial points
rat_sp <- terra::vect(rat, geom = c("ACTUAL_LON", "ACTUAL_LAT"),
                      crs = "epsg:4326") %>%
  terra::project(crs(ras))

# crop to points within zone 16?
rat_sp_z <- terra::crop(rat_sp, zone)

# convert to data frame
rat_z <- data.frame(rat_sp_z)

# remove actual rat tif - not needed
rm(rat)

# Calc frequency for all vars in RAT
#------------------------------------------#

rat_freq_all <- list()

for (i in seq_along(eval_vars)) {

#var = names(rat_z)[i]
  var = eval_vars[i]

# get frequency table
  f_out <- rat_z %>%
    select(all_of(var)) %>%
    table() %>%
    data.frame() 
  
  # add normalized frequency
  f_out$Freq_norm = f_out$Freq/sum(f_out$Freq)
  
  # join with other outputs
  rat_freq_all = c(rat_freq_all, list(f_out))
  
}

names(rat_freq_all) <- eval_vars

# inspect
#rat_freq_all

# Render report
#-----------------------------------------------------#

rmarkdown::render(rmd_path, 
                  output_format = "word_document",
                  output_file = glue::glue("{cur_zone_zero}_{output_name}_eval_report_{eval_type}"),
                  output_dir = tmpout_dir,
                  #build df of params to share with rmd
                  params = list(raster_name = raster_name, 
                                zone_num = zone_num,
                                eval_type = eval_type,
                                eval_vars= eval_vars, 
                                cms_path = cms_path)
)

# File Organization
#------------------------------------------------#

# move report from tmpout dir to desired out dir
file.copy(from=glue::glue("{tmpout_dir}/{cur_zone_zero}_{output_name}_eval_report_{eval_type}.docx"),
          to= glue::glue("{eval_dir}/04_Eval_Reports/"),
          overwrite = TRUE, recursive = FALSE,
          copy.mode = TRUE)

# remove file from tmp location
file.remove(glue::glue("{tmpout_dir}/{cur_zone_zero}_{output_name}_eval_report_{eval_type}.docx"))
