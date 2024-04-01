# Generate zonal report from Rmd file
# Written by Lila Leatherman (Lila.Leatherman@usda.gov)

# Last updated: 3/28/2024


# TO DO:


### SETUP AND RUN
######################################

# Specific inputs
#----------------------------------------------#

# zone number
zone_num <- 16

# Project name
#project_name <- "2016_GTAC_Test"
project_name <- "2016_GTAC_LCMSDist"

# output name - name of raster and CM outputs
#output_name <- "2016_Orig_Test_keepinbag_ntree250"
output_name <- "2016_GTAC_LCMSDist"

# name of raster to validate
#raster_name <- glue::glue("2016_Orig_Test_keepinbag_ntree250_tilesz2000_nT36")
raster_name <- glue::glue("2016_GTAC_LCMSDist_tilesz2000_nT36")

# Eval report for OOB or derived vars
# options: "OOB" or "LFComparison"
eval_vars <- "LFComparison"
#eval_vars <- "OOB" 

# list variables to evaluate
var_names <- c("canopy_cover", "canopy_height", "EVT_GP", "disturb_code")
#var_names <- c("canopy_cover", "canopy_height", "EVT_GP")

# Other settings
#--------------------------------------------------#

# set number of digits to round to
round_dig <- 4

home_dir <- "//166.2.126.25/TreeMap/"

# set location of raster attribute table
rat_path <- glue::glue("{home_dir}01_Data/01_TreeMap2016_RDA/RDS-2021-0074_Data/Data/")

# Path to X table
xtable_path <- glue::glue("{home_dir}03_Outputs/06_Reference_Data/v2016_RMRS/X_table_all_singlecondition.txt")

# set path to landfire vector data
lf_zones_path <- glue::glue("{home_dir}01_Data/02_Landfire/LF_zones/Landfire_zones/refreshGeoAreas_041210.shp")


# Other options
# --------------------------------#

# Allow for sufficient digits to differentiate plot cn numbers

options("scipen" = 100, "digits" = 8)

# Load library
#-------------------------------------------------------#

this.path <- this.path::this.path()
spl <- stringr::str_split(this.path, "/")[[1]]

# # get path to input script with settings for imputation
# input_script.path <- paste( c(spl[c(1:(length(spl)-2))],
#                               "03_Imputation/00_inputs_for_imp.R" ),
#                             collapse = "/")
# 
# source(input_script.path)

# get path to library script
spl1 <- stringr::str_split(this.path, "/")[[1]]
lib.path <- paste( c(spl1[c(1:(length(spl1)-2))],
                     "00_Library/treemapLib.R" ),
                   collapse = "/")

source(lib.path)

# Prep constructed paths
#----------------------------------------------#

# get path to rmd
rmd_path <- paste( c(spl[c(1:(length(spl)-1))],
                     '03b_zonal_eval_report_generator.Rmd' ),
                   collapse = "/")

# set dir for temporary outputs - needs to be a place w/ write permissions for R (network drives aren't allowed)
tmpout_dir <- paste( c(spl[c(1:(length(spl)-1))],
                       'tmp/' ),
                     collapse = "/")

# Set zone name options
cur.zone <- glue::glue('z{zone_num}')
cur_zone_zero <- if(zone_num < 10) {
  glue::glue('z0{zone_num}') } else {
    cur.zone
  }

#set path for assembled rasters
assembled_dir <- glue::glue('{home_dir}/03_Outputs/99_Projects/{project_name}/02_Assembled_model_outputs/{cur_zone_zero}/')

# Evaluation dir
eval_dir <- glue::glue('{home_dir}/03_Outputs/99_Projects/{project_name}/03_Evaluation/{cur_zone_zero}')

# Load evaluation data
#------------------------------------------------------#

#load raw imputation output raster
ras <- terra::rast(glue::glue('{assembled_dir}/01_Imputation/{raster_name}.tif'))

# load RDS of cm files
# -> point to appropriate dir

if(eval_vars == "OOB") {
  
  cms_path <- glue::glue('{eval_dir}/01_OOB_Evaluation/{output_name}_CMs_{eval_vars}.RDS')
  ref_type <- "Out of Bag FIA plots"

} else if(eval_vars == "LFComparison") {
  
  cms_path <- glue::glue('{eval_dir}/02_LF_Comparison/{output_name}_CMs_{eval_vars}.RDS')
  ref_type <- "Landfire target layers"
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
zone_name <- glue::glue('LFz{zone_num}_{gsub(" ", "", zone$ZONE_NAME)}')


# Load raster attribute table and points
#-------------------------------------------------#

# load X_df
X_df <- read.csv(xtable_path) %>%
  rename(PLOTID = ID)

# load rat
rat <- terra::rast(glue::glue('{rat_path}TreeMap2016.tif'))
rat <- data.frame(cats(rat))

rat %<>% 
  rename("SDIPCT_RMRS" = SDIPCT_RMR,
         "CARBON_DOWN_DEAD" = CARBON_DWN) %>%
  mutate(CN = as.numeric(CN)) 

# join with X df  
rat %<>%
  right_join(X_df, by = c("CN" = "CN", "tm_id" = "PLOTID")) 

# convert to spatial points
rat_sp <- terra::vect(rat, geom = c("ACTUAL_LON", "ACTUAL_LAT"), crs = "epsg:4326") %>%
  project(crs(ras))

# crop to points within zone 16? 
rat_sp_z <- terra::crop(rat_sp, zone)

# convert to data frame
rat_z <- data.frame(rat_sp_z)

# Calc frequency for all vars in RAT
#------------------------------------------#

rat_freq_all <- list()

for (i in seq_along(var_names)) {

  #var = names(rat_z)[i]
  var = var_names[i]
  
  # get frequency table
  f_out <- rat_z %>%
    select(all_of(var)) %>%
    table() %>%
    data.frame() 
  
  # calc total to use in normalizing
  total = sum(f_out$Freq)
  
  # add normalized frequency
  f_out$Freq_norm = f_out$Freq/total
  
  # join with other outputs
  rat_freq_all = c(rat_freq_all, list(f_out))
  
}

names(rat_freq_all) <- var_names

# inspect
#rat_freq_all

# Render report
#-----------------------------------------------------#

rmarkdown::render(rmd_path, 
                  output_format = "word_document",
                  output_file = glue::glue("{cur_zone_zero}_{output_name}_eval_report_{eval_vars}"),
                  output_dir = tmpout_dir,
                  #build df of params to share with rmd
                  params = list(raster_name = raster_name, 
                                zone_num = zone_num,
                                eval_vars = eval_vars,
                                var_names= var_names, 
                                cms_path = cms_path,
                                ref_type = ref_type)
)

# File Organization
#------------------------------------------------#

# move report from tmpout dir to desired out dir
file.copy(from=glue::glue('{tmpout_dir}/{cur_zone_zero}_{output_name}_eval_report_{eval_vars}.docx'),
          to= glue::glue('{eval_dir}/03_Eval_Reports/'),
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)

# remove file from tmp location
file.remove(glue::glue('{tmpout_dir}/{cur_zone_zero}_{output_name}_eval_report_{eval_vars}.docx'))
