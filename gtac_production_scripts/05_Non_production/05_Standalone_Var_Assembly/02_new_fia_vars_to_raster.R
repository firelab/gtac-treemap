######################################################
# SET INPUTS
######################################################

# Specific inputs
#-----------------------------------------------------#

# pick a TreeMap vintage to use for assembly 
tm_version = "2022_Production"
tm_project_name = "2022_Production_newXtable"

# export location
output_dir = "//afssxgtacnas311/ForestMAP/80_Workspace/40_TreeMap_Volume/Attribute_assembly/"

# name for output products
output_project_name = "TM_Volume_Calcs"

# rat path - with new or any desired variables to assemble
rat_path = glue::glue('{output_dir}00_Raster_attribute_table/TreeMap_RAT_SOUNDVOLUME.csv')
rat_path_new = "//afssxgtacnas311/ForestMAP/80_Workspace/40_TreeMap_Volume/Attribute_assembly/00_Raster_attribute_table/TreeMap_RAT_SOUNDVOLUME.csv"

# list zones of interest. options = "all" or a list of specific zones
#zones_desired = "all"
zones_desired = c(1)

# attributes to export
attributes_export = c("VOLCFSND_L_PX", "VOLCFSND_D_PX")

# Load data
#---------------------------------------------------------------------
# Allow for sufficient digits to differentiate plot cn numbers
options("scipen"=100, "digits"=8)

this_proj <- this.path::this.proj()
this_dir <- this.path::this.dir()

## load treemap library
lib_path = glue::glue('{this_proj}/gtac_production_scripts/00_Library/treeMapLib.R')
source(lib_path)

# Load project params
project_params_path = glue::glue('{this_proj}/gtac_production_scripts/03_Imputation/params/{tm_version}_imputation_inputs.RDS')


load(project_params_path)

rat_in <- read.csv(rat_path_new)

# Apply to each zone
#-------------------------------------------------------------------#

print("zones to run: ")
zones_list

# loop over each zone
for(zone in zones_list[2:length(zones_list)]){
  
  
  print(glue::glue("working on zone {zone}"))


  cur_zone_zero = ifelse(zone<10, glue::glue("z0{zone}"),
                                  glue::glue('z{zone}'))

  # - Make export folder for zone
  zone_output_dir = glue::glue("{output_dir}/01_Zones/{cur_zone_zero}")
  dir.create(zone_output_dir)

  # - get path to assembled dir
  assembled_dir = glue::glue('{home_dir}03_Outputs/07_Projects/{tm_project_name}/02_Assembled_model_outputs/{cur_zone_zero}/')


  ###########################################################
  # LOAD DATA
  ###########################################################

  message("Loading data for attribute layer assembly")

  # Imputed raster
  #-------------------------------------------#
  # name of raster to validate
  raster_name <- glue::glue("{cur_zone_zero}_{output_name}_Imputation")

  #load raw imputation output raster
  ras <- terra::rast(glue::glue("{assembled_dir}/01_Imputation/{raster_name}.tif"))

  # trim off NA values
  ras <- terra::trim(ras)

  # check projection - an inspection
  crs(ras, describe = TRUE)

  #set name of input column
  names(ras) <- c("value")

  # inspect
  ras
  plot(ras,
      main = glue::glue("Raw imputed ids: Zone {zone}"))




  ########################################################################
  # Assemble from RAT
  ########################################################################

  #assembleExport() function

  attributes_export %>%
    map( \(x) assembleExport(x, 
                        raster = ras,
                        lookup = rat_in,
                        id_field = "TM_ID",
                        export_path = glue::glue('{zone_output_dir}/{cur_zone_zero}')
                        
    ))


  }


