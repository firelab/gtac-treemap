# Zonal Validation Script for TreeMap Outputs
# Adapted from "Z01_validation_step1.py" written by Karin Riley

# Adapted by Lila Leatherman (lila.leatherman@usda.gov)

# Goals: 
# - load preliminiary imputation outputs
# - join with x-table on ID 
# - build raster of EVC, EVH, EVT_GP to assess accuracy 
# - use concat to compare EVC, EVH, etc with landfire layers 


###########################################################################
# Set inputs
###########################################################################

# Zone list
zone_list <- c(16)

# Path to X table
xtable_path <- "//166.2.126.25/TreeMap/01_Data/01_TreeMap2016_RDA/01_Input/03_XTables/X_table_all_singlecondition.txt"

# path where raw raster output(s) live (zone will be added later)
raster_dir <- '//166.2.126.25/TreeMap/03_Outputs/07_Raw_model_outputs/2016_Original_Test/'

# raster name
raster_name <- "testRows_7500_7550"

# desired projection
prj <- terra::crs("//166.2.126.25/TreeMap/01_Data/02_Landfire/landfire_crs.prj")

# path to landfire layers
landfire_dir <- "//166.2.126.25/TreeMap/01_Data/01_TreeMap2016_RDA/02_Target/"

# path to TreeMap library
lib_path <- "C:/Users/lleatherman/Documents/GitHub/gtac-treemap/gtac_production_scripts/treemapLib.R"

# list layers to export
layers_export <- c("canopy_cover", "canopy_height", "EVT_GP",
                   "disturb_code", "disturb_year")

# Paths for exporting data
#--------------------------------------#

# set path to save output rasters
# this directory will be created if it does not already exist
output_dir <- "//166.2.126.25/TreeMap/03_Outputs/07_Raw_model_outputs/2016_Original_Test/"

# Output imputation name
output_name <- "2016_Orig_Test"

# set tmp directory
tmp_dir <- "D:/tmp/"

###########################################################################
# Set up libraries and directories
###########################################################################

# Packages and functions
#---------------------------------#

# packages required
list.of.packages <- c("terra", "tidyverse", "magrittr", "glue", "tictoc", "caret")

#check for packages and install if needed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages)

# load all packages
vapply(list.of.packages, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)

# load custom library
source(lib_path)

# Set up temp directory 
#----------------------------------#

# check if tmp directory exists 
if (file.exists(tmp_dir)){
  
} else {
  # create a new sub directory inside the main path
  dir.create(tmp_dir)
}

# set temp directory - helps save space with R terra
write(paste0("TMPDIR = ", tmp_dir), file=file.path(Sys.getenv('R_USER'), '.Renviron'))
#empty temp dir
do.call(file.remove, list(list.files(tmp_dir, full.names = TRUE)))
#remove unused memory
gc()


# Terra options
# --------------------------------#

#increase memory fraction available
terraOptions(memfrac = 0.8)

# Other options
# --------------------------------#

# Allow for sufficient digits to differentiate plot cn numbers
options("scipen"=100, "digits"=8)


#####################################################################
# Load data
####################################################################


# load x table
xtable <- read.csv(xtable_path)


#####################################################################
# Start
####################################################################


#for(i in 1:length(zone_list)) {

  
  zone_num <- zone_list[1]

  # Set zone name options and variables
  #---------------------------------------------------------#
  
  # Set zone name options
  cur.zone <- glue('z{zone_num}')
  cur.zone.zero <- if(zone_num < 10) {
    glue('z0{zone_num}') } else {
      cur.zone
    }
  
  # Set folder paths
  raster_dir = glue('{raster_dir}{cur.zone.zero}/raster/')
  output_dir = glue('{output_dir}{cur.zone.zero}/validation/')
  landfire_dir = glue('{landfire_dir}{cur.zone.zero}/')
  
  # create output folder if it does not exist
  if(!file.exists(output_dir)){
    dir.create(output_dir, )
  }

  # Load data
  #--------------------------------------------#

  #load raw output raster
  raster_path = glue('{raster_dir}{raster_name}.tif')
  ras <- terra::rast(raster_path)
  
  # trim off NA values
  ras <- trim(ras)
  
  # # check projection - reproject to desired proj if it doesn't match
  # if(!identical(crs(ras), crs(prj))){
  #   #project to desired projection
  #   ras %<>% terra::project(prj)
  # }
  
  # inspect
  ras
  
  #set name of input column
  names(ras) <- c("value")
  
  #convert to integer
  ras <- as.int(ras)
  
  
  #####
  
  # get list of IDs
  id_list<- freq(ras)$value %>%
    sort() %>%
    as.data.frame() 
  names(id_list) <- "PLOTID"
  
  # join with x table to create lookup table
  lookup <- left_join(id_list, xtable, by = c("PLOTID" = "ID")) %>%
    select(PLOTID, CN, canopy_height, canopy_cover, EVT_GP, disturb_code, disturb_year) %>%
    mutate(across(where(is.numeric), ~na_if(., NA)))
    
  
  # # Write function to reclass and export
  # #------------------------------------------------------#
  # lookupExport <- function(layer_field, raster, lookup, id_field, export_path) {
  #   
  #   lt <- cbind(lookup[id_field], lookup[layer_field])
  #   #print(head(lt))
  #   rout <- terra::classify(raster, lt)
  #   writeRaster(rout,
  #               glue('{export_path}_{layer_field}.tif'),
  #               overwrite = TRUE)
  #   rm(rout)
  #   gc(verbose = FALSE)
  #   
  # }
  
  
  
  # # apply function - to only one layer for testing
  # lookupExport("canopy_height", ras, lookup, "PLOTID",  
  #              glue('{output_dir}{cur.zone.zero}'))
  
  #lapply (can i tidyverse map? )
  lapply(layers_export, lookupExport, 
         # additional options for function
         ras = ras, lookup = lookup, id_field = "PLOTID",
         export_path = glue('{output_dir}{cur.zone.zero}'))
  
  
  
  
  ###########################
  # next up : concats() for selected fields to see diff vs. landfire
  # ----------------------------------------------------#

  # list landfire rasters in dir
  target_files <- list.files(landfire_dir, pattern = ".tif$", recursive = TRUE, full.names = TRUE)

  #read in as vrt
  lf <- terra::vrt(target_files, filename = "lf.vrt", options = "-separate", overwrite = TRUE)

  #apply names
  names(lf) <- target_files %>%
    str_extract(., "z[0-9][0-9]/([^.])*") %>%
    str_replace("z[0-9][0-9]/", "")
  
  #     bELOW: REPLACED BY FUNCTION
  #################################################
  
  # # make raster to compare
  # imp1 <-  terra::classify(ras, cbind(lookup$PLOTID, lookup$canopy_cover)) %>%
  #   terra::project(crs(lf)) %>%
  #   as.int()
  # 
  # # get single lf raster
  # lf1 <- lf$canopy_cover
  # 
  # # conditionally calculate confusion matrix
  # 
  #   print("calculating and exporting confusion matrix")
  #   
  #   t <- data.frame(ref = terra::values(lf1),
  #                   pred = terra::values(imp1))
  #   
  #   names(t) <- c("ref", "pred")
  #   
  #   levels_t <- unique(c(as.numeric(unlist((unique(imp1)))), 
  #                        as.numeric(unlist((unique(lf1))))))
  #   levels_t <- sort(levels_t)
  #   
  #   # ensure columns are factors with the same levels
  #   t %<>%
  #     mutate(ref = factor(ref, levels = levels_t),
  #            pred = factor(pred, levels = levels_t)) 
  #   
  #   # get confusion matrices
  #   # confusion matrix
  #   cm <- caret::confusionMatrix(t$pred, # pred
  #                                t$ref # ref
  #                                )
  # 
  #   # process data frames for export
  #   #---------------------------------#
  #   
  #   # raw confusion matrix
  #   cm_raw_out <- as.table(cm)
  #   cm_raw_out <- addmargins(cm_raw_out)
  #   
  #   # make data frame of classes
  #   cm_t_classes <- data.frame(as.matrix(cm, what = "classes"))
  #   names(cm_t_classes) <- levels(t$pred)
  #   cm_t_classes %<>% 
  #     rownames_to_column(., var = 'metric')
  #   
  #   # overall eval stats
  #   cm_t_overall <- data.frame(as.matrix(cm, what = "overall"))
  #   names(cm_t_overall) <- c("value")
  # 
  # # get difference; set to NA where layers are the same
  # diff <- imp1-as.int(lf1)
  # diff %<>% terra::classify(cbind(0,NA))
  # 
  # # make both rasters categorical - get levels
  # levels <- data.frame(id = sort(unique(lookup$canopy_cover)),
  #                 levels = levels(as.factor(lookup$canopy_cover)))
  # 
  # # set levels
  # levels(imp1) <- levels
  # levels(lf1) <- levels
  # 
  # 
  # # concat and mask with difference
  # out1 <- terra::concats(imp1, lf1) 
  # out1 %<>% terra::mask(diff)
  # 
  # # TO DO: 
  # #remove non-existent levels in raster
  # # l <- levels(out1)[[1]][,2]
  # # l1 <- data.frame(id = 1:length(l), 
  # #                  l1 = l)
  # # 
  # # lp <- freq(out1)$value
  # # l2 <- data.frame(l2  = l[l %in% lp],
  # #                  level = l[l %in% lp])
  # # 
  # # relevel <- left_join(l1, l2, by = c("l1" = "level")) %>%
  # #   dplyr::rename('level' = l2) %>%
  # #   dplyr::select(id, level)
  # # 
  # # levels(out1) <- relevel
  # 
  # #export
  # writeRaster(out1, 
  #             glue('{output_dir}/{cur.zone.zero}_canopy_cover_vLandfire.tif'),
  #             overwrite = TRUE)
  
  ####################### write a function
  
  # lookupConcat <- function(layer_field, raster, lookup, id_field, 
  #                          stackin_compare, stackin_compare_name, export_path, 
  #                          cm) {
  #   
  #   print(glue('lookupConcat: {layer_field}'))
  #   
  #   #print("make lookup table")
  #   #make lookup table
  #   lt <- cbind(lookup[id_field], lookup[layer_field])
  #   
  #   #print("make imp1")
  #   # make raster to compare
  #   imp1 <-  terra::classify(raster, lt) %>%
  #     terra::project(crs(stackin_compare)) %>%
  #     as.int()
  #   
  #   #print("get lf1")
  #   # get single lf raster
  #   lf1 <- lf[layer_field]
  #   
  #   # mask with input raster - necessary for testing on subset 
  #   lf1 <- terra::mask(lf1, imp1)
  #   
  #   #print("make diff")
  #   # get difference; set to NA where layers are the same
  #   diff <- imp1-as.int(lf1)
  #   diff %<>% terra::classify(cbind(0,NA))
  # 
  #   #print("get levels")
  #   # make both rasters categorical - get levels of layer field
  #   levels <- data.frame(id = sort(unique(lt[,2])),
  #                        levels = levels(as.factor(lt[,2])))
  # 
  #   #print("set levels")
  #   # set levels for rasters to make them categorical
  #   levels(imp1) <- levels
  #   levels(lf1) <- levels
  # 
  #   #print("concat")
  #   # concat and mask with difference
  #   out1 <- terra::concats(imp1, lf1)
  # 
  #   #option to calc confusion matrix here!!!!
  #   out1 %<>% terra::mask(diff)
  #   
  #   # conditionally calculate confusion matrix
  #   if(isTRUE(cm)){
  #     print("calculating and exporting confusion matrix")
  #     t <- data.frame(ref = terra::values(lf1),
  #                     pred = terra::values(imp1))
  #     
  #     names(t) <- c("ref", "pred")
  #     
  #     levels_t <- unique(c(as.numeric(unlist((unique(imp1)))), 
  #                          as.numeric(unlist((unique(lf1))))))
  #     levels_t <- sort(levels_t)
  #     
  #     # ensure columns are factors with the same levels
  #     t %<>%
  #       mutate(ref = factor(ref, levels = levels_t),
  #              pred = factor(pred, levels = levels_t)) 
  #     
  #     # get confusion matrices
  #     # confusion matrix
  #     cm <- caret::confusionMatrix(t$pred, # pred
  #                                  t$ref # ref
  #     )
  #     
  #     # process data frames for export
  #     #---------------------------------#
  #     
  #     # raw confusion matrix
  #     cm_raw_out <- as.table(cm)
  #     cm_raw_out <- addmargins(cm_raw_out)
  #     
  #     # make data frame of classes
  #     cm_t_classes <- data.frame(as.matrix(cm, what = "classes"))
  #     names(cm_t_classes) <- levels(t$pred)
  #     cm_t_classes %<>% 
  #       rownames_to_column(., var = 'metric')
  #     
  #     # overall eval stats
  #     cm_t_overall <- data.frame(as.matrix(cm, what = "overall"))
  #     names(cm_t_overall) <- c("value")
  #     
  #     #export confusion matrices to given path
  #     write.csv(cm_raw_out, 
  #               glue('{export_path}_{layer_field}_v{stackin_compare_name}_cmRaw.csv'))
  #     write.csv(cm_t_classes, 
  #               glue('{export_path}_{layer_field}_v{stackin_compare_name}_cmClasses.csv'))
  #     write.csv(cm_t_overall, 
  #               glue('{export_path}_{layer_field}_v{stackin_compare_name}_cmOverall.csv'))
  #     rm(t)
  #   }
  #   
  #   #print("export")
  #   
  #   #export
  #   writeRaster(out1, 
  #               glue('{export_path}_{layer_field}_v{stackin_compare_name}.tif'),
  #               overwrite = TRUE)
  #   rm(imp1, lf1, out1)
  #   gc()
  # }
  # 
  # test function
  lookupConcat("canopy_cover", ras, lookup, "PLOTID",
               lf, "Landfire", glue('{output_dir}{cur.zone.zero}'),
               cm = TRUE)
  
  ##############################################################
  
  #lapply function
  lapply(layers_export, lookupConcat, # list to apply over, function to apply
         # additional arguments to function
         ras = ras, lookup = lookup, id_field = "PLOTID",
         stackin_compare = lf, stackin_compare_name = "Landfire",
         export_path = glue('{output_dir}{cur.zone.zero}'), cm = TRUE)
  
  ####################################
  
  
  
#}
