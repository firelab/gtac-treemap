# This library contains helper functions and dictionaries for TreeMap production scripts.

# Author: Lila Leatherman (lila.leatherman@usda.gov)

# Last Updated:
# 2/7/2024

#################################################################
# Load required packages
#################################################################

# packages required
list.of.packages <- c("terra", "tidyverse", "magrittr", 
                      "glue", "tictoc", "caret", "yaImpute", "this.path")

#check for packages and install if needed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages)

# load all packages
vapply(list.of.packages, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)

###########################
# Helper functions
###########################

# make 'notin' function
`%notin%` <- Negate('%in%')


###########################
# Run other scripts in this folder
###########################

# Id where script is located
this.path <- this.path::this.path()

spl <- str_split(this.path, "/")[[1]]
spl <- c(spl[c(1:length(spl)-1)])

script.path <- paste(spl, collapse = "/")

# List other scripts in library
snames <- list.files(script.path, full.names = TRUE)
snames <- snames[snames != this.path]

# source other scripts in library
# how do i make this silent? 
lapply(snames, source)

###########################
# Dictionaries
###########################


###########################
# Validation
###########################

# Function to get out-of-bag cn predictions from a given yai object
# returns a list of reference (ref) IDs, predicted (pred) IDs, tree number (1-ntrees in mode),
# and the # of times that ref was included in-bag
# so, where inbag_count = 0, that was an out-of-bag observatin for that tree


# Function for Evaluation - Confusion matrices 
# Takes a two-column data frame as an input
# with PREDICTED VALUES as column 1
# and REFERENCE VALUES as column 2
# Produces confusion matrix tables formatted the way I like them :)
########################################################################

eval_cm_function <- function(t, noDataVal) {
  
  #require(c(tidyverse, caret))
  
  #apply column names
  names(t) <- c("pred", "ref")
  
  # set levels for factors
  # get maximum value of table that's not the noDataValue
  tn <- t
  tn[tn == noDataVal] <- NA
  
  # get levels - all levels that appear in each of pred and ref
  levels_t <- unique(c(as.numeric(unlist((unique(t$pred)))),
                       as.numeric(unlist((unique(t$ref))))))
  levels_t <- sort(levels_t)
  
  # ensure columns are factors with the same levels
  t %<>%
    mutate(ref = factor(ref, levels = levels_t),
           pred = factor(pred, levels = levels_t)) 
  
  # calculate frequency table
  tfreq <- 
    cbind(table(t$ref),
          table(t$pred)) %>%
    data.frame()
  
  # manipulate frequency table
  names(tfreq) <- c("ref", "pred")
  tfreq$class <- rownames(tfreq)
  rownames(tfreq) <- NULL
  tfreq %<>%
    dplyr::select(class, ref, pred)
  
  # confusion matrix
  cm <- caret::confusionMatrix(t$pred, # pred
                               t$ref # ref
  )
  
  # process data frames for export
  #---------------------------------#
  
  # raw confusion matrix
  cm_raw_out <- as.table(cm)
  cm_raw_out <- addmargins(cm_raw_out)
  
  # make data frame of classes
  cm_t_classes <- data.frame(as.matrix(cm, what = "classes"))
  names(cm_t_classes) <- levels(t$pred)
  cm_t_classes %<>% 
    rownames_to_column(., var = 'metric')
  
  # overall eval stats
  cm_t_overall <- data.frame(as.matrix(cm, what = "overall"))
  names(cm_t_overall) <- c("value")
  cm_t_overall$metric <- rownames(cm_t_overall)
  
  # format output
  # ---------------------------- #
  out_list <- list(cm_raw_out,
                   cm_t_classes,
                   cm_t_overall,
                   tfreq)
  
  names(out_list) <- c("raw", "classes", "overall", "freq")
  
  return(out_list)
  
}

################################################################################
# Write function to reclass rasters from id field to variable field, and export
##################################################################

assembleExport <- function(layer_field, raster, lookup, id_field, export_path) {
  
  print(glue('assembleExport: {layer_field}'))
  lt <- cbind(lookup[id_field], lookup[layer_field])
  #print(head(lt))
  rout <- terra::classify(raster, lt)
  writeRaster(rout,
              glue('{export_path}_{layer_field}.tif'),
              overwrite = TRUE)
  rm(rout)
  gc(verbose = FALSE)
  
}

########################################################################
# Returns a confusion matrix of lookup raster vs. reference raster
###############################################################################

assembleCM <- function(layer_field, raster, lookup, id_field, 
                       stackin_compare, stackin_compare_name,  
                       remapEVT_GP, EVT_GP_remap_table) {
  
  print(glue('assembleCM: {layer_field}'))
  
  #print("make lookup table")
  #make lookup table
  lt <- cbind(lookup[id_field], lookup[layer_field])
  
  #print("make imp1")
  # make raster to compare
  imp1 <-  terra::classify(raster, lt) %>%
    terra::project(crs(stackin_compare)) %>%
    as.int()
  
  #print("get lf1")
  # get single lf raster
  lf1 <- lf[layer_field]
  
  # mask reference raster with input raster - necessary for testing on subset 
  lf1 <- terra::crop(lf1, imp1, mask = TRUE)
  
  # Conditionally remap EVT
  if(remapEVT_GP & layer_field == "EVT_GP") {
    
    # load remap table
    lt_evg <- EVT_GP_remap_table
    names(lt_evg) <- c("EVT_GP", "EVT_GP_remap")
    lt_evg %<>%
      select(EVT_GP_remap, EVT_GP)
    
    #remap
    lf1 <- terra::classify(lf1, lt_evg)
  } 

  
  #print("get levels")
  # make both rasters categorical - get levels of layer field
  levels <- data.frame(id = sort(unique(lt[,2])),
                       levels = levels(as.factor(lt[,2])))
  
  #print("set levels")
  # set levels for rasters to make them categorical
  levels(imp1) <- levels
  levels(lf1) <- levels
  
  # Calculate confusion matrix
  #--------------------------------------#
  
  #print("calculating and exporting confusion matrix")
  t <- data.frame(pred = terra::values(imp1),
                  ref = terra::values(lf1))
  
  # calculate cms 
  cms<- eval_cm_function(t, NA)
  
  rm(imp1, lf1)
  gc()
  
  return(cms)
}

######################################################################################
# Function to lookup variable by id from lookup table,
# and concat against variable with the same name in another raster stack (stackin_compare)
# Produces a concat raster with categorical values formatted like p_r
# where p = the predicted value and
# where r = the reference value
##################################################################################


assembleConcat <- function(layer_field, raster, lookup, id_field, 
                         stackin_compare, stackin_compare_name, export_path, 
                         remapEVT_GP, EVT_GP_remap_table) {
  
  print(glue('assembleConcat: {layer_field}'))
  
  #print("make lookup table")
  #make lookup table
  lt <- cbind(lookup[id_field], lookup[layer_field])
  
  #print("make imp1")
  # make raster to compare
  imp1 <-  terra::classify(raster, lt) %>%
    terra::project(crs(stackin_compare)) %>%
    as.int()
  
  #print("get lf1")
  # get single lf raster
  lf1 <- lf[layer_field]
  
  # mask reference raster with input raster - necessary for testing on subset 
  lf1 <- terra::crop(lf1, imp1, mask = TRUE)
  
  # Conditionally remap EVT
  if(remapEVT_GP & layer_field == "EVT_GP") {
    
    # load remap table
    lt_evg <- EVT_GP_remap_table
    names(lt_evg) <- c("EVT_GP", "EVT_GP_remap")
    lt_evg %<>%
      select(EVT_GP_remap, EVT_GP)
    
    #remap
    lf1 <- terra::classify(lf1, lt_evg)
  } 
  
  #print("make diff")
  # get difference; set to NA where layers are the same
  diff <- imp1-as.int(lf1)
  diff %<>% terra::classify(cbind(0,NA))
  
  #print("get levels")
  # make both rasters categorical - get levels of layer field
  levels <- data.frame(id = sort(unique(lt[,2])),
                       levels = levels(as.factor(lt[,2])))
  
  #print("set levels")
  # set levels for rasters to make them categorical
  levels(imp1) <- levels
  levels(lf1) <- levels
  
  #print("concat")
  # concat and mask with difference
  out1 <- terra::concats(imp1, lf1)
  out1 %<>% terra::mask(diff)
  
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
  
  #export
  writeRaster(out1, 
              glue('{export_path}_{layer_field}_v{stackin_compare_name}.tif'),
              overwrite = TRUE)
  rm(out1)
  gc()
  
  
}

