# This library contains helper functions and dictionaries for TreeMap production scripts.

# Author: Lila Leatherman (lila.leatherman@usda.gov)

# Last Updated:
# 6/17/2024

#################################################################
# Load required packages
#################################################################

# packages required
list.of.packages <- c("glue", "this.path", "rprojroot", "terra", "tidyverse", 
                      "magrittr", "tictoc", "caret", "randomForest", 
                      "Metrics", "foreach", "doParallel", "yaImpute", "docstring",
                      "stringr", "stringi")

# Install dev version of yaImpute - to make sure we get the option to retain OOB obs
message("Installing dev version of yaImpute package")
devtools::install_github("https://github.com/jeffreyevans/yaImpute")

# #check for packages and install if needed
new.packages <- tryCatch(
  
  # try to get list of packages installed
  {suppressWarnings(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]) }, 
  
  error= function(cond) {
    message("Can't access list of packages")
    message("Here's the original error message:")
    message(conditionMessage(cond))
    
    # return value in case of error:
    NA
  }

  )
  
if(length(new.packages) > 0) install.packages(new.packages)

# load all packages
vapply(list.of.packages, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)

# remove unused objects
rm(list.of.packages, new.packages)

#########################################################################
# General options
#########################################################################

# Terra options
# --------------------------------#

#increase memory fraction available
terraOptions(memfrac = 0.8)

# Other options
# --------------------------------#

# Allow for sufficient digits to differentiate plot cn numbers
options("scipen" = 100, "digits" = 8)

###########################
# Run other scripts in this folder
###########################

# Id where script is located
this_path <- this.path::this.path()
this_dir <- this.path::this.dir()

# List other scripts in library
snames <- list.files(this_dir, full.names = TRUE, pattern = ".R$")
snames <- snames[snames != this_path]

# source other scripts in library
lapply(snames, source)

# remove unused objects
rm(this_path, this_dir, snames)

###########################
# Dictionaries
###########################


###########################
# Helper functions
###########################

# make 'notin' function
`%notin%` <- Negate('%in%')

# make a mode function
mode <- function(x, na.rm){
  
  if(missing(na.rm)) {
    na.rm = FALSE
  }
  
  if(na.rm == TRUE) {
    x <- x[!is.na(x)]
  }
  which.max(tabulate(x))
}

# which.max analog
# in the case of a tie, returns the index with the highest value
which.max.hightie <- function(x) {
  
  if(all(is.na(x))) {
    NA
  } else {
    z <- which(x==max(x, na.rm = TRUE))
    as.integer(z[length(z)])
  }
}

#####################################################
# Parallel processing
#####################################################
#Progress combine function
# use in foreach loop 
# ex. 

# # Run the loop in parallel
# k <- foreach(i = icount(n), .final=sum, .combine=f()) %dopar% {
#   log2(i)
# }

progress_foreach <- function(n){
  pb <- txtProgressBar(min=1, max=n-1,style=3)
  count <- 0
  function(...) {
    count <<- count + length(list(...)) - 1
    setTxtProgressBar(pb,count)
    Sys.sleep(0.01)
    flush.console()
    c(...)
  }
}


#####################################################
# Raster operations
#####################################################

# Load target rasters
#####################################################

load_target_rasters <- function(flist_tif, n) {
  
  require(terra)
  
  # Objective: Load target rasters from a list into a single raster object
  
  # handle missing n param
  if(missing(n)) {
    n <- NA
  }
  
  if(!is.na(n)) {
    flist_tif <- flist_tif[n]
  }
  
  rs2 <- terra::rast(flist_tif)
  
  # get raster layer names
  layer_names <- flist_tif %>%
    str_extract(., "z[0-9][0-9]/([^.])*") %>%
    str_replace("z[0-9][0-9]/", "") %>%
    str_remove("_LFLCMS") %>%
    str_remove("_LF")
  
  #add names to raster list
  names(rs2) <- layer_names
  
  return(rs2)
}

# Filter to disturbance rasters of interest

# filter to target rasters of interest
#####################################

filter_disturbance_rasters <- function(flist_tif, dist_layer_type){
  
  # get list of disturbance rasters
  flist_dist <- flist_tif %>%
    str_subset("disturb") %>%
    str_subset(glue::glue('{dist_layer_type}.tif'))
  
  # get list of all other rasters
  flist_nondist <- flist_tif %>%
    str_subset("disturb", negate = TRUE) 

# recombine lists
flist_tif <- c(flist_dist, flist_nondist) %>%
  sort()

return(flist_tif)

}

#########################################################################

fill_matrix_to_raster <- function(mout, ncol_r, nrow_r, row1) { 
  
  require(terra) 

  # make rows with NAs to make full raster of test 
  ncols.out <- ncol_r
  nrows.out <- nrow_r
  d <- rep(NA, ncols.out)
  blank_rows_top <- do.call("rbind", replicate(row1-1, d, simplify = FALSE))
  blank_rows_bottom <- do.call("rbind", replicate(nrows.out-nrow(mout), d, simplify = FALSE))
  
  # will the output raster, with blank rows, be the same size as the input raster?
  #identical(as.numeric(nrows.out), as.numeric(nrow(blank_rows_top) + nrow(mout) + nrow(blank_rows_bottom)))
  
  #bind test rows with NAs to make full raster
  tile_out <- terra::rast(rbind(blank_rows_top,
                                mout,
                                blank_rows_bottom))
  
  return(tile_out)

}


###########################
# Validation
###########################

# Get Predicted and Reference from a Random Forest object and the X table used to build it
###########################################################################################

get_pr_RF <- function(rf_in, X_df) {

  # get predictions from random forest model and convert to data frame
  preds <- rf_in[[1]]$predicted
  ID_col <- names(preds)
  preds_df <- data.frame(ID = ID_col, 
                         pred = as.numeric(preds))
  rownames(preds_df) <- NULL
  
  # get references from X_df
  refs_df <- X_df %>%
    dplyr::select(all_of(var)) %>%
    dplyr::rename(ref = `var`) %>%
    rownames_to_column("ID")
  
  # join refs and preds
  p_r <- full_join(preds_df, refs_df, by = c("ID")) %>%
    select(-ID)
  
  # if values are not identical, make a key
  
  if(!identical(sort(unique(p_r$pred)), sort(unique(p_r$ref)))) {
    
    # make a key for pred to ref
    #--------------------------------#
    
    # get reference vars
    key_r <- p_r %>%
      select(ref) %>%
      rename(key_r = ref) %>%
      distinct() %>%
      arrange(key_r)
    
    # get pred vars
    key_p <- p_r %>%
      select(pred) %>%
      rename(key_p = pred) %>%
      distinct() %>%
      arrange(key_p)
    
    # make sure pred vars are the same length as ref vars
    if(nrow(key_p) != nrow(key_r)) {
      
      diff = abs(nrow(key_p) - nrow(key_r))
      
      # add an NA for each missing 
      for(i in 1:diff){
        key_p = rbind(key_p, 'NA')
      }
      
    }
    
    # make sure preds are a factor if ref is a factor
    if(is.factor(key_r$key_r)) {
      p_r$pred <- factor(p_r$pred, levels = levels(key_r$key_r))
      key_p$key_p <- factor(key_p$key_p, levels = levels(key_r$key_r))
    }
    
    key <- cbind(key_p, key_r)
    
    # reclass preds to match refs
    p_r_out <- left_join(p_r, key, by = c("pred" = "key_p") ) %>%
      mutate(pred = key_r) %>%
      select(pred, ref)
    
    p_r <- p_r_out
    
  } 
  
  return(p_r)

}

###############################################################################

# Function for Evaluation - Confusion matrices 
# Takes a two-column data frame as an input
# with PREDICTED VALUES as column 1
# and REFERENCE VALUES as column 2
# Produces confusion matrix tables formatted the way I like them :)
########################################################################

eval_cm_function <- function(t, noDataVal) {
  
  #require(c(tidyverse, caret))
  
  # handle missing param
  if(missing(noDataVal)) {
    noDataVal <- NA
  }
  
  #apply column names
  names(t) <- c("pred", "ref")
  
  # set levels for factors
  # get maximum value of table that's not the noDataValue
  tn <- t
  tn[tn == noDataVal] <- NA
  
  # get levels - all levels that appear in each of pred and ref
  levels_t <- unique(c(as.numeric(unlist((unique(tn$pred)))),
                       as.numeric(unlist((unique(tn$ref))))))
  levels_t <- sort(levels_t)
  
  # ensure columns are factors with the same levels
  tn %<>%
    mutate(pred = factor(pred, levels = levels_t),
           ref = factor(ref, levels = levels_t)
    )  
  
  # Get confusion  matrix
  #---------------------------------------------#
  
  # confusion matrix
  cm <- caret::confusionMatrix(tn$pred, # pred
                               tn$ref # ref
  )
  
  # Process data frames for export
  #---------------------------------#
  
  # raw confusion matrix
  cm_raw_out <- as.table(cm)
  cm_raw_out <- addmargins(cm_raw_out)
  
  # make data frame of classes
  cm_t_classes <- data.frame(as.matrix(cm, what = "classes"))
  
  if(ncol(cm_t_classes) < 2) {
    names(cm_t_classes) <- "1"
  } else {
    names(cm_t_classes) <- levels_t 
  }
  cm_t_classes %<>% 
    rownames_to_column(., var = 'metric')
  
  # overall eval stats
  cm_t_overall <- data.frame(as.matrix(cm, what = "overall"))
  names(cm_t_overall) <- c("value")
  cm_t_overall$metric <- rownames(cm_t_overall)
  
  # calculate frequency table
  tfreq <- 
    cbind(table(tn$pred),
          table(tn$ref)
    ) %>%
    data.frame()
  
  # manipulate frequency table
  names(tfreq) <- c("pred", "ref")
  tfreq$class <- factor(rownames(tfreq), levels = levels_t)
  rownames(tfreq) <- NULL
  
  #Calculate normalized frequency table also
  #----------------------------------------------#

  # calc total to use in normalizing
  total_pred = sum(tfreq$pred)
  total_ref = sum(tfreq$ref)
  
  # add normalized frequency
  tfreq_norm <- tfreq %>%
    mutate(pred = pred/total_pred, 
           ref = ref/total_ref)
  
  # format output
  # ---------------------------- #
  out_list <- list(cm_raw_out,
                   cm_t_classes,
                   cm_t_overall,
                   tfreq,
                   tfreq_norm)
  
  names(out_list) <- c("raw", "classes", "overall", "freq", "freq_norm")
  
  return(out_list)
  
}

################################################################################
# Write function to reclass rasters from id field to variable field, and export
##################################################################

assembleExport <- function(layer_field, raster, lookup, id_field, export_path) {
  
  require(tictoc)
  
  tic()
  
  print(glue('assembleExport: {layer_field}'))
  lt <- cbind(lookup[id_field], lookup[layer_field])
  #print(head(lt))
  rout <- terra::classify(raster, lt)
  writeRaster(rout,
              glue('{export_path}_{layer_field}.tif'),
              overwrite = TRUE)
  rm(rout)
  toc()
  gc()
  
}

########################################################################
# Returns a confusion matrix of lookup raster vs. reference raster
###############################################################################

assembleCM <- function(layer_field, raster, lookup, id_field, 
                       stackin_compare, stackin_compare_name,  
                       remapEVT_GP, EVT_GP_remap_table,
                       exportTF, export_path) {
  
  print(glue::glue("assembleCM: {layer_field}"))
  print(glue::glue("export?: {exportTF}"))
  
  #print("make lookup table")
  #make lookup table
  lt <- cbind(lookup[id_field], lookup[layer_field])
  
  #print("make imp1")
  # make raster to compare
  imp1 <-  terra::classify(raster, lt) 
  
  # if(!identical(crs(imp1), crs(stackin_compare))) {
  #   imp1 %<>% terra::project(crs(stackin_compare))
  # }
  
  if(exportTF) {
    writeRaster(imp1,
                glue::glue("{export_path}_{layer_field}.tif"),
                overwrite = TRUE)
  }
  
  gc()
  
  #print("get lf1")
  # get single lf raster
  lf1 <- stackin_compare[layer_field]
  lf1 %<>% terra::trim() # remove NA values on borders
  
  # crop and mask reference raster with input raster
  # necessary for testing on subset 
  if(!compareGeom(lf1, imp1, ext = TRUE)) { # if the extents don't match
    lf1 <- terra::crop(lf1, imp1, mask = TRUE) # crop and mask 
  }
  
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

  gc()
  
  #print("get levels")
  # make both rasters categorical - get levels of layer field
  levels <- data.frame(id = sort(unique(lt[,2])),
                       levels = levels(as.factor(lt[,2])))
  
  # #print("set levels")
  # # set levels for rasters to make them categorical
  # levels(imp1) <- levels
  # levels(lf1) <- levels
  
  # Calculate confusion matrix
  #--------------------------------------#
  
  #print("calculating and exporting confusion matrix")
  t <- data.frame(cbind(terra::values(imp1),
                        terra::values(lf1)))
  
  #update names
  names(t) <- c("pred", "ref")
  
  # replace any NaN with NA
  t %<>% mutate_all(~ifelse(is.nan(.), NA, .))
  
  # remove rows that are only na
  t %<>% drop_na()
  
  # calculate cms 
  cms <- eval_cm_function(t, NA)
  
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



