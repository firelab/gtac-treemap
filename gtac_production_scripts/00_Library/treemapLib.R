# This library contains helper functions and dictionaries for TreeMap production scripts.

# Author: Lila Leatherman (lila.leatherman@usda.gov)

# Last Updated:


#################################################################
# Load required packages
#################################################################

# packages required
list.of.packages <- c("terra", "tidyverse", "magrittr", 
                      "glue", "tictoc", "caret")

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
# Dictionaries
###########################


###########################
# Imputation
###########################



impute.row <- function(currow, yai, ras, test)  { 
  
  # #for testing
  # currow <- 75
  # yai <- yai.treelist.bin
  # ras <- ras
  # test <- FALSE
  
  # Manage inputs
  #---------------------------------------#
  
  # handle missing test param
  if(missing(test)) {
    test <- FALSE
  }
  
  # handle a wrapped raster as input
  if(is(ras) == "PackedSpatRaster") {
    ras <- terra::unwrap(ras)}
  
  # Get data from yai
  id.table <- as.numeric(row.names(yai$xRefs))
  maxrow <- max(id.table)
  
  # Get data from raster
  #------------------------------------------------#
  
  #### Get dimensions of input raster stack
  nrows.out <- dim(ras)[1]
  ncols.out <- dim(ras)[2]
  
  # get cell numbers and raster values for current row
  rsvals <- terra::cellFromRowCol(ras, row = currow, col = 1:ncols.out)
  rsmat <- ras[rsvals]
  extract.currow <- data.frame(rsmat)
  
  #### Get coordinates from current row of raster
  xycoords <- terra::xyFromCell(ras, rsvals)
  xycoords <- data.frame(xycoords)
  
  #### Get dimensions of current row
  colseq <- 1:length(extract.currow[,1])
  valid.cols <- colseq[as.logical(1-is.na(extract.currow[,1]))]
  ncols.df <- dim(extract.currow)[2]
  
  #### Get coords of current row
  extract.currow$POINT_X <- xycoords$x
  extract.currow$POINT_Y <-xycoords$y
  
  # Remove NAs
  extract.currow <- na.exclude(extract.currow)
  
  # convert to data frame
  X.df.temp <- data.frame(extract.currow)
  
  # Set up template for output of imputation
  nrows.orig <- dim(extract.currow)[1] # number of values to impute - without dummy rows for non-appearing evgs
  nrow.temp <- dim(X.df.temp)[1] # number of values to impute - with dummy rows for non-appearing evs
  nc.orig <-length(rsvals) # number of values in row, including NAs
  
  # Default output from imputation - all NAs 
  impute.out <- rep(NA,nc.orig) 
  
  # CHECK FOR NA VALUES
  #------------------------#
  if(nrow.temp > 0) { # if there are any non-NA pixels in the row we're imputing
    
    #### Convert aspect to northing and easting
    #------------------------------------------------------#
    X.df.temp <-
      X.df.temp %>%
      dplyr::mutate(radians = (pi/180)*ASPECT,           
                    NORTHING = cos(radians),
                    EASTING = sin(radians)) %>%
      dplyr::select(-radians)
    
    # EVG handling - 
    #### Identify EVGs in zone that don't appear in X.df   
    #-------------------------------------------------------#
    evg.orig <- levels(yai$xRefs$EVT_GP)
    evg.val.temp <- X.df.temp$EVT_GP  
    n.evgs.orig <- length(sort(unique(evg.orig)))  
    evg.orig.seq <- 1:n.evgs.orig  
    
    nonappearing.evgs <- evg.orig[-sort(unique(as.numeric(as.character(evg.val.temp))))]  
    n.dummy.rows <- length(nonappearing.evgs)  
    
    # Create dummy rows for non-appearing EVGs
    if(n.dummy.rows > 0)
    {    
      dummy.rows <- X.df.temp[1:n.dummy.rows,]    
      tempchar <- as.character(X.df.temp$EVT_GP)    
      X.df.temp$EVT_GP <- tempchar    
      dummy.rows$EVT_GP <- as.character(nonappearing.evgs)    
      X.df.temp <- rbind(X.df.temp, dummy.rows)    
    }
    
    # Set factor levels
    #make sure they match input factor levels in reference data used in model
    #-------------------------------------------------------#
    X.df.temp <- 
      X.df.temp %>%
      dplyr::mutate(EVT_GP = factor(EVT_GP, levels = levels(yai$xRefs$EVT_GP)),
                    disturb_code = factor(disturb_code, levels = levels(yai$xRefs$disturb_code))) %>%
      # put columns in order expected
      dplyr::select(names(yai$xRefs))
    
    # Run imputation
    #--------------------------------------------------------#
    
    # Option for TESTING  - skip imputation
    if(test == TRUE){
      # test output - simple extract the same size ans format as impute.row
      test.out.tmp <- as.numeric(unlist(extract.currow[2]))
      test.out <- impute.out
      test.out[valid.cols] <- test.out.tmp
      
      return(test.out)
    }
    
    else{
      # Format row names for X.df.temp - cannot overlap with input X.df
      colseq.out <- 1:dim(X.df.temp)[1]
      rownames.all <- colseq.out+maxrow
      rownames(X.df.temp) <- paste0("T- ", rownames.all)
      
      ### Perform imputation
      # take object from formed random forests model and use X.df.temp dataframe to make predictions
      temp.newtargs <- yaImpute::newtargets(yai, newdata = X.df.temp)
      
      #### Get outputs of interest
      #out.trgrows <- temp.newtargs$trgRows # row names for target observations
      #temp.xall <- temp.newtargs$xall # x-variables (predictors) for all observations
      out.neiIds <- temp.newtargs$neiIdsTrgs # a matrix of reference identifications that correspond to neiDstTrgs (distances between target and ref).
      
      #### Format outputs into imputation results
      yrows <- as.numeric(out.neiIds[,1]) # get list of plotIds; rowname = rowname from X.df.temp - corresponds to cell
      #id.out <- id.table[yrows] # subset id table to only the ids that appear in the output
      impute.out[valid.cols] <- yrows[1:nrows.orig] # for each valid column: match it with the output row from imputation
      
      # garbage collection
      gc()
    }
  }
  
  return(impute.out)
  
}


# add temp file / tile assemblage - make a raster with 
# desired extent from an input that's just a few rows

###########################
# Validation
###########################

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

# Write function to reclass rasters from id field to variable field, and export
##################################################################

# maybe call this "assembleExport"
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


# Function to lookup variable by id from lookup table,
# and concat against variable with the same name in another raster stack (stackin_compare)
# Produces a concat raster with categorical values formatted like p_r
# where p = the predicted value and
# where r = the reference value
#   Can also output confusion matrix of lookup raster vs. reference raster
##################################################################################

#maybe call this... "assembleValidation"

assembleConcat <- function(layer_field, raster, lookup, id_field, 
                         stackin_compare, stackin_compare_name, export_path, 
                         cm, remapEVT_GP, EVT_GP_remap_table) {
  
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
  lf1 <- terra::mask(lf1, imp1)
  
  # Conditionally remap EVT
  if(remapEVT_GP) {
    
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
  
  # conditionally calculate confusion matrix
  if(isTRUE(cm)){
    
    print("calculating and exporting confusion matrix")
    t <- data.frame(pred = terra::values(imp1),
                    ref = terra::values(lf1))
    
    # calculate cms 
    cms<- eval_cm_function(t, NA)
    
    #export confusion matrices to given path
    write.csv(cms$raw, 
              glue('{export_path}_{layer_field}_v{stackin_compare_name}_cmRaw.csv'),
              row.names = TRUE) # row names list classes on y axis
    write.csv(cms$classes, 
              glue('{export_path}_{layer_field}_v{stackin_compare_name}_cmClasses.csv'),
              row.names = FALSE)
    write.csv(cms$overall, 
              glue('{export_path}_{layer_field}_v{stackin_compare_name}_cmOverall.csv'),
              row.names = FALSE)
    write.csv(cms$freq,
             glue('{export_path}_{layer_field}_v{stackin_compare_name}_cmFreq.csv'),
             row.names=FALSE)
  
    rm(t)
  }
  rm(imp1, lf1)
  gc()
  
}