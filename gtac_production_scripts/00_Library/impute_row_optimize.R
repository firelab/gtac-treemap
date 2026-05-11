library(docstring)

impute_row_optimize <- function(dat, yai) {

  #' Function to make new imputation predictions given a data frame input
  #' 
  #' @param dat The data frame, for TreeMap, represents one row of a multi-layer raster
  #' Where each column in the data frame represents all values in the row for a given layer of the raster. Each column represents a different layer. 
  #' @param yai model created by the `yaImpute` function
  #' @param test default FALSE. test = TRUE skips the imputation portion and returns a data frame of the input ids
  #' @return data frame of imputed ids in the same shape and size as the input `dat`
  #' @export
  #'
  #' @examples 
  require(yaImpute)
  require(dplyr)
  require(tidyverse)
  require(magrittr)
  
  # Identify valid pixels
  # Check the first column for NA's
  valid_cols<- which(!is.na(dat[[1]]))
  
  # Initialize imputed ID output with NA's
  impute_out<- rep(NA_integer_, nrow(dat))
  
  # Exit if the whole row is NA
  if (length(valid_cols) == 0) return(impute_out)
  
  # Subset dataframe to valid data only
  X_temp <- dat[valid_cols, names(yai$xRefs), drop = FALSE]
  
  # Set factor levels
  #make sure they match input factor levels in reference data used in model
  #-------------------------------------------------------#
  X_temp <- 
    X_temp %>%
    dplyr::mutate(evt_gp_remap = factor(evt_gp_remap, levels = levels(yai$xRefs$evt_gp_remap)),
                  disturb_code_bin = factor(disturb_code_bin, levels = levels(yai$xRefs$disturb_code_bin))) %>%
    # put columns in order expected
    dplyr::select(names(yai$xRefs))
  
  
  # Change row names so they don't match rownames of references used to build the model
  attr(X_temp, "row.names") <- as.integer(seq_len(nrow(X_temp)) + 1000000)
  
  
  # Run imputation on the data
  temp_newtargs <- yaImpute::newtargets(yai, newdata = X_temp)
  
  # Extract imputed ID
  yrows <- as.numeric(temp_newtargs$neiIdsTrgs[,1])
  
  impute_out[valid_cols] <- yrows
  #
  return(impute_out)
}
