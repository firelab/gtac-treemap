impute_row_optimize <- function(dat, yai) {
  
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
