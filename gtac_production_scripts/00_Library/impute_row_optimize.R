impute_row <- function(dat, yai) {
  
  # Identify valid pixels
  # Check the first column for NA's
  valid_cols<- which(!is.na(dat[[1]]))
  
  # Initialize imputed ID output with NA's
  impute_out<- rep(NA_integer_, nrow(dat))
  
  # Exit if the whole row is NA
  if (length(valid_cols) == 0) return(impute_out)
  
  # Subset dataframe to valid data only
  X_temp <- dat[valid_cols, names(yai$xRefs), drop = FALSE]
  
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


