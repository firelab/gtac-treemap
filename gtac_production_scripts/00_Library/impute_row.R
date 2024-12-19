# Function to make new imputation predictions given a raster input
# This function is applied on a DATA FRAME (representing one row of raster) INSTEAD OF RASTER

library(docstring)

impute_row <- function(dat, yai, test = FALSE)  { 
  
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
  
  # Manage inputs
  #---------------------------------------#
  
  #give dat the name we use in this function
  extract.currow <- data.frame(dat)
  
  
  #### Get dimensions of current row
  colseq <- 1:length(extract.currow[,1])
  valid.cols <- colseq[as.logical(1-is.na(extract.currow[,1]))]
  ncols.df <- dim(extract.currow)[2]
  
  # Remove NAs
  extract.currow <- na.exclude(extract.currow)
  
  # convert to data frame
  X.df.temp <- data.frame(extract.currow)
  
  # Set up template for output of imputation
  nrows.orig <- dim(extract.currow)[1] # number of values to impute - without dummy rows for non-appearing evgs
  nrow.temp <- dim(X.df.temp)[1] # number of values to impute - with dummy rows for non-appearing evs
  nc.orig <-length(colseq) # number of values in row, including NAs
  
  # Default output from imputation - all NAs 
  impute.out <- rep(NA,nc.orig) 
  
  # CHECK FOR NA VALUES
  #------------------------#
  if(nrow.temp > 0) { # if there are any non-NA pixels in the row we're imputing
    
    # Get data from yai
    #-----------------------------------------------#
    id.table <- as.numeric(row.names(yai$xRefs))
    maxrow <- max(id.table)
    
    # EVG handling - 
    #### Identify EVGs in zone that don't appear in X.df.tmp   
    #-------------------------------------------------------#
    evg.orig <- levels(yai$xRefs$evt_gp_remap)
    evg.val.temp <- X.df.temp$evt_gp_remap  
    n.evgs.orig <- length(sort(unique(evg.orig)))  
    
    nonappearing.evgs <- evg.orig[-sort(unique(as.numeric(as.character(evg.val.temp))))]  
    n.dummy.rows <- length(nonappearing.evgs)  
    
    # Create dummy rows for non-appearing EVGs
    # Question: are dummy rows necessary? 
    if(n.dummy.rows > 0) {      
      dummy.rows <- X.df.temp[1:n.dummy.rows,]    
      tempchar <- as.character(X.df.temp$evt_gp_remap)    
      X.df.temp$evt_gp_remap <- tempchar    
      dummy.rows$evt_gp_remap <- as.character(nonappearing.evgs) 
      dummy.rows$disturb_code_bin <- rep(0, n.dummy.rows) # make sure there's disturb code in the dummy rows
      X.df.temp <- rbind(X.df.temp, dummy.rows)    
    }
    
    # Set factor levels
    #make sure they match input factor levels in reference data used in model
    #-------------------------------------------------------#
    X.df.temp <- 
      X.df.temp %>%
      dplyr::mutate(evt_gp_remap = factor(evt_gp_remap, levels = levels(yai$xRefs$evt_gp_remap)),
                    disturb_code_bin = factor(disturb_code_bin, levels = levels(yai$xRefs$disturb_code_bin))) %>%
      # put columns in order expected
      dplyr::select(names(yai$xRefs))
    
    # Run imputation
    #--------------------------------------------------------#
    
    # Option for TESTING  - skip imputation
    if(test == TRUE){
      # test output - simple extract the same size and format as impute.row
      test.out.tmp <- as.numeric(unlist(extract.currow[2]))
      test.out <- impute.out
      test.out[valid.cols] <- test.out.tmp
      
      impute.out <- test.out
    }
    
    else{
      # Format row names for X.df.temp - cannot overlap with input X.df
      colseq.out <- 1:dim(X.df.temp)[1]
      rownames.all <- colseq.out+maxrow
      rownames(X.df.temp) <- paste0("T- ", rownames.all)
      
      ### Perform imputation
      # take object from already-made yaImpute model and use X.df.temp dataframe to make predictions
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
  
  # return at end of function
  impute.out
  
}