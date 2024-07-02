# Function to make new imputation predictions given a raster input
# This function is applied on a DATA FRAME (representing one row of raster) INSTEAD OF RASTER

impute_row <- function(dat, yai, test)  { 
  
  require(yaImpute)
  require(dplyr)
  
  # #for testing
  #currow <- 400
  #yai <- yai.treelist.bin
  #ras <- ras
  #test <- FALSE
  
  # Manage inputs
  #---------------------------------------#
  
  #give dat the name we use in this function
  extract.currow <- data.frame(dat)
  
  # handle missing test param
  if(missing(test)) {
    test <- FALSE
  }
  
  # # handle a wrapped raster as input
  # if(is(ras) == "PackedSpatRaster") {
  #   ras <- terra::unwrap(ras)}
  
  
  # # Get data from raster
  # #------------------------------------------------#
  # 
  # #### Get dimensions of input raster stack
  # nrows.out <- dim(ras)[1]
  # ncols.out <- dim(ras)[2]
  # 
  # # get cell numbers and raster values for current row
  # rsvals <- terra::cellFromRowCol(ras, row = currow, col = 1:ncols.out)
  # rsmat <- ras[rsvals]
  # extract.currow <- data.frame(rsmat)
  # 
  # #### Get coordinates from current row of raster
  # xycoords <- terra::xyFromCell(ras, rsvals)
  # xycoords <- data.frame(xycoords)
  
  # #### Get coords of current row
  # extract.currow$POINT_X <- xycoords$x
  # extract.currow$POINT_Y <-xycoords$y
  
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
    #### Identify EVGs in zone that don't appear in X.df   
    #-------------------------------------------------------#
    evg.orig <- levels(yai$xRefs$EVT_GP)
    evg.val.temp <- X.df.temp$EVT_GP  
    n.evgs.orig <- length(sort(unique(evg.orig)))  
    
    nonappearing.evgs <- evg.orig[-sort(unique(as.numeric(as.character(evg.val.temp))))]  
    n.dummy.rows <- length(nonappearing.evgs)  
    
    # Create dummy rows for non-appearing EVGs
    # Question: are dummy rows necessary? 
    if(n.dummy.rows > 0)
    {    
      dummy.rows <- X.df.temp[1:n.dummy.rows,]    
      tempchar <- as.character(X.df.temp$EVT_GP)    
      X.df.temp$EVT_GP <- tempchar    
      dummy.rows$EVT_GP <- as.character(nonappearing.evgs) 
      dummy.rows$disturb_code <- rep(0, n.dummy.rows) # make sure there's disturb code in the dummy rows
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