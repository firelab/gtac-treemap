# Function to get out-of-bag cn predictions from a given yai object
# returns a list of reference (ref) IDs, predicted (pred) IDs, tree number (1-ntrees in mode),
# and the # of times that ref was included in-bag
# so, where inbag_count = 0, that was an out-of-bag observation for that tree

library(docstring)

get_OOBs_yai <- function(yai) {
  
  #' Get out-of-bag observations from a yai-impute Random Forest model
  #'
  #' @param yai model created by the `yaImpute` function, using the Dev option oob = TRUE
  #'
  #' @return table of observed vs predicted IDs
  #' @export
  #'
  #' @examples 
  
  require(yaImpute)
  require(tidyverse)
  require(magrittr)
  
  # Set basic inputs
  #---------------------------------#
  ranForest <- yai$ranForest
  xall <- yai$xall
  yall=na.omit(as.data.frame(yai$yRefs))
  refs=intersect(rownames(yall),rownames(xall))
  xRefs=xall[refs,,drop=FALSE]
  
  oob_preds <- NULL
  # nodes <- NULL
  # inbagc <- NULL
  
  # Get a) terminal node predictions and b) out-of-bag counts from each RF model
  #--------------------------------------------#
  for (i in 1:length(ranForest)) {
    
    rf = ranForest[[i]]
    
    # get terminal node predictions
    nodeset=attr(predict(rf,
                         newdata = xall,
                         proximity=FALSE,nodes=TRUE, ),"nodes")
    if (is.null(nodeset)) stop("randomForest did not return nodes")
    colnames(nodeset)=paste(colnames(nodeset),i,sep=".")
    #nodes=if (is.null(nodes)) nodeset else cbind(nodes,nodeset)
    #rm(nodeset)
    nodes= nodeset
    
    # get in-bag / out of bag counts
    inbagset=rf$inbag
    #inbagc = if (is.null(inbagc)) inbagset else cbind(inbagc, inbagset)
    #rm(inbagset)
    inbagc = inbagset
  
    # Pull the data together
    #-------------------------------------#
    
    # set attributes of nodes
    refNodes = nodes[rownames(xRefs),]
    
    # set attributes of xRefs to be able to join later
    xRefs$ID <- as.numeric(row.names(xRefs))
    
    ####refNodes = refNodes[c(1:6),] # TO TEST
    
    # get dimensions of table
    INTnrow = as.integer(nrow(refNodes))
    INTncol = as.integer(ncol(nodes))
    
    # convert node index to integer
    INTrefNodes=as.integer(refNodes)
    
    # crosswalk node index to xRefs table
    tNodes = xRefs[INTrefNodes, "ID"]
    
    # convert back to matrix
    dim(tNodes) = c(INTnrow, INTncol)
    
    #str(tNodes) # inspect
    
    # set row names
    rownames(tNodes) <- rownames(refNodes)
    
    # combine inbag counts with node predictions
    #-----------------------------------------------#
    
    # convert tnodes to long data frame
    tNodes %<>% data.frame() %>%
      cbind(ref = row.names(tNodes)) %>%
      pivot_longer(-ref, 
                   names_to = "tree_num", 
                   values_to = "pred") %>%
      dplyr::mutate(ref = as.numeric(ref), 
                    tree_num = as.numeric(gsub("X", "", tree_num)))
    
    # convert in bag to long data frame
    inbagc %<>% data.frame() %>%
      cbind(ref = row.names(inbagc)) %>%
      pivot_longer(-ref, 
                   names_to = "tree_num",
                   values_to = "inbag_count") %>% 
      mutate(ref = as.numeric(ref), 
             tree_num = as.numeric(gsub("X", "", tree_num)) )
    
    # join to get all predicted ids for each reference for each tree, with inbag_count
    pred_ref_oob_all <- left_join(tNodes, inbagc, by = c("ref", "tree_num")) %>%
      dplyr::select(tree_num, ref, pred, inbag_count)
    
    # get the mode of the pred oob observations for each reference id
    # or some other way to compute which pred is "nearest" -- will have to snag from yai source code
    pred_ref_oob <- pred_ref_oob_all %>%
      filter(inbag_count <1) %>%
      group_by(ref) %>%
      summarize(pred = mode(pred)) %>%
      mutate(forest = i)
    
    gc() #
    
    # bind to outputs from other forests
    oob_preds =if (is.null(oob_preds)) pred_ref_oob else rbind(oob_preds, pred_ref_oob)
    
  } # end loop over forest
  
  return(oob_preds)

}

# how we can check is if the oob preds match the results from the oob eval 
