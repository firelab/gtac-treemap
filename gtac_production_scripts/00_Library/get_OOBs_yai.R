### Function to get out-of-bag predictions and counts for all trees in a yai model
# returns the row name, or index, of an input reference that is the nearest neighbor
# for eachreference prediction for each tree

get_OOBs_yai <- function(yai) {
  
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
  
  nodes <- NULL
  inbagc <- NULL
  
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
    nodes=if (is.null(nodes)) nodeset else cbind(nodes,nodeset)
    rm(nodeset)
    
    # get in-bag / out of bag counts
    inbagset=rf$inbag
    inbagc = if (is.null(inbagc)) inbagset else cbind(inbagc, inbagset)
    rm(inbagset)
  } 
  
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
  
  pred_ref_oob <- left_join(tNodes, inbagc, by = c("ref", "tree_num")) %>%
    dplyr::select(tree_num, ref, pred, inbag_count)
  
  return(pred_ref_oob)

}

