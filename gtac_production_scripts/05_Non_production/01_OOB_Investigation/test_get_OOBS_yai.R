# possible sources: 

# https://stackoverflow.com/questions/14676810/extracting-the-terminal-nodes-of-each-tree-associated-with-a-new-observation

#


# load library 
this_proj = this.path::this.proj()
lib_path = glue::glue('{this_proj}/gtac_production_scripts/00_Library/treeMapLib.R')
source(lib_path)


yai <- readRDS("//166.2.126.25/TreeMap/03_Outputs/07_Projects/2016_GTAC_Test/01_Raw_model_outputs/z16/model/z16_2016_GTAC_Test_ntree250_yai_treelist_bin.RDS")

oobs_default <- get_OOBs_yai

# get_OOBs_yai <- function(yai) {
  
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
    
    # for testing
    i = 1
    
    rf = ranForest[[i]]
    
    # get terminal node predictions
    #-----------------------------------#
    
    # make predictions on original x data and return nodes
    # Source: https://github.com/cran/yaImpute/blob/d75a5edbaa3855d0369de618b4ad52c3ab3bf0de/R/yai.R#L634
    
    pred = predict(rf, 
                   newdata = xall,
                   proximity = FALSE, 
                   nodes = TRUE)

    nodenames = attr(pred, "names")
    nodeset = attr(pred, "nodes")
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
  
  # set attributes of xRefs to be able to join later
  xRefs$ID <- as.numeric(row.names(xRefs))
  
  # set attributes of nodes
  refNodes = nodes[xRefs$ID,]
  
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
  
  gc() #
  
  #return(pred_ref_oob)
  
#}

