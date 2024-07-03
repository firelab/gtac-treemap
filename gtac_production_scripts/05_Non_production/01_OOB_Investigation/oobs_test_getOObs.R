# possible sources: 

# https://stackoverflow.com/questions/14676810/extracting-the-terminal-nodes-of-each-tree-associated-with-a-new-observation

# load library 
this_proj = this.path::this.proj()
lib_path = glue::glue('{this_proj}/gtac_production_scripts/00_Library/treeMapLib.R')
source(lib_path)


yai <- readRDS("//166.2.126.25/TreeMap/03_Outputs/07_Projects/2016_GTAC_Test/01_Raw_model_outputs/z16/model/z16_2016_GTAC_Test_ntree250_yai_treelist_bin.RDS")

xtable <- read.csv("//166.2.126.25/TreeMap/03_Outputs/07_Projects/2016_GTAC_Test/01_Raw_model_outputs/z16/xytables/z16_2016_Orig_Test_keepinbag_ntree250_Xdf_bin.csv")

oobs_default <- get_OOBs_yai(yai)

# Function to get out-of-bag cn predictions from a given yai object
# returns a list of reference (ref) IDs, predicted (pred) IDs, tree number (1-ntrees in mode),
# and the # of times that ref was included in-bag
# so, where inbag_count = 0, that was an out-of-bag observation for that tree

#library(docstring)

#' get_OOBs_yai <- function(yai) {
#'   
#'   #' Get out-of-bag observations from a yai-impute Random Forest model
#'   #'
#'   #' @param yai model created by the `yaImpute` function, using the Dev option oob = TRUE
#'   #'
#'   #' @return table of predicted vs observed
#'   #' @export
#'   #'
#'   #' @examples 
#'   
#'   require(yaImpute)
#'   require(tidyverse)
#'   require(magrittr)
  
  # Set basic inputs
  #---------------------------------#
  ranForest <- yai$ranForest
  xall = yai$xall
  yall=na.omit(as.data.frame(yai$yRefs))
  refs=intersect(rownames(yall),rownames(xall))
  xRefs=xall[refs,,drop=FALSE]
  
  oob_preds <- NULL
  
  
  # Get a) terminal node predictions and b) out-of-bag counts from each RF model and return c) mode of imputed id for each reference for each tree, based on OOBs
  #--------------------------------------------#
  for (i in 1:length(ranForest)) {
    
    i = 1
    
    rf = ranForest[[i]]
    
    
    # get index of terminal nodes in each tree
    nodeset=attr(predict(rf,
                         newdata = xall,
                         predict.all = TRUE,
                         proximity=FALSE,nodes=TRUE, ),"nodes")
    if (is.null(nodeset)) stop("randomForest did not return nodes")
    colnames(nodeset)=paste(colnames(nodeset),i,sep=".")
    nodes= nodeset
    
    # get in-bag / out of bag counts
    #in-bag counts = a vector of how many times each observation was used in a tree
    inbagset=rf$inbag
    inbagc = inbagset
  
    # Pull the data together
    #-------------------------------------#
  
    # link node info with xRef ids
    refNodes = nodes[rownames(xRefs),]
  
    # set attributes of xRefs to be able to join later
    xRefs$ID <- as.numeric(row.names(xRefs))
  
    # TEST OPTION
    #refNodes = refNodes[c(1:6),] # TO TEST - on just 6 obs
  
    # get dimensions of table
    INTnrow = as.integer(nrow(refNodes))
    INTncol = as.integer(ncol(nodes))
  
    # convert node index to integer
    INTrefNodes=as.integer(refNodes)
  
    # Create INTsort table - used in original RF call
    INTsort = INTrefNodes
    dim(INTsort) = c(INTnrow,INTncol)
    INTsort=apply(INTsort,2,function (x) sort(x,index.return = TRUE, 
                                              decreasing = FALSE)$ix-1)
    attributes(INTsort)=NULL
    INTsort = as.integer(INTsort)
    
    # crosswalk node index to xRefs table
    tNodes = xRefs[INTrefNodes, "ID"]
  
    # convert back to matrix
    dim(tNodes) = c(INTnrow, INTncol)
  
    str(tNodes) # inspect
  
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
      summarize(pred = mode(pred))
  
    gc() #
  
    # bind to outputs from other forests
    oob_preds =if (is.null(oob_preds)) pred_ref_oob else cbind(oob_preds, pred_ref_oob)
  
    }
  
  return(oob_preds)
  
#}




