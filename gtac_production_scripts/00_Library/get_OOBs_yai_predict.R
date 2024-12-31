# Function to get out-of-bag cn predictions from a given yai object
# returns a list of reference (ref) IDs and predicted (pred) IDs

library(docstring)

get_OOBs_yai_predict <- function(yai) {
  
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

  # get new predictions
  predict_yai <- predict(yai) 
  
  # update row names so we can use these as new target data
  maxv <- max(as.numeric(rownames(predict_yai)))
  nrow <- nrow(predict_yai)
  new_names <- seq(from = maxv+1, to = maxv+nrow, by = 1)
  rownames(predict_yai) <- new_names
  
  # get only predicted values
  # fields with "o" = observed values; remove these fields
  predict_yai %<>%
    select(sort(names(predict_yai))) %>%
    select(-matches(".o$")) #%>%
    #rename_with(~str_remove(., "[.]o$")) 
  
  # get new targets based on these predictions
  # takes some time to run
  newt <- newtargets(yai, predict_yai)
  
  out.neiIds <- newt$neiIdsTrgs # a matrix of reference identifications that correspond to neiDstTrgs (distances between target and ref).
  
  #### Format outputs into imputed ids
  yrows <- as.numeric(out.neiIds[,1]) 
  
  # bind with original
  out <- data.frame(cbind(as.numeric(rownames(yai$xall)), 
                          as.numeric(yrows)))
  names(out) <- c("ref_id", "pred_id")
  
  # how many imputed ids are different? 
  out_diff <- out %>%
    mutate(diff = ref_id - pred_id) %>%
    filter(diff != 0)
  
  # difference in IDs from imputed IDs to original ids
  pct_diff = nrow(out_diff)/nrow(out) * 100
  
  print("Percent of imputed ids that are different from reference ids:")
  print(pct_diff)
  
  return(out)
}