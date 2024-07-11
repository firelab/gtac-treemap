# possible sources: 

# https://stackoverflow.com/questions/14676810/extracting-the-terminal-nodes-of-each-tree-associated-with-a-new-observation

#https://stackoverflow.com/questions/60729974/randomforest-meaning-of-predicted-component-and-its-relation-to-out-of-bag-er

#

# load library 
this_proj = this.path::this.proj()
lib_path = glue::glue('{this_proj}/gtac_production_scripts/00_Library/treeMapLib.R')
source(lib_path)


yai <- readRDS("//166.2.126.25/TreeMap/03_Outputs/07_Projects/2016_GTAC_Test/01_Raw_model_outputs/z16/model/z16_2016_GTAC_Test_yai_treelist_bin.RDS")

xtable <- read.csv("//166.2.126.25/TreeMap/03_Outputs/07_Projects/2016_GTAC_Test/01_Raw_model_outputs/z16/xytables/z16_2016_GTAC_Test_Xdf_bin.csv")


# list vars used for RF models
y_vars <- c("canopy_cover", "canopy_height", "EVT_GP", "disturb_code")

# get out of bag predictions from the RF models for each var
oob_preds1 <- data.frame(cbind(
  predict(yai$ranForest$canopy_cover),
  predict(yai$ranForest$canopy_height),
  predict(yai$ranForest$EVT_GP),
  predict(yai$ranForest$disturb_code)
          ))

oob_preds <- NULL

for(i in seq_along(y_vars)) {
  
  #i = 1
  
  var <- y_vars[i]
  
  rf <- yai$ranForest[var]
  
  p <- data.frame(predict(rf))
  
  names(p) <- var
  
  if(is.null(oob_preds)) {
    oob_preds <- p
  } else {
    oob_preds <- cbind(oob_preds, p)
  }
  
  
}



# Function to remap factor levels, based on an input variable name, input df with levels of desired variables, and input df that contains the var with the levels we want to remap 
remap_levels_var <- function(var_name, xdf, join_df) {
  
  # # for testing 
  # var_name = "canopy_cover"
  # xdf = yai$xall
  # join_df = oob_preds
  
  
  # reclass back to factor levels
  #------------------------------#
  
  # get original levels
  levels <-unlist(xdf[var_name])
  levels <- levels(as.factor(levels))
  
  # make reclass matrix
  rcl_var <- data.frame(cbind(as.numeric(levels),
                              seq(1:length(levels))))
  names(rcl_var) <- c("orig", var_name)
  
  # join
  join_out <- join_df %>%
    dplyr::select(any_of(var_name)) %>%
    dplyr::mutate_all(as.numeric) %>%
    left_join(rcl_var) %>%
    dplyr::select(-any_of(var_name)) %>%
    dplyr::mutate(!!var_name := orig) %>%
    dplyr::select(!!var_name)
  
  return(join_out)
  
}

# apply remap levels function
for(i in seq_along(y_vars)) {

  var_name <- y_vars[i]

  oob_preds[var_name] <- remap_levels_var(var_name, yai$xall, oob_preds)

}


###################################
oob_preds %<>%
  mutate(disturb_code = factor(disturb_code),
         EVT_GP = factor(EVT_GP),
         ID = row.names(oob_preds)) %>%
  select(c(ID, any_of(y_vars)))

#row.names(oob_preds) <- NULL

# keep id for this application
predict_yai <- predict(yai)
predict_yai$ID <- row.names(predict_yai)

# get only predicted values
predict_yai %<>%
  select(sort(names(predict_yai))) %>%
  select(-matches(".o$"))
  #select(c(ID, matches(".o$"))) %>% rename_with(~str_remove(., "[.]o$")) # OTHER OPTION - THIS GETS REFERENCE OBS

# update row names so we can use these as new target data
maxv <- max(as.numeric(rownames(predict_yai)))
nrow <- nrow(predict_yai)
new_names <- seq(from = maxv+1, to = maxv+nrow, by = 1)
rownames(predict_yai) <- new_names

#############################################################################
# are these predictions the same as those derived from impute(yai)? 
# IF SO, that confirms that impute(yai) returns oob predictions
# ANSWER: YES, SAME AS THE ".o" FIELDS
####################################################################

# get only predicted values
predict_yai_1 <- predict_yai %>%
  select(sort(names(predict_yai))) %>%
  select(c(ID, matches(paste(y_vars, collapse = "|")))) %>%
  select(c(ID, all_of(y_vars))) %>% # order the fields
  data.frame()
  
  

# inspect if they're different - visually
head(cbind(oob_preds, predict_yai_1))
identical(head(oob_preds), head(predict_yai_1))
identical(oob_preds, predict_yai_1)
str(oob_preds)
str(predict_yai_1)

######################################
# inspect these data sets - using confusion matrix
for(i in seq_along(y_vars)) {
  
  #i = 1
  var <- y_vars[i]
  print(var)
  print(
  confusionMatrix(factor(oob_preds[,var]), factor(predict_yai_1[,var]))
  )
}

###################################

# USE NEW/OOB PREDICTIONS AS TARGET DATA

# # get new predictions
# predict_yai <- predict(yai)
# 
# 
# # get only predicted values
# predict_yai %<>%
#   select(sort(names(predict_yai))) %>%
#   select(matches(".o$")) %>%
#   rename_with(~str_remove(., "[.]o$"))

# remove ID field
predict_yai %<>% 
  select(-ID)

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

# how many imputed ids are different from the reference id? 
out_diff <- out %>%
  mutate(diff = ref_id - pred_id) %>%
  filter(diff != 0)

# difference in IDs from imputed IDs to original ids
pct_diff = nrow(out_diff)/nrow(out) * 100

print("Percent of imputed ids that are different from reference ids:")
print(pct_diff)




