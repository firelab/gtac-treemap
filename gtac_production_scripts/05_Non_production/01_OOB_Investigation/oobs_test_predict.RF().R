# possible sources: 

# https://stackoverflow.com/questions/14676810/extracting-the-terminal-nodes-of-each-tree-associated-with-a-new-observation

#

# load library 
this_proj = this.path::this.proj()
lib_path = glue::glue('{this_proj}/gtac_production_scripts/00_Library/treeMapLib.R')
source(lib_path)


yai <- readRDS("//166.2.126.25/TreeMap/03_Outputs/07_Projects/2016_GTAC_Test/01_Raw_model_outputs/z16/model/z16_2016_GTAC_Test_ntree250_yai_treelist_bin.RDS")

xtable <- read.csv("//166.2.126.25/TreeMap/03_Outputs/07_Projects/2016_GTAC_Test/01_Raw_model_outputs/z16/xytables/z16_2016_Orig_Test_keepinbag_ntree250_Xdf_bin.csv")


# list vars used for RF models
y_vars <- c("canopy_cover", "canopy_height", "EVT_GP", "disturb_code")

# get out of bag predictions from the RF models for each var
oobs_new <- data.frame(cbind(
  predict(yai$ranForest$canopy_cover),
  predict(yai$ranForest$canopy_height),
  predict(yai$ranForest$EVT_GP),
  predict(yai$ranForest$disturb_code)
          ))

names(oobs_new) <- y_vars

# Function to remap factor levels, based on an input variable name, input df with levels of desired variables, and oinput df that contains the var with the levels we want to remap 
remap_levels_var <- function(var_name, xdf, join_df) {
  
  # for testing 
  # var_name = "canopy_cover"
  # xdf = yai$xall
  # join_df = oobs_new
  
  
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
    left_join(rcl_var) %>%
    dplyr::select(-any_of(var_name)) %>%
    dplyr::mutate(!!var_name := orig) %>%
    dplyr::select(!!var_name)
  
  return(join_out)
  
}

for(i in seq_along(y_vars)) {
  
  var_name <- y_vars[i]
  
  oobs_new[var_name] <- remap_levels_var(var_name, yai$xall, oobs_new)
  
}


###################################

oobs_new$disturb_code <- factor(oobs_new$disturb_code)
oobs_new$EVT_GP <- factor(oobs_new$EVT_GP)

newt_test <- predict(yai, oobs_new) # can't predict using newtargets() without ALL input vars


###################################

impute_yai <- impute(yai, observed = TRUE) # same as predict()


# get new predictions
predict_yai <- predict(yai) 
predict_yai_names <- predict(yai)

# update row names so we can use these as new target data
maxv <- max(as.numeric(rownames(predict_yai)))
nrow <- nrow(predict_yai)
new_names <- seq(from = maxv+1, to = maxv+nrow, by = 1)
rownames(predict_yai) <- new_names

# get only observed values
predict_yai %<>%
  select(sort(names(predict_yai))) %>%
  select(matches(".o$")) %>%
  rename_with(~str_remove(., "[.]o$")) 
  
# impute based on these predictions
# new targets
newt <- newtargets(yai, predict_yai)

out.neiIds <- newt$neiIdsTrgs # a matrix of reference identifications that correspond to neiDstTrgs (distances between target and ref).

#### Format outputs into imputated ids
yrows <- as.numeric(out.neiIds[,1]) # get list of plotIds; rowname = rowname from X.df.temp - corresponds to cell

# bind with original
out <- data.frame(cbind(as.numeric(rownames(predict_yai_names)), 
                        as.numeric(yrows)))
names(out) <- c("ref_id", "pred_id")
# how many imputed is are different? 
out_diff <- out %>%
  mutate(diff = ref_id - pred_id) %>%
  filter(diff != 0)

# difference in IDs from imputed IDs to original ids
nrow(out_diff)/nrow(out)


