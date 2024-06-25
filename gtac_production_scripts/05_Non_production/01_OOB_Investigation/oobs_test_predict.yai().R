# possible sources: 

# https://stackoverflow.com/questions/14676810/extracting-the-terminal-nodes-of-each-tree-associated-with-a-new-observation

#

# load library 
this_proj = this.path::this.proj()
lib_path = glue::glue('{this_proj}/gtac_production_scripts/00_Library/treeMapLib.R')
source(lib_path)


yai <- readRDS("//166.2.126.25/TreeMap/03_Outputs/07_Projects/2016_GTAC_Test/01_Raw_model_outputs/z16/model/z16_2016_GTAC_Test_ntree250_yai_treelist_bin.RDS")

xtable <- read.csv("//166.2.126.25/TreeMap/03_Outputs/07_Projects/2016_GTAC_Test/01_Raw_model_outputs/z16/xytables/z16_2016_Orig_Test_keepinbag_ntree250_Xdf_bin.csv")

oobs_default <- get_OOBs_yai(yai)

yai_out <- foruse(yai)
yai_out1 <- foruse(yai, kth = 1)

# get new predictions / out of bag (?) predictions
oobs_new <- predict(yai$ranForest$canopy_cover) # returns predictions for one variable

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


