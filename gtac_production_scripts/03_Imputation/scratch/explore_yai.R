# explore yai object

###########################################################################
# Set inputs
###########################################################################

# # Zone list
# zone_list <- c(16)
# 
# #home_dir
# #home_dir <- "D:/LilaLeatherman/01_TreeMap/"
# home_dir<- "//166.2.126.25/TreeMap/"
# 
# # Directory where target rasters live
# target_dir <- glue::glue("{home_dir}01_Data/01_TreeMap2016_RDA/02_Target/")
# 
# # Paths for exporting data
# #--------------------------------------#
# 
# # set path to save output rasters
# # this directory will be created if it does not already exist
# output_dir <- glue::glue('{home_dir}03_Outputs/07_Raw_model_outputs/2016_Original_Test/')
# 
# # Output imputation name
# #output_name <- "2016_Orig_TestLL"
# # Output imputation name
# output_name <- "2016_Orig_Test_keepinbag"
# 
# # set tmp directory
# tmp_dir <- "D:/tmp/"
# 
# # set zone_num
# zone_num <- zone_list[1]
# 
# # Set zone name options
# cur.zone <- glue::glue('z{zone_num}')
# cur.zone.zero <- if(zone_num < 10) {
#   glue('z0{zone_num}') } else {
#     cur.zone
#   }
# 
# 
# # Update output and target dir with zone
# # -----------------------------------------#
# # Set folder paths
# target_dir = glue::glue('{target_dir}/{cur.zone.zero}/')
# output_dir = glue::glue('{output_dir}/{cur.zone.zero}/')


###################################################################
##################################################
# Set inputs
###################################################

# Set inputs - from input script
# Id where script is located
this.path <- this.path::this.path()

# get path to input script
spl <- stringr::str_split(this.path, "/")[[1]]
input_script.path <- paste( c(spl[c(1:length(spl)-1)],
                              "00_inputs_for_imp.R" ),
                            collapse = "/")

source(input_script.path)


# Model inputs
#----------------------------------#

# Path where model is located
model_path <- glue::glue('{output_dir}/model/{cur.zone.zero}_{output_name}_yai_treelist_bin.RDS')

# path to x table
x_table_path <- glue::glue('{output_dir}/xytables/{cur.zone.zero}_{output_name}_Xdf_bin.csv')

# path to y table
y_table_path <- glue::glue('{output_dir}/xytables/{cur.zone.zero}_{output_name}_Ydf_bin.csv')

# Path to X table - orig input
xtable_orig <- glue::glue("{home_dir}01_Data/01_TreeMap2016_RDA/01_Input/03_XTables/X_table_all_singlecondition.txt")


#################################
# Load

# load model
yai <- readr::read_rds(model_path)

#yai <- yai.treelist.bin

#X.df.KR <- read.csv(xtable_orig)

# load x table
X.df <- read.csv(x_table_path)

# load y table
#Y.df <- read.csv(y_table_path)

# set up X table
rownames(X.df) <- X.df$X

X.df %<>% 
  mutate(
    disturb_code = factor(disturb_code),
    disturb_year = factor(disturb_year),
    EVT_GP = factor(EVT_GP),
    canopy_cover = factor(canopy_cover),
    canopy_height = factor(canopy_height)) %>%
  rename("PLOTID" = X)


###########################
# explore yai object

# # Confusion matrices
# cm_EVC <- yai$ranForest$canopy_cover$confusion
# cm_EVH <- yai$ranForest$canopy_height$confusion
# cm_EVT_GP <- yai$ranForest$EVT_GP$confusion
# cm_DC <- yai$ranForest$disturb_code$confusion
# 
# # other yai attributes
# #######################################
# 
# yai$call
# yai$obsDropped
# yai$yDrop
# yai$bootstrap
# yai$trgRows
# yai$cancor
# yai$theFormula
# yai$ftest
# 
# str(yai$neiDstRefs)
# head(yai$neiDstRefs)
# str(yai$neiIdsRefs)
# head(yai$neiIdsRefs)

# yai node attributes from yaImpute - try
###############################
source("C:/Users/lleatherman/Documents/GitHub/gtac-treemap/gtac_production_scripts/00_Library/get_OOBs_yai.R")

# attempt
oobs <- get_OOBs_yai(yai)


###################################################################

###################################################################

# list columns that we're interested in using for validation
val_columns <- c("disturb_code", "disturb_year", "canopy_cover", "canopy_height", "EVT_GP")

# validation ---

# load lookup table - table with all ref vars, including those calculated from FIA
# don't have this YET

# make reference table
refs <- left_join(oobs, X.df, 
                   by = c("ref" = "PLOTID")) %>%
  select(c(tree_num, ref, pred, inbag_count, val_columns)) %>%
  rename_at(vars(val_columns), ~paste0(., "_ref"))

# join oobs with X.df
preds <- left_join(oobs, X.df,
              by = c("pred" = "PLOTID")) %>%
  select(c(tree_num, ref, pred, inbag_count, val_columns)) %>%
  rename_at(vars(val_columns), ~paste0(., "_pred"))

# example validation table

val_var <- "canopy_cover"

p_r <- inner_join(refs, preds, by = c("tree_num", "ref", "pred", "inbag_count")) %>%
  filter(inbag_count != 0) # get only OOB observations

library(caret)

confusionMatrix(p_r$disturb_code_ref, p_r$disturb_code_pred)
confusionMatrix(p_r$canopy_cover_ref, p_r$canopy_cover_pred)  
confusionMatrix(p_r$canopy_height_ref, p_r$canopy_height_pred)  
confusionMatrix(p_r$disturb_year_ref, p_r$disturb_year_pred)
confusionMatrix(p_r$EVT_GP_ref, p_r$EVT_GP_pred)

# compare in-bag and out-of-bag accuracy for each metric and class


eval_cm_function(table(p_r$disturb_code_ref,
                       p_r$disturb_code_pred))

####################################################################
# # set up basic inputs
# 
# XDrop <- NULL
# bootstrap = FALSE
# k = 1
# 
# neiDstRefs <- NULL
# neiIDsRefs <- NULL
# 
# ranForest <- yai$ranForest
# rf1 <- yai$ranForest[[1]]
# xall <- yai$xRefs
# yall=na.omit(as.data.frame(yai$yRefs))
# 
# refs=intersect(rownames(yall),rownames(xall))
# 
# xRefs=xall[refs,,drop=FALSE]
# 
# trgs=setdiff(rownames(xall),refs)
# 
# xTrgs=xall[trgs,1,drop=FALSE]
# 
# 
# # run node extract
# nodes=NULL
# 
# for (i in 1:length(ranForest))
#   {
#   nodeset=attr(predict(ranForest[[i]],xall,
#                        proximity=FALSE,nodes=TRUE),"nodes")
#   if (is.null(nodeset)) stop("randomForest did not return nodes")
#   colnames(nodeset)=paste(colnames(nodeset),i,sep=".")
#   nodes=if (is.null(nodes)) nodeset else cbind(nodes,nodeset)
# } 
# 
# refNodes = nodes[rownames(xRefs),]
# 
# ### TEST
# #refNodes = refNodes[c(1:6),]
# 
# 
# INTrefNodes=as.integer(refNodes)
# INTnrow=as.integer(nrow(xRefs))
# INTncol=as.integer(ncol(nodes))
# # INTnrow = as.integer(nrow(refNodes))
# # INTncol = as.integer(ncol(nodes))
# INTsort = INTrefNodes
# dim(INTsort) = c(INTnrow,INTncol)
# # sort all 
# INTsort=apply(INTsort,2,function (x) sort(x,index.return = TRUE, 
#                                           decreasing = FALSE)$ix-1)
# attributes(INTsort)=NULL
# INTsort = as.integer(INTsort)
# # attr(ranForest,"rfRefNodeSort") = list(INTrefNodes=INTrefNodes, 
# #                                        INTnrow=INTnrow, INTncol=INTncol, INTsort=INTsort)
# 
# rfRefNodeSort = list(INTrefNodes=INTrefNodes,
#                      INTnrow=INTnrow, INTncol=INTncol, INTsort=INTsort)
# 
# 
# # test refNodes
# refNodes <- refNodes[c(1:6),]
# 
# 
# # to get distances: 
# {
#   prox=lapply(apply(refNodes,1,as.list),function (x) 
#   {
#     prx=.Call("rfoneprox", INTrefNodes, INTsort, INTnrow, INTncol,
#               as.integer(x), vector("integer",INTnrow)) 
#     if (k > 1) px=sort(prx,index.return = TRUE, decreasing = TRUE)$ix[2:l]
#     else
#     { 
#       px=which.max(prx)
#       prx[px]=-1
#       px=which.max(prx)
#     }
#     c(prx[px],px)  # counts followed by pointers to references
#   })
#   for (i in 1:k)
#   {
#     i = 1 # for testing
#     neiDstRefs[,i]=unlist(lapply(prox,function (x,i) (INTncol-x[i])/INTncol,i))
#     neiIdsRefs[,i]=unlist(lapply(prox,function (x,i,k,Rnames) 
#       Rnames[x[k+i]],i,k,rownames(xRefs)))
#   } 
# }
# 
# prx <- prox
# 
# px = lapply(prx, which.max)
# prx[px] = -1
# head(px)
# 
# # inspect noted sort
# 
# yai$ranForest$canopy_cover$rfRefNoteSort
# 
# 
# # try to get all terminal node predictions for each row
# 
# 
# 
# ################
# # inspect attributes of prediction
# pred <- predict(ranForest[[i]],xall,
#                              proximity=FALSE,nodes=TRUE, predict.all = TRUE)
# 
# 
# nodeset <- nodeset=attr(pred,"nodes")
# # random forest attributes
# ##########################################
# yai$ranForest$canopy_cover$forest$treemap
# yai$ranForest$canopy_cover$oob.times
# 
# summary(yai$ranForest$canopy_cover)
# 
# yai$ranForest$canopy_cover$y
# yai$ranForest$canopy_cover$oob.times
# 
# # manually create confusion matrix
# d <- cbind(oob.times = yai$ranForest$canopy_cover$oob.times,
#       y = yai$ranForest$canopy_cover$y,
#       predicted = yai$ranForest$canopy_cover$predicted) %>%
#   data.frame() %>%
#   mutate(y = as.factor(y),
#          predicted = as.factor(predicted))
# 
# caret::confusionMatrix(d$y, d$predicted)
# 
# head(yai$ranForest$canopy_cover$y)
# head(yai$ranForest$canopy_cover$predicted)
# 
# # other random forest attributes
# yai$ranForest$canopy_cover$inbag[1,]
# yai$ranForest$canopy_height$inbag
# yai$ranForest$canopy_cover$predicted
# 
# yai$ranForest$canopy_cover$oob.times
# yai$ranForest$canopy_cover$votes
# yai$ranForest$canopy_cover$forest
# 
# yai$ranForest$canopy_cover$predicted
# 
# # trying to get OOB predictions
# 
# ##########################################################
# 
#   # create test df with oob 
# cc_ib <- data.frame(yai$ranForest$canopy_cover$inbag)
# cc_ib$ID <- rownames(cc_ib)
# cc_ib$predicted_cc <- yai$ranForest$canopy_cover$predicted
# 
# cc_pred <- data.frame
# 
# # convert from wide to long
# cc_ib %<>%
#   pivot_longer(!c(ID, predicted_cc), names_to = "tree", values_to = "in.bag") 
# 
# # mess with X.df table to use for validation
# X.df.val <- X.df
# X.df$ID <- rownames(X.df)
# 
# val_df <- cc_ib %>%
#   left_join(X.df, by = "ID") %>%
#   filter(in.bag == 0)
# 
# 
# #### dummy test
# data("iris")
# 
# iris_train <- sample(1:nrow(iris), size=floor(nrow(iris)*0.8))
# 
# 
# rf <- randomForest(formula=Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
#                      data=iris[iris_train,],
#                      keep.inbag=TRUE)
# 
# #The inbag gives us, for each tree, a vector of how many times each observation was used in the tree. We can use this to "mask" predictions back to the whole data set.
# 
# ibcs <- rf$inbag
# 
# # Convert to an n-observation by n-trees matrix:
# #ibcs <- do.call(cbind, list(ibcs))
# 
# # Get predictions from all trees
# preds <- predict(rf, iris[iris_train,], predict.all = TRUE)$individual
# 
# # Set in-bag predictions to NA using the ibcs matrix
# preds[which(ibcs > 0)] <- NA
# 
# #Check that the average across rows gets the same result as predictions
# all.equal(rf$predicted, rowMeans(preds, na.rm=TRUE))
# 
# ##################################
# # try on RF from yai
# 
# ibcs <- rf1$inbag
# 
# rf2 <- yai$ranForest$canopy_height
# 
# preds <- predict(rf2, X.df, predict.all = TRUE)
# 
# # Set in-bag predictions to NA using the ibcs matrix
# preds[which(ibcs > 0)] <- NA
# 
# #Check that the average across rows gets the same result as predictions
# all.equal(rf1$predicted, rowMeans(preds, na.rm=TRUE))
# 
# length(yai$ranForest)
# 
# 
# # write a function to get OOB ref x predicted for yai 
# 
# for( i in 1: length(yai$ranForest)) {
#   
#   i = 1 # for testing
#   
#   # get random forest
#   rf <- yai$ranForest[i]
#   
#   # get in-bag counts
#   ibcs <- rf$inbag
#   
#   # get predictions
#   preds <- predict(rf, X.df, predict.all - TRUE)$individual
#   
#   # Set in-bag predictions to NA using the ibcs matrix
#   preds[which(ibcs > 0)] <- NA
#   
#   
# }
#   

