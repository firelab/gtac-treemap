assembleCM <- function(layer_field, raster, lookup, id_field, 
                       stackin_compare, stackin_compare_name,  
                       remapEVT_GP, EVT_GP_remap_table,
                       exportTF, export_path) {
  
  layer_field<<- layer_field
  #print("make lookup table")
  #make lookup table
  lt <- cbind(lookup[id_field], lookup[layer_field])
  
  #print("make imp1")
  # make raster to compare
  imp1 <-  terra::classify(raster, lt) 
  
  # if(!identical(crs(imp1), crs(stackin_compare))) {
  #   imp1 %<>% terra::project(crs(stackin_compare))
  # }
  
  
  if(exportTF) {
    writeRaster(imp1,
                glue::glue("{export_path}{layer_field}.tif"),
                overwrite = TRUE)
  }
  
  print(paste0("Saved ",layer_field, " raster"))
  
  gc()
  
  #print("get lf1")
  # get single lf raster
  lf1 <- stackin_compare[layer_field]
  lf1 <- terra::trim(lf1) # remove NA values on borders
  
  # crop and mask reference raster with input raster
  # necessary for testing on subset 
  if(!compareGeom(lf1, imp1, ext = TRUE)) { # if the extents don't match
    lf1 <- terra::crop(lf1, imp1, mask = TRUE) # crop and mask 
  }
  
  # Conditionally remap EVT
  if(layer_field == "evt_gp") {
    
    # load remap table
    lt_evg <- evg_reclass
    names(lt_evg) <- c("EVT_GP", "EVT_remap")
    
    #change column order
    lt_evg<- lt_evg[,c(2,1)]
    lt_evg$EVT_remap<- as.numeric(as.character(lt_evg$EVT_remap))
    
    #remap
    lf1 <- terra::classify(lf1, lt_evg)
    
  } 
  
  gc()
  
  #print("get levels")
  # make both rasters categorical - get levels of layer field
  levels <- data.frame(id = sort(unique(lt[,2])),
                       levels = levels(as.factor(lt[,2])))
  
  
  # Calculate confusion matrix
  #--------------------------------------#
  
  
  #print("calculating and exporting confusion matrix")
  t <- data.frame(cbind(terra::values(imp1),
                        terra::values(lf1)))
  t<- t[complete.cases(t),]
  
  #update names
  names(t) <- c("pred", "ref")
  
  # replace any NaN with NA
  
  t$pred[is.nan(t$pred)]<-NA
  t$ref[is.nan(t$ref)]<-NA
  
  t<- na.omit(t)
  
  # calculate cms 
  cms <- eval_cm_function(t, NA)
  
  rm(imp1, lf1)
  gc()
  
  return(cms)
}



eval_cm_function <- function(t, noDataVal) {
  
  #require(c(tidyverse, caret))
  
  # handle missing param
  if(missing(noDataVal)) {
    noDataVal <- NA
  }
  
  #apply column names
  names(t) <- c("pred", "ref")
  
  # set levels for factors
  # get maximum value of table that's not the noDataValue
  tn <- t
  tn[tn == noDataVal] <- NA
  
  # get levels - all levels that appear in each of pred and ref
  levels_t <- unique(c(as.numeric(unlist((unique(tn$pred)))),
                       as.numeric(unlist((unique(tn$ref))))))
  levels_t <- sort(levels_t)
  
  # ensure columns are factors with the same levels
  tn<- tn %>% mutate(pred = factor(pred, levels = levels_t),
                     ref = factor(ref, levels = levels_t))  
  
  # Get confusion  matrix
  #---------------------------------------------#
  
  # confusion matrix
  cm <- caret::confusionMatrix(data = tn$pred, # pred
                               reference = tn$ref # ref
  )
  
  # Process data frames for export
  #---------------------------------#
  
  # raw confusion matrix
  cm_raw_out <- as.table(cm)
  cm_raw_out <- addmargins(cm_raw_out)
  
  # make data frame of classes
  cm_t_classes <- data.frame(as.matrix(cm, what = "classes"))
  
  ifelse(layer_field=="binary_disturbance",
         names(cm_t_classes) <- "binary_disturbance",
         names(cm_t_classes) <- levels_t)
  
  #names(cm_t_classes) <- levels_t
    #cm_t_classes <- data.frame(as.matrix(cm, what = "classes"))
  cm_t_classes<- cm_t_classes %>% rownames_to_column(., var = 'metric')
  
  # overall eval stats
  cm_t_overall <- data.frame(as.matrix(cm, what = "overall"))
  names(cm_t_overall) <- c("value")
  cm_t_overall$metric <- rownames(cm_t_overall)
  
  # calculate frequency table
  tfreq <- 
    cbind(table(tn$pred),
          table(tn$ref)
    ) %>%
    data.frame()
  
  # manipulate frequency table
  names(tfreq) <- c("pred", "ref")
  tfreq$class <- factor(rownames(tfreq), levels = levels_t)
  rownames(tfreq) <- NULL
  
  #Calculate normalized frequency table also 
  
  # calc total to use in normalizing
  total_pred = sum(tfreq$pred)
  total_ref = sum(tfreq$ref)
  
  # add normalized frequency
  tfreq_norm <- tfreq %>%
    mutate(pred = pred/total_pred, 
           ref = ref/total_ref)
  
  # format output
  # ---------------------------- #
  out_list <- list(cm_raw_out,
                   cm_t_classes,
                   cm_t_overall,
                   tfreq,
                   tfreq_norm)
  
  names(out_list) <- c("raw", "classes", "overall", "freq", "freq_norm")
  
  return(out_list)
  
}


assembleExport <- function(layer_field, raster, lookup, id_field, export_path) {
  
  print(glue('assembleExport: {layer_field}'))
  lt <- cbind(lookup[id_field], lookup[layer_field])
  #print(head(lt))
  rout <- terra::classify(raster, lt)
  writeRaster(rout,
              glue('{export_path}{layer_field}.tif'),
              overwrite = TRUE)
  rm(rout)
  gc()
  
}


assembleConcat <- function(layer_field, raster, lookup, id_field, 
                           stackin_compare, stackin_compare_name, export_path, 
                           remapEVT_GP, EVT_GP_remap_table) {
  
  print(glue('assembleConcat: {layer_field}'))
  
  #print("make lookup table")
  #make lookup table
  lt <- cbind(lookup[id_field], lookup[layer_field])
  
  #print("make imp1")
  # make raster to compare
  imp1 <-  terra::classify(raster, lt) %>%
    terra::project(crs(stackin_compare)) %>%
    as.int()
  
  #print("get lf1")
  # get single lf raster
  lf1 <- lf[layer_field]
  
  # mask reference raster with input raster - necessary for testing on subset 
  lf1 <- terra::crop(lf1, imp1, mask = TRUE)
  
  # Conditionally remap EVT
  if(remapEVT_GP & layer_field == "EVT_GP") {
    
    # load remap table
    lt_evg <- EVT_GP_remap_table
    names(lt_evg) <- c("EVT_GP", "EVT_GP_remap")
    lt_evg %<>%
      select(EVT_GP_remap, EVT_GP)
    
    #remap
    lf1 <- terra::classify(lf1, lt_evg)
  } 
  
  #print("make diff")
  # get difference; set to NA where layers are the same
  diff <- imp1-as.int(lf1)
  diff %<>% terra::classify(cbind(0,NA))
  
  #print("get levels")
  # make both rasters categorical - get levels of layer field
  levels <- data.frame(id = sort(unique(lt[,2])),
                       levels = levels(as.factor(lt[,2])))
  
  #print("set levels")
  # set levels for rasters to make them categorical
  levels(imp1) <- levels
  levels(lf1) <- levels
  
  #print("concat")
  # concat and mask with difference
  out1 <- terra::concats(imp1, lf1)
  out1 %<>% terra::mask(diff)
  
  #export
  writeRaster(out1, 
              glue('{export_path}_{layer_field}_v{stackin_compare_name}.tif'),
              overwrite = TRUE)
  rm(out1)
  gc()
  
}

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
  
  gc() #
  
  return(pred_ref_oob)
  
}


FitFlextableToPage <- function(ft, pgwidth){
  
  ft_out <- ft %>% autofit()
  
  ft_out <- flextable::width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
  return(ft_out)
}
