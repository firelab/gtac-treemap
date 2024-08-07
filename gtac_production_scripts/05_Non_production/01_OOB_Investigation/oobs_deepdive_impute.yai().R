# Based on Ian's suggestion:
# Get predicted values of response variables, and all other x variables, from original yai model

# But how do I know these are out of bag? 

# Then, input those predicted values into the model again, as response variables
# Use those new ids to assess accuracy 


# load library 
this_proj = this.path::this.proj()
lib_path = glue::glue('{this_proj}/gtac_production_scripts/00_Library/treeMapLib.R')
source(lib_path)


yai <- readRDS("//166.2.126.25/TreeMap/03_Outputs/07_Projects/2016_GTAC_Test/01_Raw_model_outputs/z16/model/z16_2016_GTAC_Test_yai_treelist_bin.RDS")

xtable <- read.csv("//166.2.126.25/TreeMap/03_Outputs/07_Projects/2016_GTAC_Test/01_Raw_model_outputs/z16/xytables/z16_2016_GTAC_Test_Xdf_bin.csv")

#oobs_default <- get_OOBs_yai(yai)

#oobs_new <- get_OOBs_yai_predict(yai)

# yai_out <- foruse(yai)
# yai_out1 <- foruse(yai, kth = 1)

# get new predictions / out of bag (?) predictions
preds <- predict(yai$ranForest$canopy_cover) # returns predictions for one variable

# get new predictions
predict_yai <- predict(yai) 

impute_yai <- impute(yai, observed = TRUE) # same as predict.yai(). returns  

identical(predict_yai, impute_yai)

###################################################
# investigate inner workings of impute.yai()
# copied from: https://github.com/jeffreyevans/yaImpute/blob/cc2eed212857b3951bc8e051ac29eab97f856662/R/impute.R#L242C4-L285C5

# prediction functions from package
#############################################################

# LOOKS LIKE IT JUST TAKES THE VARIABLE VALUES FROM THE NEAREST NEIGHBOR

# FUNCTION FOR PREDICTING CONTINUOUS
pred.c <- function (refs,ids,w=NULL,method="closest",k=1,vars,observed)
{
  if (is.null(vars)) vars <- colnames(refs)
  else vars <- intersect(vars,colnames(refs))
  if (is.null(vars) | length(vars)==0) return (NULL)
  
  if (method=="closest" || k==1) 
  {
    ans <- refs[ids[,1],vars,FALSE] # THIS GETS USED FOR US
  } else if (method=="mean" || method== "median")
  {
    rws <- match(ids[,1:k],rownames(refs))
    ans <- lapply(vars, function (v,rws,refs,nr,func)
    {
      rfs <- refs[rws,v]
      dim(rfs) <- c(nr,length(rfs)/nr)
      apply(rfs,1,func)
    }, rws, refs, nrow(ids), if (method=="mean") mean else median)
    names(ans) <- vars
    ans <- as.data.frame(ans)
    
  } else if (method=="dstWeighted")
  {
    if (is.null(w))
    {
      warning ("w is required when method is dstWeighted")
      return(NULL)
    } 
    wei <- t(apply(w[,1:k,drop=FALSE],1,
                   function (x) {x <- 1/(1+x); x/sum(x)}))
    rws <- match(ids[,1:k],rownames(refs))
    ans <- lapply(vars, function (v,rws,refs,wei)
    {
      rfs <- refs[rws,v]
      dim(rfs) <- dim(wei)
      apply(rfs*wei,1,sum)
    }, rws, refs, wei)
    names(ans) <- vars
    ans <- as.data.frame(ans)
  }
  rownames(ans) <- rownames(ids)
  
  if (observed)
  {
    obs <- matrix(data=NA, nrow = nrow(ans), ncol = ncol(ans))
    rownames(obs) <- rownames(ans)
    colnames(obs) <- vars
    obs <- as.data.frame(obs)
    commonRows <- intersect(rownames(ans),rownames(refs))
    if (length(commonRows)>0) obs[commonRows,vars] <- refs[commonRows,vars]
    colnames(obs) <- paste(vars,"o",sep=".")
    ans <- cbind(ans,obs)
  }
  ans
}

# FUNCTION FOR PREDICTING FACTORS
pred.f <- function (refs,ids,w=NULL,method="closest",k=1,vars,observed)   
{
  if (is.null(vars)) vars <- colnames(refs)
  else vars <- intersect(vars,colnames(refs))
  if (is.null(vars) | length(vars)==0) return (NULL)
  
  if (method=="closest" || k==1) 
  {
    ans <- data.frame(refs[ids[,1],vars,FALSE])
  } else 
  {
    wei <- if (method != "dstWeighted" || is.null(w)) NULL else  
      t(apply(w[,1:k,drop=FALSE],1,
              function (x) {x <- 1/(1+x); x/sum(x)}))
    rws <- match(ids[,1:k],rownames(refs))
    ans <- lapply(vars, function (v,rws,refs,wei,nr)
    {
      rfs <- as.character(refs[rws,v])
      dim(rfs) <- c(nr,length(rfs)/nr)
      if (is.null(wei))
      {
        apply(rfs,1,function (x)
        {
          t <- table(x)
          t <- t + (runif(length(t)) * 0.01)
          names(which.max(t))
        })
      } else
      {
        a <- vector("character",nrow(wei))
        for (i in 1:nrow(wei))
        {
          t <- tapply(wei[i,],rfs[i,],sum)
          t <- t + (runif(length(t)) * 0.01 * min(t))
          a[i] <- names(which.max(t))
        }
        a
      }
    }, rws, refs, wei, nrow(ids))
    names(ans) <- vars 
    ans <- as.data.frame(ans,rownames=rownames(ids))
  }
  
  rownames(ans) <- rownames(ids)
  if (observed)
  {
    obs <- matrix(data=NA, nrow = nrow(ans), ncol = ncol(ans))
    rownames(obs) <- rownames(ans)
    colnames(obs) <- vars
    obs <- as.data.frame(obs)
    commonRows <- intersect(rownames(ans),rownames(refs))
    if (length(commonRows)>0)
    {
      for (var in vars)
      {
        obs[commonRows,var] <- levels(refs[,var])[refs[commonRows,var]]
        obs[,var] <- factor(obs[,var])
      }
    }
    names(obs) <- paste(vars,"o",sep=".")
    ans <- cbind(ans,obs)
  }
  ans
}

# FUNCTION THAT PULLS TOGETHER CONTINUOUS AND FACTOR PREDICTIONS
# pred function from yaImpute:
pred <- function (refs,ids,w=NULL,method="closest",
                  method.factor="closest",k=1,vars,observed)
{
  factors <- findFactors(refs)
  nfactors <- sum(factors)
  if (nfactors>0 && method.factor != "closest" && k==1)
  {
    warning ("method.factor was set to closest because k=1")
    method.factor <- "closest"
  }
  if (nfactors == 0)
    out <- pred.c(refs=refs,ids=ids,w=w,method=method,
                  k=k,vars=vars,observed=observed)
  else if (nfactors == ncol(refs))
    out <- pred.f(refs=refs,ids=ids,w=w,method=method.factor,
                  k=k,vars=vars,observed=observed)
  else
  {
    tmp <- data.frame(refs[,!factors],row.names=rownames(refs))
    colnames(tmp) <- colnames(refs)[!factors]
    p1 <- pred.c(refs=tmp,ids=ids,w=w,method=method,
                 k=k,vars=vars,observed=observed)
    tmp <- data.frame(refs[,factors],row.names=rownames(refs))
    colnames(tmp) <- colnames(refs)[factors]
    p2 <- pred.f(refs=tmp,ids=ids,w=w,method=method.factor,
                 k=k,vars=vars,observed=observed)
    if      (is.null(p1) && is.null(p2)) out <- NULL
    else if (is.null(p1)) out <- p2
    else if (is.null(p2)) out <- p1
    else                  out <- cbind(p1,p2)
  }
  out
}

findFactors =  get("findFactors",asNamespace("yaImpute"))

###################################

# set params needed to make predictions

object <- yai 
method <- "closest"
method.factor <- method
k = 1
vars <- NULL
ancillaryData <- NULL
observed <- TRUE

if (is.null(vars))
{
  if (is.null(ancillaryData)) 
  {
    if (object$method != "randomForest") vars <- yvars(object)
    else if (names(object$ranForest)[[1]] == "unsupervised") 
      vars <- xvars(object)
  }            
  else 
  {
    if (! is.data.frame(ancillaryData)) 
      ancillaryData <- as.data.frame(ancillaryData)
    vars <- colnames(ancillaryData)
  }
}
if (is.null(ancillaryData))
{
  r <- NULL
  if (length(object$neiIdsRefs)>0)
  {
    if (!(ncol(object$yRefs) == 1 && names(object$yRefs)[1]=="ydummy"))
      yPredRefs <- pred(refs=object$yRefs,ids=object$neiIdsRefs,
                        w=object$neiDstRefs,method=method,method.factor=method.factor,
                        k=k,vars=vars,observed=observed)
    else yPredRefs <- NULL
    yPredRefs <- NULL
    xPredRefs <- pred(refs=object$xall,ids=object$neiIdsRefs, ### THIS IS BEING RUN
                      w=object$neiDstRefs,method=method,method.factor=method.factor,
                      k=k,vars=vars,observed=observed)
    if      (is.null(yPredRefs) && is.null(xPredRefs)) r <- NULL
    else if (is.null(yPredRefs)) r <- xPredRefs ### THIS IS BEING USED
    else if (is.null(xPredRefs)) r <- yPredRefs
    else                         r <- cbind(yPredRefs,xPredRefs)
    
  }
  t <- NULL
  if (length(object$neiIdsTrgs)>0)
  {
    if (!(ncol(object$yRefs) == 1 && names(object$yRefs)[1]=="ydummy"))
      yPredTrgs <- pred(refs=object$yRefs,ids=object$neiIdsTrgs,
                        w=object$neiDstTrgs,method=method,method.factor=method.factor,
                        k=k,vars=vars,observed=observed)
    else yPredTrgs <- NULL # THIS IS BEING USED
    xPredTrgs <- pred(refs=object$xall,ids=object$neiIdsTrgs,
                      w=object$neiDstTrgs,method=method,method.factor=method.factor,
                      k=k,vars=vars,observed=observed)
    
    if      (is.null(yPredTrgs) && is.null(xPredTrgs)) t <- NULL
    else if (is.null(yPredTrgs)) t <- xPredTrgs
    else if (is.null(xPredTrgs)) t <- yPredTrgs
    else                         t <- cbind(yPredTrgs,xPredTrgs)
  }
  # if      (is.null(r) && is.null(t)) out <- NULL
  # else if (is.null(r)) out <- t
  # else if (is.null(t)) out <- r
  # else                 out <- rbind(r,t)
  
  out <- r
  
  scale <- data.frame(center=c(object$xScale$center,object$yScale$center),
                      scale=c(object$xScale$scale, object$yScale$scale))
  }

###################################################

# deep dive into predict function

refs=object$xall
ids=object$neiIdsRefs
w=object$neiDstRefs
method=method
method.factor=method.factor
k=k
vars=NULL
observed=observed

#{
  factors <- findFactors(refs)
  nfactors <- sum(factors)
  if (nfactors>0 && method.factor != "closest" && k==1)
  {
    warning ("method.factor was set to closest because k=1")
    method.factor <- "closest"
  }
  # if (nfactors == 0)
  #   out <- pred.c(refs=refs,ids=ids,w=w,method=method,
  #                 k=k,vars=vars,observed=observed)
  # else if (nfactors == ncol(refs))
  #   out <- pred.f(refs=refs,ids=ids,w=w,method=method.factor,
  #                 k=k,vars=vars,observed=observed)
  # else
  # {
    tmp <- data.frame(refs[,!factors],row.names=rownames(refs))
    colnames(tmp) <- colnames(refs)[!factors]
    p1 <- pred.c(refs=tmp,ids=ids,w=w,method=method,
                 k=k,vars=vars,observed=observed)
    tmp <- data.frame(refs[,factors],row.names=rownames(refs))
    colnames(tmp) <- colnames(refs)[factors]
    p2 <- pred.f(refs=tmp,ids=ids,w=w,method=method.factor,
                 k=k,vars=vars,observed=observed)
    if      (is.null(p1) && is.null(p2)) out <- NULL
    else if (is.null(p1)) out <- p2
    else if (is.null(p2)) out <- p1
    else                  out <- cbind(p1,p2)
  }
  out

  ##############################
  
  
  #else vars <- intersect(vars,colnames(refs))
  
  tmp <- data.frame(refs[,!factors],row.names=rownames(refs))
  colnames(tmp) <- colnames(refs)[!factors]
  
  vars <- colnames(tmp)
  
  ans1 <- tmp[ids[,1],vars,FALSE]
  
  identical(ans1, ans2)
  
  p1 <- pred.c(refs=tmp,ids=ids,w=w,method=method,
               k=k,vars=vars,observed=FALSE)
  tmp <- data.frame(refs[,factors],row.names=rownames(refs))
  colnames(tmp) <- colnames(refs)[factors]
  p2 <- pred.f(refs=tmp,ids=ids,w=w,method=method.factor,
               k=k,vars=vars,observed=observed)
  if      (is.null(p1) && is.null(p2)) out <- NULL
  else if (is.null(p1)) out <- p2
  else if (is.null(p2)) out <- p1
  else                  out <- cbind(p1,p2)
  
  #ans <- refs[ids[,1],vars,FALSE]
  