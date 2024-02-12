data(MoscowMtStJoe) 
#convert polar slope and aspect measurements to cartesian 
#(which is the same as Stage's (1976) transformation). 

polar<-MoscowMtStJoe[,40:41] 
polar[,1]<-polar[,1]*.01 #slope proportion 
polar[,2]<-polar[,2]*(pi/180) #aspect radians 

cartesian<-t(apply(polar,1,function(x)
  {return(c(x[1]*cos(x[2]),x[1]*sin(x[2])))}))

colnames(cartesian)<-c("xSlAsp","ySlAsp") 
x<-cbind(MoscowMtStJoe[,37:39],cartesian,MoscowMtStJoe[,42:64]) 
y<-MoscowMtStJoe[,1:35] 

msn<-yai(x=x,y=y,method="msn",k=1) 
mal<-yai(x=x,y=y,method="mahalanobis",k=1) 

#the results can b eplotted. 
plot(mal,vars=yvars(mal)[1:16]) 

#compare these results using the generalized mean distances.. 
grmsd(mal,msn) 

#try method="gower" 
if(require(gower)) { 
  gow<-yai(x=x,y=y,method="gower",k=1) 
  # compare these results using the generalized meandistances.. 
  grmsd(mal,msn,gow) } 

#trymethod="randomForest" 

if(require(randomForest)) { 
  
  # reduce the plant community data for randomForest. 
  yba <-MoscowMtStJoe[,1:17] 
  ybaB<-whatsMax(yba,nbig=7) #see help on whatsMax yba
  rf<-yai(x=x,y=ybaB,method="randomForest",k=1) 
  
  #build the imputations for the original y's 
  rforig<-impute(rf,ancillaryData=y) 
  
  #compare the results using individual rmsd's 
  compare.yai(mal,msn,rforig) 
  plot(compare.yai(mal,msn,rforig)) 
  
  #build another random Forest case forcing regression 
  #to be used for continuous variables. The answers differ 
  #but one is not clearly better than th eother. 
  
  rf2<-yai(x=x,y=ybaB,method="randomForest",rfMode="regression") 
  rforig2<-impute(rf2,ancillaryData=y) 
  compare.yai(rforig2,rforig) 
}


rf$ranForest$yba.maxCol$predicted

rf$neiDstTrgs
rf$neiDstRefs
rf$neiIdsRefs

newImp <- impute(yai, X.df )
