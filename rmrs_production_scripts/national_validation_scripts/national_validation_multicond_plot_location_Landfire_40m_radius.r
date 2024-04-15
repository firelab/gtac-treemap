# national-scale validation for tree list c2014
# written by Karin Riley, 10/28/2019, updated 5/4/2021

library (foreign)
library(RSQLite)
library(sqldf)
library(DBI)
library(data.table)

options(scipen = 999)

# compare characteristics of plot location to imputed data c2014 (cover, height, forest type, and disturbance)

# make table with these variables for TreeMap raster/plots used in imputation
# read in x table for imputation
xtable <- read.table("G:\\TreeMap2016\\x_table\\X_table_all_singlecondition.txt", sep=",", header=T)
# add forest type from FIA data (FLDTYPCD)
states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
plotcount <- as.numeric()
fldtypmaster <- NULL
for (j in 1:length(states))
{
  print(states[j])
  # set up database connection to RSQLite database for each of 50 states
  curdb <- paste("G:\\FIA\\DataMart_SQL_3-10-2021\\FIADB_", states[j], ".db", sep="")
  con = dbConnect(RSQLite::SQLite(), dbname=curdb)
  
  ##tree_table <- NULL
  # get a list of all tables
  ##alltables = dbListTables(con)
  ##alltables
  
  # make table with needed fields from FIA data (all stands)
  fldtyp <- as.data.table(dbGetQuery(con, 'select PLT_CN, FLDTYPCD, CONDID, CONDPROP_UNADJ from COND'))
  ##cnchar <- as.character(fldtyp$PLT_CN)
  ##fldtyp <- cbind(fldtyp, cnchar)
  # subset to list of plots used in TreeMap 2016 in this state
  invec <- xtable$CN %in% fldtyp$PLT_CN
  plotcount[j] <- sum(invec)
  cur_target_stands <- xtable[(invec==TRUE),]
  cnchar <- as.character(cur_target_stands$CN)
  cur_target_stands <- cbind(cur_target_stands, cnchar)
  
  # subset to list of plots used in TreeMap 2016
  fldtyp_filtered <- merge(fldtyp, cur_target_stands, by.x = "PLT_CN", by.y = "cnchar") 
  
  # make master table from all states
  fldtypmaster <- rbind(fldtypmaster, fldtyp_filtered)
  
  dbDisconnect(con)
}
sum(plotcount) # 65652 -- all in x table
# there appears to be on multicondition plot
sort(unique(fldtypmaster$CONDID)) # 1 2
which(fldtypmaster$CONDID==2)
fldtypmaster[c(45327,45328),] # the second condition has only 0.002 of the area and the FLDTYPCD is the same so delete row 45328
fldtypmaster <- fldtypmaster[-45328,]
# columns of interest are FLDTYPCD, disturb_code, canopy_cover, and canopy_height

# now make similar table for the multicondition validation plots ------------------------------------------------------
##multi <- read.dbf("G:\\TreeMap2016\\FIA\\CUI_working_KLR\\FIA_MultiCond_INVYR2016_Albers.dbf")
# read in table that has cover and height assigned for these plots
covht <- read.table("G:\\TreeMap2016\\FVS\\cover_and_height\\allplots_cover_and_height.txt", header=T) # 2984
cvhtgt10 <- covht[(covht$coverpct>=10),]  # 2937
##sum(cvhtgt10$PLT_CN %in% multi$PLT_CN) # 2937 plots
# get disturbance code & field type code

fldtypmaster2 <- NULL
plotcount <- as.numeric()
for (j in 1:length(states))
{
  print(states[j])
  # set up database connection to RSQLite database for each of 50 states
  curdb <- paste("G:\\FIA\\DataMart_SQL_3-10-2021\\FIADB_", states[j], ".db", sep="")
  con = dbConnect(RSQLite::SQLite(), dbname=curdb)
  
  ##tree_table <- NULL
  # get a list of all tables
  ##alltables = dbListTables(con)
  ##alltables
  
  # make table with needed fields from FIA data (all stands)
  fldtyp <- as.data.table(dbGetQuery(con, 'select PLT_CN, FLDTYPCD, CONDID, CONDPROP_UNADJ, DSTRBCD1, DSTRBYR1, DSTRBCD2, DSTRBYR2, DSTRBCD3, DSTRBYR3 from COND'))

  # subset to list of plots used in TreeMap 2016 in this state
  invec <- cvhtgt10$PLT_CN %in% fldtyp$PLT_CN
  plotcount[j] <- sum(invec)
  cur_target_stands <- cvhtgt10[(invec==TRUE),]
  cnchar <- as.character(cur_target_stands$PLT_CN)
  cur_target_stands <- cbind(cur_target_stands, cnchar)
  
  # subset to list of validation plots
  plots_covht <- merge(fldtyp, cur_target_stands, by.x = "PLT_CN", by.y = "cnchar") 
  
  # get MEASYR from the PLOT table
  plot_table <- as.data.table(dbGetQuery(con, 'select CN, INVYR, MEASYEAR from PLOT'))
  cnchar2 <- as.character(plot_table$CN)
  plot_table <- cbind(plot_table, cnchar2)
  
  plots_covht2 <- merge(plots_covht, plot_table, by.x = "PLT_CN", by.y = "cnchar2") 
  
  # make master table
  fldtypmaster2 <- rbind(fldtypmaster2, plots_covht2)
  
}
length(unique(fldtypmaster2$PLT_CN)) # 2937 check
# because these are multicondition, I'll need to summarize the plots by major condition
plotlist <- unique(fldtypmaster2$PLT_CN)
ft <- as.numeric()
covervec <- as.numeric()
distcd1 <- as.numeric()
distyr1 <- as.numeric()
distcd2 <- as.numeric()
distyr2 <- as.numeric()
distcd3 <- as.numeric()
distyr3 <- as.numeric()
measyrvec <- as.numeric()
for (j in 1:length(plotlist))
{
  curmat <- fldtypmaster2[(fldtypmaster2$PLT_CN==plotlist[j]),]
  ftlist <- unique(curmat$FLDTYPCD)
  curcond <- as.numeric()
  # assign forest type of which covers majority of the plot
  for (k in 1:length(ftlist))
  {
    curmat2 <- curmat[(curmat$FLDTYPCD==ftlist[k]),]
    curcond[k] <- sum(curmat2$CONDPROP_UNADJ)
  }
  position <- which(curcond==max(curcond))
  if (length(position)==1) { ft[j] <- ftlist[position] }
  if (length(position)>1) { ft[j] <- ftlist[position[1]] }
  # assign disturbance by majority condition
  position2 <- which(curmat$CONDPROP_UNADJ==max(curmat$CONDPROP_UNADJ))
  if (length(position2)>1) { position2 <- position2[1] }
  distcd1[j] <- curmat$DSTRBCD1[position2]
  distyr1[j] <- curmat$DSTRBYR1[position2]
  distcd2[j] <- curmat$DSTRBCD2[position2]
  distyr2[j] <- curmat$DSTRBYR2[position2]
  distcd3[j] <- curmat$DSTRBCD3[position2]
  distyr3[j] <- curmat$DSTRBYR3[position2]
  # grab MEASYR
  measyrvec[j] <- curmat$MEASYEAR[1]
}
unique(measyrvec) #  2015 2016 2018 2017
sum(measyrvec==2015) # 255
sum(measyrvec==2016) # 1939
sum(measyrvec==2017) # 627
sum(measyrvec==2018) # 116
outmat <- cbind(plotlist, measyrvec, ft, distcd1, distyr1, distcd2, distyr2, distcd3, distyr3) 
outmat <- data.frame(outmat)
# discard plots measured in 2018
outmat <- outmat[(outmat$measyrvec!=2018),] # 2821 plots now
outmat2 <- merge(outmat, cvhtgt10, by.x = "plotlist", by.y = "PLT_CN") 

# assign disturbance codes and years as in other TreeMap efforts
# use 2 for insect and disease damage (FIA codes are 10, 12, 20, 22)
# use 1 for fire damage (FIA codes are 30, 31, 32)
tldist1 <- as.numeric()
tldist2 <- as.numeric()
tldist3 <- as.numeric()
for (j in 1:dim(outmat2)[[1]])
{
  # assign TreeMap disturbance codes to DSTRBCD1
  if (outmat2$distcd1[j]==10 | outmat2$distcd1[j]==12 | outmat2$distcd1[j]==20 | outmat2$distcd1[j]==22) { tldist1[j] <- 2 }
  if (outmat2$distcd1[j]==30 | outmat2$distcd1[j]==31 | outmat2$distcd1[j]==32) { tldist1[j] <- 1 }
  if (outmat2$distcd1[j]!=10 & outmat2$distcd1[j]!=12 & outmat2$distcd1[j]!=20 & outmat2$distcd1[j]!=22 & outmat2$distcd1[j]!=30 & outmat2$distcd1[j]!=31 & outmat2$distcd1[j]!=32) { tldist1[j] <- 0 }
  # assign TreeMap disturbance codes to DSTRBCD2
  if (outmat2$distcd2[j]==10 | outmat2$distcd2[j]==12 | outmat2$distcd2[j]==20 | outmat2$distcd2[j]==22) { tldist2[j] <- 2 }
  if (outmat2$distcd2[j]==30 | outmat2$distcd2[j]==31 | outmat2$distcd2[j]==32) { tldist2[j] <- 1 }
  if (outmat2$distcd2[j]!=10 & outmat2$distcd2[j]!=12 & outmat2$distcd2[j]!=20 & outmat2$distcd2[j]!=22 & outmat2$distcd2[j]!=30 & outmat2$distcd2[j]!=31 & outmat2$distcd2[j]!=32) { tldist2[j] <- 0 }
  # assign TreeMap disturbance codes to DSTRBCD3
  if (outmat2$distcd3[j]==10 | outmat2$distcd3[j]==12 | outmat2$distcd3[j]==20 | outmat2$distcd3[j]==22) { tldist3[j] <- 2 }
  if (outmat2$distcd3[j]==30 | outmat2$distcd3[j]==31 | outmat2$distcd3[j]==32) { tldist3[j] <- 1 }
  if (outmat2$distcd3[j]!=10 & outmat2$distcd3[j]!=12 & outmat2$distcd3[j]!=20 & outmat2$distcd3[j]!=22 & outmat2$distcd3[j]!=30 & outmat2$distcd3[j]!=31 & outmat2$distcd3[j]!=32) { tldist3[j] <- 0 }
}
sort(unique(tldist1))
sort(unique(tldist2))
sort(unique(tldist3))

# if plot disturbed by fire that trumps any insect and disease
disttype <- as.numeric()
distyear <- as.numeric()
for (j in 1:dim(outmat2)[[1]])
{
  testvec <- c(tldist1[j]==1, tldist2[j]==1, tldist3[j]==1) 
  yearvec <- c(outmat2$distyr1[j], outmat2$distyr2[j], outmat2$distyr3[j])
  # check for any fires and if any exist, assign that year
  if (sum(testvec)>=1) { 
    disttype[j] <- 1
    curdist <- which(testvec==1) 
    if (length(curdist)>1)
    {
      distyear[j] <- max(yearvec[curdist], na.rm=TRUE)
      
    }
    if (length(curdist)==1) {
      distyear[j] <- yearvec[curdist]
    }
  }
  # if no fire, check for insect and disease and assign that year
  if (sum(testvec)==0) {
    testvec2 <- c(tldist1[j]==2, tldist2[j]==2, tldist3[j]==2) 
    if (sum(testvec2)==0) { 
      disttype[j] <- 0 
      distyear[j] <- 99 }
    if (sum(testvec2)>=1) {
      disttype[j] <- 2
      curdist <- which(testvec2==1)
      if (length(curdist)>1)
      {
        distyear[j] <- max(yearvec[curdist], na.rm=TRUE)
        
      }
      if (length(curdist)==1) {
        distyear[j] <- yearvec[curdist]
      }
    }
  }
  
}
checkmat <- cbind(outmat2, distyear, disttype)
sum(is.na(distyear)) # none
sum(is.na(disttype)) # none
which((outmat2$distcd1==10 | outmat2$distcd1==12 | outmat2$distcd1==20 | outmat2$distcd1==22) & (outmat2$distcd1==30 | outmat2$distcd1==31 | outmat2$distcd1==32))
# weird, they don't show any plots with both fire and insect and disease
# find disturbance year
sort(unique(checkmat$distyear))
disturb_year <- as.numeric()
for (j in 1:dim(checkmat)[[1]])
{ 
  if (checkmat$distyear[j]==99) { disturb_year[j] <- 99 }
  if (checkmat$distyear[j]==9999) { disturb_year[j] <- 0 }
  if (checkmat$distyear[j]!=99 & checkmat$distyear[j]!=9999)  { disturb_year[j] <- as.numeric(checkmat$measyrvec[j]) - as.numeric(checkmat$distyear[j]) }
}
sort(unique(disturb_year))
checkmat2 <- cbind(checkmat, disturb_year)

# info on cover at plot = coverpct (needs to be rounded to midpoint of bin)
# info on height at plot = heightm (" = 3  8 18 38 70)
covround <- as.numeric()
heightround <- as.numeric()
for (j in 1:dim(checkmat2)[[1]])
{
  # round cover values
  if (checkmat2$coverpct[j]>=10 & checkmat2$coverpct[j]<20) { covround[j] <- 15 }
  if (checkmat2$coverpct[j]>=20 & checkmat2$coverpct[j]<30) { covround[j] <- 25 }
  if (checkmat2$coverpct[j]>=30 & checkmat2$coverpct[j]<40) { covround[j] <- 35 }
  if (checkmat2$coverpct[j]>=40 & checkmat2$coverpct[j]<50) { covround[j] <- 45 }
  if (checkmat2$coverpct[j]>=50 & checkmat2$coverpct[j]<60) { covround[j] <- 55 }
  if (checkmat2$coverpct[j]>=60 & checkmat2$coverpct[j]<70) { covround[j] <- 65 }
  if (checkmat2$coverpct[j]>=70 & checkmat2$coverpct[j]<80) { covround[j] <- 75 }
  if (checkmat2$coverpct[j]>=80 & checkmat2$coverpct[j]<90) { covround[j] <- 85 }
  if (checkmat2$coverpct[j]>=90 & checkmat2$coverpct[j]<=100) { covround[j] <- 95 }
  # round height values
  if (checkmat2$heightm[j]<=5) { heightround[j] <- 3 }
  if (checkmat2$heightm[j]>5 & checkmat2$heightm[j]<=10) { heightround[j] <- 8 }
  if (checkmat2$heightm[j]>10 & checkmat2$heightm[j]<=25) { heightround[j] <- 18 }
  if (checkmat2$heightm[j]>25 & checkmat2$heightm[j]<=50) { heightround[j] <- 38 }
  if (checkmat2$heightm[j]>50) { heightround[j] <- 70 }
}
sort(unique(covround))
covdiff <- abs(covround - checkmat2$coverpct)
hist(covdiff)
sort(unique(heightround))
heightdiff <- abs(heightround - checkmat2$heightm)
hist(heightdiff)
# subset to only columns needed
checkmat3 <- cbind(checkmat2, covround, heightround)
checkmat4 <- checkmat3[,c(1,3,14:17)]
# do some manual checks -- looks good
which(covht$PLT_CN=='14546618020004')
covht[2866,] # cov=25, height=3
which(fldtypmaster2$PLT_CN==14546618020004)
fldtypmaster2[c(5072,5073),]


# now we can finally do some spatial validation!! thank goodness
combine = read.dbf("G:\\TreeMap2016\\FIA\\CUI_working_KLR\\TreeMap_MultiCond_combine_table.dbf")
names(combine)
# ID field in fldtypmaster (raster) corresponds to TreeMap201 field
# need to add ID field to checkmat4
lookup <- read.csv("G:\\TreeMap2016\\FIA\\CUI_coords_2021\\Riley_MultiCond_100AccessibleForest_2016and2018.csv")
ids <- seq(1,dim(lookup)[[1]],by=1)
lookup2 <- cbind(lookup, ids)
lookup2[3419,]
colnames(lookup2) <- c("PLT_CN", "STATE", "COUNTY", "INVYR", "LAT", "LON", "PERCENT_SAMPLED_FOREST", "COND_COUNT", "ids")
checkmat5 <- merge(checkmat4, lookup2, by.x = "plotlist", by.y = "PLT_CN") 
multi <- checkmat5[,c(14,1:6)]
# now we should be golden, this has been awful

# subset combined data to eliminate MEASYEAR=2018 plots
##combine2 <- combine[(combine$FIA_MultiC %in% multi$ids),]
combine2 <- merge(combine, multi, by.x = "FIA_MultiC", by.y = "ids")
length(unique(combine2$TreeMap201)) # 5786
length(unique(combine2$FIA_MultiC)) #  2749
# of 2821 FIA plots in INVYR=2016, 2749 (97.4%) had at least one forested pixel with cell center within 44m (the plot radius)

# spatial validation
anymatchvec <- as.numeric()
plotlist <- sort(unique(combine2$FIA_MultiC))
covanymatchvec <- as.numeric()
weightcovvec <- as.numeric()
htanymatchvec <- as.numeric()
weighthtvec5 <- as.numeric()
weighthtvec10 <- as.numeric()
evganymatchvec <- as.numeric()
distanymatchvec <- as.numeric()
for (j in 1:length(plotlist))
{
  curmat <- combine2[(combine2$FIA_MultiC==plotlist[j]),]
  # see if cover matches...
  currownum <- which(multi$ids==plotlist[j])
  currow <- multi[currownum,] # multicondition plots
  plotcov <- currow$covround
  #...in any pixels or weighted value of all pixels within 40m range (within 10%)
  numimpplot <- dim(curmat)[[1]]
  covmatchcheck <- as.logical()
  pixelccvec <- as.numeric()
  for (k in 1:numimpplot)
  {
    curplot <- curmat$TreeMap201[k]
    curplotrownum <- which(fldtypmaster$ID==curplot)
    curplotrow <- fldtypmaster[curplotrownum,]
    diff <- plotcov - curplotrow$canopy_cover
    if ((diff>=0 & diff<5) | (diff<0 & diff>(-5))) {
      covmatchcheck[k] <- 1
    } else {
      covmatchcheck[k] = 0  } 
    pixelccvec[k] <- curplotrow$canopy_cover
  }
  if (sum(covmatchcheck)==0) { covanymatchvec[j]=0 }
  if (sum(covmatchcheck)>0) { covanymatchvec[j]=1 }
  weightedcover <- sum((pixelccvec * curmat$Count))/sum(curmat$Count)
  if (abs(plotcov-weightedcover)<=10) { weightcovvec[j] <- 1 }
  if (abs(plotcov-weightedcover)>10) { weightcovvec[j] <- 0 }
  # see if height matches in any pixels
  ##plotheightunbin <- currow$height/3.28084 # convert to feet
  ##if (plotheightunbin<=5) { plotheight <- 3 }
  ##if (plotheightunbin>5 & plotheightunbin<=10) { plotheight <- 8 }
  #if (plotheightunbin>10 & plotheightunbin<=25) { plotheight <- 18 }
  ##if (plotheightunbin>25 & plotheightunbin<=50) { plotheight <- 38 }
  ##if (plotheightunbin>50) { plotheight <- 70 }
  htmatchcheck <- as.logical()
  pixelhtvec <- as.logical()
  for (k in 1:numimpplot)
  {
    curplot <- curmat$TreeMap201[k]
    curplotrownum <- which(fldtypmaster$ID==curplot)
    curplotrow <- fldtypmaster[curplotrownum,] # imputed plot
    htmatchcheck[k] <- curplotrow$canopy_height==currow$heightround
    pixelhtvec[k] <- curplotrow$canopy_height
  }
  if (sum(htmatchcheck)==0) { htanymatchvec[j]=0 }
  if (sum(htmatchcheck)>0) { htanymatchvec[j]=1 }
  weightedheight <- sum((pixelhtvec * curmat$Count))/sum(curmat$Count)
  if (abs(currow$heightround-weightedheight)<=10) { weighthtvec10[j] <- 1 }
  if (abs(currow$heightround-weightedheight)>10) { weighthtvec10[j] <- 0 }
  if (abs(currow$heightround-weightedheight)<=5) { weighthtvec5[j] <- 1 }
  if (abs(currow$heightround-weightedheight)>5) { weighthtvec5[j] <- 0 }
  # see if forest type matches in any pixels
  plotevg <- currow$ft
  evgmatchcheck <- as.logical()
  pixelevgvec <- as.logical()
  for (k in 1:numimpplot)
  {
    curplot <- curmat$TreeMap201[k]
    curplotrownum <- which(fldtypmaster$ID==curplot)
    curplotrow <- fldtypmaster[curplotrownum,]
    evgmatchcheck[k] <- curplotrow$FLDTYPCD==plotevg
    pixelevgvec[k] <- curplotrow$FLDTYPCD
  }
  if (sum(evgmatchcheck)==0) { evganymatchvec[j]=0 }
  if (sum(evgmatchcheck)>0) { evganymatchvec[j]=1 }
  # check to see if disturbance matches on any pixels
  plotdist <- currow$disttype
  if (plotdist==2) { plotdist <- 1 } # recode to binary disturbance
  distmatchcheck <- as.logical()
  pixeldistvec <- as.logical()
  for (k in 1:numimpplot)
  {
    curplot <- curmat$TreeMap201[k]
    curplotrownum <- which(fldtypmaster$ID==curplot)
    curplotrow <- fldtypmaster[curplotrownum,]
    distrow <- curplotrow$disturb_code
    if (distrow==2) { distrow <- 1 }
    distmatchcheck[k] <- distrow==plotdist
    pixeldistvec[k] <- distrow
  }
  if (sum(distmatchcheck)==0) { distanymatchvec[j]=0 }
  if (sum(distmatchcheck)>0) { distanymatchvec[j]=1 }
}
sum(covanymatchvec) # the cover of at least one pixel within a 44m radius matched the plot value in 1580/2749 cases (57.5%, versus 44.0% in TreeMap 2014)
sum(weightcovvec) # the weighted cover of pixels within a 44m radius is within 10% of the plot value in 1674/2749 cases (60.9% versus 48.7% in TreeMap 2014)
sum(htanymatchvec) # the height of at least one pixel within a 44m radius matched the plot value in 2200/2749 cases (80.0% versus 85.7% in TreeMap 2014)
sum(weighthtvec5) # the weighted height of pixels within a 44m radius is within 5m of the plot value in 2008/2749 cases (73.0% versus 70.3% in TreeMap 2014)
sum(weighthtvec10) # the weighted height of pixels within a 44m radius is within 10m of the plot value in 2219/2749 cases (80.7% versus 84.0% in TreeMap 2014)
sum(evganymatchvec) # the forest type matched in at least one pixel within a 44m radius of the plot center in 1424/2749 cases (51.8%)
sum(distanymatchvec) # disturbance code matched in at least one pixel within a 44m radius of the plot center in 2404/2749 cases (87.4% versus 90.6% in TreeMap 2014)


# now check major tree species
multitree <- read.csv("G:\\TreeMap2016\\tree_and_stand_tables\\tree_table_Multicond_STATUSCD1_2_INVYR2016.csv")
sort(unique(multitree$STATUSCD))
# subset to live trees
multitreelive <- multitree[(multitree$STATUSCD==1),]
treeba <- (multitreelive$DIA ^ 2) * 0.005454
rowba <- treeba * multitreelive$TPA_UNADJ
plots <- unique(multitreelive$PLT_CN)
idlist <- unique(multitreelive$tl_id)
multitreelive <- cbind(multitreelive, rowba)
baout <- NULL
sp1 <- as.numeric()
sp2 <- rep(NA,length(plots))
errorvec <- rep(NA, length(plots))
for (j in 1:length(plots))
{
  curmat <- multitreelive[(multitreelive$PLT_CN==plots[j]),]
  # sum by species code
  species <- sort(unique(curmat$SPCD))
  spba <- as.numeric()
  for (k in 1:length(species))
  {
    spmat <- curmat[(curmat$SPCD==species[k]),]
    spba[k] <- sum(spmat$rowba)
  }
  plotvec <- rep(curmat$PLT_CN[1], length(species))
  idvec <- rep(curmat$tl_id[1], length(species))
  plotmat <- cbind(idvec, plotvec, species, spba)
  baout <- rbind(baout, plotmat)
  # make vector with top two species for each plot
  sortmat <- plotmat[order(plotmat[,4], decreasing=TRUE),]
  sortmat <- matrix(sortmat, ncol=4)
  sp1[j] <- sortmat[1,3] 
  if (dim(sortmat)[[1]]>1) { sp2[j] <- sortmat[2,3] }
}
write.table(baout, "G:\\TreeMap2016\\working_KLR\\BA_by_species_and_multicond_plot.txt")
sum(is.na(sp1)) # none
maxsp2 <- cbind(idlist, plots, sp1, sp2)
maxspm <- data.frame(maxsp2)
write.table(maxsp2, "G:\\TreeMap2016\\working_KLR\\species_with_highest_BA_for_multicond_plots.txt")

# now calculate this for the plots used in the imputation
targettree <- read.csv("G:\\TreeMap2016\\tree_and_stand_tables\\tree_table_national_STATUSCD1_2.csv")
sort(unique(targettree$STATUSCD))
# subset to live trees
targettreelive <- targettree[(targettree$STATUSCD==1),]
treeba <- (targettreelive$DIA ^ 2) * 0.005454
rowba <- treeba * targettreelive$TPA_UNADJ
plots <- unique(targettreelive$PLT_CN)
idlist <- unique(targettreelive$tl_id)
targettreelive <- cbind(targettreelive, rowba)
baout <- NULL
sp1 <- as.numeric()
sp2 <- rep(NA,length(plots))
errorvec <- rep(NA, length(plots))
for (j in 1:length(plots))
{
  curmat <- targettreelive[(targettreelive$PLT_CN==plots[j]),]
  # sum by species code
  species <- sort(unique(curmat$SPCD))
  spba <- as.numeric()
  for (k in 1:length(species))
  {
    spmat <- curmat[(curmat$SPCD==species[k]),]
    spba[k] <- sum(spmat$rowba)
  }
  plotvec <- rep(curmat$PLT_CN[1], length(species))
  idvec <- rep(curmat$tl_id[1], length(species))
  plotmat <- cbind(idvec, plotvec, species, spba)
  baout <- rbind(baout, plotmat)
  # make vector with top two species for each plot
  sortmat <- plotmat[order(plotmat[,4], decreasing=TRUE),]
  sortmat <- matrix(sortmat, ncol=4)
  sp1[j] <- sortmat[1,3] 
  if (dim(sortmat)[[1]]>1) { sp2[j] <- sortmat[2,3] }
}
write.table(baout, "G:\\TreeMap2016\\working_KLR\\BA_by_species_and_multicond_plot_target_plots.txt")
sum(is.na(sp1)) # none
maxsp <- cbind(idlist, plots, sp1, sp2) # 55609 plots
maxspt <- data.frame(maxsp)
write.table(maxsp, "G:\\TreeMap2016\\working_KLR\\species_with_highest_BA_for_target_plots.txt")


# now compare species in multicond and plots used in TreeMap2016
# subset to plots with cover greater than 10% (done already) and with tree species populated
##combine3 <- merge(combine2, maxspm, by.x = "FIA_MultiC", by.y = "idlist")
plots <- unique(combine2$FIA_MultiC)
sp1match <- as.logical()
sp2match <- as.logical()
sp1_2match <- as.logical()
sp2_1match <- as.logical()
errorvec <- as.numeric()
for (j in 1:length(plots))
{
  # subset to one plot at a time in the combine
  curmat <- combine2[(combine2$FIA_MultiC==plots[j]),]
  # lookup row in species table for multicondition plots
  multirow <- maxspm[(maxspm$idlist==curmat$FIA_MultiC[1]),]
  sp1temp <- as.logical()
  sp2temp <- as.logical()
  sp1_2temp <- as.logical()
  sp2_1temp <- as.logical()
  for (k in 1:dim(curmat)[[1]])
       {
         # lookup row in species table for target plots
          targetrow <- maxspt[(maxspt$idlist==curmat$TreeMap201[k]),]
          # check for matches in species
          sp1temp[k] <- multirow$sp1==targetrow$sp1
          ##if (length(multirow$sp1==targetrow$sp1)>1) { errorvec <- append(errorvec, j)  }
          sp2temp[k] <- multirow$sp2==targetrow$sp2
          sp1_2temp[k] <- multirow$sp1==targetrow$sp2
          sp2_1temp[k] <- multirow$sp2==targetrow$sp1
       }
  sp1match[j] <- "TRUE" %in% sp1temp
  sp2match[j] <- "TRUE" %in% sp2temp
  sp1_2match[j] <- "TRUE" %in% sp1_2temp
  sp2_1match[j] <- "TRUE" %in% sp2_1temp
}
sum(sp1match) # 1385/2749 = 50.3%
sum(sp2match) # 743
sum(sp1_2match) # 786
sum(sp2_1match) # 771

allvec <- (sp1match=="TRUE" | sp2match=="TRUE" | sp1_2match=="TRUE" | sp2_1match=="TRUE")
sum(allvec)  # 2198/2749 = 80.0%