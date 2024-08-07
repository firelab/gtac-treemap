#yaimpute example
library(yaImpute)


# load data
data("MoscowMtStJoe")
x <- MoscowMtStJoe[, c("EASTING", "NORTHING", "ELEVMEAN", 
                       "SLPMEAN", "ASPMEAN", "INTMEAN", "HTMEAN", "CCMEAN")]
x[, 5] <- (1 - cos((x[, 5] - 30) * pi/180))/2 
names(x)[5] = "TrASP" 
y <- MoscowMtStJoe[, c(1, 9, 12, 14, 18)] 

# run models
mal <- yai(x = x, y = y, method = "mahalanobis") 
msn <- yai(x = x, y = y, method = "msn") 
gnn <- yai(x = x, y = y, method = "gnn") 
ica <- yai(x = x, y = y, method = "ica")

#Method randomForest works best when there are few variables and when factors are used rather than continuous variables. The whatsMax function is used to create a data frame of containing a list of the species of maximum basal area, and two other related variables.

y2 <- cbind(whatsMax(y[, 1:4]), y[, 5]) 
names(y2) <- c("MajorSpecies", "BasalAreaMajorSp", "TotalBA") 
rf <- yai(x = x, y = y2, method = "randomForest") 
head(y2)

foruse(rf, 2)
