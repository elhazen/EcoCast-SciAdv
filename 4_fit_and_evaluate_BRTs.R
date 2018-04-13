## Fits and evaluates Boosted Regression Tree models 

## define fit and evaluate functions
fit.brt <- function(x,gbm.x,gbm.y,tree.com,bag.frac,lr){
    y <- gbm.step(x, 
    gbm.x = gbm.x,
    gbm.y = gbm.y,
    family = "bernoulli",
    tree.complexity = tree.com,
    learning.rate = lr, 
    bag.fraction = bag.frac)
    y$gbm.call$dataframe<-x
    return(y)
}

validate.brt <- function(model,valdata){
	preds <- predict.gbm(model, valdata, n.trees=model$gbm.call$best.trees, type='response')
	auc <- roc(valdata$presabs, preds)  ##using auc as validation statistic 
	return(auc)
}

## read in species records and associated environmental data
records <-readRDS('outdir/obsdata.rds')
records$TotCat[is.na(records$TotCat)] <- 0 ; unique(records$TotCat)
records$X <- seq(1, length(records[,1])) ; records$presabs <- NA
records$presabs[records$TotCat==0] <- 0 ; records$presabs[records$TotCat>0] <- 1 ; unique(records$presabs)
records$dt <- as.POSIXct(records$dt, '%m/%d/%y', tz='UTC') ; records$month <- format(records$dt, '%m')
range(records$dt)

## fit and validate model
recordsSubset <- subset(records, select=c('X','trip_set','TotCat','presabs','dt','lat','lon','jpl_new_mean','jpl_new_sd',
    'log_blendChl','windy_new_mean','eke_mean','AVISOh_new_mean','AVISOh_new_sd','z','zsd','lunillum'))
head(recordsSubset)
length(names(recordsSubset))

recordsSubset$RN <- NA
for (i in 1:length(recordsSubset[,1])){
    recordsSubset$RN[i] <- sample.int(99,size=1,replace=T,prob=NULL)
}

lr = 0.045
fittedModel <- fit.brt(recordsSubset,gbm.x=8:18,gbm.y=4,tree.com=3,bag.frac=0.6,lr=lr)
validatedModel <- validate.brt(model=fittedModel,valdata=recordsSubset)
saveRDS(fittedModel,'modelDirectory/fittedModel.rds')
gbm.plot.mac(fittedModel, n.plots=11, write.title = F, smooth=T) 

## Cross-validation: leave one out by year  
lr=0.045 
year_seq <- seq(1990,2015,by=1)

# make list of training datasets with each year in sequence removed (list1) and held back for validation (list2)
records.trainL <- list()
for (i in 1:length(year_seq)){
  selRows <- records$yr %in% year_seq[i]
  records.trainL[[i]] <- records[!selRows,]
}
records.testL <- list()
for (i in 1:length(year_seq)){
  selRows <- records$yr %in% year_seq[i]
  records.testL[[i]] <- records[selRows,]
}
names(records.trainL)<-sprintf("records%i",1:length(records.trainL))
names(records.testL)<-sprintf("records%i",1:length(records.testL))
head(records.trainL[[1]])
length(records.trainL)
head(records.trainL[[1]][,c(6:18,21)])

for (i in 1:length(records.trainL)){
    records.trainL[[i]]$month <- as.factor(records.trainL[[i]]$month)
}

## Pseudo-R2
pseudoR2.BRT <- function(x){
    d2 <- 1-(x$self.statistics$mean.resid/x$self.statistics$mean.null)
    return(d2)
}
pseudoR2 <- pseudoR2.BRT(fittedModel)

## Explained deviance
DN <- fittedModel$self.statistics$mean.null 
DR <- fittedModel$cv.statistics$deviance.mean 
DE <- (DN-DR)*100/DN; DE

## K-folds cross validation
year_seq <- seq(1990,2015,by=1)
records.trainL <- list()
for (i in 1:length(year_seq)){
    selRows <- records$yr %in% year_seq[i]
    records.trainL[[i]] <- records[!selRows,]
}
records.testL <- list()
for (i in 1:length(year_seq)){
    selRows <- records$yr %in% year_seq[i]
    records.testL[[i]] <- records[selRows,]
}
names(records.trainL)<-sprintf("sw%i",1:length(records.trainL))
names(records.testL)<-sprintf("sw%i",1:length(records.testL))

head(records.trainL[[1]])
names(records.trainL[[1]])

fittedModelKfolds <- lapply(swN.trainL, FUN=fit.brt,gbm.x=c(7,12:13,19:20,22),gbm.y=9,tree.com=1,bag.frac=0.6,lr=lr) 
saveRDS(fittedModelKfolds,'modelDirectory/fittedModelKfolds.rds')
validatedModelKfolds <- mapply(FUN=validate.brt, x=fittedModelKfolds, y=records.testL)
mean(validatedModelKfolds) 




