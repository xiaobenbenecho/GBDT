library(glmnet)
library(data.table)
library(here)
library(fasttime)
library(fst)
library(xgboost)
library(dygraphs)
library(precrec)
library(ggplot2)
library(lubridate)
library(plotROC)
library(gtools)
library(Ckmeans.1d.dp)
library(pROC)
library(e1071)
library(lattice)
library(caret)

###################################

###################################


# train data
#trainfile <- paste0(name,"traindpca.RData")
# Load train matrix, vars etc.
load(here::here(name,"traindpca.RData"))


#load("resdpca.RData")
######
## The XGB starts here
#####

# XGB matrix
Xtrain <- Xtrain1

dtrain <- xgb.DMatrix(Xtrain, label=ytrain)
# Keep only the matrix to save space
rm(list=setdiff(ls(),c("dtrain","name","param","ytrain")))

# set the seed, for partial reproducibility
set.seed(14)

# equisized folds, there are better approaches but this one is quite handy
folds <- 4L
fvalues <- round(quantile(1:nrow(dtrain),seq(0,1,length.out=(folds+1L))))
custom.folds <- vector("list",folds)
for(i in 1:folds){
  custom.folds[[i]] <- fvalues[[i]]:fvalues[[i+1L]]
}

# Cross-validated xgboost
cvxgb <- xgb.cv(params=param,dtrain,stopping_metric="auc",metrics="auc",stratified = True,folds=custom.folds,
                early_stopping_rounds=100,nrounds=10000,prediction = TRUE)


# Train model
trainxgb <- xgb.train(params=param,dtrain,nrounds=cvxgb$best_iteration)


# Train diagnostics
#dir.create(here::here(name))


# Parameters
write.csv(param,file=here::here(name,"param.txt"),row.names = FALSE)

# Performance
fwrite(cvxgb$evaluation_log[cvxgb$best_iteration,],
       file=here::here(name,"perfcv.txt"))



# Model deepness
deepness <- xgb.ggplot.deepness(trainxgb)
ggsave(file=here::here(name,"deepness1.png"),
       plot=deepness[[1]], height=5,width=8)
ggsave(file=here::here(name,"deepness2.png"),
       plot=deepness[[2]], height=5,width=8)


# importance matrix
importance_matrix <- xgb.importance(colnames(dtrain), model = trainxgb)
ggsave(file=here::here(name,"importance.png"),
       plot=xgb.ggplot.importance(head(importance_matrix,40), rel_to_first = FALSE, xlab = "Importance") + scale_fill_discrete(guide=FALSE),
       height=8,width=8)
write.csv(importance_matrix,file=here::here(name,"importanceMatrix.csv"))

# xgb model
xgb.save(trainxgb,here::here(name,"xgbmodel.model"))



# cut point
# Used to decide the cut point
predictioncv <- cvxgb$pred
approxRoc <- data.table(calculate_roc(predictioncv,ytrain))
approxRoclogit <- data.table(calculate_roc(gtools::logit(predictioncv),ytrain))


fwrite(approxRoc,here::here(name,"cutpoints.csv"))
rocdata <- data.table(predictionlogit=gtools::logit(predictioncv),ytrain) 



#logit Roc
logitroc <- ggplot(rocdata, aes(m = predictionlogit, d = ytrain)) + geom_roc()
ggsave(file=here::here(name,"logitroc.png"),
       plot=logitroc, height=8,width=8)
# cvscreo
fwrite(cvxgb$evaluation_log[cvxgb$best_iteration,],
       file=here::here(name,"perfcv.txt"))


rm(dtrain)

######³
######################read xgb model#######################
xgb.load(here::here(name,"xgbmodel.model"))
# Now the test related stuff
######
#testfile <- paste0(name,"testdpca.RData")
load(here::here(name,"testdpca.RData"))

Xtest <- Xtest1
dtest <-  xgb.DMatrix(Xtest)

# predict value
#testvals[,prediction:=predict(trainxgb,dtest)]
#testvals[,logitprediction:=gtools::logit(prediction)]

prediction <- predict(trainxgb,dtest)
testvals$prediction = prediction
testvals$logitprediction = gtools::logit(prediction)

testvals[,Daytime:=Time-max(Time),Date]
testvals[Daytime==0,BlockNumber_Clean:=NA]

testvals[inRun!=1,prediction:=NA]

# AUC value in test set
write(testvals[inRun==1L,pROC::auc(as.double(stopvalid), prediction)],
      file=here::here(name, "perfecttest.txt"))

max_acc = 0
for (number in seq(min(prediction), max(prediction), by = 0.00001)){
  prediction.01<- ifelse(prediction > number, '1', '0')#quantile(prediction, 0.92), '1', '0')
  xtab.01 <- table(as.factor(prediction.01), as.factor(testvals$stopvalid))
  cm <- caret::confusionMatrix(xtab.01, positive = '1')
  temp <- cm$byClass['Balanced Accuracy']
  if (temp > max_acc){
    max_acc <- temp
    print(number)
    print(cm$byClass['Balanced Accuracy'])
  }
}
prediction.01<- ifelse(prediction > 0.04152203 , '1', '0')#quantile(prediction, 0.92), '1', '0')
xtab.01 <- table(as.factor(prediction.01), as.factor(testvals$stopvalid))
cm <- caret::confusionMatrix(xtab.01, positive = '1')
print(cm)
write(cm$byClass['Balanced Accuracy'],
      file=here::here(name, "baccuracy.txt"))
#beta <- 10000
#f1 <- (1+beta^2)*cm$byClass['Precision']*cm$byClass['Recall']/((beta^2 * cm$byClass['Precision'])+cm$byClass['Recall'])
#print(f1)

# Result charts
# sscurves AUC
sscurves <- testvals[inRun==TRUE,evalmod(scores = prediction, labels = stopvalid)]
ggsave(file=here::here(name,"sscurves.png"),plot=autoplot(sscurves),
       height=5,width=8)


# distribution of response
resulttable <- testvals[inRun==1L,list(ytest=factor(as.double(stopvalid)),
                            ypredtest=gtools::logit(prediction))]

p <- ggplot(resulttable,aes(x=ypredtest,fill=ytest,colour=ytest))+
  geom_density(alpha=0.4)+
  scale_fill_manual(values=c("#E69F00", "#56B4E9"))+
  scale_color_manual(values=c("black","black"))

ggsave(file=here::here(name,"logitdistri.png"),
       plot=p,height=8,width=5)

# Generate Dygraph
d <- dygraph(testvals[,.(Time,prediction,BlockNumber_Clean)])%>%
  dySeries("BlockNumber_Clean",axis="y2")

Intervals <- testvals[inRun==1L&stopvalid==TRUE,.(miTi=min(Time),mxTi=max(Time)),
                  by=c("stopintervals")]
for(i in 1:Intervals[,.N]){
  d <- d%>%dyShading(Intervals[i,miTi],Intervals[i,mxTi],color = "red")
}
# Save dygraph
try(htmlwidgets::saveWidget(d,file=here::here(name,"result.html")))


testvals[,prediction:=(prediction-min(prediction,na.rm=TRUE))/
           (max(prediction,na.rm=TRUE)-min(prediction,na.rm=TRUE))]

d <- dygraph(testvals[,.(Time,prediction,BlockNumber_Clean)])%>%
  dySeries("BlockNumber_Clean",axis="y2")

for(i in 1:Intervals[,.N]){
  d <- d%>%dyShading(Intervals[i,miTi],Intervals[i,mxTi],color = "red")%>%
    dyAxis(name="y",valueRange=c(0,1))
}
# Save dygraph
try(htmlwidgets::saveWidget(d,file=here::here(name,"result2.html")))

d <- dygraph(testvals[,.(Time,logitprediction,BlockNumber_Clean)])%>%
  dySeries("BlockNumber_Clean",axis="y2")

for(i in 1:Intervals[,.N]){
  d <- d%>%dyShading(Intervals[i,miTi],Intervals[i,mxTi],color = "red")
}
# Save dygraph
try(htmlwidgets::saveWidget(d,file=here::here(name,"resultlogit.html")))

# experimental sum(ytrain)/length(ytrain)/10 is arbitrary
# it somehow weight the class imbalance an takes the 10%
fpfprop <- sum(ytrain)/length(ytrain)*0.1
cpoint <- min(approxRoclogit[FPF<fpfprop]$c)

testvals[,iswarn:=as.integer(logitprediction>cpoint)]
testvals[iswarn==0L,iswarn:=NA]

d <- dygraph(testvals[,.(Time,BlockNumber_Clean,iswarn)])%>%
  dySeries("BlockNumber_Clean",axis="y2")%>%
  dySeries("iswarn",drawPoints=TRUE)%>%
  dyAxis(name="y",valueRange=c(0,1.1))

for(i in 1:Intervals[,.N]){
  d <- d%>%dyShading(Intervals[i,miTi],Intervals[i,mxTi],color = "red")
}
try(htmlwidgets::saveWidget(d,file=here::here(name,"resultwarns.html")))

d <- dygraph(testvals[,.(Time,Cnc_Program_BlockNumber_EC,iswarn)])%>%
  dySeries("Cnc_Program_BlockNumber_EC",axis="y2")%>%
  dySeries("iswarn",drawPoints=TRUE)%>%
  dyAxis(name="y",valueRange=c(0,1.1))

for(i in 1:Intervals[,.N]){
  d <- d%>%dyShading(Intervals[i,miTi],Intervals[i,mxTi],color = "red")
}
try(htmlwidgets::saveWidget(d,file=here::here(name,"resultwarns2.html")))

