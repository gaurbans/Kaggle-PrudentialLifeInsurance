setwd('~/Prudential') #Set to whatever working directory is. 

#Load data
trn <- read.csv("train.csv", header=T)
tst <- read.csv("test.csv", header=T)

#Convert all variables to categorical
trn <- as.data.frame(apply(trn,2, as.factor))
tst <- as.data.frame(apply(tst,2, as.factor))

#Convert numeric variables to numeric
trn$Id <- as.numeric(as.character(trn$Id))
trn$Product_Info_4 <- as.numeric(as.character(trn$Product_Info_4))
trn$Ins_Age <- as.numeric(as.character(trn$Ins_Age))
trn$Ht <- as.numeric(as.character(trn$Ht))
trn$Wt <- as.numeric(as.character(trn$Wt))
trn$BMI <- as.numeric(as.character(trn$BMI))
trn$Employment_Info_1 <- as.numeric(as.character(trn$Employment_Info_1))
trn$Employment_Info_4 <- as.numeric(as.character(trn$Employment_Info_4))
trn$Employment_Info_6 <- as.numeric(as.character(trn$Employment_Info_6))
trn$Insurance_History_5 <- as.numeric(as.character(trn$Insurance_History_5))
trn$Family_Hist_2 <- as.numeric(as.character(trn$Family_Hist_2))
trn$Family_Hist_3 <- as.numeric(as.character(trn$Family_Hist_3))
trn$Family_Hist_4 <- as.numeric(as.character(trn$Family_Hist_4))
trn$Family_Hist_5 <- as.numeric(as.character(trn$Family_Hist_5))

tst$Id <- as.numeric(as.character(tst$Id))
tst$Product_Info_4 <- as.numeric(as.character(tst$Product_Info_4))
tst$Ins_Age <- as.numeric(as.character(tst$Ins_Age))
tst$Ht <- as.numeric(as.character(tst$Ht))
tst$Wt <- as.numeric(as.character(tst$Wt))
tst$BMI <- as.numeric(as.character(tst$BMI))
tst$Employment_Info_1 <- as.numeric(as.character(tst$Employment_Info_1))
tst$Employment_Info_4 <- as.numeric(as.character(tst$Employment_Info_4))
tst$Employment_Info_6 <- as.numeric(as.character(tst$Employment_Info_6))
tst$Insurance_History_5 <- as.numeric(as.character(tst$Insurance_History_5))
tst$Family_Hist_2 <- as.numeric(as.character(tst$Family_Hist_2))
tst$Family_Hist_3 <- as.numeric(as.character(tst$Family_Hist_3))
tst$Family_Hist_4 <- as.numeric(as.character(tst$Family_Hist_4))
tst$Family_Hist_5 <- as.numeric(as.character(tst$Family_Hist_5))

#Convert discrete variables to integer
trn$Medical_History_1 <- as.integer(as.character(trn$Medical_History_1))
trn$Medical_History_10 <- as.integer(as.character(trn$Medical_History_10))
trn$Medical_History_15 <- as.integer(as.character(trn$Medical_History_15))
trn$Medical_History_24 <- as.integer(as.character(trn$Medical_History_24))
trn$Medical_History_32 <- as.integer(as.character(trn$Medical_History_32))

tst$Medical_History_1 <- as.integer(as.character(tst$Medical_History_1))
tst$Medical_History_10 <- as.integer(as.character(tst$Medical_History_10))
tst$Medical_History_15 <- as.integer(as.character(tst$Medical_History_15))
tst$Medical_History_24 <- as.integer(as.character(tst$Medical_History_24))
tst$Medical_History_32 <- as.integer(as.character(tst$Medical_History_32))

#Convert Response to integer
trn$Response <- as.integer(as.character(trn$Response))

#Convert Medical_KeyWord_.* variables to numeric as well
trnMK <- data.frame(sapply(trn[grep('Medical_Keyword', names(trn))], function(x) as.numeric(as.character(x))))
trn <- trn[-grep('Medical_Keyword', names(trn))]
trn <- cbind(trnMK, trn)

tstMK <- data.frame(sapply(tst[grep('Medical_Keyword', names(tst))], function(x) as.numeric(as.character(x))))
tst <- tst[-grep('Medical_Keyword', names(tst))]
tst <- cbind(tstMK, tst)

rm(trnMK, tstMK)

#Remove Id as a predictor
trn <- trn[-grep('Id', names(trn))]

tstId <- tst[grep('Id', names(tst))]
tst <- tst[-grep('Id', names(tst))]

#Plot Response by each predictor 
for (i in 1:(length(trn)-1)){
  png(file=paste0("Respby", names(trn)[i], ".png"), width=800, height=600)
  plot(trn[,i], trn$Response, main=paste("Response By", names(trn)[i], sep=" "), xlab=names(trn)[i])
  dev.off()
}

#Look at missing values
trnNAs <- sapply(trn, function(x) sum(is.na(x))/nrow(trn))
trnNAcols <- which(trnNAs>0)
trnNAs <- sort(trnNAs[which(trnNAs>0)])

tstNAs <- sapply(tst, function(x) sum(is.na(x))/nrow(tst))
tstNAcols <- which(tstNAs>0)
tstNAs <- sort(tstNAs[which(tstNAs>0)])
##All missing values are in numeric or integer data.

#Separate out numeric and categorical data
trnnum <- trn[sapply(trn, is.numeric)]
trnfac <- trn[sapply(trn, is.factor)]

tstnum <- tst[sapply(tst, is.numeric)]
tstfac <- tst[sapply(tst, is.factor)]

rm(trn, tst)

#Look at variable ranges for missing and non-missing data for each variable with missing data
rangediff <- function(variable){
  dif <- data.frame()
  Missing <- trnnum[is.na(trnnum[,variable]), ]
  Complete <- trnnum[!is.na(trnnum[,variable]), ]
  MissingRange <- data.frame(sapply(Missing, range, na.rm=T))
  CompleteRange <- data.frame(sapply(Complete, range, na.rm=T))
  for(i in 1:length(CompleteRange)){
    if(CompleteRange[1,i]!=MissingRange[1,i] & CompleteRange[2,i]!=MissingRange[2,i]){
      dif <- data.frame(c(dif, rbind(MissingRange[i], CompleteRange[i])))
    }
  }
  print(variable)
  print(dif)
}
sapply(names(trnNAs), rangediff)
##Notice that Family_Hist_4 missing values occur when Family_Hist_5 does not have missing values and vice versa.
##Family_Hist_2 missing values occur when Family_Hist_3 does not have missing values and vice versa.

#Use column medians to impute missing values for Employment_Info_1 and Employment_Info_6
trnnum[is.na(trnnum$Employment_Info_1), 'Employment_Info_1'] <- median(trnnum$Employment_Info_1, na.rm=T)
trnnum[is.na(trnnum$Employment_Info_4), 'Employment_Info_4'] <- median(trnnum$Employment_Info_4, na.rm=T)

tstnum[is.na(tstnum$Employment_Info_1), 'Employment_Info_1'] <- median(tstnum$Employment_Info_1, na.rm=T)
tstnum[is.na(tstnum$Employment_Info_4), 'Employment_Info_4'] <- median(tstnum$Employment_Info_4, na.rm=T)

#Create binary variables to indicate where missing values are
trnnum$Medical_History_1missing <- 0
trnnum$Employment_Info_6missing <- 0
trnnum$Family_Hist_4missing <- 0
trnnum$Insurance_History_5missing <- 0
trnnum$Family_Hist_2missing <- 0
trnnum$Family_Hist_3missing <- 0
trnnum$Family_Hist_5missing <- 0
trnnum$Medical_History_15missing <- 0
trnnum$Medical_History_24missing <- 0
trnnum$Medical_History_32missing <- 0
trnnum$Medical_History_10missing <- 0
trnnum[is.na(trnnum$Medical_History_1), 'Medical_History_1missing'] <- 1
trnnum[is.na(trnnum$Employment_Info_6), 'Employment_Info_6missing'] <- 1
trnnum[is.na(trnnum$Family_Hist_4), 'Family_Hist_4missing'] <- 1
trnnum[is.na(trnnum$Insurance_History_5), 'Insurance_History_5missing'] <- 1
trnnum[is.na(trnnum$Family_Hist_2), 'Family_Hist_2missing'] <- 1
trnnum[is.na(trnnum$Family_Hist_3), 'Family_Hist_3missing'] <- 1
trnnum[is.na(trnnum$Family_Hist_5), 'Family_Hist_5missing'] <- 1
trnnum[is.na(trnnum$Medical_History_15), 'Medical_History_15missing'] <- 1
trnnum[is.na(trnnum$Medical_History_24), 'Medical_History_24missing'] <- 1
trnnum[is.na(trnnum$Medical_History_32), 'Medical_History_32missing'] <- 1
trnnum[is.na(trnnum$Medical_History_10), 'Medical_History_10missing'] <- 1

tstnum$Medical_History_1missing <- 0
tstnum$Employment_Info_6missing <- 0
tstnum$Family_Hist_4missing <- 0
tstnum$Insurance_History_5missing <- 0
tstnum$Family_Hist_2missing <- 0
tstnum$Family_Hist_3missing <- 0
tstnum$Family_Hist_5missing <- 0
tstnum$Medical_History_15missing <- 0
tstnum$Medical_History_24missing <- 0
tstnum$Medical_History_32missing <- 0
tstnum$Medical_History_10missing <- 0
tstnum[is.na(tstnum$Medical_History_1), 'Medical_History_1missing'] <- 1
tstnum[is.na(tstnum$Employment_Info_6), 'Employment_Info_6missing'] <- 1
tstnum[is.na(tstnum$Family_Hist_4), 'Family_Hist_4missing'] <- 1
tstnum[is.na(tstnum$Insurance_History_5), 'Insurance_History_5missing'] <- 1
tstnum[is.na(tstnum$Family_Hist_2), 'Family_Hist_2missing'] <- 1
tstnum[is.na(tstnum$Family_Hist_3), 'Family_Hist_3missing'] <- 1
tstnum[is.na(tstnum$Family_Hist_5), 'Family_Hist_5missing'] <- 1
tstnum[is.na(tstnum$Medical_History_15), 'Medical_History_15missing'] <- 1
tstnum[is.na(tstnum$Medical_History_24), 'Medical_History_24missing'] <- 1
tstnum[is.na(tstnum$Medical_History_32), 'Medical_History_32missing'] <- 1
tstnum[is.na(tstnum$Medical_History_10), 'Medical_History_10missing'] <- 1

#Set remaining missing values to 0 
trnnum[is.na(trnnum)] <- -1
tstnum[is.na(tstnum)] <- -1

#Set levels in tstfac that do not exist in trnfac to NA
for(i in 1:length(tstfac)){
  tstfac[which(!(tstfac[,i] %in% trnfac[,i])), i] <- NA
}
tstfac <- droplevels(tstfac)

#Use knn imputation to impute 
library(DMwR)
tstfac <- knnImputation(tstfac)

#Insert into tst levels for categorical data from trn 
for(i in 1:length(tstfac)){
  levels(tstfac[,i]) <- levels(trnfac[,i])
}

#Merge trnfac with trnnum and tstfac with tstnum
trn <- cbind(trnfac, trnnum)
tst <- cbind(tstfac, tstnum)
rm(trnnum, trnfac, tstnum, tstfac)
rm(trnNAcols, trnNAs, tstNAcols, tstNAs)

#Convert all categorical variables to binary dummy variables
library(caret)
trndmy <- dummyVars(~.-Response, trn)
trndmy <- data.frame(predict(trndmy, trn))
trndmy <- cbind(trndmy, trn$Response)
names(trndmy)[length(trndmy)] <- "Response"

tstdmy <- dummyVars(~., tst)
tstdmy <- data.frame(predict(tstdmy, tst))

#Look at correlations in data
cortrn <- abs(cor(trndmy[-length(trndmy)]))
highCortrn <- findCorrelation(cortrn, cutoff=0.80)
trndmy <- trndmy[, -highCortrn]
tstdmy <- tstdmy[, -highCortrn]

rm(cortrn, highCortrn)

#Remove variables with two unique values but only one observation of one of them (ie variance <1.7e-5)
lowVartrn <- sapply(trndmy, var)
lowVarColstrn <- which(lowVartrn<1.7e-5)
lowVartrn <- lowVartrn[lowVarColstrn]

trndmy <- trndmy[-lowVarColstrn]
tstdmy <- tstdmy[-lowVarColstrn]

rm(lowVartrn, lowVarColstrn)

#Remove zero variance and near zero variance variables and convert Response to categorical variable
nearzero <- preProcess(trndmy, method='nzv')
nzvtrndmy <- predict(nearzero, trndmy)
nzvtstdmy <- predict(nearzero, tstdmy)

rm(nearzero)

#Run pca on data
pca <- preProcess(trndmy[-length(trndmy)], method=c('center', 'scale', 'pca'))
pcatrndmy <- predict(pca, trndmy[-length(trndmy)])
pcatrndmy <- cbind(pcatrndmy, trndmy$Response)
pcatstdmy <- predict(pca, tstdmy)

rm(pca)

#Linear regression 
lmfit <- train(Response~., data=trndmy, method='lm')

lmpred <- predict(lmfit, tstdmy)
lmpred[which(lmpred<1)] <- 1
lmpred[which(lmpred>8)] <- 8
lmpred <- round(lmpred)

lmsubmission <- cbind(tstId, lmpred)
names(lmsubmission)[2] <- "Response"
write.csv(lmsubmission, 'lmSubmission.csv', row.names=F)

#LDA model with Response as categorical without zero and near zero variance variables
ldafit <- train(factor(Response)~., data=nzvtrndmy, method='lda')

ldapred <- predict(ldafit, nzvtstdmy)
ldasubmission <- cbind(tstId, ldapred)
names(ldasubmission)[2] <- 'Response'
write.csv(ldasubmission, 'ldasubmission.csv', row.names=F)

#xgboost linear model with Response as integer without zero and near zero variance variables
s <- Sys.time()
xgblfit <- train(Response~., data=nzvtrndmy, method='xgbLinear')
Sys.time()-s

xgblpred <- predict(xgblfit, nzvtstdmy)
xgblpred[which(xgblpred<1)] <- 1
xgblpred[which(xgblpred>8)] <- 8
xgblpred <- round(xgblpred)

xgblsubmission <- cbind(tstId, xgblpred)
names(xgblsubmission)[2] <- 'Response'
write.csv(xgblsubmission, 'xgblsubmission.csv', row.names=F)

#xgboost linear model with Response as integer top 21 variables based on xgblfit
varImp(xgblfit)

s <- Sys.time()
xgbl21fit <- train(Response~BMI+Medical_History_15+Medical_History_4.2+Product_Info_4+Ins_Age+Medical_Keyword_15+
  Medical_History_3.91+InsuredInfo_6.2+Employment_Info_1+Medical_History_1+Medical_History_13.1+Family_Hist_4+
  Product_Info_2.D1+Ht+Medical_History_2.82+Medical_History_18.2+Medical_History_3.33+Employment_Info_3.3+
  Medical_History_6.1+Employment_Info_2..9+InsuredInfo_1.2, data=nzvtrndmy, method='xgbLinear')
Sys.time()-s

xgbl21pred <- predict(xgbl21fit, nzvtstdmy)
xgbl21pred[which(xgbl21pred<1)] <- 1
xgbl21pred[which(xgbl21pred>8)] <- 8
xgbl21pred <- round(xgbl21pred)

xgbl21submission <- cbind(tstId, xgbl21pred)
names(xgbl21submission)[2] <- 'Response'
write.csv(xgbl21submission, 'xgbl21submission.csv', row.names=F)

#xgboost linear model with Response as integer top 21 variables based on xgblfit plus 13 from lmfit
varImp(xgblfit)
varImp(lmfit)

s <- Sys.time()
xgbl34fit <- train(Response~BMI+Medical_History_15+Medical_History_4.2+Product_Info_4+Ins_Age+InsuredInfo_5.3+
  Medical_Keyword_15+Medical_History_39.1+InsuredInfo_6.2+Employment_Info_1+Medical_History_1+Medical_History_13.1+
  Family_Hist_4+Product_Info_2.D1+Ht+Medical_History_28.2+Medical_History_18.2+Medical_History_3.3.3+
  Employment_Info_3.3+Medical_History_6.1+Employment_Info_2..9+InsuredInfo_1.2+Medical_History_15missing+
  Medical_Keyword_3+Medical_History_30.3+Medical_History_5.2+Product_Info_2.A7+Insurance_History_2.3+
  Medical_History_40.1+Medical_History_20.1+Medical_History_11.2+Medical_History_27.1+Medical_History_13.1+
  Medical_History_35.3, data=trndmy, method='xgbLinear')
Sys.time()-s

xgbl34pred <- predict(xgbl34fit, tstdmy)
xgbl34pred[which(xgbl34pred<1)] <- 1
xgbl34pred[which(xgbl34pred>8)] <- 8
xgbl34pred <- round(xgbl34pred)

xgbl34submission <- cbind(tstId, xgbl34pred)
names(xgbl34submission)[2] <- 'Response'
write.csv(xgbl34submission, 'xgbl34submission.csv', row.names=F)

##Try rescaling instead of cutting off predictions at 1 and 8
xgbl34pred <- predict(xgbl34fit, tstdmy)
xgbl34pred <- (8-1)/(max(xgbl34pred)-min(xgbl34pred))*(xgbl34pred-min(xgbl34pred))+1
xgbl34pred <- round(xgbl34pred)

xgbl34scalesubmission <- cbind(tstId, xgbl34pred)
names(xgbl34scalesubmission)[2] <- 'Response'
write.csv(xgbl34scalesubmission, 'xgbl34scalesubmission.csv', row.names=F)

##Try offsetting predictions based on Response
xgbl34pred <- predict(xgbl34fit, trndmy)
xgbl34comp <- data.frame(cbind(trndmy$Response, xgbl34pred))
names(xgbl34comp)[1] <- 'Response'
xgbl34predmedians <- tapply(xgbl34comp$xgbl34pred, xgbl34comp$Response, median)

offsets <- numeric(length(xgbl34predmedians))
for(i in 1:length(xgbl34predmedians)){
  offsets[i] <- xgbl34predmedians[i]-i
}

xgbl34pred <- predict(xgbl34fit, tstdmy)
xgbl34pred[xgbl34pred<4.5] <- xgbl34pred[xgbl34pred<4.5]-2
xgbl34pred[xgbl34pred>4.65 & xgbl34pred<5.591283] <- xgbl34pred[xgbl34pred>4.65 & xgbl34pred<5.591283]+.35
xgbl34pred[xgbl34pred>5.591283 & xgbl34pred<6.22273] <- xgbl34pred[xgbl34pred>5.591283 & xgbl34pred<6.22273]+.4
xgbl34pred[xgbl34pred>6.22273 & xgbl34pred<7.163] <- xgbl34pred[xgbl34pred>6.22273 & xgbl34pred<7.163]+.77
xgbl34pred[xgbl34pred>7.163] <- xgbl34pred[xgbl34pred>7.163]+.83

xgbl34pred[which(xgbl34pred<1)] <- 1
xgbl34pred[which(xgbl34pred>8)] <- 8
xgbl34pred <- round(xgbl34pred)

xgbl34offsubmission <- cbind(tstId, xgbl34pred)
names(xgbl34offsubmission)[2] <- 'Response'
write.csv(xgbl34offsubmission, 'xgbl34offsubmission.csv', row.names=F)

##Build separate xgb Linear models for Response<=4 and Response>=4
varImp(xgblfit)
varImp(lmfit)

lowtrndmy <- trndmy[trndmy$Response<=4,]
hightrndmy <- trndmy[trndmy$Response>=4,]

s <- Sys.time()
lowxgbl34fit <- train(Response~BMI+Medical_History_15+Medical_History_4.2+Product_Info_4+Ins_Age+InsuredInfo_5.3+
  Medical_Keyword_15+Medical_History_3.91+InsuredInfo_6.2+Employment_Info_1+Medical_History_1+Medical_History_13.1+
  Family_Hist_4+Product_Info_2.D1+Ht+Medical_History_2.82+Medical_History_18.2+Medical_History_3.33+
  Employment_Info_3.3+Medical_History_6.1+Employment_Info_2..9+InsuredInfo_1.2+Medical_History_15missing+
  Medical_Keyword_3+Medical_History_3.03+Medical_History_5.2+Product_Info_2.A7+Insurance_History_2.3+
  Medical_History_4.01+Medical_History_2.01+Medical_History_11.2+Medical_History_2.71+Medical_History_13.1+
  Medical_History_3.53, data=lowtrndmy, method='xgbLinear')
Sys.time()-s

s <- Sys.time()
highxgbl34fit <- train(Response~BMI+Medical_History_15+Medical_History_4.2+Product_Info_4+Ins_Age+InsuredInfo_5.3+
  Medical_Keyword_15+Medical_History_3.91+InsuredInfo_6.2+Employment_Info_1+Medical_History_1+Medical_History_13.1+
  Family_Hist_4+Product_Info_2.D1+Ht+Medical_History_2.82+Medical_History_18.2+Medical_History_3.33+
  Employment_Info_3.3+Medical_History_6.1+Employment_Info_2..9+InsuredInfo_1.2+Medical_History_15missing+
  Medical_Keyword_3+Medical_History_3.03+Medical_History_5.2+Product_Info_2.A7+Insurance_History_2.3+
  Medical_History_4.01+Medical_History_2.01+Medical_History_11.2+Medical_History_2.71+Medical_History_13.1+
  Medical_History_3.53, data=hightrndmy, method='xgbLinear')
Sys.time()-s

lowxgbl34pred <- predict(lowxgbl34fit, lowtrndmy)
lowxgbl34comp <- data.frame(cbind(lowtrndmy$Response, lowxgbl34pred))
names(lowxgbl34comp)[1] <- 'Response'
lowxgbl34predmedians <- tapply(lowxgbl34comp$lowxgbl34pred, lowxgbl34comp$Response, median)

highxgbl34pred <- predict(highxgbl34fit, hightrndmy)
highxgbl34comp <- data.frame(cbind(hightrndmy$Response, highxgbl34pred))
names(highxgbl34comp)[1] <- 'Response'
highxgbl34predmedians <- tapply(highxgbl34comp$highxgbl34pred, highxgbl34comp$Response, median)

lowxgbl34pred <- predict(lowxgbl34fit, tstdmy)
highxgbl34pred <- predict(highxgbl34fit, tstdmy)
xgbl34pred <- cbind(tstId, lowxgbl34pred, highxgbl34pred)

xgbl34pred[which(xgbl34pred<1)] <- 1
xgbl34pred[which(xgbl34pred>8)] <- 8
xgbl34pred <- round(xgbl34pred)

xgbl34submission <- cbind(tstId, xgbl34pred)
names(xgbl34submission)[2] <- 'Response'
write.csv(xgbl34submission, 'xgbl34submission.csv', row.names=F)


#xgboost tree model with Response as integer without zero and near zero variance variables
s <- Sys.time()
xgbtfit <- train(Response~., data=nzvtrndmy, method='xgbTree')
Sys.time()-s

xgbtpred <- predict(xgbtfit, nzvtstdmy)
xgbtsubmission <- cbind(tstId, xgbtpred)
names(xgbtsubmission)[2] <- 'Response'
write.csv(xgbtsubmission, 'xgbtsubmission.csv', row.names=F)

#xgboost tree model with Response as integer top 20 variables based on xgbtfit plus 13 from lmfit
varImp(xgbtfit)
varImp(lmfit)

xgbl33control <- trainControl(method="repeatedcv", number=10, repeats=2)
xgbl33grid <- expand.grid(nrounds=c(100, 150, 200, 300), max_depth=c(2,3,4,5), eta=c(.1,.2,.3,.4),
  gamma=c(0,1,2), colsample_bytree=c(.7,.8,.9), min_child_weight=c(1,2,3))

s <- Sys.time()
xgbt33fit <- train(Response~BMI+Medical_History_15+Medical_Keyword_15+Medical_History_4.2+Product_Info_4+
  Medical_History_3.91+Ins_Age+InsuredInfo_6.2+Medical_History_13.1+Product_Info_2.D1+Medical_History_2.82+
  Family_Hist_4+Medical_History_1+Medical_History_18.2+Medical_History_3.33+Employment_Info_1+Ht+
  Employment_Info_2..9+Medical_History_6.1+Insurance_History_8.1+Medical_History_15missing+Medical_Keyword_3+
  Medical_History_3.03+Medical_History_5.2+Product_Info_2.A7+Insurance_History_2.3+InsuredInfo_5.3+
  Medical_History_4.01+Medical_History_2.01+Medical_History_18.2+Medical_History_11.2+Medical_History_2.71+
  Medical_History_3.53, data=trndmy, method='xgbTree', trControl=xgbl33control, tuneGrid=xgbl33grid)
Sys.time()-s

xgbt33pred <- predict(xgbt33fit, tstdmy)
xgbt33pred[which(xgbt33pred<1)] <- 1
xgbt33pred[which(xgbt33pred>8)] <- 8
xgbt33pred <- round(xgbt33pred)

xgbt33submission <- cbind(tstId, xgbt33pred)
names(xgbt33submission)[2] <- 'Response'
write.csv(xgbt33submission, 'xgbt33submission.csv', row.names=F)

#xgboost model with Response as integer top 21 variables based on xgblfit plus 13 from lmfit
varImp(xgbtfit)
varImp(lmfit)

features <- c("BMI", "Medical_History_15", "Medical_History_4.2", "Product_Info_4", "Ins_Age", "InsuredInfo_5.3", 
  "Medical_Keyword_15", "Medical_History_3.91", "InsuredInfo_6.2", "Employment_Info_1", "Medical_History_1", 
  "Medical_History_13.1", "Family_Hist_4", "Product_Info_2.D1", "Ht", "Medical_History_2.82", "Medical_History_18.2", 
  "Medical_History_3.33", "Employment_Info_3.3", "Medical_History_6.1", "Employment_Info_2..9", "InsuredInfo_1.2", 
  "Medical_History_15missing", "Medical_Keyword_3", "Medical_History_3.03", "Medical_History_5.2", 
  "Product_Info_2.A7", "Insurance_History_2.3", "Medical_History_4.01", "Medical_History_2.01", 
  "Medical_History_11.2", "Medical_History_2.71", "Medical_History_13.1", "Medical_History_3.53")

xgbtrndmy <- trndmy[,which(names(trndmy) %in% features)]
xgbtrndmy <- cbind(xgbtrndmy, trndmy$Medical_History_13.1)
xgbtstdmy <- tstdmy[,which(names(tstdmy) %in% features)]
xgbtstdmy <- cbind(xgbtstdmy, tstdmy$Medical_History_13.1)
rm(features)

library(xgboost)
xgb34fit <- xgboost(data=as.matrix(xgbtrndmy), label=trndmy$Response, nround=100, objective='reg:linear', verbose=0)

xgb34pred <- predict(xgb34fit, as.matrix(xgbtstdmy))
xgb34pred[which(xgb34pred<1)] <- 1
xgb34pred[which(xgb34pred>8)] <- 8
xgb34pred <- round(xgb34pred)

xgb34submission <- cbind(tstId, xgb34pred)
names(xgb34submission)[2] <- 'Response'
write.csv(xgb34submission, 'xgb34submission.csv', row.names=F)

#xgboost model with Response as integer with all variables 
library(xgboost)
xgbfit <- xgboost(data=as.matrix(trndmy[-634]), label=trndmy$Response, nround=100, objective='reg:linear', verbose=0)

xgbpred <- predict(xgbfit, as.matrix(tstdmy))
xgbpred[which(xgbpred<1)] <- 1
xgbpred[which(xgbpred>8)] <- 8
xgbpred <- round(xgbpred)

xgbsubmission <- cbind(tstId, xgbpred)
names(xgbsubmission)[2] <- 'Response'
write.csv(xgbsubmission, 'xgbsubmission.csv', row.names=F)


#RFE for xgbLinear to find optimal number of predictors
xgblcontrol <- trainControl(method='repeatedcv', number=10, repeats=2)
xgblgrid <- expand.grid(nrounds=50, lambda=0.1, alpha=0)

#rmse <- 100
#x <- trndmy[,-634]
#i <- 1
#repeat{
#  set.seed(50)
#  xgblfit <- train(x, trndmy$Response, method='xgbLinear', trControl=xgblcontrol, tuneGrid=xgblgrid)
#  if(xgblfit$results[[4]][1] < rmse){
#    print(xgblfit$results[[4]][1])
#    print(length(x))
#    features <- names(x)
#    ith <- x[,length(x)]
#    x <- x[,-length(x)]
#    rmse <- xgblfit$results[[4]][1]
#    i <- 1
#  }else if(xgblfit$results[[4]][1] > rmse & i < length(x)){
#    x <- cbind(x, ith)
#    ith <- x[,length(x)-i]
#    x <- x[,-length(x)-i]
#    i <- i+1
#  }else if(xgblfit$results[[4]][1] > rmse & i >= length(x)){
#    break
#  }
#}

xgblfit <- train(trndmy[,-634], trndmy$Response, method='xgbLinear', trControl=xgblcontrol, tuneGrid=xgblgrid)
xgblimp <- varImp(xgblfit)
xgblImp <- row.names(xgblimp$importance)

rmse <- 10
iopt <- 20
for(i in 20:length(xgblImp)){
  set.seed(50)
  xgbl <- train(trndmy[which(names(trndmy) %in% xgblImp[1:i])], trndmy$Response, method='xgbLinear', trControl=xgblcontrol, tuneGrid=xgblgrid)
  if(xgbl$results[[4]][1]<rmse){
    rmse <- xgbl$results[[4]][1]
    iopt <- i
  }
  print(i)
  print(xgbl$results[[4]][1])
}

xgbl102fit <- train(trndmy[which(names(trndmy) %in% xgblImp[1:iopt])], trndmy$Response, method='xgbLinear', trControl=xgblcontrol, tuneGrid=xgblgrid)

xgbl102pred <- predict(xgbl102fit, tstdmy[which(names(tstdmy) %in% xgblImp[1:iopt])])
xgbl102pred[which(xgbl102pred<1)] <- 1
xgbl102pred[which(xgbl102pred>8)] <- 8
xgbl102pred <- round(xgbl102pred)

xgbl102submission <- cbind(tstId, xgbl102pred)
names(xgbl102submission)[2] <- 'Response'
write.csv(xgbl102submission, 'xgbl102submission.csv', row.names=F)


#RFE for xgbTree to find optimal number of predictors
xgbtcontrol <- trainControl(method='repeatedcv', number=10, repeats=2)
xgbtgrid <- expand.grid(nrounds=50, max_depth=3, eta=0.3, gamma=0, colsample_bytree=0.8, min_child_weight=1)

rmse <- 100
x <- trndmy[,-634]
i <- 1
repeat{
  set.seed(50)
  xgbtfit <- train(x, trndmy$Response, method='xgbTree', trControl=xgbtcontrol, tuneGrid=xgbtgrid)
  if(xgblfit$results[[4]][1] < rmse){
    print(xgblfit$results[[4]][1])
    print(length(x))
    features <- names(x)
    ith <- x[,length(x)]
    x <- x[,-length(x)]
    rmse <- xgblfit$results[[4]][1]
    i <- 1
  }else if(xgblfit$results[[4]][1] > rmse & i < length(x)){
    x <- cbind(x, ith)
    ith <- x[,length(x)-i]
    x <- x[,-length(x)-i]
    i <- i+1
  }else if(xgblfit$results[[4]][1] > rmse & i >= length(x)){
    break
  }
}













