setwd('~/Prudential') #Set to whatever working directory is. 

library(caret)
library(DMwR)
library(Hmisc)
library(Metrics)

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

#Linear regression 
lmfit <- train(Response~., data=trndmy, method='lm')

#xgboost linear model with Response as integer without zero and near zero variance variables
xgblfit <- train(Response~., data=nzvtrndmy, method='xgbLinear')

#xgboost linear model with Response as integer top 21 variables based on xgblfit plus 13 from lmfit
varImp(xgblfit)
varImp(lmfit)

fit <- train(Response~BMI+Medical_History_15+Medical_History_4.2+Product_Info_4+Ins_Age+InsuredInfo_5.3+
  Medical_Keyword_15+Medical_History_39.1+InsuredInfo_6.2+Employment_Info_1+Medical_History_1+Medical_History_13.1+
  Family_Hist_4+Product_Info_2.D1+Ht+Medical_History_28.2+Medical_History_18.2+Medical_History_3.3.3+
  Employment_Info_3.3+Medical_History_6.1+Employment_Info_2..9+InsuredInfo_1.2+Medical_History_15missing+
  Medical_Keyword_3+Medical_History_30.3+Medical_History_5.2+Product_Info_2.A7+Insurance_History_2.3+
  Medical_History_40.1+Medical_History_20.1+Medical_History_11.2+Medical_History_27.1+Medical_History_13.1+
  Medical_History_35.3, data=trndmy, method='xgbLinear')

#Predict fit on training data
trnpred <- predict(fit, trndmy)

#Create function to calculate ScoreQuadraticWeightedKappa based on cuts
SQWKfun <- function(initcuts, pred, actual) {
  cuts <- c(min(pred), initcuts, max(pred))
  preds <- as.numeric(cut2(pred, cuts))
  err <- -ScoreQuadraticWeightedKappa(preds, actual, 1, 8)
  return(err)
}

#Set initcuts to midway between each Response category and then optimize cuts
initcuts <- seq(1.5, 7.5, by = 1)
optimcuts <- optim(initcuts, SQWKfun, pred=trnpred, actual=trndmy$Response)

#Predict fit on test data and use optimcuts to categorize predictions
pred <- predict(fit, tstdmy)
predcut <- as.numeric(cut2(pred, c(min(pred), optimcuts$par, max(pred))))

#Write out submission
submission <- cbind(tstId, predcut)
names(submission)[2] <- 'Response'
write.csv(submission, 'submission.csv', row.names=F)



