#Nomis challenge solution
library(GGally) #for ggpairs
library(ggplot2)
library(reshape2) # for melt
library(lattice)
library(gridExtra)
setwd(getwd())
trainData <- read.csv("training.csv")
#parse date and convert to epoch numeric
trainData <- within(trainData, {app_date <- as.numeric(as.Date(app_date, format = "%m/%d/%y"))})
trainDataFactor <- within(trainData, {booking_ind <- as.factor(booking_ind)})
################################## Part 1: Modeling #####################################

#exploratory analyses
source("exploratory_analyses.r", echo=T)

### logistic regression ###
#no intercept, as no interpretation
lrTime <- system.time(trainFullModel <- glm(booking_ind~.-1,data=trainData,family=binomial(link='logit')))
summary(trainFullModel)
## model selection for logistic regression ##
#null model
trainNullModel <- glm(booking_ind~amount-1,data=trainData,family=binomial(link='logit'))
#forward model selection
trainStepwise <- step(trainNullModel, scope = list(upper=trainFullModel), data=trainData, direction="forward")
#best model from foward selection removes cust_seg

############### statistical tests on continuous data ############
# wald test
library(aod)
#Term: tell R which terms in the model are to be tested:
logit.wald <- wald.test(b = coef(trainStepwise), Sigma = vcov(trainStepwise), Terms=c(1,3,4,57:60))
# score test
library(AssotesteR)
logit.score <- SCORE(trainData$booking_ind, trainData[,-c(1,7,9,11,12)])
# can also do a likelihood ratio test
##############################

######## feature selection for SVM, and learning curve plotting #############
### this may take a long time, as training svm with large data sets is slow ###
source("model_selection.r", echo=T)

######### plot ROC and calculate AUC ##############
library(pROC)
lrPred <- predict(trainStepwise, trainData[, -c(1,12)], type="response")
pdf(file="roc_logit.pdf", width = 6, height = 6, colormodel = "cmyk")
lrRoc <- plot.roc(trainData$booking_ind, lrPred)
dev.off()
print(lrRoc)#AUC: 0.8639

library(e1071)
svmTime <- system.time(trainSvm <- svm(booking_ind~., data=trainDataFactor[,-c(7,12)], gamma=0.004, probability = T))
svmPred <- predict(trainSvm, trainData[, -c(1,7,12)], probability = T)
svmProbs <- attr(svmPred, "probabilities")[,2]
pdf(file="roc_svm.pdf", width = 6, height = 6, colormodel = "cmyk")
svmRoc <- plot.roc(trainData$booking_ind, svmProbs)
dev.off()
print(svmRoc)#AUC: 0.863
################################################

### output results ###
testData <- read.csv("test.csv")
testData <- within(testData, {app_date <- as.numeric(as.Date(app_date, format = "%m/%d/%y"))})
testPred <- predict(trainStepwise, newdata=testData[,-11], type="response")#no cust_seg
testPredSvm <- predict(trainSvm, newdata=testData[,-c(6,11)], probability = T)
predRes <- data.frame(ID=seq(1, nrow(testData)), LogisticRegression=testPred, SVM=testPredSvm)
write.csv(predRes, file="test_predict.csv", row.names=F)

################################## Part 2: Optimization #############################################
source("price_optimization.r", echo=T)
