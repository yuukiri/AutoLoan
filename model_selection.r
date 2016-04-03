#setwd("~/Documents/R_Projects/NomisChallenge/Data/")
#trainData <- read.csv("training.csv")
#parse date and convert to epoch numeric
#trainData <- within(trainData, {app_date <- as.numeric(as.Date(app_date, format = "%m/%d/%y"))})
#trainDataFactor <- within(trainData, {booking_ind <- as.factor(booking_ind)})


library(e1071)

## generate learning rate curve data, similar to learing_curve_dat in caret, but also works for svm from e1071
learning_curve_dat <- function(formula, data, predIndex, model, method="Classification", cycle=10, testRatio=1/5, ...){
	## simple cutting
	cvFold <- round(1/testRatio)
	n <- nrow(data)
	cvIndices <- split(seq(1, n), rep(1:cvFold, each=n/cvFold)[1:n])
	indices<-do.call(cbind, lapply(cvIndices, unlist))
	total.trains <-round(n*(1-testRatio))
	trainSizes <- round(total.trains*seq(from=1/cycle, to=1, by=1/cycle))
	trainErrors <- matrix(nrow=cvFold, ncol=length(trainSizes))
	testErrors <- matrix(nrow=cvFold, ncol=length(trainSizes))
	for (i in seq(cvFold)){
		print(paste0("cross validation No.", i))
		testset <- data[indices[,i],]
		trainset <- data[-indices[,i],]
		for (k in seq(length(trainSizes))){
			print(paste0("training: ", round(trainSizes[k]/total.trains*100), "% , n = ", trainSizes[k]))
			trainInd <- sample(1:nrow(trainset), size=trainSizes[k], replace=F)
			fit <- model(formula, data=trainset[trainInd,], ...)
			if (method=="Classification"){
				trainPred <- predict(fit, newdata=trainset[trainInd,-predIndex])
				testPred <- predict(fit, newdata=testset[,-predIndex])
			} else {
				trainPred <- predict(fit,type="response")
				trainPred <- as.numeric(trainPred >= 0.5)
				testPred <- predict(fit, newdata=testset[,-predIndex], type="response")
				testPred <- as.numeric(testPred>=0.5)
			}
			train.error <- sum(trainPred!=trainset[trainInd,predIndex])/length(trainInd)
			test.error <- sum(testPred!=testset[, predIndex])/nrow(testset)
			trainErrors[i, k] <- train.error
			testErrors[i, k] <- test.error
		}
	}
	trainningError <- colMeans(trainErrors)
	testingError <- colMeans(testErrors)
	smooth.train <- lowess(trainSizes,trainningError)
	smooth.test <- lowess(trainSizes,testingError)
	err <- c(smooth.train$y, smooth.test$y)#c(trainningError, testingError)
	lRes <- data.frame(Training_Size=rep(trainSizes,2), Error = err, Data=c(rep("Training Error", length(trainSizes)), rep("Test Error", length(trainSizes))))
	return(lRes)
}

### feature selection for SVM ###
feature_selection <- function(data, predIndex, nullIndex, model, method="Classification", cvFold=5, predtype="response", ...){
  testRatio <- 1/cvFold
  n <- nrow(data)
	cvIndices <- split(seq(1, n), rep(1:cvFold, each=n/cvFold)[1:n])
	indices<-do.call(cbind, lapply(cvIndices, unlist))
	total.trains <-round(n*(1-testRatio))
  #forward feature selection
  params <- names(data)
  form <- paste(params[predIndex], "~", params[nullIndex])
  acc <- rep(0, cvFold)
  print(form)
  for (i in seq(cvFold)){
    #print(paste0("cross validation No.", i))
    testset <- data[indices[,i],]
    trainset <- data[-indices[,i],]
    fit <- model(as.formula(form), data=trainset, ...)
    #print(fit)
    if(method == "Classification"){
      testPred <- predict(fit, newdata = testset[,-predIndex])
    } else {
      testPred <- predict(fit, newdata = testset[,-predIndex], type=predtype)
      testPred <- as.numeric(testPred>=0.5)
    }
    #Accuracy
    testAcc <- sum(testPred==testset[,predIndex])/nrow(testset)
    acc[i] <- testAcc
  }
  test.acc <- mean(acc)

  cat("Null Accuracy: ", test.acc*100, "%\n")
  for (param in params[-c(predIndex,nullIndex)]){
    newForm <- paste(form, "+", param)
    print(newForm)
    err <- rep(1, cvFold)
    for (i in seq(cvFold)){
  		#print(paste0("cross validation No.", i))
  		testset <- data[indices[,i],]
  		trainset <- data[-indices[,i],]
      fit=model(as.formula(newForm), data=trainset,...)
      #calculate test error
      if(method == "Classification"){
        testPred <- predict(fit, newdata = testset[,-predIndex])
      } else {
        testPred <- predict(fit, newdata = testset[,-predIndex], type=predtype)
        testPred <- as.numeric(testPred>=0.5)
      }
      testAcc <- sum(testPred==testset[,predIndex])/nrow(testset)
      acc[i] <- testAcc
    }
    cat("average testing accuracy: ", mean(acc)*100, "%\n")
    if(test.acc <= mean(acc)){
      form = newForm
      test.acc = mean(acc)
    }
  }
  #final.fit <- model(as.formula(form), data=data,...)
  cat("final model: ", form, "\n")
  cat("test accuracy: ", test.acc, "\n")
  return(as.formula(form))
}

# this takes super long
# so I limited the data to around 10% of the size, but for the result in presentation
# I did run with full data, so this may yield a slightly different result
svmFs <- feature_selection(data=trainDataFactor[1:10000,], predIndex = 1, nullIndex = 2, model=svm)
print(svmFs)

# svm: booking_ind ~ customer_rate + comp_rate + app_date + amount + credit_score + term + car_type + prime + partner_cat
# feature selection removes state, and cust_seg

# tune parameter
# this also takes super long
# the result may vary, as it is doing 10-fold cv
test.svm <- tune.svm(booking_ind~., data=trainDataFactor[1:10000,-c(7,12)], cost=2^(0:2), gamma=seq(from=1e-3, to=0.01, by=1e-3))
print(test.svm)

#cost = 1, gamma = 0.004, err = 0.1



### calculate the learning curve data and plot learning curves ###
## the curve may look different each time, but the shape and trend remain the same

lcLogit <- learning_curve_dat(booking_ind~., data = trainData[, -c(7,12)], predIndex = 1, model = glm, method = "Regression", family=binomial)
pdf(file="learning_curve_logit_2.pdf", width = 6, height = 6, colormodel = "cmyk");
ggplot(lcLogit, aes(x = Training_Size, y = Error, color = Data)) + geom_smooth(method = loess, span = .8) + ggtitle("Linear Regression Learning Curve")
dev.off()
lcSvm <- learning_curve_dat(booking_ind~., data = trainDataFactor[, -c(7,12)], predIndex = 1, model = svm, method = "Classification", gamma=0.004)
pdf(file="learning_curve_svm.pdf", width = 6, height = 6, colormodel = "cmyk");
ggplot(lcSvm, aes(x = Training_Size, y = Error, color = Data)) + geom_smooth(method = loess, span = .8) + ggtitle("SVM Learning Curve")
dev.off()
