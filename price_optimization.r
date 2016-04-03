### part 2, price optimization ###
#library(ggplot2)
#trainData <- read.csv("training.csv")
#trainData <- within(trainData, {app_date <- as.numeric(as.Date(app_date, format = "%m/%d/%y"))})
#trainFullModel <- glm(booking_ind~.-1,data=trainData,family=binomial(link='logit'))
#trainNullModel <- glm(booking_ind~amount-1,data=trainData,family=binomial(link='logit'))
#trainStepwise <- step(trainNullModel, scope = list(upper=trainFullModel), data=trainData, direction="both")
beta <- coef(trainStepwise)

priceData <- read.csv("price_opt.csv")
priceData <- within(priceData, {app_date <- as.numeric(as.Date(app_date, format = "%m/%d/%y"))})

################################## functions used in Part 2 ######################################

#this is the rate function, data must be the one excluding booking_ind, and customer_rate
# the advantage of this is that this is tolerant to extra data, such as cust_seg
f_rate<-function(rate, model, data){
  beta <- coef(model)
  eta <- rep(0, nrow(data))#matrix(nrow=nrow(data), ncol=ncol(data))
  for(i in seq(nrow(data))){
    total = 0
    for(k in seq(ncol(data))){
      if(class(data[i,k])=="factor"){
        varname <- paste(names(data)[k], data[i,k], sep="")
        beta.ind <- which(names(beta)==varname)
        val <- ifelse(length(beta.ind), beta[beta.ind], 0)
      } else {
        varname <- names(data)[k]
        beta.ind <- which(names(beta)==varname)
        val <- data[i,k]*beta[beta.ind]
      }
      total = total + val
    }
    beta.ind <- which(names(beta)=="customer_rate")
    val <- rate * beta[beta.ind]
    total = total + val
    eta[i] <- total
  }
  return(exp(eta)/(1+exp(eta)))
}

#calculate the sensitivity to rate change at the customer_rate or given rate
#d_f(rate) = f(rate) * (1-f(rate)) * beta_customer_rate
# this is roughly (f_rate(rate+delta) - f_rate(rate-delta))/2/delta, assuming locally at rate it is linear
# take delta = 0.1, the results are very close
f_rate_deriv <- function(model, data){
  beta <- coef(model)
  df <- rep(0, nrow(data))
  rate.ind <- which(names(beta)=="customer_rate")
  for(i in seq(nrow(data))){
    total = 0
    for(k in seq(ncol(data))){
      if(class(data[i,k])=="factor"){
        varname <- paste(names(data)[k], data[i,k], sep="")
        beta.ind <- which(names(beta)==varname)
        val <- ifelse(length(beta.ind), beta[beta.ind], 0)
      } else {
        varname <- names(data)[k]
        beta.ind <- which(names(beta)==varname)
        val <- data[i,k]*beta[beta.ind]
      }
      total = total + val # this is eta_i
    }
    sensitivity <- (exp(total)/(1+exp(total))^2) * beta[rate.ind]
    df[i] <- sensitivity
  }
  return(df)
}

#this outputs the same as f_rate, but slightly faster
f_rate2 <- function(rate, model, data){
  newdata <- cbind(customer_rate=rep(rate, nrow(data)), data, row.names=NULL)
  probs <- predict(model, newdata=newdata, type="response")
  return(probs)
}
#probs <- f_rate(data, rate, factorCols)

#this is the rate function, data must be the one excluding booking_ind, and customer_rate
#rate is actual rate times 100
#negative sign is for optim, as it tries to minimize the function instead of maximize
pi_rate<-function(rate, model, data, negative=F){
  probs <- f_rate2(rate, model, data)
  profits <- probs*data$amount*(rate-2.5)/100*data$term/12
  profit <- sum(profits)
  if(negative){return(-profit)}
  else{return(profit)}
}

###################################### Part 2.1: Single Rate #############################################

optProf <- optim(1, pi_rate, model=trainStepwise, data=priceData[,-c(1,11)], negative=T, method = "L-BFGS-B", lower=0, upper=13)
## best rate for q1 in part 2
bestRate <- optProf$par
print(bestRate)
## max profit for q1 in part 2
maxProfit <- -optProf$value
print(maxProfit)

### plot the profits as a function of single rates
rates <- seq(0, 10, 0.005)
profits <- rep(0, length(rates))
for(i in seq(length(rates))){
  profits[i] <- pi_rate(rates[i], trainStepwise, priceData[,-c(1,11)])
}
profitData <- data.frame(Rate=rates, Profit=profits)
pdf(file="profits_single.pdf", width = 6, height = 6, colormodel = "cmyk")
ggplot(profitData, aes(x = Rate, y = Profit)) + geom_line(colour="blue") + geom_vline(xintercept=bestRate, colour="red", size=0.5, linetype=3) + geom_hline(yintercept=maxProfit, colour="red", size=0.5, linetype=3) + ggtitle("Profit v.s. Single Rate")
dev.off()



##################################### Part 2.2: two segments #############################################

#calculate in-situ rate sensitivities
sensitivities <- f_rate_deriv(trainStepwise, priceData[,-11])
summary(sensitivities)
sensKmeans <- kmeans(sensitivities, centers=2)
# plot clusters #
clustering <- data.frame(Row=seq(1, length(sensitivities)), Sensitivity=sensitivities, Color=as.factor(sensKmeans$cluster))
pdf(file="sens_cluster.pdf", width = 6, height = 6, colormodel = "cmyk")
ggplot(clustering, aes(x = Row, y = Sensitivity, color = Color)) + geom_point() + ggtitle("Sensitivity Clustering") + geom_hline(yintercept = mean(sensitivities), linetype=3, colour="purple")
dev.off()
# the difference between using k-means and using mean (percentile), is 11%
# but the advantage of k-means, is that it can do more than 2 segments
priceSeg1 <- priceData[which(sensKmeans$cluster==1),]
priceSeg2 <- priceData[which(sensKmeans$cluster==2),]
optProfSeg1 <- optim(1, pi_rate, model=trainStepwise, data=priceSeg1[,-c(1,11)], negative=T, method = "L-BFGS-B", lower=0, upper=13)
optProfSeg2 <- optim(1, pi_rate, model=trainStepwise, data=priceSeg2[,-c(1,11)], negative=T, method = "L-BFGS-B", lower=0, upper=13)
## the two best rates ##
bestRateSeg1 <- optProfSeg1$par
bestRateSeg2 <- optProfSeg2$par
print(bestRateSeg1)
print(bestRateSeg2)
## max profit of the two rates
maxProfitSeg <- -optProfSeg1$value - optProfSeg2$value
print(maxProfitSeg)

###################################### Pilot study #############################################

## pilot study, multiple segmentations
max_profit <- function(model, data, segments=2){
  sensitivities <- f_rate_deriv(model, data)
  sensKmeans <- kmeans(sensitivities, centers=segments)
  #clustering <- data.frame(Row=seq(1, length(sensitivities)), Sensitivity=sensitivities, Color=as.factor(sensKmeans$cluster))
  results <- data.frame(BestRate=rep(0, segments), MaxProfit=rep(0, segments))
  for(i in seq(segments)){
    priceSeg <- data[which(sensKmeans$cluster==i),]
    #1st column is the rate used to calculate the sensitivities
    optProfSeg <- optim(1, pi_rate, model=model, data=priceSeg[,-1], negative=T, method = "L-BFGS-B", lower=0, upper=13)
    results$BestRate[i] <- optProfSeg$par
    results$MaxProfit[i] <- -optProfSeg$value
  }
  return(results)
}

totalSegs <- 30
segDat <- data.frame(Segments=seq(totalSegs), MaxProfit=rep(0, totalSegs))
for(i in seq(totalSegs)){
  res <- max_profit(trainStepwise, priceData[,-11], segments = i)
  segDat$MaxProfit[i] <- sum(res$MaxProfit)
}
pdf(file="segment_profit.pdf", width = 6, height = 6, colormodel = "cmyk")
ggplot(segDat, aes(x = Segments, y = MaxProfit)) + geom_line(colour="blue") + geom_point(colour="green", shape="x")  + ggtitle("Maximum Profit v.s. Segments")#+ geom_smooth(method = loess, span = .8)
dev.off()
