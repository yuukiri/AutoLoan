### exploratory plotting ###
summary(trainData)
#careful with this, very slow
#pdf(file="cont_var_dist_cor.pdf", width = 6, height = 6, colormodel = "cmyk");
png(file="cont_var_dist_cor.png", width = 6, height = 6, units = "in", res=300)
ggpairs(trainData[, c("customer_rate", "comp_rate", "amount", "credit_score", "prime")])+theme(axis.title.y=element_text(margin=margin(0,20,0,0)))
dev.off()
#distribution of continuous variables
#note, term here is NOT actually continuous
cont <- melt(trainData[, c("booking_ind", "customer_rate", "comp_rate", "amount", "credit_score", "term", "prime")], id.vars="booking_ind")
pdf(file="cont_var_dist.pdf", width = 6, height = 6, colormodel = "cmyk");
ggplot(cont, aes(factor(booking_ind), y = value, fill=factor(booking_ind))) + geom_boxplot() + facet_wrap(~variable, scales="free_y")
dev.off()
index0 <- which(trainData$booking_ind==0)
index1 <- which(trainData$booking_ind==1)
# distribution of state variable
pdf(file="state_dist.pdf", width = 12, height = 6, colormodel = "cmyk");
state0 <- table(trainData$state[index0])
state1 <- table(trainData$state[index1])
barchart(cbind(state0, state1), stack = T, horizontal=F, auto.key=list(corner = c(0.95, 0.95)))
dev.off()
#distribution of factor variable
pdf(file="factor_var_dist.pdf", width = 6, height = 6, colormodel = "cmyk");
term0 <- table(trainData$term[index0])
term1 <- table(trainData$term[index1])
plt1 <- barchart(cbind(term0, term1), stack = T, auto.key=T)
car_type0 <- table(trainData$car_type[index0])
car_type1 <- table(trainData$car_type[index1])
plt2 <- barchart(cbind(car_type0, car_type1), stack = T, auto.key=T)
partner_cat0 <- table(trainData$partner_cat[index0])
partner_cat1 <- table(trainData$partner_cat[index1])
plt3 <- barchart(cbind(partner_cat0, partner_cat1), stack = T, auto.key=T)
cust_seg0 <- table(trainData$cust_seg[index0])
cust_seg1 <- table(trainData$cust_seg[index1])
plt4 <- barchart(cbind(cust_seg0, cust_seg1), stack = T, auto.key=T)
grid.arrange(plt1, plt2, plt3, plt4, nrow=2, ncol=2)
dev.off()

#case study
laCust <- trainData[which(trainData$state=="LA"),]
highcredit <- laCust[which(laCust$credit_score>715),]
takeRate <- length(which(highcredit$booking_ind==1))/nrow(highcredit)
print(takeRate)
#0.28
loanTaken <- trainData[which(trainData$booking_ind==1),]
cat1Rate <- length(which(loanTaken$partner_cat=="Cat 1"))/nrow(loanTaken)
print(cat1Rate)
#0.54
highcreditLoan <- length(which(loanTaken$credit_score>=715))/nrow(loanTaken)
print(highcreditLoan)
#0.6
summary(loanTaken)
lowerRate <- length(which(loanTaken$customer_rate<loanTaken$comp_rate))/nrow(loanTaken)
print(lowerRate)
#0.33
higherRate <- length(which(loanTaken$customer_rate>loanTaken$comp_rate))/nrow(loanTaken)
print(higherRate)
#0.39
