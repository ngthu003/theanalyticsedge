### Unit 3
# Assignment 3.3: Predicting Loan Repayment

# 1.1
loans <- read.csv('loans.csv')
table(loans$not.fully.paid)
# 1.2
summary(loans)
# number of NAs
# table(is.na(loans))
# 1.3
# Getting observations with NAs
missing = subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))
table(missing$not.fully.paid)

########################################################

loans <- read.csv('loans_imputed.csv')
# Splitting data
set.seed(144)
library(caTools, lib = 'C:/temp')
split <- sample.split(loans$not.fully.paid, SplitRatio = .7)
train <- subset(loans, split == TRUE)
test  <- subset(loans, split == FALSE)

# 2.1
mod <- glm(not.fully.paid ~ ., data = train, family = 'binomial')
summary(mod)

# 2.2
# Logit(A) - Logit(B)
-9.317e-03 * 10
# O(A) / O(B)
exp(-9.317e-03 * -10)

# 2.3
pred <- predict(mod, newdata = test, type = 'response')
# Confusion matrix
table(pred >= .5, test$not.fully.paid)

# 2.4



########################################################

# 3.1
mod2 <- glm(not.fully.paid ~ int.rate, data = train, family = 'binomial')
summary(mod2)

# 3.2
pred2 <- predict(mod2, newdata = test, type = 'response')
max(pred2)

# Curve of ROCR
ROCRpred <- prediction(pred2, test$not.fully.paid)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
plot(ROCRperf)
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
# AUC
as.numeric(performance(ROCRpred, "auc")@y.values)

########################################################
# 5.1
# Profit
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
10*max(test$profit)

########################################################

# 6.1
test$predicted.risk <- pred
highInterest <- subset(test, int.rate >= .15)
mean(highInterest$profit)
table(highInterest$not.fully.paid)

# 6.2
cutoff <- sort(highInterest$predicted.risk, decreasing = FALSE)[100]
selectedLoans <- subset(highInterest, predicted.risk <= cutoff)
sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)




