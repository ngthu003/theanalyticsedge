# 4.1 - Understanding Why People Vote

# 1.1
gerber <- read.csv('gerber.csv')
str(gerber)
table(gerber$voting)
108696/nrow(gerber)
# 1.2
# table(gerber$voting == 1, gerber$hawthorne)
# table(gerber$voting == 1, gerber$civicduty)
# table(gerber$voting == 1, gerber$neighbors)
# table(gerber$voting == 1, gerber$self)
# 12316/sum(gerber$hawthorne)
# 12021/sum(gerber$civicduty)
# 14438/sum(gerber$neighbors)
# 13191/sum(gerber$self)
tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$neighbors, mean)
tapply(gerber$voting, gerber$self, mean)

# 1.3 - Logistic Regression
LogModel <- glm(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, family="binomial")
summary(LogModel)
# 1.4 - Threshold: .3
predictLog <- predict(LogModel, type="response")
table(gerber$voting, predictLog > 0.3)
(134513+51966)/nrow(gerber)
# 1.5 - Threshold: .5
table(gerber$voting, predictLog > 0.5)
(235388)/nrow(gerber)

# 1.6 - AUC
library(ROCR)
ROCRpred <- prediction(predictLog, gerber$voting)
as.numeric(performance(ROCRpred, "auc")@y.values)

#########################################################

# 2.1
library(rpart)
library(rpart.plot)
CARTmodel <- rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

# 2.2
CARTmodel2 <- rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

# 2.4
CARTmodel3 <- rpart(voting ~ sex + civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel3)

#########################################################

# 3.1
str(gerber)
CARTmodel4 <- rpart(voting ~ control, data=gerber, cp=0.0)
prp(CARTmodel4, digits = 6)
abs(.34 - .296638)

# 3.2
CARTmodel5 <- rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTmodel5, digits = 6)
abs(.345818 - .302795)
abs(.334176 - .290456)

# 3.3
LogModelSex <- glm(voting ~ sex + control, data=gerber, family="binomial")
summary(LogModelSex)

# 3.4
Possibilities <- data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(LogModelSex, newdata=Possibilities, type="response")

# 3.5
LogModel2 <- glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)

# 3.6
predict(LogModel2, newdata=Possibilities, type="response")
