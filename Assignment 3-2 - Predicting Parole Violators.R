### Unit 3
# Assignment 3.2: Predicting Parole Violators

# 1.1
parole <- read.csv('parole.csv')
# 1.2
table(parole$violator)

########################################################
# 2.1
str(parole)
# 2.2
parole$state <- as.factor(parole$state)
parole$crime <- as.factor(parole$crime)
str(parole)

########################################################
# 3.1
set.seed(144)
library(caTools, lib = 'C:/temp')
split <- sample.split(parole$violator, SplitRatio = .7)
train <- subset(parole, split == TRUE)
test  <- subset(parole, split == FALSE)

########################################################
# 4.1
mod <- glm(violator ~ ., data = train, family = 'binomial')
summary(mod)
# 4.3
x = -4.2411574 + 0.3869904 + 0.8867192 + (-0.0001756)*50 + 3*(-0.1238867) + 12*0.0802954 + 0.6837143
# P(violator = 1)
y = 1 / (1 + exp(-x))
# ODDs
y / (1 - y)

########################################################
# 5.1
pred <- predict(mod, newdata = test, type = 'response')
max(pred)
# 5.2: assume threshold = .5
table(test$violator, pred >= .5)
# Sensitivity
12/(12+11)
# Specificity
167/(167+12)
# Accuracy
(167+12)/nrow(test)
# 5.3