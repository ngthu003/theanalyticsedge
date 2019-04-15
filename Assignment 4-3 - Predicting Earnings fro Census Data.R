# 4.3 - Predicting Earnings from Census Data

census <- read.csv('census.csv')
library(caTools)
set.seed(2000)
spl <- sample.split(census$over50k, SplitRatio = .6)
train <- subset(census, spl==TRUE)
test <- subset(census, spl==FALSE)

# 1.1 - Logistic Regression
censusglm <- glm(over50k ~ ., data = train, family = 'binomial')
summary(censusglm)
# 1.2
predictTest = predict(censusglm, newdata = test, type = "response")
table(test$over50k, predictTest >= .5)
(9051 + 1888) / nrow(test)
# 1.3 - Baseline on test set
table(test$over50k)
9713/nrow(test)
# 1.4 - AUC
library(ROCR)
ROCRpred <- prediction(predictTest, test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)

#########################################################
library(rpart)
library(rpart.plot)

# 2.1 - CART
censustree <- rpart(over50k ~ ., data = train, method = 'class')
prp(censustree)
# 2.2 - Predictions
predictTest <- predict(censustree, newdata = test)
table(test$over50k, predictTest[,2] >= .5)
(9243 + 1596) / nrow(test)
# 2.3 - AUC
ROCRpred <- prediction(predictTest[,2], test$over50k)
as.numeric(performance(ROCRpred, 'auc')@y.values)

#########################################################
library(randomForest)
set.seed(1)
trainSmall <- train[sample(nrow(train), 2000),]

# 3.1 - Random Forest
set.seed(1)
censusrf <- randomForest(over50k ~ ., data = train)
predictTest <- predict(censusrf, newdata = test)
table(test$over50k, predictTest)
(9015 + 2059) / nrow(test)

# 3.2 - Repeatedly used variables
vu <- varUsed(censusrf, count = TRUE)
vusorted <- sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(censusrf$forest$xlevels[vusorted$ix]))

# 3.3 - Impurity
varImpPlot(censusrf)

#########################################################

# 4.1 - Cross-validation
library(caret)
library(e1071)
set.seed(2)
numFolds <- trainControl(method = 'cv', number = 10)
cartGrid <- expand.grid(.cp = seq(.002, .1, .002))
train(over50k ~ ., data = train, method = "rpart", trControl = numFolds, tuneGrid = cartGrid)

# 4.2 - CV: Model
censuscv <- rpart(over50k ~ ., data = train, method = 'class', cp = .002)
predictTest <- predict(censuscv, newdata = test)
table(test$over50k, predictTest[,2] >= .5)
(9178 + 1838)/nrow(test)

# 4.3 - CV: tree
prp(censuscv)
