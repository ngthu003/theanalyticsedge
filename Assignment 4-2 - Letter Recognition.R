# 4.2 - Letter Recognition

# 1.1
letters <- read.csv("letters_ABPR.csv")
# Making new isB binary rv
letters$isB <- as.factor(letters$letter == 'B')
library(caTools)
set.seed(1000)
split <- sample.split(letters$isB, SplitRatio = 0.5)
train <- subset(letters, split==TRUE)
test <- subset(letters, split==FALSE)
# Baseline
table(test$isB)
1175/nrow(test)

# 1.2 - Classification Tree
CARTb <- rpart(isB ~ . -letter, data = train, method = 'class')
predictions <- predict(CARTb, newdata = test, type = 'class')
table(predictions, test$isB)
(1118 + 340) / nrow(test)

# 1.3 - Random Forest
library(randomForest)
set.seed(1000)
RFb = randomForest(isB ~ . -letter, data = train)
predictions2 = predict(RFb, newdata = test)
table(predictions2, test$isB)
(1163 + 374) / nrow(test)

#########################################################

# 2.1
letters$letter <- as.factor(letters$letter)
set.seed(2000)
spl <- sample.split(letters$letter, SplitRatio = .5)
train2 <- subset(letters, spl==TRUE)
test2 <- subset(letters, spl==FALSE)
table(test2$letter)
401/nrow(test2)

# 2.2 - Classification Tree on all 4; A,B,P,R
CARTletter <- rpart(letter ~ . -isB, data = train2, method = 'class')
predictLetter <- predict(CARTletter, newdata = test2, type = 'class')
table(predictLetter, test2$letter)
(348 + 318 + 363 + 340) / nrow(test2)

# 2.3 - Random Forest on all 4: A,B,P,R
set.seed(1000)
RFLetter = randomForest(letter ~ . -isB, data = train2)
predictLetter2 = predict(RFLetter, newdata = test2)
table(predictLetter2, test$letter)
(391 + 380 + 394 + 362) / nrow(test2)
