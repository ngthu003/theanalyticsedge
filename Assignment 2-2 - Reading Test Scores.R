### 2.2: Reading Test Scores

# 1.1
pisaTrain <- read.csv('pisa2009train.csv')
pisaTest <- read.csv('pisa2009test.csv')

# 1.2
tapply(pisaTrain$readingScore, pisaTrain$male, mean)

# 1.3
summary(pisaTrain)

# 1.4
pisaTrain <- na.omit(pisaTrain)
pisaTest <- na.omit(pisaTest)

#####################################################

# 3.1
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

lmScore = lm(readingScore ~ ., data = pisaTrain)
summary(lmscore)

# 3.2: SSE from model
SSE = sum(lmScore$residuals^2)
RMSE = sqrt(SSE / nrow(pisaTrain))

#####################################################

# 4.1: prediction
predTest = predict(lmScore, newdata=pisaTest)
summary(predTest)

# 4.2: SSE from prediction
sum((predTest-pisaTest$readingScore)^2)
sqrt(mean((predTest-pisaTest$readingScore)^2))

# 4.3: Baseline prediction & test-set SSE
baseline = mean(pisaTrain$readingScore)
sum((baseline-pisaTest$readingScore)^2).

# 4.4: Test-set R-squared
SSE = sum((predTest - pisaTest$readingScore)^2)
SST = sum((mean(pisaTrain$readingScore) - pisaTest$readingScore)^2)
R2 = 1 - SSE/SST
R2
