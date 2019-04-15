### 2.3: Detecting Flu Epidemics

### Load data
FluTrain <- read.csv('FluTrain.csv')

# 1.1
subset(FluTrain, ILI == max(ILI))
subset(FluTrain, Queries == max(Queries))

# 1.2
hist(FluTrain$ILI)

# 1.3
# Due to the distribution of ILI being right-skew, take the ln(ILI)
plot(FluTrain$Queries, log(FluTrain$ILI))

##################################################

# 2.2
FluTrend1 = lm(log(ILI)~Queries, data=FluTrain)
summary(FluTrend1)

# 2.3
# cor = (R^2)^2

##################################################

# 3.1
# Build Prediction, remember to take exp()
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
# Look for % of ILI-related visits during
which(FluTest$Week == "2012-03-11 - 2012-03-17")
PredTest1[11]

# 3.2
# Evaluate "Correctness": relative error:
# (Observed ILI - Estimated ILI)/Observed ILI
(FluTest$ILI[11] - PredTest1[11]) / FluTest$ILI[11]

# 3.3: RMSE on Test-set
SSE = sum((PredTest1-FluTest$ILI)^2)
RMSE = sqrt(SSE / nrow(FluTest))

##################################################

### Time Series Model

# 4.1
library(zoo)
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)

# 4.2
plot(log(FluTrain$ILILag2), log(FluTrain$ILI))

# 4.3
FluTrend2 = lm(log(ILI)~Queries+log(ILILag2), data=FluTrain)
summary(FluTrend2)

##################################################

# 5.1
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)

# 5.3
# Fill in NAs in FluTest
FluTest$ILILag2[1] = FluTrain$ILI[nrow(FluTrain) - 1]
FluTest$ILILag2[2] = FluTrain$ILI[nrow(FluTrain)]

# 5.4
PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
SSE = sum((PredTest2-FluTest$ILI)^2)
RMSE = sqrt(SSE / nrow(FluTest))
