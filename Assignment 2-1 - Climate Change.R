### Unit 2
# Assignment 2.1: Climate Change

# 1.1
climate = read.csv("climate_change.csv")
train = subset(climate, Year <= 2006)
test = subset(climate, Year > 2006)

# build model
climatelm = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=train)
summary(climatelm)

# 2.2
cor(train)

# 3
LinReg = lm(Temp ~ MEI + N2O + TSI + Aerosols, data=train)
summary(LinReg)

# 4
newlm <- step(climatelm)
summary(newlm)

# 5
tempPredict = predict(climateStep, newdata = test)
SSE = sum((tempPredict - test$Temp)^2)
SST = sum( (mean(train$Temp) - test$Temp)^2)
R2 = 1 - SSE/SST