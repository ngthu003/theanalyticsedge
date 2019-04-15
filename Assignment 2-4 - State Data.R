### 2.4: State Data

### Load data
data(state)
statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center,  state.division, state.name, state.region)

# 1.1
plot(statedata$x, statedata$y)

# 1.2
tapply(statedata$HS.Grad, statedata$state.region, mean)

# 1.3: Boxplot
boxplot(Murder ~ state.region, data = statedata)

# 1.4: Outlier from boxplot
temp <- subset(statedata, state.region == 'Northeast')
temp1 <- subset(temp, Murder == max(Murder))

########################################################

# 2.1
LinReg = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data=statedata)
summary(LinReg)

# 2.3
plot(statedata$Income, statedata$Life.Exp)

########################################################

# 3.1
# Try removing variables until all have p-value ~ < .05
pred2 = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost, data = statedata)
summary(pred2)
pred3 = lm(Life.Exp ~ Population + Income + Murder + HS.Grad + Frost, data = statedata)
summary(pred3)
pred4 = lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data = statedata)
summary(pred4)

# 3.3
pred = predict(pred4)
sort(pred)
which.min(statedata$Life.Exp)
statedata$state.name[40]

# 3.4
which.max(statedata$Life.Exp)
statedata$state.name[11]

# 3.5
sort(abs(model$residuals))