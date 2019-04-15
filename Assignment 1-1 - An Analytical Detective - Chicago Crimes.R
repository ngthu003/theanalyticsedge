# Loading data
mvt <- read.csv('mvtWeek1.csv')
colnames(mvt)

# 1.3
max(mvt$ID)
# 1.4
min(mvt$Beat)
# 1.5
sum(mvt$Arrest)
# 1.6
nrow(subset(mvt, LocationDescription == 'ALLEY'))

####################################################

# 2.2
# Converting to Datetime
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)

# 2.3
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert
table(mvt$Month)

# 2.4
table(mvt$Weekday)

####################################################

# 3.1
hist(mvt$Date, breaks = 100)

# 3.2
# Plotting boxplot Date vs. Arrest
boxplot(mvt$Date ~ mvt$Arrest)

# 3.3
table(mvt$Arrest, mvt$Year == 2001)

# 4.1
# Sorting a table from small to large
sort(table(mvt$LocationDescription))

# 4.2
TopLocations = c("STREET", "PARKING LOT/GARAGE(NON.RESID.)", "ALLEY", "GAS STATION", "DRIVEWAY - RESIDENTIAL")
Top5 = subset(mvt, LocationDescription %in% TopLocations)

# 4.3
Top5$LocationDescription = factor(Top5$LocationDescription)
table(Top5$Arrest, Top5$LocationDescription)

# 4.4
table(Top5$LocationDescription, Top5$Weekday)