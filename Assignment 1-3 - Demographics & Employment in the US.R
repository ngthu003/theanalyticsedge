# Load data
cps <- read.csv('CPSData.csv')

# 1.2, 1.3,
sort(table(cps$Industry))
sort(table(cps$State))

# 1.4
table(cps$Citizenship)
1 - (7590/nrow(cps))

# 1.5
table(cps$Race, cps$Hispanic)

##########################################

# 2.1
summary(cps)

# 2.2
table(cps$Region, is.na(cps$Married))
table(cps$Sex, is.na(cps$Married))
table(cps$Age, is.na(cps$Married))
table(cps$Citizenship, is.na(cps$Married))

# 2.3
table(cps$State, is.na(cps$MetroAreaCode))
# All in Metro <=> is.na(MetroAreaCode)|| T = 0
# No-one in Metro <=> is.na(MetroAreaCode)|| F = 0

# 2.4
table(cps$Region, is.na(cps$MetroAreaCode))

# 2.5
# Using tapply to speed up proportion finding
sort(tapply(is.na(cps$MetroAreaCode), cps$State, mean))

##########################################

# 3.1
MetroAreaMap = read.csv('MetroAreaCodes.csv')
CountryMap = read.csv('CountryCodes.csv')

# 3.2
cps = merge(cps, MetroAreaMap, by.x = 'MetroAreaCode', by.y = 'Code', all.x = TRUE)
colnames(cps)
sum(is.na(cps$MetroArea))

# 3.3
head(sort(table(cps$MetroArea), decreasing = TRUE), 10)

# 3.4
head(sort(tapply(cps$Hispanic, cps$MetroArea, mean), decreasing = TRUE),5)

# 3.5
head(sort(tapply(cps$Race == 'Asian', cps$MetroArea, mean), decreasing = TRUE),10)
# or
table(tapply(cps$Race == 'Asian', cps$MetroArea, mean) >= .2)

# 3.6
head(sort(tapply(cps$Education == 'No high school diploma', cps$MetroArea, mean, na.rm = TRUE)))

##########################################

# 4.1
# Merging 2 tables
cps = merge(cps, CountryMap, by.x = 'CountryOfBirthCode', by.y = 'Code', all.x = TRUE)
colnames(cps)
sum(is.na(cps$Country))

# 4.2
head(sort(table(cps$Country), decreasing = T),5)

# 4.3
table(cps$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", cps$Country != "United States")
# or
tapply(cps$Country != 'United States', cps$MetroArea == 'New York-Northern New Jersey-Long Island, NY-NJ-PA', mean, na.rm = TRUE)
# answer: .309 or 1668/(1668+3736)

# 4.4
head(sort(tapply( cps$Country == 'India', cps$MetroArea, sum, na.rm=T), decreasing = T),5)
head(sort(tapply( cps$Country == 'Brazil', cps$MetroArea, sum, na.rm=T), decreasing = T),5)
head(sort(tapply( cps$Country == 'Somalia', cps$MetroArea, sum, na.rm=T), decreasing = T),5)

