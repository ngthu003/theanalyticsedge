### Unit 3
# Assignment 3.1: Popularity of Music Records

# 1.1
songs <- read.csv('songs.csv')
table(songs$year)

# 1.2
table(songs$artistname == 'Michael Jackson')

# 1.3
subset(songs, artistname == 'Michael Jackson' & Top10 == 1)['songtitle']

# 1.4
unique(songs$timesignature)
table(songs$timesignature)

# 1.5
songs[which.max(songs$tempo), 'songtitle']

########################################################

# 2.1
SongsTrain <- subset(songs, year <= 2009)
SongsTest  <- subset(songs, year == 2010)

# 2.2
# Building Logistic Regression model
nonvars <- c('year', 'songtitle', 'artistname', 'songID', 'artistID')
SongsTrain <- SongsTrain[, !(names(SongsTrain) %in% nonvars)]
SongsTest  <- SongsTest[, !(names(SongsTest) %in% nonvars)]

SongsLog1 <- glm(Top10 ~ ., data = SongsTrain, family = binomial)
summary(SongsLog1)

########################################################

# 3.1
cor(SongsTrain$loudness, SongsTrain$energy)

# 3.2
# Model 2: keep 'energy', remove 'loudness'
SongsLog2 <- glm(Top10 ~ . -loudness, data = SongsTrain, family = binomial)
summary(SongsLog2)

# 3.3
# Model 3: remove 'energy', keep 'loudness'
SongsLog3 <- glm(Top10 ~ . -energy, data = SongsTrain, family = binomial)
summary(SongsLog3)

########################################################

# 4.1
# Prediction
pred3 <- predict(SongsLog3, newdata = SongsTest, type = 'response')
table(SongsTest$Top10, pred3 >= .45)

# 4.2: Baseline: default to not Top10
table(SongsTest$Top10)
314/nrow(SongsTest)

# 4.3
table(SongsTest$Top10, pred3 >= .45)

# 4.4
# sensitivity
19/(19+40)
# specificity
309/(309+5)
