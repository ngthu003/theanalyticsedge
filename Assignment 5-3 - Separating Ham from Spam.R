# 5.3 - Separating Spam from Ham - 1

# 1.1
emails <- read.csv('emails.csv', stringsAsFactors = FALSE)
# 1.2
table(emails$spam)
# 1.3
emails$text[1]
# 1.5
summary(nchar(emails$text))
which.min(nchar(emails$text))

#############################################################

# 2.1 Preparing the Corpus
library(tm)
corpus <- VCorpus(VectorSource(emails$text))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus <- tm_map(corpus, stemDocument)
dtm <- DocumentTermMatrix(corpus)
dtm

# 2.2 Sparse 5%
spdtm <- removeSparseTerms(dtm, .95)
spdtm

# 2.3 Data.frame
emailsSparse <- as.data.frame(as.matrix(spdtm))
which.max(colSums(emailsSparse))

# 2.4
emailsSparse$spam <- emails$spam
temp <- subset(emailsSparse, spam == 0)
table(colSums(temp) >= 5000)

# 2.5
temp <- subset(emailsSparse, spam == 1)
sort(colSums(temp) >= 1000)

#############################################################

emailsSparse$spam <- as.factor(emailsSparse$spam)
library(caTools)
set.seed(123)
spl <- sample.split(emailsSparse$spam, SplitRatio = .7)
train <- subset(emailsSparse, spl == TRUE)
test <- subset(emailsSparse, spl == FALSE)

# 3.1 Machine Learning Models
# Logistic Reg.
spamLog <- glm(spam ~ ., data = train, family = 'binomial')
summary(spamLog)
# CART
library(rpart)
library(rpart.plot)
spamCART <- rpart(spam ~ ., data = train, method = 'class')
# Random Forest
library(randomForest)
set.seed(123)
spamRF <- randomForest(spam ~ ., data = train)
spamRF <- randomForest(spam ~ ., data = train)


tweetRF = randomForest(Negative ~ ., data=trainSparse)

# Make predictions:
predictRF = predict(tweetRF, newdata=testSparse)