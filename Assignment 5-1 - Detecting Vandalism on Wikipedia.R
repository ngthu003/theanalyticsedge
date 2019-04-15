# 5.1 - Detecting Vandalism on Wikipedia

# 1.1
wiki <- read.csv('wiki.csv', stringsAsFactors = FALSE)
wiki$Vandal <- as.factor(wiki$Vandal)
table(wiki$Vandal)

# 1.2 - Bags of Words - Pre-Process
library(tm)
corpusAdded <- VCorpus(VectorSource(wiki$Added))
corpusAdded[[1]]$content
corpusAdded <- tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded <- tm_map(corpusAdded, stemDocument)
corpusAdded[[1]]$content

dtmAdded <- DocumentTermMatrix(corpusAdded)
dtmAdded

# 1.3 - Sparse-frequencies
# Remove sparse terms
sparseAdded <- removeSparseTerms(dtmAdded, 0.997)
sparseAdded
# 1.4 - data.frame
wordsAdded <- as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) <- paste("A", colnames(wordsAdded))

# Similar for Removed words
corpusRemoved <- VCorpus(VectorSource(wiki$Removed))
corpusRemoved <- tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved <- tm_map(corpusRemoved, stemDocument)
dtmRemoved <- DocumentTermMatrix(corpusRemoved)
dtmRemoved
sparseRemoved <- removeSparseTerms(dtmRemoved, 0.997)
sparseRemoved
wordsRemoved <- as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) <- paste("R", colnames(wordsRemoved))

# 1.5 - Combine Added + Removed words
wikiWords <- cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal <- wiki$Vandal
table(wikiWords$Vandal)
# Partition into Train and Test data sets
library(caTools)
set.seed(123)
spl <- sample.split(wikiWords$Vandal, SplitRatio = .7)
wikiTrain <- subset(wikiWords, spl == TRUE)
wikiTest <- subset(wikiWords, spl == FALSE)
# Baseline
table(wikiTest$Vandal)
618/(618+545)

# 1.6 - CART
library(rpart)
library(rpart.plot)
wikiCART <- rpart(Vandal ~ ., data = wikiTrain, method = 'class')
testPredictCART <- predict(wikiCART, newdata = wikiTest, type = 'class')
table(wikiTest$Vandal, testPredictCART)
(618+12)/nrow(wikiTest)

# 1.7 - CART tree
prp(wikiCART)

###########################################################

# 2.1 - Problem-Specific Knowledge
wikiWords2 <- wikiWords
wikiWords2$HTTP <- ifelse(grepl('http', wiki$Added, fixed = TRUE), 1, 0)
table(wikiWords2$HTTP)

# 2.2
wikiTrain2 <- subset(wikiWords2, spl == TRUE)
wikiTest2 <- subset(wikiWords2, spl == FALSE)
wikiCART2 <- rpart(Vandal ~ ., data = wikiTrain2, method = 'class')
testPredictCART2 <- predict(wikiCART2, newdata = wikiTest2, type = 'class')
table(wikiTest2$Vandal, testPredictCART2)
(609 + 57)/nrow(wikiTest2)

# 2.3
wikiWords2$NumWordsAdded <- rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved <- rowSums(as.matrix(dtmRemoved))
summary(wikiWords2$NumWordsAdded)

# 2.4
wikiTrain3 <- subset(wikiWords2, spl == TRUE)
wikiTest3 <- subset(wikiWords2, spl == FALSE)
wikiCART3 <- rpart(Vandal ~ ., data = wikiTrain3, method = 'class')
testPredictCART3 <- predict(wikiCART3, newdata = wikiTest3, type = 'class')
table(wikiTest3$Vandal, testPredictCART3)
(514+248)/nrow(wikiTest3)

###########################################################

# 3.1 - Non-Textual data
wikiWords3 <- wikiWords2
wikiWords3$Minor <- wiki$Minor
wikiWords3$Loggedin <- wiki$Loggedin
wikiTrain4 <- subset(wikiWords3, spl == TRUE)
wikiTest4 <- subset(wikiWords3, spl == FALSE)
wikiCART4 <- rpart(Vandal ~ ., data = wikiTrain4, method = 'class')
testPredictCART4 <- predict(wikiCART4, newdata = wikiTest4, type = 'class')
table(wikiTest4$Vandal, testPredictCART4)
(595 + 241) / nrow(wikiTest4)

# 3.2
prp(wikiCART4)
