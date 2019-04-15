# 5.2 - Automating Reviews in Medicine

# 1.1
trials <- read.csv('clinical_trial.csv', stringsAsFactors = FALSE)
str(trials)
summary(nchar(trials$abstract))

# 1.2 No abstract
table(nchar(trials$abstract) == 0)

# 1.3 Min nchar(title)
which.min(nchar(trials$title))
trials$title[1258]

################################################################

# 2.1 Preparing the Corpus
library(tm)
corpusTitle <- VCorpus(VectorSource(trials$title))
corpusTitle <- tm_map(corpusTitle, content_transformer(tolower))
corpusTitle <- tm_map(corpusTitle, removePunctuation)
corpusTitle <- tm_map(corpusTitle, removeWords, stopwords('english'))
corpusTitle <- tm_map(corpusTitle, stemDocument)
dtmTitle <- DocumentTermMatrix(corpusTitle)
dtmTitle
dtmTitle <- removeSparseTerms(dtmTitle, .95)
dtmTitle
# Abstract
corpusAbstract <- VCorpus(VectorSource(trials$abstract))
corpusAbstract <- tm_map(corpusAbstract, content_transformer(tolower))
corpusAbstract <- tm_map(corpusAbstract, removePunctuation)
corpusAbstract <- tm_map(corpusAbstract, removeWords, stopwords('english'))
corpusAbstract <- tm_map(corpusAbstract, stemDocument)
dtmAbstract <- DocumentTermMatrix(corpusAbstract)
dtmAbstract
dtmAbstract <- removeSparseTerms(dtmAbstract, .95)
dtmAbstract

# 2.3
dtmTitle <- as.data.frame(as.matrix(dtmTitle))
dtmAbstract <- as.data.frame(as.matrix(dtmAbstract))
which.max(colSums(dtmAbstract))

################################################################

# 3.1
colnames(dtmTitle) <- paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) <- paste0("A", colnames(dtmAbstract))
dtm <- cbind(dtmTitle, dtmAbstract)
dtm$trial <- trials$trial

# 3.3 Baseline Model
library(caTools)
set.seed(144)
spl <- sample.split(dtm$trial, SplitRatio = .7)
train <- subset(dtm, spl == TRUE)
test <- subset(dtm, spl == FALSE)
table(train$trial)
730/nrow(train)

# 3.4 CART
library(rpart)
library(rpart.plot)
trialCART <- rpart(trial ~ ., data = train, method = 'class')
prp(trialCART)
# 3.5
pred <- predict(trialCART)
summary(pred[,2])
# 3.7
table(train$trial, pred[,2] >= .5)
# Training set accuracy
(631 + 441) / nrow(train)
# Sensitivity: TP/(TP+FN)
441/(441+131)
# Specificity: TN/(TN+FP)
631/(631+99)

################################################################

# 4.1
predTest <- predict(trialCART, newdata = test)
table(test$trial, predTest[,2] >= .5)
(261+162)/nrow(test)

# 4.2 ROCR
library(ROCR)
predROCR = prediction(predTest[,2], test$trial)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
# Compute AUC
performance(predROCR, "auc")@y.values
