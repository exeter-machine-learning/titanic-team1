require(Hmisc)
require(plyr)
require(stringr)
require(caret)

# Read data
readData <- function(path.name, file.name, column.types, missing.types) {
    read.csv(paste(path.name, file.name, sep=""),
        colClasses=column.types,
        na.strings=missing.types)
}

# File paths
Titanic.path <- "data/"
train.data.file <- "train.csv"
test.data.file <- "test.csv"

# Data types
missing.types <- c("NA", "")
train.column.types <- c('integer',      # PassengerId
                        'factor',       # Survived
                        'factor',       # Pclass
                        'character',    # Name
                        'factor',       # Sex
                        'numeric',      # Age
                        'integer',      # SibSp
                        'integer',      # Parch
                        'character',    # Ticket
                        'numeric',      # Fare
                        'character',    # Cabin   
                        'factor'        # Embarked
                        )

test.column.types <- train.column.types[-2]

train.raw <- readData(Titanic.path, train.data.file, train.column.types, missing.types)
df.train <- train.raw
test.raw <- readData(Titanic.path, test.data.file, test.column.types, missing.types)
df.infer <- test.raw

# Feature normalization - helper functions
getTitle <- function(data) {
    title.dot.start <- regexpr("\\, [A-Z ]{1,20}\\.", data$Name, TRUE)
    title.comma.end <- title.dot.start + attr(title.dot.start, "match.length") - 1

    data$Title <- substr(data$Name, title.dot.start + 2, title.comma.end - 1)

    return (data$Title)
}

changeTitle <- function(data, old.titles, new.title) {
    for (title in old.titles) {
        data$Title[which(data$Title == title)] <- new.title
    }

    return (data$Title)
}

consolidatedTitles <- function(data){
    data$Title <- getTitle(data)
    data$Title <- changeTitle(data, c('Capt', 'Col', 'Don', 'Jonkheer', 'Major', 'Sir', 'Lady', 'Dr', 'Rev'), 'Noble')
    data$Title <- changeTitle(data, c('the Countess', 'Dona', 'Ms'), 'Mrs')
    data$Title <- changeTitle(data, c('Mlle', 'Mme'), 'Miss')
    data$Title <- as.factor(data$Title)

    return (data$Title)
}

imputeMedian <- function(impute.var, filter.var, var.levels) {
    for (v in var.levels) {
        impute.var[which(filter.var == v)] <- impute(impute.var[which(filter.var == v)])
    }
    return (impute.var)
}

# Feature normalization
featureNorm <- function(data){
    data$Title <- consolidatedTitles(data)

    data$Embarked[which(is.na(data$Embarked))] <- 'S'

    data$Fare[which(data$Fare == 0)] <- NA
    data$Fare <- imputeMedian(data$Fare, data$Pclass, as.numeric(levels(df.train$Pclass)))

    data$Age <- imputeMedian(data$Age, data$Title, c("Noble", "Master", "Mr", "Mrs", "Miss"))

    return (data)
}

# Feature engineering - helper functions
isEven <- function(x) x %in% c("0", "2", "4", "6", "8")
isOdd <- function(x) x %in% c("1", "3", "5", "7", "9")


# Feature engineering
featureEng <- function(data) {
    data$Fate <- data$Survived
    data$Fate <- revalue(data$Fate, c("1" = "Survived", "0" = "Perished"))

    data$isChild <- "No"
    data$isChild[which(data$Age < 15)] <- "Yes"

    data$Family <- data$SibSp + data$Parch

    data$Deck <- substring(data$Cabin, 1, 1)
    data$Deck[which(is.na(data$Deck))] <- "UNK"
    data$Deck <- as.factor(data$Deck)

    data$cabin.last.digit <- str_sub(data$Cabin, -1)
    data$Side <- "UNK"
    data$Side[which(isEven(data$cabin.last.digit))] <- "port"
    data$Side[which(isOdd(data$cabin.last.digit))] <- "starboard"
    data$Side <- as.factor(data$Side)
    data$cabin.last.digit <- NULL

    return (data)
}

train.keeps <- c("Fate", "Sex", "isChild", "Age", "Pclass", "Deck", "Side", "Fare", "Embarked", "Family", "Title")

df.train.normalized <- featureNorm(df.train)
df.train.munged <- featureEng(df.train.normalized)[train.keeps]

set.seed(23)
training.rows <- createDataPartition(df.train.munged$Fate, p = 0.8, list = FALSE)
train.batch <- df.train.munged[training.rows,]
test.batch <- df.train.munged[-training.rows,]

# Logistic regression models
Titanic.logit.1 <- glm(Fate ~ Sex + Age + Pclass + Embarked + Title + Family + Deck + Side,
        data = train.batch,
        family = binomial("logit"))

cv.ctrl <- trainControl(method = "repeatedcv",
    repeats = 3,
    summaryFunction = twoClassSummary,
    classProb = TRUE)

set.seed(35)
# 76.077%
glm.tune.0 <- train(Fate ~ Sex + Age + Pclass + Embarked + Title + Family + Deck + Side,
    data = train.batch,
    method = "glm",
    metric = "ROC",
    trControl = cv.ctrl)

# 76.555%
glm.tune.1 <- train(Fate ~ Sex + Age + Pclass + I(Embarked == "S") + Title + Family + Deck + Side,
    data = train.batch,
    method = "glm",
    metric = "ROC",
    trControl = cv.ctrl)

# 76.077%
glm.tune.2 <- train(Fate ~ Pclass + I(Title=="Mr") + I(Title=="Lady") + Age + Family + I(Embarked=="S") + I(Title=="Mr"&Pclass=="Third"),
    data = train.batch,
    method = "glm",
    metric = "ROC",
    trControl = cv.ctrl)

# 76.077%
glm.tune.3 <- train(Fate ~ Sex + Age + Pclass + Family,
    data = train.batch,
    method = "glm",
    metric = "ROC",
    trControl = cv.ctrl)

#77.033%
glm.tune.4 <- train(Fate ~ Sex + Age + Pclass
    + I(Title=="Mr") + I(Title=="Noble") + I(Embarked=="S")
    + I(Title=="Mr"&Pclass==3),
    data = train.batch,
    method = "glm",
    metric = "ROC",
    trControl = cv.ctrl)

# 77.990%
glm.tune.5 <- train(Fate ~ isChild + Age + Pclass
    + I(Title=="Mr") + I(Title=="Noble") + I(Embarked=="S")
    + I(Title=="Mr"&Pclass==3),
    data = train.batch,
    method = "glm",
    metric = "ROC",
    trControl = cv.ctrl)

# 78.947%
glm.tune.6 <- train(Fate ~ isChild + Sex + Pclass
    + I(Title=="Mr") + I(Title=="Noble") + I(Embarked=="S")
    + I(Title=="Mr"&Pclass==3),
    data = train.batch,
    method = "glm",
    metric = "ROC",
    trControl = cv.ctrl)

# 76.555%
glm.tune.7 <- train(Fate ~ isChild + Sex + Pclass + Family
    + I(Title=="Mr") + I(Title=="Noble") + I(Embarked=="S")
    + I(Title=="Mr"&Pclass==3),
    data = train.batch,
    method = "glm",
    metric = "ROC",
    trControl = cv.ctrl)

# 78.947%
glm.tune.8 <- train(Fate ~ isChild + Sex + Pclass
    + I(Title=="Mr") + I(Title=="Noble") + I(Embarked=="S")
    + I(Title=="Mr"&Pclass==3),
    data = train.batch,
    method = "glmnet",
    metric = "ROC",
    trControl = cv.ctrl)

df.infer.normalized <- featureNorm(df.infer)
df.infer.munged <- featureEng(df.infer.normalized)

test.keeps <- train.keeps[-1]
glm.predict <- df.infer.munged[test.keeps]

Survived <- predict(glm.tune.9, newdata = glm.predict)

Survived <- revalue(Survived, c("Survived" = 1, "Perished" = 0))
predictions <- as.data.frame(Survived)
predictions$PassengerId <- df.infer$PassengerId

write.csv(predictions[,c("PassengerId", "Survived")],
    file = "output/predictions9.csv",
    row.names = FALSE,
    quote = FALSE)