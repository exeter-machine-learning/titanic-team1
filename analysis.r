require(Hmisc)
require(stringr)
require(caret)

# Read data
readData <- function(path.name, file.name, column.types, missing.types) {
    read.csv(paste(path.name, file.name, sep=""), colClasses=column.types, na.strings=missing.types)
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

imputeMedian <- function(impute.var, filter.var, var.levels) {
    for (v in var.levels) {
        impute.var[which(filter.var == v)] <- impute(impute.var[which(filter.var == v)])
    }
    return (impute.var)
}

# Feature normalization
df.train$Title <- getTitle(df.train)

df.train$Embarked[which(is.na(df.train$Embarked))] <- 'S'

df.train$Fare[which(df.train$Fare == 0 )] <- NA
df.train$Fare <- imputeMedian(df.train$Fare, df.train$Pclass, as.numeric(levels(df.train$Pclass)))

df.train$Age <- imputeMedian(df.train$Age, df.train$Title, c("Dr", "Master", "Mrs", "Miss", "Mr"))

# Feature engineering - helper functions
isEven <- function(x) x %in% c("0", "2", "4", "6", "8")
isOdd <- function(x) x %in% c("1", "3", "5", "7", "9")


# Feature engineering
featureEng <- function(data) {
    data$FamilySize <- data$SibSp + data$Parch

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

df.train <- featureEng(df.train)
train.keeps <- c("Survived", "Sex", "Age", "Pclass", "Deck", "Side", "Fare", "Embarked", "FamilySize")
df.train.munged <- df.train[train.keeps]

set.seed(23)
training.rows <- createDataPartition(df.train.munged$Survived, p= 0.8, list = FALSE)
train.batch <- df.train.munged[training.rows,]
test.batch <- df.train.munged[-training.rows,]

# Logistic regression
Titanic.logit.1 <- glm(Survived ~ Sex + Age + Pclass + Deck + Side + Embarked + FamilySize, data = train.batch, family = binomial("logit"))
