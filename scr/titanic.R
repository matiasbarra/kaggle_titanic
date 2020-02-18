## Kaggle Completition
## Titanic

# Goal
# It is your job to predict if a passenger survived the sinking of the Titanic or not.
# For each in the test set, you must predict a 0 or 1 value for the variable.

# Metric
# Your score is the percentage of passengers you correctly predict. This is known as accuracy.
#_______________________________________________________________________________________________

## Set enviroment
library(ggplot2)
library(pROC)
library(dplyr)
library(data.table) # In this exercise we going to work with data.table instead of data.frame

## Set Path
loc   <- grep("titanic.R",list.files(recursive=TRUE),value=TRUE)
iloc  <- which(unlist(gregexpr("/titanic.R",loc)) != -1)
myloc <- paste(getwd(),loc[iloc],sep="/")
setwd(substr(myloc,1,nchar(myloc)-9))


## Upload dataSets
trainSet <- fread("data/train.csv")
testSet <- fread("data/test.csv")

# we have to merge both data frames to preprocess data
# Create a column "type" to separate dataSets in future

trainSet[, type := "trainSet"]
testSet[, type := "testSet"]
testSet[, Survived := NA]

titanic <- rbind(trainSet, testSet)
str(titanic)

# as. factor
titanic$PassengerId <- as.factor(titanic$PassengerId)
titanic$Survived <- as.factor(titanic$Survived)
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Sex <- as.factor(titanic$Sex)

## Exploratory Analysis

# Survivers
ggplot(data = titanic[type == "trainSet"], aes(x = Survived, fill = Survived)) +
            geom_bar(aes(y = (..count..)/sum(..count..))) + 
            ggtitle("How many people has been survived")
  
# Survivers by Sex /*  
ggplot(data = titanic[type == "trainSet"], aes(x = Sex, fill = Survived)) +
                geom_bar(aes(y = (..count..)/sum(..count..)), position = "fill") + 
                ggtitle("How many people has been survived by Sex")

# Survivers by Age   
ggplot(data = titanic[type == "trainSet"], aes(x = Age, fill = Survived)) +
  geom_histogram(binwidth = 10) + 
  ggtitle("How many people has been survived by Age")

# Survivers by Fare   
ggplot(data = titanic[type == "trainSet"], aes(x = Fare, fill = Survived)) +
  geom_histogram(binwidth = 10) + 
  ggtitle("How many people has been survived by Age")

# Survivers by Class
ggplot(data = titanic[type == "trainSet"], aes(x = Pclass, fill = Survived)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position = "fill") + 
  ggtitle("How many people has been survived by Class")

# Survivers by Embarked
ggplot(data = titanic[type == "trainSet"], aes(x = Embarked, fill = Survived)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position = "fill") + 
  ggtitle("How many people has been survived by Embarked")

# Survivers by SibSp / Confusing and Not relevant 
ggplot(data = titanic[type == "trainSet"], aes(x = SibSp, fill = Survived)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position = "fill") + 
  ggtitle("How many people has been survived by SibSp")

# Survivers by Parch / Confusing and Not relevant 
ggplot(data = titanic[type == "trainSet"], aes(x = Parch, fill = Survived)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position = "fill") + 
  ggtitle("How many people has been survived by Parch")

summary(titanic)
# this shows us that there is a lot of NAs, we have to deal with this 

###-------------------- Fix Embarked NAs
table(titanic$Embarked)
# Assign 2 NAs to majority class (S in this case)
titanic[Embarked == "", Embarked := "S"]

###-------------------- Fix Age and Fare NAs
# Assign the median
titanic[is.na(Age), Age := median(titanic$Age, na.rm = T)]
titanic[is.na(Fare), Fare := median(titanic$Fare, na.rm = T)]

####---------------------------------------- Feature Engineering ----------------#### 

## table(titanic$Cabin) show us  1014 passengers without cabin, need to fix it
## Create hasCabin variable to see which passengers has or not has cabin
titanic$hasCabin <- as.factor(!(titanic$Cabin == ""))

# Survivers by Cabin   
ggplot(data = titanic[type == "trainSet"], aes(x = hasCabin, fill = Survived)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position = "fill") + 
  ggtitle("How many people has been survived by Cabin")
# People who has cabin survived more than people has not cabin 


# Extract the title like "Mr", "Miss", "Lord".. of the Names to clasify people denpending of their tittle
# Create Title extracting, of Name variable, all characters and coma | the dot and all after dot 
titanic$tit <- gsub('(.*, )|(\\..*)','', titanic$Name) 
table(titanic$tit)

# creat small groups
titanic[tit == "Dona", tit := "Mrs"]
titanic[tit == "Lady", tit := "Mrs"]
titanic[tit == "Ms", tit := "Miss"]

titanic[tit == "Don", tit := "Mr"]
titanic[tit == "Sir", tit := "Mr"]

titanic[tit %in% c("Capt", "Col", "Dr", "Jonkheer", "Major", "Mlle", "Mme", "Rev", "the Countess"), 
        tit := "special"]

ggplot(data = titanic[type == "trainSet"], aes(x = tit, fill = Survived)) +
  geom_bar() + ggtitle("How many people has been survived by Title")
# this plot show us a lot of titles groups, we have to reduce it



# 3 Group by Ages
titanic[Age >=0 & Age < 18, ageGroup := "chilTeen"]
titanic[Age >=18 & Age < 30, ageGroup := "young"]
titanic[Age >= 30 & Age < 60, ageGroup := "adult"]
titanic[Age >= 60, ageGroup := "old"]

ggplot(data = titanic[type == "trainSet"], aes(x = ageGroup, fill = Survived)) +
  geom_bar() + ggtitle("How many people has been survived by Age")

ggplot(data = titanic[type == "trainSet"], aes(x = ageGroup, fill = Survived)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), position = "fill") + 
  ggtitle("How many people has been survived by Age")

# Set seed
set.seed(123)

## Build First Model with GLM to create our Binary model

model_glm <- glm(Survived ~ Sex + Pclass + ageGroup + Fare + Embarked + tit + hasCabin, 
                 data = titanic[type == "trainSet"], family = binomial)

## First Predictions 
pred_glm <- predict(model_glm, titanic)

## assign Predictions
titanic[, pred := pred_glm]

## Calculate ROC
roc_glm <- roc(titanic$Survived, titanic$pred)
roc_glm
plot(roc_glm)

# Asign probabilities to 0 and 1
titanic[pred >= 0.55, pred_group := 1]
titanic[pred < 0.55, pred_group := 0]

tbl_glm <- table(titanic$Survived, titanic$pred_group)

precisition <- (tbl_glm[1,1] + tbl_glm[2,2]) / sum(tbl_glm)
# score 0.8114478

# Spliting Train DF in trainTrain and trainTest and repeat the previous model 

sizePart <- floor(0.75*nrow(titanic[type == "trainSet"]))
set.seed(123)

position <- sample(seq_len(nrow(titanic[type == "trainSet"])), size = sizePart)
trainTrain <- titanic[type == "trainSet"][position]
trainTest <- titanic[type == "trainSet"][-position]


set.seed(123)
## Build First Model with GLM to create our Binary model
Model_glm1 <- glm(Survived ~ Sex + Pclass + ageGroup + Fare + Embarked + tit + hasCabin, 
                 data = trainTrain, family = binomial)

## First Predictions 
pred_Model_glm1  <- predict(Model_glm1, trainTest)

## assign Predictions
trainTest[, pred := pred_Model_glm1]

## Calculate ROC
roc_Model_glm1 <- roc(trainTest$Survived, trainTest$pred)
roc_Model_glm1
plot(Model_glm1)

# Asign probabilities to 0 and 1
trainTest[pred >= 0.55, pred_group := 1]
trainTest[pred < 0.55, pred_group := 0]

tbl_Model_glm1 <- table(trainTest$Survived, trainTest$pred_group)

precisition1 <- (tbl_Model_glm1[1,1] + tbl_Model_glm1[2,2]) / sum(tbl_Model_glm1)
precisition1

# calculate predictions to testSet
predictions <- predict(Model_glm1, titanic[type == "testSet"])

#asign predictions
titanic[type == "testSet", pred := predictions]
# Asign probabilities to 0 and 1
titanic[type == "testSet" & pred >= 0.55, pred_group := 1]
titanic[type == "testSet" & pred < 0.55, pred_group := 0]

toExport <- titanic[type == "testSet", c("PassengerId", "pred_group")] 
setnames(toExport, "pred_group", "Survived")

fwrite(toExport, "titanicPredInitial.csv", row.names = F)

