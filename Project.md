---
title: "Course Project Machine Learning"
author: "Deborah Castillo"
date: "17/10/2017"
output: html_document
---

##First, I will load the data and corresponding packages that will be used.

```{r importing data}
testing <- read.csv(file = "pml-testing.csv", header = TRUE)
training <- read.csv(file = "pml-training.csv", header = TRUE)
library(caret)
```

##To use cross validation, it is neccesary to split the training dataframe again. Conventionally, the split is made 60% into the training set and 40 into the new testing set.

```{r split data}
inTrain <- createDataPartition(y = training$classe, p =0.6, list = FALSE)
trains <- training[inTrain, ]
tests <- testing[-inTrain, ]
```


##Next, the data needs to be cleaned. For this, I will only extract the variables I consider relevant for the analysis. This is done because I want to use the random forest method for prediction, and it takes a lot of RAM memory, so I want to keep only the neccesary variables. 
```{r cleaning data}
relvar <- names(trains) %in% c("classe", "accel_arm_x", "accel_arm_y", "accel_arm_z", "accel_belt_x", "accel_belt_y", "accel_belt_z", "accel_dumbbell_x", "accel_dumbbell_y", "accel_dumbbell_z", "accel_forearm_x", "accel_forearm_y",     "accel_forearm_z", "gyros_arm_x", "gyros_arm_y", "gyros_arm_z", "gyros_belt_x", "gyros_belt_y", "gyros_belt_z",  "gyros_dumbbell_x", "gyros_dumbbell_y", "gyros_dumbbell_z", "gyros_forearm_x", "gyros_forearm_y", "gyros_forearm_z",  "magnet_arm_x", "magnet_arm_y", "magnet_arm_z", "magnet_belt_x", "magnet_belt_y", "magnet_belt_z", "magnet_dumbbell_x", "magnet_dumbbell_y", "magnet_dumbbell_z", "magnet_forearm_x", "magnet_forearm_y", "magnet_forearm_z", "pitch_arm", "pitch_belt", "pitch_dumbbell", "pitch_forearm", "roll_arm", "roll_belt", "roll_dumbbell", "roll_forearm", "total_accel_arm", "total_accel_belt", "total_accel_dumbbell", "total_accel_forearm", "yaw_arm", "yaw_belt", "yaw_dumbbell", "yaw_forearm")
trains <- trains[relvar]
tests <- tests[relvar]
tests$problem_id <- sample(trains$classe, size = nrow(tests), replace = TRUE)
tests$problem_id <- factor(tests$problem_id)
colnames(tests)[53] <- "classe"
```

##Following, a predictive model will be made using the ranom forests package, as mentioned before

```{r creating model}
library(rpart)
library(randomForest)
fit1 <- rpart(classe ~ . , data = trains)
fit2 <- randomForest(classe ~ . , data = trains)
fit2
```

##As we can see above, the OOB is extremely high, this is why cross-validation is so important in random forest. 
##I will graph the prior, and then do some cross validation

```{r random tree graph}
library(rpart.plot)
rpart.plot(fit1)
```
```{r cross validation}
prediction <- predict(fit1, tests, type = "class")
confusionMatrix(tests$classe, prediction)
```

####As we can see above, I only obtained a 22% accuracy in the rpart method. This might be because the model does not fit the data correcly.

##Final prediction
```{r final prediction}
finalpredict <- predict(fit1, testing, type = "class")
print(finalpredict)
```
