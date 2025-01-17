library(mlbench)
library(caret)
library(yardstick)
library(tibble)

data("BostonHousing")

sum(is.na(BostonHousing))

set.seed(42)

trainingIndex <- createDataPartition(BostonHousing$medv, p=.8, list = FALSE)
trainSet <- BostonHousing[trainingIndex,]
testSet <- BostonHousing[-trainingIndex,]

model <- train(medv ~ .,
               data = trainSet,
               method = "lm",
               na.action = na.omit,
               preProcess=c("scale", "center"),
               trControl=trainControl(method = "none")
)

model.train <- predict(model, trainSet)
model.test <- predict(model, testSet)

# Scatter plot of Train Set and Test Set
plot(trainSet$medv, model.train, col = "blue")
plot(testSet$medv, model.test, col = "blue")

# Regression Evaluation Metrics
data_train <- tibble(actual = trainSet$medv, predicted = model.train)
data_test <- tibble(actual = testSet$medv, predicted = model.test)

metrics <- metric_set(mae, rmse, mape, rsq)
metrics(data_train, truth = actual, estimate = predicted)
metrics(data_test, truth = actual, estimate = predicted)

# Model performance summary
summary(model)

# Pearson's correlation coefficient
cor(model.train, trainSet$medv)
cor(model.test, testSet$medv)