library(randomForest)
library(caret)
library(datasets)

data("iris")

trainIndex <- createDataPartition(iris$Species, p=.8, list = FALSE)
trainSet <- iris[trainIndex,]
testSet <- iris[-trainIndex,]

model <- randomForest(
  Species ~ .,
  data = trainSet,
  ntree = 500,
  mtry = 4,
  importance = TRUE
)

saveRDS(model, "D:/R Projs/shiny_iris/model.rds")