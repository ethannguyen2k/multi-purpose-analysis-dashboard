library(datasets)
library(caret)

data(dhfr)

set.seed(42)

trainingIndex <- createDataPartition(dhfr$Y, p=.8, list = FALSE)
trainSet <- dhfr[trainingIndex,]
testSet <- dhfr[-trainingIndex,]

model <- train(Y ~ .,
               data = trainSet,
               method = "svmPoly",
               na.action = na.omit,
               preProcess=c("scale", "center"),
               trControl=trainControl(method = "none"),
               tuneGrid = data.frame(degree=1,scale=1,C=1))

saveRDS(model, "D:/R Projs/dhfr_model.rds")

read.model <- readRDS("D:/R Projs/dhfr_model.rds")

model.training <- predict(model, trainSet)
model.testing <- predict(model, testSet)

model.training.confusion <- confusionMatrix(model.training, trainSet$Y)
model.test.confusion <- confusionMatrix(model.testing, testSet$Y)

importance <- varImp(model)
plot(importance, top = 25)


######################################
start.time <- proc.time()
model <- train(Y ~ .,
               data = trainSet,
               method = "rf")
stop.time <- proc.time()
run.time <- stop.time - start.time
print(run.time)

# time taken 55.89

######################################
library(doParallel)
cl <- makePSOCKcluster(5)
registerDoParallel(cl)

start.time <- proc.time()
model <- train(Y ~ .,
               data = trainSet,
               method = "rf")
stop.time <- proc.time()
run.time <- stop.time - start.time
print(run.time)

stopCluster(cl)

# time taken 15.42
# 55.98/15.42
# [1] 3.63035

######################################
start.time <- proc.time()
model <- train(Y ~ .,
               data = trainSet,
               method = "rf",
               tuneGrid = data.frame(mtry=seq(5,15, by=5))
               )
stop.time <- proc.time()
run.time <- stop.time - start.time
print(run.time)

# time taken 41.13

######################################
library(doParallel)
cl <- makePSOCKcluster(5)
registerDoParallel(cl)

start.time <- proc.time()
model <- train(Y ~ .,
               data = trainSet,
               method = "rf",
               tuneGrid = data.frame(mtry=seq(5,15, by=5))
              )
stop.time <- proc.time()
run.time <- stop.time - start.time
print(run.time) 

stopCluster(cl)

# time taken 11.98
# 41.13/11.98
# [1] 3.433222