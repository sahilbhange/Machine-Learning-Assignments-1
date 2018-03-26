pollution_dt  <- read.csv(file.choose(), header = TRUE, sep = ",",fileEncoding="UTF-8-BOM")

View(pollution_dt)

#Running the linear regression to get the model
model1 = lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13++x14+x15, data=pollution_dt)




#plot only Y values from the pollution dataset 
plot(pollution_dt$y,pch=16)

plot(predY,pch=16)

# Prodict the Y values using the regression model build in the above step
predY = predict(model1,pollution_dt)


# Predicted Y values from the model (predY)
predY 
# Original Y values from the Pollution dataset
View(pollution_dt$y)

# Plot predicted Y values and original Y vlaues on the plot in sequential order in the dataset
points(predY, col="Blue", pch=4)

rmse <- function(error)
{
  sqrt(mean(error^2))
}

error1 = model1$residuals
lrPredMSE = rmse(error1)
lrPredMSE

library(e1071)

model2 = svm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13++x14+x15, pollution_dt)

PredY2 = predict(model2,pollution_dt)

#plot only original Y values from the pollution dataset 
plot(pollution_dt$y,pch=16)

#plot Predicted Y values using the SVM
points(PredY2, col="Red", pch=4)



error2 = pollution_dt$y - PredY2

svmPredRMSE = rmse(error2)
svmPredRMSE
lrPredMSE

?tune

model3 = tune(svm,y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13++x14+x15, data=pollution_dt, ranges=list(epsilon=seq(0,1,0.1),cost=seq(1,10,1)))

bestmodel = model3$best.model

bestPred = predict(bestmodel,data=pollution_dt)

#plot only original Y values from the pollution dataset 
plot(pollution_dt$y,pch=16)

#plot redicted Y values using the Best tuned SVM model
points(bestPred, col="maroon", pch=4)

best_error  = pollution_dt$y - bestPred

best_RMSE = rmse(best_error)
best_RMSE


best_RMSE
svmPredRMSE
lrPredMSE







install.packages("mlbench")

library(e1071)
library(rpart)
library(mlbench) 
data(Ozone, package="mlbench")
## split data into a train and test set
index <- 1:nrow(Ozone)
testindex <- sample(index, trunc(length(index)/3))
testset <- na.omit(Ozone[testindex,-3])
trainset <- na.omit(Ozone[-testindex,-3])

## svm
svm.model <- svm(V4 ~ ., data = trainset, cost = 1000, gamma = 0.0001)
svm.pred <- predict(svm.model, testset[,-3])
crossprod(svm.pred - testset[,3]) / length(testindex)

## rpart
rpart.model <- rpart(V4 ~ ., data = trainset)
rpart.pred <- predict(rpart.model, testset[,-3])
crossprod(rpart.pred - testset[,3]) / length(testindex)


