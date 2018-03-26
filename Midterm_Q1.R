#####################################################################################################
######  Model 1  : Implementing the Gradiend descent on Age and Disease type field
#####################################################################################################
test_derma_dta <- read.csv(file.choose(), header = TRUE, sep = ",",fileEncoding="UTF-8-BOM")
derma_dta_GD <- test_derma_dta[,c(34,35)]

# Since age field contains invalid (NA/?) values removing them for processing
derma_dta_GD <- subset(derma_dta_GD, Age!='?')

View(derma_dta_GD)
colnames(derma_dta_GD)[2] <- "Disease_type"

# Converting the Age field to numeric since it was read as factor
derma_dta_GD$Age <- as.numeric(as.character(derma_dta_GD$Age))
is.numeric(derma_dta_GD$Age)

ggplot(derma_dta_GD, aes(x=Age, y=Disease_type)) + geom_point()

model <- lm(Disease_type~Age,derma_dta_GD)
summary(model)

ggplot(derma_dta_GD, aes(x=Age, y=Disease_type)) + geom_point() +
  stat_smooth(method="lm")


plot(Age, Disease_type, col = rgb(0.2,0.4,0.6,0.4), main = "Linear regression")
abline(model, col = "blue")

attach(derma_dta_GD)


# Define cost function
cost <- function(X, Disease_type, theta)
{
  sum(X%*% theta - Disease_type)^2/(2*length(Disease_type))
}

# Initialize coefficients
theta <- matrix(c(0,0), nrow = 2)
num_iterations <- 500
alpha <- 0.001

# Store the history
cost_history <- double(num_iterations)
theta_history <- list(num_iterations)


X<-cbind(1, matrix(derma_dta_GD$Age))

# Gradient descent algorithm
for(i in 1:num_iterations){
  error <- (X %*% theta - Disease_type)
  delta <- t(X) %*% error/length(Disease_type)
  theta <- theta - alpha * delta
  cost_history[i] <- cost(X, Disease_type, theta)
  theta_history[[i]] <- theta
}

print(theta)

# Plot data and various models built along the way
plot(Age,Disease_type, main = "Gradient descent")
abline(coef = theta_history[[1]])
abline(coef = theta_history[[2]])
abline(coef = theta_history[[3]])
abline(coef = theta_history[[4]])
abline(coef = theta_history[[5]])

# Redraw the plot and the lines using different theta values
plot(Age,Disease_type, main = "Gradient descent")
# Draw the first few lines and then draw every 10th line

for(i in c(1,2,3,4,5,seq(6,num_iterations, by = 10)))
{
  abline(coef = theta_history[[i]], col=rgb(0.8,0,0,0.3))
}

# Draw the final model
abline(coef = theta, col = 'blue')

# Visualize the cost
plot(cost_history, type = 'line', col = 'blue', lwd=2, main = 'Cost function', ylab='cost', xlab = 'Iterations')



#####################################################################################################
######  Model 2  : Implementing the Random forest
#####################################################################################################

derma_dta <- read.csv(file.choose(), header = TRUE, sep = ",",fileEncoding="UTF-8-BOM")
View(derma_dta)
attach(derma_dta)

#Rename the 35 column to Disease_type
colnames(derma_dta)[35] <- "Disease_type"
str(derma_dta)
is.numeric(derma_dta$Age)
derma_dta$Age <- is.factor(derma_dta$Age)
derma_dta$Age = as.numeric(derma_dta$Age)
#Since the age has 61 levels in factors (As interger values entered as string) convert it to numeric. for factoring
derma_dta$Age <- as.numeric(as.character(derma_dta$Age))

set.seed(1234)
population <- sample(nrow(derma_dta), 0.75 * nrow(derma_dta)) 
derma_train = derma_dta[population,] 
derma_test = derma_dta[-population,]


#install.packages("randomForest")
library(randomForest)

model <- randomForest(derma_train$Disease_type~ ., data = derma_train)
model

prediction <- predict(model, newdata = derma_test)

#Since the prediction values are contineous, rouding them for confusion matrix
table(round(prediction), derma_test$Disease_type)
Accuracy = (26+11+17+13+11+3)/nrow(derma_test)
Accuracy


#####################################################################################################
######  Model 3  : Implementing the kNN
#####################################################################################################

View(derma_dta)

set.seed(1234)

pop = sample(2,nrow(derma_dta),replace = TRUE, prob = c(0.75 , 0.25))

derma_dta.training = derma_dta[pop==1,1:34]
derma_dta.test = derma_dta[pop==2,1:34]
derma_dta.trainLabels = derma_dta[pop==1, 35]
derma_dta.testLabels = derma_dta[pop ==2, 35]

library(class)

#KNN for k=3
derma_pred <- knn(train = derma_dta.training, test = derma_dta.test, cl = derma_dta.trainLabels, k=3)

# Compute the cross tabulation (confusion matrix)
library(gmodels)
table(x=derma_pred, y=derma_dta.testLabels)

AccuracyKnn = (28+8+18+12+10+5) / nrow(derma_dta.test)

AccuracyKnn

#kNN for k=4
derma_pred <- knn(train = derma_dta.training, test = derma_dta.test, cl = derma_dta.trainLabels, k=4)

# Compute the corss tabulation (confusion matrix)
library(gmodels)
table(x=derma_pred, y=derma_dta.testLabels)

AccuracyKnn = (28+11+18+10+10+5) / nrow(derma_dta.test)

AccuracyKnn

xtab=table(derma_pred, derma_dta.testLabels)
library(caret) 
confusionMatrix(xtab)

#####################################################################################################
######  Model 4 and 5 : Implementing Agglomerative and Divisive Clustering
#####################################################################################################

#Model 4 - Agglomerative Clustering

View(derma_dta)

derma_dta <- read.csv(file.choose(), header = TRUE, sep = ",",fileEncoding="UTF-8-BOM")

response <- derma_dta[,35]
predictor <- derma_dta[,1:34]


library(cluster)

clusters <- agnes(x=predictor, diss = FALSE, stand = TRUE, method = "average")
DendCluster <- as.dendrogram(clusters)
plot(DendCluster)


clustersComplete <- agnes(x=predictor, diss = FALSE, stand = TRUE, method = "complete")
DendClusterComplete <- as.dendrogram(clustersComplete)
plot(DendClusterComplete)


clustersSingle <- agnes(x=predictor, diss = FALSE, stand = TRUE, method = "single")
DendClusterSingle <- as.dendrogram(clustersSingle)
plot(DendClusterSingle)


#Model 5 - Divisive Clustering
View(derma_dta)
fit <- diana(derma_dta, metric = "manhattan", stand = TRUE)
plot(fit)





