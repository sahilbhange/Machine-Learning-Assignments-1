
# Logistic regression for the OVerdrawn data set.


Overdrawndt <- read.csv(file.choose(), header = TRUE, sep = ",")
View(Overdrawndt)

dim(Overdrawndt)

#Removing the rows containing the N/A values
Overdrawndt <- na.omit(Overdrawndt)
dim(Overdrawndt)

set.seed(1234)

View(Overdrawndt)
Overdrawndt$DyDrnkCat <- with(smplp, ifelse(Overdrawndt$DaysDrink < 7, 0,
                            ifelse(Overdrawndt$DaysDrink <= 14, 1,2)))



# Since the Overdrawn==1 class is rare class i.e. only 60 instances for Overdrawn==1 out of 450
# Distributing the Overdrawn==1 class approximately equally to train and test data

population <- sample(nrow(Overdrawndt), 0.75 * nrow(Overdrawndt))
Overdrawn_train = Overdrawndt[population,]
Overdrawn_test = Overdrawndt[-population,]

summary(Overdrawndt$Overdrawn==1)
summary(Overdrawn_train$Overdrawn==1)
summary(Overdrawn_test$Overdrawn==1)


mdl = glm(Overdrawn~Age+Sex+DyDrnkCat, family = binomial(link = "logit"),data = Overdrawn_train)

summary(mdl)

rs  = predict(mdl,newdata = Overdrawn_test,type="response")

rs1 = ifelse(rs > 0.5,1,0)

misClass = mean(rs1 != Overdrawn_test$Overdrawn)

accry = 1 - misClass
accry


#KNN

names(Overdrawndt)

#Selecting only required columns
Overdrawndt_new = Overdrawndt[c("Age","Sex","DaysDrink","Overdrawn")]


mpldta = sample(2,nrow(Overdrawndt_new),replace = TRUE, prob = c(0.75 , 0.25)) 


Overdrawndt_new.training = Overdrawndt_new[ind==1,1:3] 
Overdrawndt_new.test = Overdrawndt_new[ind==2,1:3] 

Overdrawndt_new.trainLabels = Overdrawndt_new[ind==1, 4] 
Overdrawndt_new.testLabels = Overdrawndt_new[ind ==2, 4]


View(Overdrawndt_new.test )
View(Overdrawndt_new.training )

library(class)

#kNN for  k=2

Overdrawn_pred <- knn(train = Overdrawndt_new.training, test = Overdrawndt_new.test, cl = Overdrawndt_new.trainLabels, k=2)

library(gmodels)
CrossTable(x=Overdrawn_pred, y=Overdrawndt_new.testLabels, prop.chisq = FALSE)

# Accuracy is [(101+0)/116 = 87%]



#kNN for  k=3

Overdrawn_pred <- knn(train = Overdrawndt_new.training, test = Overdrawndt_new.test, cl = Overdrawndt_new.trainLabels, k=3)

library(gmodels)
CrossTable(x=Overdrawn_pred, y=Overdrawndt_new.testLabels, prop.chisq = FALSE)

# Accuracy is [(103+0)/116 = 88.79%]


#kNN for  k=4

Overdrawn_pred <- knn(train = Overdrawndt_new.training, test = Overdrawndt_new.test, cl = Overdrawndt_new.trainLabels, k=4)

library(gmodels)
CrossTable(x=Overdrawn_pred, y=Overdrawndt_new.testLabels, prop.chisq = FALSE)

# Accuracy is [(104+0)/116 = 89.65%], this could be the case of overfitting


# Agglomerative Clusterinng on data

View(Overdrawndt_new)

response <- Overdrawndt_new[,4]
predictor <- Overdrawndt_new[,1:3]  

View(predictor)

# Proceed with clustering
library(cluster)

clusters <- agnes(x=predictor, diss = FALSE, stand = TRUE, method = "average")
DendCluster <- as.dendrogram(clusters)
plot(DendCluster)




