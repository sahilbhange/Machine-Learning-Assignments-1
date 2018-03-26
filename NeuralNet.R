
install.packages("neuralnet")


library(neuralnet)


library(data.table)



# Read data using fread from the given URL
still_data = fread("https://archive.ics.uci.edu/ml/machine-learning-databases/00198/Faults.NNA")

View(still_data)


# Removing the 12th and 13th categorical variable

data=still_data[,c(-12,-13)]
View(data)

# Scale only contineous variables from the dataset and exclude categorical variables for scaling

scale_data = data[,1:25]

# Extract max and min value from each column for scaling
max=apply(scale_data, 2, max)
min=apply(scale_data, 2, min)

scaled = as.data.frame(scale(scale_data,center = min,scale = max-min))

View(scaled)
#lets predict all the 7 variable in one model for the still dataset

#bind the  7 variable column with the scaled data


Zscratch_data <- cbind(scaled,data[,26:32])

View(Zscratch_data)

names(Zscratch_data)


index = sample(1:nrow(Zscratch_data),round(0.75*nrow(Zscratch_data)))

train = Zscratch_data[index,]
test = Zscratch_data[-index,]

head(train)

head(Zscratch_data)

names = names(Zscratch_data)

f = as.formula(paste("V28 + V29 + V30 + V31 + V32 + V33 + V34 ~ ",paste(names[!names %in% c("V28","V29","V30","V31","V32","V33","V34")], collapse = "+")))


?neuralnet

# Neural Net for layer (3,2) 
#nn = neuralnet(f,data=train, hidden=c(3,2), linear.output = FALSE,act.fct = "logistic")


# Neural Net for layer (3,2)
nn = neuralnet(f,data=train, hidden=c(3,2), linear.output = T,act.fct = "logistic",stepmax = 1000000)

plot(nn)

# Compute the values of 7 predictor variable using test data
test_nn = compute(nn,test[,1:25])


test_cat = ifelse(test_nn$net.result > 0.5,1,0)

head(test_cat)

table(test_cat)

library(caret)
confusionMatrix(test_cat[,1],test$V28)
confusionMatrix(test_cat[,2],test$V29)
confusionMatrix(test_cat[,3],test$V30)
confusionMatrix(test_cat[,4],test$V31)
confusionMatrix(test_cat[,5],test$V32)
confusionMatrix(test_cat[,6],test$V33)
confusionMatrix(test_cat[,7],test$V34)


# Neural Net for layer (4,3) 
# make linear.output = FALSE as it is binary classification problem

nn4 = neuralnet(f,data=train, hidden=c(3,3), linear.output = T,act.fct = "logistic",stepmax = 10000000)

plot(nn4)

test_nn4 = compute(nn4,test[,1:25])

test_cat4 = ifelse(test_nn4$net.result > 0.5,1,0)

confusionMatrix(test_cat4[,1],test$V28)
confusionMatrix(test_cat4[,2],test$V29)
confusionMatrix(test_cat4[,3],test$V30)
confusionMatrix(test_cat4[,4],test$V31)
confusionMatrix(test_cat4[,5],test$V32)
confusionMatrix(test_cat4[,6],test$V33)
confusionMatrix(test_cat4[,7],test$V34)