

library(mclust)

concrete_slump <- read.csv(file.choose(), header = TRUE, sep = ",")
View(concrete_slump)



summary(concrete_slump)

mod4 = densityMclust(concrete_slump_new)
summary(mod4)

plot(mod4, what = "BIC")


X = concrete_slump[,c(2,3,4,5,6,7,8,9,10,11)]  

View(X)
  
class.slump = concrete_slump[,c(9)]
class.flow = concrete_slump[,c(10)]
class.cmpstr = concrete_slump[,c(11)]


clPairs(X, class.slump)
clPairs(X, class.flow)
clPairs(X, class.cmpstr)

table(class.slump)

fit <- Mclust(X)
fit

summary(fit)

plot(fit, what = "BIC")
plot(fit, what = "classification")
plot(fit, what = "density")

BIC = mclustBIC(X)

summary(BIC)

plot(BIC)

ICL = mclustICL(X)
summary(ICL)

plot(ICL) # Only ICL plot






##################################




