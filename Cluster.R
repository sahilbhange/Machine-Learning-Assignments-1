
# Divisive Clustering approach

airline <- read.csv(file.choose(), header = TRUE, sep = ",")
View(airline)
fit <- diana(airline, metric = "manhattan", stand = TRUE)
plot(fit)

#(35)malaysian airline, (17) China Airline ,(6)Air France

sub = subset(airline, ï..airline == 35 | ï..airline == 17)
View(sub)
names(airline)

MA = airline[35,]
AC = airline[53,]
AF = airline[17,]

airdata = rbind(MA,AC,AF)

View(airdata)

View(airline[35,])
  
dim(airline)

# Agglomarative
response <- airline[,1]
predictor <- airline[,2:(dim(airline)[2])]

View(predictor)
View(response)

# Proceed with clustering
library(cluster)

clusters <- agnes(x=predictor, diss = FALSE, stand = TRUE, method = "average")
DendCluster <- as.dendrogram(clusters)
plot(DendCluster)
# (30)Kenya, (6)Air France and (35)malaysian airline show higher fatalities thus should not fly


clustersComplete <- agnes(x=predictor, diss = FALSE, stand = TRUE, method = "complete")
DendClusterComplete <- as.dendrogram(clustersComplete)
plot(DendClusterComplete)

# (30)Kenya, (6)Air France and (35)malaysian airline show higher fatalities thus should not fly


clustersSingle <- agnes(x=predictor, diss = FALSE, stand = TRUE, method = "single")
DendClusterSingle <- as.dendrogram(clustersSingle)
plot(DendClusterSingle)

# Can not say it exactly