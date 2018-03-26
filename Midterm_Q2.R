#question 2.1

#####################################################################################################
######  Model 1  : Implementing the linear regression to relate income inequality and hate crime rate
#####################################################################################################

hatecrime_dt <- read.csv(file.choose(), header = TRUE, sep = ",",fileEncoding="UTF-8-BOM")

View(hatecrime_dt)

names(hatecrime_dt)
library(ggplot2)
ggplot(hatecrime_dt, aes(x=gini_index, y=hate_crimes_per_100k_splc)) + geom_point()

ggplot(hatecrime_dt, aes(x=gini_index, y=avg_hatecrimes_per_100k_fbi)) + geom_point()


model2 = lm(hate_crimes_per_100k_splc~gini_index+avg_hatecrimes_per_100k_fbi, hatecrime_dt)
summary(model2)

model3 = lm(avg_hatecrimes_per_100k_fbi~gini_index, hatecrime_dt)
summary(model3)

ggplot(hatecrime_dt, aes(x=gini_index, y=hate_crimes_per_100k_splc)) + geom_point() +
  stat_smooth(method="lm")

ggplot(hatecrime_dt, aes(x=gini_index, y=avg_hatecrimes_per_100k_fbi)) + geom_point() +
  stat_smooth(method="lm")


#####################################################################################################
## Model 2 : Implementing the linear regression to relate population race/nature and hate crime rate
#####################################################################################################
names(hatecrime_dt)

model = lm (hate_crimes_per_100k_splc~share_non_white+share_white_poverty+share_non_citizen, hatecrime_dt)

summary(model)

model = lm (hate_crimes_per_100k_splc~share_non_white+share_voters_voted_trump+share_unemployed_seasonal, hatecrime_dt)

summary(model)

model = lm (hate_crimes_per_100k_splc~share_non_white+share_voters_voted_trump+share_unemployed_seasonal+avg_hatecrimes_per_100k_fbi, hatecrime_dt)

summary(model)


#####################################################################################################
## Q3. US Heat map to present the number of hate crimes varying across states
#####################################################################################################


library(ggplot2)
install.packages("maps")
library("maps")
states <- map_data("state")
# Here I'm creating a sample dataset  
# The dataset will have 2 columns: The region (or state)
# and a number that will represent the value that you
# want to plot (here the value is just the numerical order of the states).

View(hatecrime_dt_st)

hatecrime_dt_st=hatecrime_dt

#Removing the Alaska and Hawaii as it is not presesnt in the Heat map data

hatecrime_dt_st <- subset(hatecrime_dt_st, state!='Hawaii')
hatecrime_dt_st <- subset(hatecrime_dt_st, state!='Alaska')

#Renaming the state columns name to region to take the join with the map data
colnames(hatecrime_dt_st)[1] <- "region"

View(hatecrime_dt_st)

#lower the state value to match the state value
hatecrime_dt_st$region <- tolower(hatecrime_dt_st$region)

# Then we merge our dataset with the geospatial data:
sim_data_geo <- merge(states, hatecrime_dt_st, by="region")


qplot(long, lat, data=sim_data_geo, geom="polygon", fill=hate_crimes_per_100k_splc, group=group,color="Yellow") + theme(axis.title=element_text(face="bold.italic",size="10", color="brown"))



ggplot(sim_data_geo, aes(long, lat)) + geom_polygon(aes(group=group, fill=hate_crimes_per_100k_splc)) 


#####################################################################################################
## Model 3 : mplementing Agglomerative Clustering to find the similarity between the sates.
#####################################################################################################


hatecrime_dt <- read.csv(file.choose(), header = TRUE, sep = ",",fileEncoding="UTF-8-BOM")

response <- hatecrime_dt[,1]
predictor <- hatecrime_dt[,2:12]
library(cluster)
clusters <- agnes(x=predictor, diss = FALSE, stand = TRUE, method = "average")
DendCluster <- as.dendrogram(clusters)
plot(DendCluster)

View(hatecrime_dt[c(31,22,24,48,38),])


clustersComplete <- agnes(x=predictor, diss = FALSE, stand = TRUE, method = "complete")
DendClusterComplete <- as.dendrogram(clustersComplete)
plot(DendClusterComplete)


View(hatecrime_dt[c(9,22,7,48,38),])

clustersSingle <- agnes(x=predictor, diss = FALSE, stand = TRUE, method = "single")
DendClusterSingle <- as.dendrogram(clustersSingle)
plot(DendClusterSingle)


#################################################################################################


