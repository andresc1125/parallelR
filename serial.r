setwd("/Users/andrescobos/statistics_master/to_update/scalated_distributed/Assignment1")
library(tidyr)
library(ggplot2)
library(reshape2)

data= read.csv("computers.csv")

data$multi<- ( ifelse(data$multi=='yes', 1,0))
data$cd<- ( ifelse(data$cd=='yes', 1,0))
data$premium<-(ifelse(data$premium=='yes', 1,0))

data_to_cluster = data[,c(2:11)]

data_to_cluster <- as.data.frame(lapply(data_to_cluster, as.numeric))
original_data_to_cluster <- data_to_cluster


##Normalize numeric values between 0 and 1
data_to_cluster[,c(1:5,9,10)] <- as.data.frame(lapply(data_to_cluster[,c(1:5,9,10)], function(x){y<-(x-min(x))/(max(x)-min(x))}))
#Construct the elbow graph and find the optimal cluster number
set.seed(10)

elbow_data = c()
system.time(
  for(clus_num in 1:20){
    elbow_data[clus_num] = kmeans( ( data_to_cluster),clus_num, nstart = 25)$tot.withinss
  }
)
#time for elbow computations
#   user  system elapsed 
#   4.865   0.248   6.339 

plot(elbow_data, main = "Elbow Graph", ylab = "SSE")
lines(elbow_data)
diff(elbow_data)

#optimum value seems to be seven
optimum_value=5
set.seed(10)
#2 Cluster de data using the optimum value.
cluster = kmeans( ( data_to_cluster),optimum_value)


#3.- Find the cluster with the highest price average.
data_clustered = original_data_to_cluster
data_clustered$cluster_id = cluster$cluster

means_vect = rep(0, max(unique(data_clustered$cluster_id ))) 
system.time(
  for (i in 1:max(unique(data_clustered$cluster_id ))){
    means_vect[i] = mean(data_clustered[data_clustered$cluster_id == i,c("price")])
  } 
)

# times for vector of means
# user  system elapsed 
# 0.005   0.000   0.005 

max(means_vect)
means_vect
#the cluster with the max mean is the number 2 with a mean of 2853.538

which.max(means_vect)
str(data_to_cluster)

centroids <- cluster$centers
ggplot(melt(cluster$centers), aes(Var2,Var1, fill=value)) + geom_raster()





