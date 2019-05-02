setwd("/Users/andrescobos/statistics_master/to_update/scalated_distributed/Assignment1")
library(tidyr)
library(ggplot2)
library(reshape2)
library(doParallel)


data= read.csv("computers.csv")

data$multi<- ( ifelse(data$multi=='yes', 1,0))
data$cd<- ( ifelse(data$cd=='yes', 1,0))
data$premium<-(ifelse(data$premium=='yes', 1,0))

data_to_cluster = data[,c(2:11)]
#Construct the elbow graph and find the optimal cluster number

#set the number of cores to use
cores_to_use = 2

cl <- makeCluster(cores_to_use)
registerDoParallel(cl)
system.time(
    elbow_data_parallel = foreach(clus_num = 1:20) %dopar% {
    set.seed(10)
    sum(kmeans( scale ( data_to_cluster),clus_num)$withinss)
  }
)
stopCluster(cl)

#Times for parallel elbow
#   user  system elapsed 
#   0.026   0.020   0.236 

#unlist as parallel returns a list of vectors instead of a vector
vector_elbow_data = unlist(elbow_data_parallel, use.names=FALSE)
plot(vector_elbow_data)
lines(vector_elbow_data)


#optimum value seems to be seven

#2 Cluster de data using the optimum value.
cluster = kmeans( scale ( data_to_cluster),optimum_value)


#3.- Find the cluster with the highest price average.
data_clustered = data_to_cluster
data_clustered$cluster_id = cluster$cluster

means_vect = rep(0, max(unique(data_clustered$cluster_id ))) 


cl <- makeCluster(cores_to_use)
registerDoParallel(cl)
system.time(
  foreach(clus_num = 1:max(unique(data_clustered$cluster_id )) ) %dopar% {
    means_vect = mean(data_clustered[data_clustered$cluster_id == clus_num,c("price")])
  }
)
# Times for vector of means
#   user  system elapsed 
#   0.010   0.001   0.036 
#
stopCluster(cl)
which.max(means_vector)
#the cluster with the max mean is the number 2 with a mean of 2853.538











