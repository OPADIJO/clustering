
#import the dataset
wine<- read.csv("wine-clustering.csv", header = T)

#inspect the dataset
names(wine)
head(wine)
tail(wine)  
summary(wine)
str(wine)

#dimension of the dataset
dim(wine)
nrow(wine)
ncol(wine)

#because the values in the various column are in different scales so we have to normalize so we can compare them to each other 
norma<- function(x){(x-min(x))/(max(x)-min(x))}

wine_norma<- as.data.frame(lapply(wine[1:13], norma))
summary(wine_norma)

#KMEANS CLUSTERING METHOD
library(factoextra)

#checking if the dataset has meaningful clusters
tendency<- get_clust_tendency(wine_norma, 20, graph= T)
#if the value of the Hopkins statistics is close to zero then we have meaningful clusters.
tendency$hopkins_stat
#the fviz_nbclust helps to know the appropriate number of clusters. this is found at the bend/knee of the trend line 
fviz_nbclust(wine_norma, kmeans, method = "wss")

#performing kmeans cluster where k=3
set.seed(234)
kmeans_wine<- kmeans(wine_norma, 3, nstart = 20)
kmeans_wine$cluster
kmeans_wine$size

#visualizing the cluster
fviz_cluster(kmeans_wine, wine_norma)



#HIERARCHICAL CLUSTERING METHOD 

#calculate the distance between each variables
dis<- dist(wine_norma)

#hierarchacal clustering 
hc_wine<- hclust(dis)
hc_wine

#plot the dendogram 
plot(hc_wine)




