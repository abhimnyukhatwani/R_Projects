#Clear environment
rm(list = ls())

#Setting working directory
setwd("C:/Users/abhim/OneDrive/Documents/Courses/Data Wrangling")
getwd()

#import libraries
#install.packages('fpc') if not already installed
library(fpc)
#To assess if data is clustarable HOPKINS
install.packages("factoextra")
library(factoextra)
# Visualising the clusters
library(cluster)

# K-MEANS CLUSTERING
set.seed(12948181)
data <- read.csv("seeds_dataset.csv")
seeds_train = data

#Check for NA values
head(seeds_train)
nrow(seeds_train)


#Check for NA values for X and X.1 column
sum(is.na(seeds_train$X))
sum(is.na(seeds_train$X.1))


#Remove columns with NA values
seeds_train <- seeds_train[1:8]




gc <- get_clust_tendency(seeds_train[1:7], n = nrow(seeds_train[1:7])-1, graph = FALSE, seed = 12948181)
gc$hopkins_stat





# Using the elbow method to find the optimal number of clusters
set.seed(12948181)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(seeds_train[1:7], i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'K Value',
     ylab = 'WCSS')





# Fitting K-Means to the dataset
set.seed(12948181)
kmeans = kmeans(x = seeds_train[1:7], centers = 3)
y_kmeans = kmeans$cluster
#?kmeans
y_kmeans

kmeans



table(seeds_train[,8]  , y_kmeans)




plot(seeds_train[1:7], col = y_kmeans)




clusplot(seeds_train[1:7],
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 0,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Seeds Data Cluster (K=3)'))



# Using the dendrogram to find the optimal number of clusters
dendrogram = hclust(d = dist(data[1:7], method = 'euclidean'), method = 'average')
plot(dendrogram,
     main = paste('Dendrogram'),
     xlab = 'Data Points',
     ylab = 'Euclidian Distance')

# Fitting Hierarchical Clustering to the dataset
y_hc = cutree(dendrogram, 3)


clusplot(seeds_train[1:7],
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Seeds Hierarchial Cluster Diagram'))



#compute distance matrix
dis = dist(seeds_train[1:7])^2
#silhoutte coefficient for Kmeans
sil_kmeans<-silhouette(y_kmeans,dis)
#silhoutte coefficient for Hierarchial
sil_hc<-silhouette(y_hc,dis)
#plot for K means
plot(sil_kmeans,col='red')
#plot for Hierarchial
plot(sil_hc,col='blue')


