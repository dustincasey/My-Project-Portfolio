# Project 1 Code

library(tidyverse)
library(skimr)
library(readr)
library(scales)
library(stringr)
library("readxl")
library(dbplyr)
library(gmodels)
library(ggpubr)
library(factoextra)

set.seed(777)

segmentation_data <- read_csv('Mall_Customers.csv')

# Transforming the Gender column to numeric. 1 for Male, 2 for Female
segmentation_data$Gender <- ifelse(segmentation_data$Gender == 'Male', 1, 2)

# EDA

## Data Structure
str(segmentation_data)

## Summary Statistics of the data.
summary(segmentation_data)

## Histogram of Age
hist(segmentation_data$Age)

## The largest bucket of our customer base fall between 30 and 35 years old.
## Overall, the customer base is heavily favored to be under 50 years old.


## Histogram of Annual Income
hist(segmentation_data$`Annual Income (k$)`)

## The largest bucket here shows customers making between $70K and $80K per year.
## There is a small amount who make more than $80K per year.


## Histogram of Spending Score
hist(segmentation_data$`Spending Score (1-100)`)

## This metric has some variability, but the largest two buckets show between 40 and 60 for spending score.


## Gender Counts
table(segmentation_data$Gender)

## The majority of our customer base is female, but there is still a decent ratio of female to male.


# Removing the CustomerID as fields used to cluster.
segmentation_data_training <- segmentation_data %>% 
  subset(select = -c(CustomerID))

# K-Means Clustering

## Cluster by Age

## Determining the number of clusters to use for the data.
training_set <- segmentation_data_training %>% 
  subset(select = c(Age,`Spending Score (1-100)`))

wss <- (nrow(training_set)-1)*sum(apply(training_set,2,var))

for (i in 2:15) wss[i] <- sum(kmeans(training_set,
                                     centers=i)$withinss)

plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

## Based off of the graph, using the elbow method, this dataset should use 4 clusters.

## Using K-Means to cluster similar customers that will help us with our segmentation and marketing efforts.

km.res <- kmeans(training_set, 4, nstart = 10)

segmentation_data_clustered <- cbind(training_set, cluster = km.res$cluster)
segmentation_data_clustered

## Cluster Size
km.res$size

## Good distribution between the 5 clusters

## Visualizing the clusters by Age
fviz_cluster(km.res, data = training_set,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#4F2683"),
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

segmentation_data_clustered %>% 
  group_by(cluster) %>% 
  summarize(Avg_Spending_Score = mean(`Spending Score (1-100)`)) %>% 
  print()

## We would want to target customers in cluster 4 as they have the highest average spending scores.


## Cluster by Annual Income

## Determining the number of clusters to use for the data.
training_set <- segmentation_data_training %>% 
  subset(select = c(`Annual Income (k$)`,`Spending Score (1-100)`))

wss <- (nrow(training_set)-1)*sum(apply(training_set,2,var))

for (i in 2:15) wss[i] <- sum(kmeans(training_set,
                                     centers=i)$withinss)

plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

## Based off of the graph, using the elbow method, this dataset should use 5 clusters.

## Using K-Means to cluster similar customers that will help us with our segmentation and marketing efforts.

km.res <- kmeans(training_set, 5, nstart = 10)

segmentation_data_clustered <- cbind(segmentation_data, cluster = km.res$cluster)
segmentation_data_clustered

## Cluster Size
km.res$size

## Good distribution between the 5 clusters

## Visualizing the clusters by Age
fviz_cluster(km.res, data = training_set,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#4F2683", "#003399"),
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

segmentation_data_clustered %>% 
  group_by(cluster) %>% 
  summarize(Avg_Spending_Score = mean(`Spending Score (1-100)`)) %>% 
  print()

## We would want to target customers in cluster 3 & 5 as they have the highest average spending scores.
