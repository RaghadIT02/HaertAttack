---
title: "HeartAttack project"
output: html_document
date: "2023-10-09"
---
## The goal of collecting this dataset

our goal is to Identify whether the chances of of a heart attack occurrence is high or low for the patient



The dataset was sourced from the kaggle website in this URL: <https://www.kaggle.com/datasets/pritsheta/heart-attack>

## General information

1- Number of Attributes: 14 2- Number of Objects: 303 3- Type of Attributes:

| Attribute name | Data type | Description                                             | possible values                                                           |
|------------------|------------------|-------------------|------------------|
| age            | Numeric   | Age (in years)                                          | range between 29-77                                                       |
| sex            | Binary    | gender                                                  | 1 = male; 0 =female                                                       |
| cp             | Numeric   | Chest Pain type                                         | 1: typical angina,2: atypical angina,3: non-anginal pain ,4: asymptomatic |
| trestbps       | Numeric   | resting blood pressure                                  | range beteen 94-200                                                       |
| chol           | Numeric   | serum cholesterol in mg/dL                              | range between 126-564                                                     |
| fbs            | Binary    | fasting blood sugar \> 120 mg/dL (likely to be diabetic | 1=true,0=false                                                            |
| Restecg        | Binary    | Resting electrocardiogram                               | 0=normal,1=abnormal                                                       |
| Oldpeak        | Numeric   | ST depression                                           | range between 0-6.20                                                      |
| thalach        | Numeric   | heart rate achieved                                     | range between 71-202                                                      |
| exang          | Binary    | exercise induced angina                                 | 1 = yes; 0 = no                                                           |
| slope          | Numeric   | the slope of the peak exercise ST segment               | range between 0-2                                                         |
| Ca             | Numeric   | number of major vessels                                 | range between 0-4                                                         |
| Thal           | Numeric   | Thalassemia                                             | range between 0-3                                                         |
| Target         | Binary    | if the target has disease or not                        | 0 = no disease, 1 = disease                                               |

Class label:
-target of Heart Attack accurrance 
-not target of Heart Attack accurrance
-------------------
view dataset:
-------------------
```{r}
install.packages("readxl")
library(readxl)
dataset <- read_excel("C:\\Users\\sma12\\Desktop\\Heart Attack Data Set.xlsx")
View(dataset)
```
-----------------------
Summary of dataset:
-----------------------

```{r}
nrow(dataset) 
ncol(dataset) 
```



```{r}
summary(dataset$age)
summary(dataset$cp)
summary(dataset$trestbps)
summary(dataset$chol)
summary(dataset$oldpeak)
summary(dataset$thalach)
summary(dataset$slope)
summary(dataset$ca)
summary(dataset$thal)
```

-------------------
Missing values and Null values
-------------------
```{r}
 is.na(dataset)
sum(is.na(dataset))

```
The output is false and 0 this indicates that the element in our dataset is not missing

--------------
Graphs
--------------
Boxplot
```{r,echo=FALSE}
install.packages("ggplot")
library(ggplot2)
ggplot(data = dataset, aes(x = "", y = trestbps)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(
    title = "Box Plot for resting blood pressure  ",
    y = "trestbps",
    x=NULL
  )+theme_minimal()
ggplot(data = dataset, aes(x = "", y =chol)) +
  geom_boxplot(fill = "lightyellow", color = "black") +
  labs(
    title = "Box Plot for cholestero",
    y = "chol",
    x = NULL 
  ) +theme_minimal()
ggplot(data = dataset, aes(x = "", y = thalach)) +
  geom_boxplot(fill = "lightpink", color = "black") +
  labs(
    title = "Box Plot for heart rate achieved ",
    y = "heart rate",
    x = NULL  
  ) +theme_minimal()
ggplot(data = dataset, aes(x = "", y = oldpeak)) +
  geom_boxplot(fill = "purple", color = "black") +
  labs(
    title = "Box Plot for ST depression",
    y = " ST depressione",
    x = NULL  
  ) +
  theme_minimal()
ggplot(data = dataset, aes(x = "", y = age)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(
    title = "Box Plot for Age",
    y = "Age",
    x = NULL  
  ) + theme_minimal()


```
we can also detect summary of each attribute from the box plot , and we use box plot to detect  the outlier.

Histogram
```{r}
hist(dataset$chol)
```
The graph represents the frequency of cholesterol in the data set. After observing the values, we concluded that most of the values fall in the range higher than the normal range, as the normal range falls between  125-200.


Scatter plot
```{r}
with(dataset, plot(age,chol, main = "Scatter Plot", xlab = "Age", ylab="chol"))
```
Because high cholesterol is one of the leading causes of heart attacks, we wanted to see if age and cholesterol are correlated, but the scatter plot shows us that the is no correlation between the two attributes.

Bar plot
There is no point in using bar plots because  the frequency of one symptom (ex: high blood pressure alone ) is not enough to decide whether the patients are likely to have a heart attack or not




--------------
 outliers:
--------------
Detecting the outliers:
```{r}
 boxplot.stats (dataset$age)$out
  boxplot.stats (dataset$chol)$out
  boxplot.stats (dataset$trestbps)$out
  boxplot.stats (dataset$oldpeak)$out
  boxplot.stats (dataset$thalach)$out
```


Removing the outliers:
```{r}
outliers <- boxplot (dataset$chol, plot=FALSE) $out
 dataset <- dataset [-which(dataset$chol %in% outliers),]
 boxplot.stats (dataset$chol) $out
```
The outlier in the attribute wasn’t a real Outlier and if we remove all the Outliers, our dataset affected and become small so we remove the Outlier from the attribute control that we noticed it is real Outlier.

--------------------------------
Transformation and Normalization
--------------------------------
Discretization:
```{r,echo=FALSE}
dataset$age <- cut(dataset$age, breaks = seq(29, 77, by = 10),right=TRUE)
```

Normalization:
```{r}
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

dataset$trestbps <- normalize(dataset$trestbps)
dataset$chol <- normalize(dataset$chol)
dataset$thalach <- normalize(dataset$thalach)
dataset$oldpeak <- normalize(dataset$oldpeak)
dataset$cp <- normalize(dataset$cp)
dataset$oldpeak <- normalize(dataset$oldpeak)
dataset$slope <- normalize(dataset$slope)


```

------------------------------------
feature selection
------------------------------------
we did not applly feature selection beacause the number of attribute is small and we will  neeed them.

------------------------------------
print the final preprocessed dataset
------------------------------------
```{r}
print(dataset)

```


Optimal number of clusters:
```{r}


oldDataset<-dataset
dataset=dataset[,-14]

fviz_nbclust(dataset, kmeans, method = "silhouette")+labs(subtitle ="Silhouette method")



set.seed(123)  # Set seed for reproducibility


wss <- numeric(10)  # Vector to hold within-cluster sum of squares

for (k in 1:10) {
  # Run the k-means algorithm on the dataset
  model <- kmeans(data, centers = k, nstart = 25)
  # Store the within-cluster sum of squares
  wss[k] <- model$tot.withinss
}

# Plot the elbow plot
plot(1:10, wss, type = "b", xlab = "Number of Clusters", ylab = "Within-cluster sum of squares",
     main = "Elbow Method for Optimal Number of Clusters")
     
``` 

---------------------------------------
k-means clustring k=2:
```{r}
set.seed(8953)
dataset=scale(dataset)
kmeans.result <- kmeans(dataset, 2)
# print the clusterng result
kmeans.result
```
visualize clustering:
```{r}
#install.packages("factoextra")
library(factoextra)
fviz_cluster(kmeans.result, data = dataset)
```
Total within-cluster-sum of square:
```{r}
kmeans.result$tot.withinss
```
BCubed precision and recall:
```{r}

cluster_assignments <- c(kmeans.result$cluster)
ground_truth_labels <- c(oldDataset$target)

data <- data.frame(cluster = cluster_assignments, label = ground_truth_labels)

# Function to calculate BCubed precision and recall
calculate_bcubed_metrics <- function(data) {
  n <- nrow(data)
  precision_sum <- 0
  recall_sum <- 0

  for (i in 1:n) {
    cluster <- data$cluster[i]
    label <- data$label[i]
    
# Count the number of items from the same category within the same cluster
same_category_same_cluster <- sum(data$label[data$cluster == cluster] == label)
    
# Count the total number of items in the same cluster
total_same_cluster <- sum(data$cluster == cluster)
    
# Count the total number of items with the same category
total_same_category <- sum(data$label == label)
    
# Calculate precision and recall for the current item and add them to the sums
precision_sum <- precision_sum + same_category_same_cluster /total_same_cluster
recall_sum <- recall_sum + same_category_same_cluster / total_same_category
  }

  # Calculate average precision and recall
  precision <- precision_sum / n
  recall <- recall_sum / n

  return(list(precision = precision, recall = recall))
}

# Calculate BCubed precision and recall
metrics <- calculate_bcubed_metrics(data)

# Extract precision and recall from the metrics
precision <- metrics$precision
recall <- metrics$recall

# Print the results
cat("BCubed Precision:", precision, "\n")
cat("BCubed Recall:", recall, "\n")
```
 

k-means clustring k=3: 
```{r}
set.seed(8953)
dataset=scale(dataset)
kmeans.result <- kmeans(dataset, 3)
# print the clusterng result
kmeans.result
```

visualize clustering
```{r}
#install.packages("factoextra")
library(factoextra)
fviz_cluster(kmeans.result, data = dataset)
```
Total within-cluster-sum of square:
```{r}
kmeans.result$tot.withinss
```
BCubed precision and recall:
```{r}

cluster_assignments <- c(kmeans.result$cluster)
ground_truth_labels <- c(oldDataset$target)

data <- data.frame(cluster = cluster_assignments, label = ground_truth_labels)

# Function to calculate BCubed precision and recall
calculate_bcubed_metrics <- function(data) {
  n <- nrow(data)
  precision_sum <- 0
  recall_sum <- 0

  for (i in 1:n) {
    cluster <- data$cluster[i]
    label <- data$label[i]
    
# Count the number of items from the same category within the same cluster
same_category_same_cluster <- sum(data$label[data$cluster == cluster] == label)
    
# Count the total number of items in the same cluster
total_same_cluster <- sum(data$cluster == cluster)
    
# Count the total number of items with the same category
total_same_category <- sum(data$label == label)
    
# Calculate precision and recall for the current item and add them to the sums
precision_sum <- precision_sum + same_category_same_cluster /total_same_cluster
recall_sum <- recall_sum + same_category_same_cluster / total_same_category
  }

  # Calculate average precision and recall
  precision <- precision_sum / n
  recall <- recall_sum / n

  return(list(precision = precision, recall = recall))
}

# Calculate BCubed precision and recall
metrics <- calculate_bcubed_metrics(data)

# Extract precision and recall from the metrics
precision <- metrics$precision
recall <- metrics$recall

# Print the results
cat("BCubed Precision:", precision, "\n")
cat("BCubed Recall:", recall, "\n")
``` 

k-means clustring k=4: 
```{r}
set.seed(8953)
dataset=scale(dataset)
kmeans.result <- kmeans(dataset, 4)
# print the clusterng result
kmeans.result
```
visualize clustering
```{r}
#install.packages("factoextra")
library(factoextra)
fviz_cluster(kmeans.result, data = dataset)
``` 
Total within-cluster-sum of square:
```{r}
kmeans.result$tot.withinss
```
BCubed precision and recall:
```{r}
cluster_assignments <- c(kmeans.result$cluster)
ground_truth_labels <- c(oldDataset$target)

data <- data.frame(cluster = cluster_assignments, label = ground_truth_labels)

# Function to calculate BCubed precision and recall
calculate_bcubed_metrics <- function(data) {
  n <- nrow(data)
  precision_sum <- 0
  recall_sum <- 0

  for (i in 1:n) {
    cluster <- data$cluster[i]
    label <- data$label[i]
    
# Count the number of items from the same category within the same cluster
same_category_same_cluster <- sum(data$label[data$cluster == cluster] == label)
    
# Count the total number of items in the same cluster
total_same_cluster <- sum(data$cluster == cluster)
    
# Count the total number of items with the same category
total_same_category <- sum(data$label == label)
    
# Calculate precision and recall for the current item and add them to the sums
precision_sum <- precision_sum + same_category_same_cluster /total_same_cluster
recall_sum <- recall_sum + same_category_same_cluster / total_same_category
  }

  # Calculate average precision and recall
  precision <- precision_sum / n
  recall <- recall_sum / n

  return(list(precision = precision, recall = recall))
}

# Calculate BCubed precision and recall
metrics <- calculate_bcubed_metrics(data)

# Extract precision and recall from the metrics
precision <- metrics$precision
recall <- metrics$recall

# Print the results
cat("BCubed Precision:", precision, "\n")
cat("BCubed Recall:", recall, "\n")
 
```
After conducting k-means clustering on the dataset and evaluating different numbers of clusters, we determined that two clusters are optimal. This decision was supported by silhouette analysis, which measures how similar an object is to its own cluster compared to other clusters. The silhouette method showed us that a two-cluster solution had the highest average silhouette width, indicating a more separated cluster compared to other k sizes. Also, the elbow method, which examines the within-cluster sum of squares, confirm this finding by showing a clear 'elbow' at two clusters, suggesting that increasing the number of clusters beyond this point would cause overlapping clusters so we chose k=2  and 2 numbers nears to it.
----------------------------------------------------------------------------------------------------------
Two means model gives us the highest quality of clusters since we were able to reach the precision of 0.70 and recall of 0.72 which is considered high. 0.70 precision indicates that our cluster is leaning towards purity and most of the objects in the same cluster truly belong to The same category (target/ not target of heart attack). 0.72 recall tells us that our data is accurately clustered meaning that a lot of the objects that are from the same category are assign to the same cluster.
-----------------------------------------------------------------------------------------------------------

Hierarchical Clustering:

```{r}
set.seed(2835)
# draw a sample of 40 records from the USArrests data, so that the clustering plot will not be over crowded
idx <- sample(1:dim(dataset)[1], 50)
dataset2 <- dataset[idx, ]
## hiercrchical clustering
library(factoextra) 
hc.cut <- hcut(dataset2, k = 2, hc_method = "complete") # Computes Hierarchical Clustering and Cut the Tree


# Visualize dendrogram
fviz_dend(hc.cut,rect = TRUE)  #logical value specifying whether to add a rectangle around groups.
# Visualize cluster
fviz_cluster(hc.cut, ellipse.type = "convex")



set.seed(2835)
# draw a sample of 40 records from the USArrests data, so that the clustering plot will not be over crowded
idx <- sample(1:dim(dataset)[1], 50)
dataset2 <- dataset[idx, ]
## hiercrchical clustering
library(factoextra) 
hc.cut <- hcut(dataset2, k = 3, hc_method = "complete") # Computes Hierarchical Clustering and Cut the Tree


# Visualize dendrogram
fviz_dend(hc.cut,rect = TRUE)  #logical value specifying whether to add a rectangle around groups.
# Visualize cluster
fviz_cluster(hc.cut, ellipse.type = "convex")




set.seed(2835)
# draw a sample of 40 records from the USArrests data, so that the clustering plot will not be over crowded
idx <- sample(1:dim(dataset)[1], 50)
dataset2 <- dataset[idx, ]
## hiercrchical clustering
library(factoextra) 
hc.cut <- hcut(dataset2, k = 4, hc_method = "complete") # Computes Hierarchical Clustering and Cut the Tree


# Visualize dendrogram
fviz_dend(hc.cut,rect = TRUE)  #logical value specifying whether to add a rectangle around groups.
# Visualize cluster
fviz_cluster(hc.cut, ellipse.type = "convex")


```
We used hierarchical clustering to see if we can get better cluster results that k-means, but since k-means gave us the least overlapping, we were able to understand or data better. We decided to use k means method instead of hierarchical since k means showed us clearer clusters.
------------------------------------------------------------------------------------------


Validation using average silhouette for each clusters:
```{r}
# Install packages if they are not already installed
if (!require("factoextra")) install.packages("factoextra")
if (!require("cluster")) install.packages("cluster")
if (!require("NbClust")) install.packages("NbClust")

# Load necessary libraries
library(factoextra)
library(cluster)
library(NbClust)

# Set seed for reproducibility
set.seed(123)

# Assuming the dataset is already read and is named 'dataset'
# The column removal should be done once, if needed, before scaling
dataset <- dataset[,-14]

# Scale the dataset (this should be done outside the loop, only once)
dataset <- scale(dataset)

# Prepare a vector to hold average silhouette widths for each k (k from 2 to 4)
avg_sil_widths <- numeric(3)

# Loop over k from 2 to 4
for (k in 2:4) {
  # Run the k-means algorithm on the dataset
  kmeans.result <- kmeans(dataset, centers = k, nstart = 25)
  # Calculate silhouette widths for each point
  sil_widths <- silhouette(kmeans.result$cluster, dist(dataset))
  # Calculate the average silhouette width
  avg_sil_widths[k-1] <- mean(sil_widths[, "sil_width"]) # corrected index k-1
}

# Print the average silhouette widths for each k
avg_sil_widths

# Function to compute average silhouette width
silhouette_score <- function(k){ 
  km <- kmeans(dataset, centers = k, nstart = 25)
  ss <- silhouette(km$cluster, dist(dataset))
  sil <- mean(ss[, 3])
  return(sil)
}

# Use silhouette method to determine the optimal number of clusters
fviz_nbclust(dataset, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

# Initialize wss for k values from 2 to 10, so it should have 9 elements
wss <- numeric(9) # Corrected to 9 to match the loop below

# Loop over k from 2 to 10
for (k in 2:10) {
  # Run the k-means algorithm on the dataset with k clusters
  kmeans.result <- kmeans(dataset, centers = k, nstart = 25)
  # Store the total within-cluster sum of squares in the wss vector
  wss[k-1] <- kmeans.result$tot.withinss # Corrected index k-1
}

# You can now plot the WSS (Elbow method) or print the WSS values
# plot(2:10, wss, type = "b", xlab = "Number of Clusters", ylab = "Within-cluster sum of squares",
#     main = "Elbow Method for Optimal Number of Clusters")


```
For Clustering, we used K-means algorithm with 3 different values for K to find the optimal number of clusters. We calculated the average silhouette width for each K, and found the following outcomes:
• Number of cluster(K)= 2, the average silhouette width=0.16
• Number of cluster(K)= 3, the average silhouette width=0.11
• Number of cluster(K)= 4, the average silhouette width=0.12
The model that has the optimal number of clusters is 2-Mean because it has the best average silhouette width and that means the objects that are in the same cluster are  closer to each other and  distanced from the objects in the other cluster.
---------------------------------------------------------------------------------------------------------
total within-cluster sum of square(graph)
```{r}     
fviz_nbclust(dataset, kmeans, method = "wss")
```
total within-cluster sum of square tells us that k=4 is the optimal number of clusters since low wss means that the cluster all well seperated from each other.
----------------------------------------------------------------------------------------------------------
Bcubed and silhouette methods show us that  2 means is the best choice for clustering, and total within-cluster sum of square was the only method that told us that 4 means is the best.But in our case,this is not accurate because low WSS is supposed to mean that the clusters don’t really overlap but in our case 4 means shows us that there is a lot of overlapping (although it has the least wss value) in the clusters and 2 means gives us the least overlapping to almost no overlapping (although it has the most wss value). so we must discard what  the total within-cluster sum of square is telling us and choose the 2 means as the optimal number of clusters.




