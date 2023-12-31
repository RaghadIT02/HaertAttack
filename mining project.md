---
title: "Mining project"
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidymodels)
library(tidyr)
library(MASS)
library(rpart)
library(rpart.plot)
library(caret)
```

## The goal of collecting this dataset

our goal is to Identify whether the chances of of a heart attack occurrence is high or low for the patient

#classification and clustering goal

Classification: We will classify target people into different groups based on appearance of muilty symptoms of Heart attack disease .This will allow us to predict the people who target to have heart attack based on certain factors .(classification done by Raghad and Taif)

Clustering: We will create a set of clusters for people who have similar symptoms. This will allow us to identify groups of people who are have the similar symptoms and target to have heart attack .(Clustring done by Sara and Leena)


We believe that by using these data mining tasks, we can build a model that can accurately predict the people target to have heart attack.


#modification on the pre processing done by Raghad and Taif.



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

library(readxl)
dataset <- read_excel("Heart Attack Data Set.xlsx")
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
The summary function provides information such as the minimum, 1st quartile, median, mean, 3rd quartile, and maximum values for each variable. It also includes the number of missing values for the thal variable.



---------------------------
Missing values and Null values
---------------------------
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
install.packages("ggplot2")
library(ggplot2)

ggplot(data = dataset, aes(x = "", y = trestbps)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(
    title = "Box Plot for resting blood pressure  ",
    y = "trestbps",
    x = NULL  # Remove x-axis label (since we only have one category)
  ) +
  theme_minimal()

ggplot(data = dataset, aes(x = "", y = thalach)) +
  geom_boxplot(fill = "lightpink", color = "black") +
  labs(
    title = "Box Plot for heart rate achieved ",
    y = "heart rate",
    x = NULL  # Remove x-axis label (since we only have one category)
  ) +
  theme_minimal()

ggplot(data = dataset, aes(x = "", y = oldpeak)) +
  geom_boxplot(fill = "purple", color = "black") +
  labs(
    title = "Box Plot for ST depression",
    y = " ST depressione",
    x = NULL  # Remove x-axis label (since we only have one category)
  ) +
  theme_minimal()
ggplot(data = dataset, aes(x = "", y =chol)) +
  geom_boxplot(fill = "lightyellow", color = "black") +
  labs(
    title = "Box Plot for cholestero",
    y = "chol",
    x = NULL  # Remove x-axis label (since we only have one category)
  ) +
  theme_minimal()

```

we can also detect summary of each attribute from the box plot , and we use box plot to detect  the outlier.

Histogram
```{r}
hist(dataset$chol)
```

The graph represents the frequency of cholesterol in the data set. After observing the values, we concluded that most of the values fall in the range higher than the normal range, as the normal range falls between  125-200.

Scatter plot
```{r}
library(ggplot2)
gg <- ggplot(dataset, aes(x=chol, y=trestbps)) + geom_point(aes(col=target, size=oldpeak)) + labs(subtitle="trestbps Vs chol", y="trestbps", x="chol", title="Scatterplot", 
       caption = "Source: midwest", bins = 30)
plot(gg)
```
The scatter plot represents a comparison between the variables 'trestbps' and 'chol'. The plot displays each data point as a dot on the chart, where the x-axis represents 'trestbps' values and the y-axis represents 'chol' values.

In this scatter plot, we can observe a pattern or trend, indicating that there might be a correlation between 'trestbps' and 'chol'. This could potentially mean that changes in one variable might be associated with changes in the other variable.

The range of 'trestbps' values is from 100 to 175, while the range of 'chol' values is from 230 to 320. This indicates that there are both positive and negative relationships between these two variables.


------------
Bar plot
------------
 There is no point in using bar plots because the frequency of one symptom (ex: high blood pressure alone ) is not enough to decide whether the patients are likely to have a heart attack or not

--------------
 outliers:
--------------

Detecting the outliers:
```{r}
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
dataset$age<-cut(dataset$age, breaks = seq(29, 77, by = 10),right=TRUE)
```
In this process, we divide the age values into five intervals with equal width. This allows us to simplify and classify the data, making it more suitable for later use in our model. By discretizing the age values, we effectively group them into five meaningful categories. This transformation will result in a clearer and more organized data set that can be easily understood and utilized in subsequent stages of our model.



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
dataset$thal <- normalize(dataset$thal)
```

In order to ensure comparability across individuals and eliminate biases resulting from differences in measurement units or scales, we have decided to normalize the following attributes: trestbps, chol, thalach, oldpeak, cp, oldpeak, slope,and thal.


------------------------------------
feature selection
------------------------------------
we did not applly feature selection beacause the number of attribute is small and we will  neeed them
------------------------------------

##Correlation Analysis: We will find the correlation between each attributes and the class label (target). For the nominal data we will use chi-square and for the numeric data we will use correlation coefficient. This will help us to determine the most important and correlated attributes to the target.
```{r}
####Sex:

csex=chisq.test(dataset$sex , dataset$target)
print(csex)
####Chest pain type (cp):

ccp=chisq.test(dataset$cp , dataset$target)
print(ccp)
####Fasting blood sugar (fbs):

cfbs=chisq.test(dataset$fbs , dataset$target)
print(cfbs)
####Resting electrocardiographic result (restecg):

crestecg=chisq.test(dataset$restecg , dataset$target)
print(crestecg)
####Exercise induced anginal (exang):

cexang=chisq.test(dataset$exang , dataset$target)
print(cexang)
####The slope of the peak exercise ST segment (slope):

cslope=chisq.test(dataset$slope , dataset$target)
print(cslope)
####Number of major vessels (ca):

cca=chisq.test(dataset$ca , dataset$target)
print(cca)
####A blood disorder (thal):

cthal=chisq.test(dataset$thal , dataset$target)
print(cthal)
####Age: The age attribute after discretization.

cage= chisq.test(dataset$age, dataset$target)
print(cage)
####Resting blood pressure (trestbps)

ctrestbps=chisq.test(dataset$trestbps ,dataset$target)
print(ctrestbps)

```
|  Attribute name | Chi-square value|Degree of freedom|Alpa|             
  |-----------------|-----------------|-----------------|-----------------|
  | A blood disorder (thal) | 85.304|3|2.2e-16|
   | Chest pain type (cp)    | 81.686 |3| 2.2e-16| 
  | Number of major vessels (ca)| 74.367|4|2.712e-15|
  | Exercise induced anginal (exang)| 55.945 |1|7.454e-14|
  |The slope of the peak exercise ST segment (slope)| 47.507|2|4.831e-11|
  | Sex            | 22.717       | 1| 1.877e-06|
  |Resting blood pressure (trestbps)|  47.706|48|0.4848|
  | Resting electrocardiographic result (restecg)| 10.023 |2|0.006661|     
  |Age(after discretization)| 15.689  |3|0.001313|      
  | Fasting blood sugar (fbs)    | 0.10627   |1|0.7444|

    
###Chi-square Results: We will sort the Chi-square values from the highest to the lowest:

1-A blood disorder (thal) with value 85.304.
2-Chest pain type (cp) with value 81.686.
3-Number of major vessels (ca) with value 74.367 .
4-Exercise induced anginal (exang) with value 55.945 .
5-resting blood pressure (trestbps) with value 47.706 .
6-The slope of the peak exercise ST segment (slope) with value 47.507 .
7-Sex with value 22.717 .
8-Age with value 15.689 .
9-Resting electrocardiographic result (restecg) with value 10.023.
10-Fasting blood sugar(fbs) with value 0.10627 .
All of the attributes is dependent to the class lable (target) except Fasting blood sugar(fbs) since the p-value is higher than chi-square value (chi-square = 0.10627 < 0.7444=p-value).   
 

----------------------
###Correlation coefficient for numeric data:
----------------------
```{r}
####Serum cholestoral (chol):

cchol=cor(dataset$chol ,dataset$target)
print(cchol)
####Maximum heart rate achieved (thalach):

cthalach=cor(dataset$thalach ,dataset$target)
print(cthalach)
####ST depression indicated by exercise relative to rest (oldpeak):

coldpeak=cor(dataset$oldpeak ,dataset$target)
print(coldpeak)
```
###Correlation coefficient Results: The highest correlation coefficient value is ST depression indicated by exercise relative to rest (oldpeak) which is negatively correlated (-0.430696) with the target. Then, the value of maximum heart rate achieved (thalach) which is positively correlated (0.4217409) with the target. Lastly, the value of Serum cholestoral (chol) which is negatively correlated (-0.08523911) with the target.

From the previous results we decided to delete the Fasting blood sugar (fbs) since it is independent from the class label (target) which means that fasting blood sugar (fbs) doesn't affect our class label (target).

```{r}
dataset <- dataset[, -which(names(dataset) == "fbs")]
head(dataset)
```


```{r}
ncol(dataset)
nrow(dataset)
```
After printing a sample from our new data set we notice that the attribute decrease from 14 to 13 since we remove Fasting blood sugar (fbs) and the rows remain the same.


print the final preprocessed dataset
------------------------------------
```{r}
print(data)
```


we apply chi-square and correlation coefficient to know the attribute that affect our class label in preprocessing ,then started work in classification.
Classification:  is a supervised learning technique where a model is trained on a labeled dataset, meaning the dataset has already been tagged or classified with the correct class labels. 
while we try to increase the accuracy, we try different way one of this way  was add  attribute and  remove attribute and we noticed that the following attribute:slope , doesn’t affect accuracy adding them or removing them doesn’t change anything.


-------------------
Classification:
-------------------


## data structure

```{r}
str(dataset)
```

## split the data in training and test data (75/25)

```{r}
set.seed(123)
data_split <- initial_split(dataset, prop = 0.75)
train_data <- training(data_split)
test_data <- testing(data_split)
```


## build decision tree for each partition and measure

```{r}
tree_1_ig <- rpart(as.factor(target) ~ ., data = train_data, method = "class", parms = list(split = "information"))

tree_1_gini <- rpart(target ~ ., data = train_data, method = "class", parms = list(split = "gini"))

tree_1_gr <- rpart(target ~ ., data = train_data, method = "class", parms = list(split = "anova"))
```


##plot the decision trees for each measure(IG)

```{r}
rpart.plot(tree_1_ig, main = "Decision Tree for 75/25 Partition using Information Gain")
```



## plot the decision trees for each measure(GINI Ratio)

```{r}
rpart.plot(tree_1_gini, main = "Decision Tree for 75/25 Partition using IG ratio")
```


## plot the decision trees for each measure(GINI index)

```{r}
rpart.plot(tree_1_gr, main = "Decision Tree for 75/25 Partition using GINI index")
```


This is a decision tree diagram for a 75/25 partition using information gain. It is a graphical representation of a decision-making process, where each node represents a decision and each branch represents a possible outcome.

The tree contains 6 variables with 6 splits. Splits have occurred on the variables ca, Cp, thal, trestbp, and sex.

## predict the target on the test set

```{r}
#predict the target on the test set
pred_1_ig <- predict(tree_1_ig, newdata = test_data, type = "class")

pred <- as.factor(pred_1_ig) 
actual <- as.factor(test_data$target)

confusionMatrix(pred, actual)

#create a confusion matrix
table(pred_1_ig, test_data$target)

#calculate the overall accuracy
mean(pred_1_ig ==  test_data$target)


#predict the target on the test set
tree_1_gini <- predict(tree_1_gini, newdata = test_data, type = "class")

#create a confusion matrix
table(tree_1_gini, test_data$target)

pred <- as.factor(tree_1_gini) 
actual <- as.factor(test_data$target)

confusionMatrix(pred, actual)



#calculate the overall accuracy
mean(tree_1_gini ==  test_data$target)

#predict the target on the test set
tree_1_gr <- predict(tree_1_gr, newdata = test_data, type = "class")

#create a confusion matrix
table(tree_1_gr, test_data$target)

pred <- as.factor(tree_1_gr) 
actual <- as.factor(test_data$target)

confusionMatrix(pred, actual)



#calculate the overall accuracy
mean(tree_1_gr ==  test_data$target)

```

the confusion matrix of the IG model showed the accuracy of 76%, th gini ration showed 75%, while the gini index showed 75% accuracy looking at the accuracy level we can that the IG model performed better at classification.


## C4.5 algorithm (75/25)

```{r}
library(RWeka)
# load the package
library(RWeka)
# load data
# fit model
tree <- J48(as.factor(target) ~ ., data = train_data)
# summarize the fit
summary(tree)
# make predictions
predictions <- predict(tree, test_data)
# summarize accuracy
table(predictions, test_data$target)
#calculate the overall accuracy
mean(predictions ==  test_data$target)
library(partykit)


```


## Cart algorithm (75/25)

```{r}
# load the package
library(ipred)
# fit model
tree <- Bagging (as.factor(target) ~ ., data = train_data)
summary(tree)
# make predictions
predictions <- predict(tree, test_data)
# summarize accuracy
table(predictions, test_data$target)
#calculate the overall accuracy
mean(predictions ==  test_data$target)

```




## split the data in training and test data (80/20)


```{r}
set.seed(123)
data_split <- initial_split(dataset, prop = 0.80)
train_data <- training(data_split)
test_data <- testing(data_split)
```

## build decision tree for each partition and measure (ID.3 algorithm)

```{r}
tree_1_ig <- rpart(target ~ ., data = train_data, method = "class", parms = list(split = "information"))

tree_1_gini <- rpart(target ~ ., data = train_data, method = "class", parms = list(split = "gini"))

tree_1_gr <- rpart(target ~ ., data = train_data, method = "class", parms = list(split = "anova"))
```


## plot the decision trees for each measure(IG)

```{r}
rpart.plot(tree_1_ig, main = "Decision Tree for 80/20 Partition using Information Gain")
```

## plot the decision trees for each measure(GINI Ratio)

```{r}
rpart.plot(tree_1_gini, main = "Decision Tree for 80/20 Partition using Information Gain")
```

##plot the decision trees for each measure(GINI index)

```{r}
rpart.plot(tree_1_gr, main = "Decision Tree for 80/20 Partition using Information Gain")
```

This is a decision tree diagram for a 80/20 partition using information gain. It is a graphical representation of a decision-making process, where each node represents a decision and each branch represents a possible outcome.

The tree contains 6 variables with 6 splits. Splits have occurred on the variables ca, Cp, thal, trestbp, and sex.


## predict the target on the test set


```{r}
#predict the species on the test set
pred_1_ig <- predict(tree_1_ig, newdata = test_data, type = "class")

#create a confusion matrix
table(pred_1_ig, test_data$target)

pred <- as.factor(pred_1_ig) 
actual <- as.factor(test_data$target)

confusionMatrix(pred, actual)

#calculate the overall accuracy
mean(pred_1_ig ==  test_data$target)


#predict the species on the test set
tree_1_gini <- predict(tree_1_gini, newdata = test_data, type = "class")

#create a confusion matrix
table(tree_1_gini, test_data$target)

pred <- as.factor(tree_1_gini) 
actual <- as.factor(test_data$target)

confusionMatrix(pred, actual)

#calculate the overall accuracy
mean(tree_1_gini ==  test_data$target)

#predict the target on the test set
tree_1_gr <- predict(tree_1_gr, newdata = test_data, type = "class")

#create a confusion matrix
table(tree_1_gr, test_data$target)

pred <- as.factor(tree_1_gr) 
actual <- as.factor(test_data$target)

confusionMatrix(pred, actual)

#calculate the overall accuracy
mean(tree_1_gr ==  test_data$target)

```
the confusion matrix of the IG model showed the accuracy of 81%, th gini ratio showed 81%, while the gini index showed 81% accuracy looking at the accuracy level we can see that all the measures performed at thesame accuracy level.


## C4.5 algorithm (80/20)

```{r}
library(RWeka)
# load the package
library(RWeka)
# load data
# fit model
tree <- J48(as.factor(target) ~ ., data = train_data)
# summarize the fit
summary(tree)
# make predictions
predictions <- predict(tree, test_data)
# summarize accuracy
table(predictions, test_data$target)
#calculate the overall accuracy
mean(predictions ==  test_data$target)
library(partykit)

```


## Cart algorithm (80/20)

```{r}
# load the package
library(ipred)
# fit model
tree <- Bagging (as.factor(target) ~ ., data = train_data)
summary(tree)
# make predictions
predictions <- predict(tree, test_data)
# summarize accuracy
table(predictions, test_data$target)
#calculate the overall accuracy
mean(predictions ==  test_data$target)

```




## split the data in training and test data (60/40)


```{r}
set.seed(123)
data_split <- initial_split(dataset, prop = 0.60)
train_data <- training(data_split)
test_data <- testing(data_split)
```


## build decision tree for each partition and measure

```{r}
tree_1_ig <- rpart(target ~ ., data = train_data, method = "class", parms = list(split = "information"))

tree_1_gini <- rpart(target ~ ., data = train_data, method = "class", parms = list(split = "gini"))

tree_1_gr <- rpart(target ~ ., data = train_data, method = "class", parms = list(split = "anova"))
```


##plot the decision trees for each measure(IG)

```{r}
rpart.plot(tree_1_ig, main = "Decision Tree for 60/40 Partition using Information Gain")
```



## plot the decision trees for each measure(GINI Ratio)

```{r}
rpart.plot(tree_1_gini, main = "Decision Tree for 60/40 Partition using Information Gain")
```

## plot the decision trees for each measure(GINI index)

```{r}
rpart.plot(tree_1_gr, main = "Decision Tree for 60/40 Partition using Information Gain")
```


This is a decision tree diagram for a 60/40 partition using information gain. It is a graphical representation of a decision-making process, where each node represents a decision and each branch represents a possible outcome.

The tree contains 6 variables with 6 splits. Splits have occurred on the variables ca, Cp, thal, trestbp, and sex.

## predict the model on the test set

```{r}

#predict the target on the test set
pred_1_ig <- predict(tree_1_ig, newdata = test_data, type = "class")

#create a confusion matrix
table(pred_1_ig, test_data$target)

pred <- as.factor(pred_1_ig) 
actual <- as.factor(test_data$target)

confusionMatrix(pred, actual)

#calculate the overall accuracy
mean(pred_1_ig ==  test_data$target)


#predict the target on the test set
tree_1_gini <- predict(tree_1_gini, newdata = test_data, type = "class")

#create a confusion matrix
table(tree_1_gini, test_data$target)

pred <- as.factor(tree_1_gini) 
actual <- as.factor(test_data$target)

confusionMatrix(pred, actual)

#calculate the overall accuracy
mean(tree_1_gini ==  test_data$target)

#predict the target on the test set
tree_1_gr <- predict(tree_1_ig, newdata = test_data, type = "class")

#create a confusion matrix
table(tree_1_gr, test_data$target)
pred <- as.factor(tree_1_gr) 
actual <- as.factor(test_data$target)
confusionMatrix(pred, actual)
#calculate the overall accuracy
mean(tree_1_gr ==  test_data$target)


```
the confusion matrix of the IG model showed the accuracy of 77%, th gini ration showed 76%, while the gini index showed 77% accuracy looking at the accuracy level we can that the IG model and gini index model measure performed better at classification.


## C4.5 algorithm (60/40)

```{r}
library(RWeka)
# load the package
library(RWeka)
# load data
# fit model
tree <- J48(as.factor(target) ~ ., data = train_data)
# summarize the fit
summary(tree)
# make predictions
predictions <- predict(tree, test_data)
# summarize accuracy
table(predictions, test_data$target)
#calculate the overall accuracy
mean(predictions ==  test_data$target)
library(partykit)
plot(tree)

```


## Cart algorithm (60/40)

```{r}
# load the package
library(ipred)
# fit model
tree <- Bagging (as.factor(target) ~ ., data = train_data)
summary(tree)
# make predictions
predictions <- predict(tree, test_data)
# summarize accuracy
table(predictions, test_data$target)
#calculate the overall accuracy
mean(predictions ==  test_data$target)

```


------------------
discusion:
------------------
from the study we observe that the split 60/40 performed better because the misclassification error is lower and the accuracy value is higer than others.
The decision tree It is a graphical representation of a decision-making process, where each node represents a decision and each branch represents a possible outcome.
The tree contains 6 variables with 6 splits. Splits have occurred on the variables ca, Cp, thal, trestbp, and sex. this are the most important best features.
the c4.5 algorithm gives a high better accuracy, by looking at the confusion matrix and calculating the accuracy of the classification.


-------------------
Clustering:
-------------------

Optimal number of clusters:

```{r}
library(factoextra)
oldDataset<-dataset
dataset=dataset[,-1  ]

fviz_nbclust(dataset, kmeans, method = "silhouette")+labs(subtitle ="Silhouette method")



set.seed(123)  # Set seed for reproducibility


wss <- numeric(10)  # Vector to hold within-cluster sum of squares

for (k in 1:10) {
  # Run the k-means algorithm on the dataset
  model <- kmeans(dataset, centers = k, nstart = 25)
  # Store the within-cluster sum of squares
  wss[k] <- model$tot.withinss
}
wss[k]
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



