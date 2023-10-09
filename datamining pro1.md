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
dataset <- read_excel("C:/Users/ragha/OneDrive/سطح المكتب/New folder/Heart Attack Data Set.xlsx")
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
dataset$thal <- normalize(dataset$thal)
```

------------------------------------
print the final preprocessed dataset
------------------------------------
```{r}
print(data)
```