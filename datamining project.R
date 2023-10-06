install.packages("readxl")
library(readxl)
dataset <- read_excel("C:/Users/ragha/OneDrive/سطح المكتب/New folder/Heart Attack Data Set.xlsx")
View(dataset)
nrow(dataset) #number of rows
ncol(dataset) #number of columns

sum(is.na(dataset))
summary(dataset)

