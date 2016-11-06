library(data.table)
library(testthat)
library(gridExtra)
library(corrplot)
library(GGally)
library(ggplot2)
library(e1071)
library(dplyr)
source("/media/manashree/Study/Kaggle/EDAhelper.r")

dt_train <- fread("/media/manashree/Study/Kaggle/Allstate/train.csv")
test <-fread("test.csv")

dim(dt_train)
cat_var <- names(dt_train)[which(sapply(dt_train, is.character))]
num_var <- names(dt_train)[which(sapply(dt_train, is.numeric))]
num_var <- setdiff(num_var, c("id", "loss"))

dt_train_cat <- dt_train[,.SD, .SDcols = cat_var]
dt_train_num <- dt_train[,.SD,.SDcols = num_var]

#Missing values
colSums(sapply(dt_train, is.na)) # 0 missing values

# Check for duplicated rows.
cat("The number of duplicated rows are", nrow(dt_train) - nrow(unique(dt_train))) #0 duplicates

correlations <- cor(dt_train_num)
corrplot(correlations, method="square", order="hclust")


doPlots(dt_train_num,fun=plotDen,ii=1:3,dt_train$loss,ncol=2)
plotBox(dt_train_cat,14,dt_train$loss)
plotBox(dt_train_num,14,dt_train$loss)
plotDen(dt_train_num,1:3,dt_train$loss)
