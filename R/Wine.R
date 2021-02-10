library(ggplot2)
library(reshape2)
source("regression_utils.R")

red_wine = read.csv("../data/WineQuality/winequality-red.csv", sep = ";")
white_wine = read.csv("../data/WineQuality/winequality-white.csv", sep = ";")

head(red_wine) 
head(white_wine)

#splitting the datasets for cross-validation 

train_index = train_test_split(red_wine, 0.80)

red_train = red_wine[train_index,] #size 1279
red_test = red_wine[-train_index,] #size 320

train_index = train_test_split(white_wine, 0.80)

white_train = white_wine[train_index,] #size 3918
white_test = white_wine[-train_index,] #size 980

##Exploratory Data Analysis 
#correlation matrices to determine collinearity 

plot_cormat(red_train)
plot_cormat(white_train)

#reproducibility for both dataframes (red_wine, white_wine)
plot_all = function(winef) { 
    for (i in 1:11) {
      scatter(winef[,i], winef$quality, winef, 
            names(winef)[i], "quality")
    } # for i
  } #plot_all


plot_all(red_train)
plot_all(white_train)


!is.null(NULL)


