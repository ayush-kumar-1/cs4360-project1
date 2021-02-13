library(ggplot2)
library(reshape2)
source("regression_utils.R")

red_wine = read.csv("../data/WineQuality/winequality-red.csv", sep = ";")
white_wine = read.csv("../data/WineQuality/winequality-white.csv", sep = ";")

head(red_wine) 
head(white_wine)

##Exploratory Data Analysis 
#correlation matrices to determine collinearity 

plot_cormat(red_wine)
plot_cormat(white_wine)
#reproducibility for both dataframes (red_wine, white_wine)

scatter(red_wine, "quality", col = 3, row = 4)
scatter(white_wine, "quality", col = 3, row = 4)

###feature selection
#feature selection using forward selection
full_formula = as.formula(quality ~ fixed.acidity + 
  volatile.acidity + citric.acid + 
  residual.sugar + chlorides + 
  free.sulfur.dioxide + total.sulfur.dioxide +
  density + pH + sulphates + alcohol) 


empty_formula = as.formula(quality~1)























