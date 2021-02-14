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
  density + pH + sulphates + alcohol + I(residual.sugar^2)) 


empty_formula = as.formula(quality~1)

#("output/wine/red_forward_selectionR.txt")

step(lm(empty_formula, data = red_wine), 
     scope=list(upper = (lm(full_formula, data = red_wine))), 
     direction = "forward")


step(lm(full_formula, data = red_wine), 
     direction = "backward") 

step(lm(empty_formula, data = red_wine), 
     scope=list(upper = (lm(full_formula, data = red_wine))), 
     direction = "both")

#sink()

models = list(lm(quality~1, data = red_wine), 
   lm(quality~alcohol, data = red_wine),                  #model1
   lm(quality~alcohol + volatile.acidity, data = red_wine),             #model2
   lm(quality~alcohol + volatile.acidity + sulphates, data = red_wine), #model3
   lm(quality~alcohol + volatile.acidity + sulphates + 
        total.sulfur.dioxide, data = red_wine),                         #model4
   lm(quality~alcohol + volatile.acidity + sulphates + 
        total.sulfur.dioxide + chlorides, data = red_wine),             #model5
   lm(quality~alcohol + volatile.acidity + sulphates + 
        total.sulfur.dioxide + chlorides + pH, data = red_wine),        #model6
   lm(quality~alcohol + volatile.acidity + sulphates + 
        total.sulfur.dioxide + chlorides + pH + 
        free.sulfur.dioxide, data = red_wine),                          #model7
   lm(quality~alcohol + volatile.acidity + sulphates + 
        total.sulfur.dioxide + chlorides + pH + 
        free.sulfur.dioxide + citric.acid, data = red_wine),            #model8
   lm(quality~alcohol + volatile.acidity + sulphates + 
        total.sulfur.dioxide + chlorides + pH + 
        free.sulfur.dioxide + citric.acid + 
        residual.sugar, data = red_wine),                               #model9
   lm(quality~alcohol + volatile.acidity + sulphates + 
        total.sulfur.dioxide + chlorides + pH + 
        free.sulfur.dioxide + citric.acid + 
        residual.sugar + density, data = red_wine),                     #model10
   lm(quality~alcohol + volatile.acidity + sulphates + 
        total.sulfur.dioxide + chlorides + pH + 
        free.sulfur.dioxide + citric.acid + 
        residual.sugar + density + fixed.acidity, data = red_wine))     #model11

aics = rep(NA, 11)
rsq = rep(NA, 11)
rsq.adj = rep(NA, 11)
rsq.cv = rep(NA, 11)

for (i in 1:11) { 
    current = models[[i]]
    call = formula(current)
    
    aics[i] = AIC(current)
    
    yhat = predict(current)
    y = red_wine$quality
    
    sse = sum((yhat-y)^2)
    sst = var(y)*length(y)
    
    rsq[i] = 1-(sse/sst)
    rsq.adj[i] = 1-((1-rsq[i])*(length(y)-1)/((length(y)-i-1)))
    rsq.cv[i] = rsq_cv(call, red_wine, 10, "quality")
} #for i 

par(mfrow=c(2,2))
plot(1:11, aics, main = "AIC vs. Model Complexity", 
     xlab = "Number of Variables", ylab = "AIC")
plot(1:11, rsq, main = "R^2 vs. Model Complexity", 
     xlab = "Number of Variables", ylab = "rsq")
plot(1:11, rsq.adj, main = "adj-R^2 vs. Model Complexity", 
     xlab = "Number of Variables", ylab = "adj-rsq")
plot(1:11, rsq.cv, main = "cv-R^2 (10-Fold) vs. Model Complexity", 
     xlab = "Number of Variables", ylab = "cv-rsq")









