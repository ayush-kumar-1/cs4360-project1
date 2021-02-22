library(ggplot2)
library(reshape2)
source("regression_utils.R")

red_wine = read.csv("../data/WineQuality/winequality-red.csv", sep = ";")


head(red_wine)

##Exploratory Data Analysis 
#correlation matrices to determine collinearity 

plot_cormat(red_wine)
#reproducibility for both dataframes (red_wine, white_wine)

scatter(red_wine, "quality", col = 3, row = 4)

###feature selection
#feature selection using forward selection
full_formula = lm(quality ~ ., data = red_wine) 


empty_formula = lm(quality ~ 1, data = red_wine)

#("output/wine/red_forward_selectionR.txt")


step(empty_formula, 
     scope=list(upper = full_formula), 
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


### Quadratic Model and Feature Selection 



full_model_quad = lm(quality ~ fixed.acidity + I(fixed.acidity^2) + volatile.acidity + I(volatile.acidity^2) + 
                       sulphates + I(sulphates^2) + citric.acid + I(citric.acid^2) + 
                       residual.sugar + I(residual.sugar^2) + chlorides + I(chlorides^2) + 
                       free.sulfur.dioxide + I(free.sulfur.dioxide^2) + total.sulfur.dioxide + I(total.sulfur.dioxide^2) + 
                       density + I(density^2) + pH + I(pH^2) + alcohol + I(alcohol^2), data = red_wine)

forward_quad = step(empty_formula, 
     scope=list(upper = full_model_quad), 
     direction = "forward", trace = FALSE)


backward_quad = step(full_model_quad, 
     direction = "backward", trace = FALSE) 

stepwise_quad = step(empty_formula, 
                    scope=list(upper = full_model_quad), 
                    direction = "both", trace = FALSE)

summary(forward_quad)
summary(backward_quad)
summary(stepwise_quad)


### Quadratic Model with Cross Terms 


full_model_quadX = lm(quality ~ (fixed.acidity + volatile.acidity + sulphates + citric.acid + residual.sugar + 
                                   chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + alcohol)^2 + 
                         I(fixed.acidity^2) +  I(volatile.acidity^2) + I(sulphates^2) +  I(citric.acid^2) + 
                         I(residual.sugar^2) +  I(chlorides^2) + I(free.sulfur.dioxide^2) +  I(total.sulfur.dioxide^2) + I(density^2) + 
                         I(pH^2) + I(alcohol^2),
                      data = red_wine)


forward_quadX = step(empty_formula, 
                    scope=list(upper = full_model_quadX), 
                    direction = "forward", trace = FALSE)


backward_quadX = step(full_model_quadX, 
                     direction = "backward", trace = FALSE) 

stepwise_quadX = step(empty_formula, 
                     scope=list(upper = full_model_quadX), 
                     direction = "both", trace = FALSE)

summary(forward_quadX)
summary(backward_quadX)
summary(stepwise_quadX)

### Cubic Regression 

full_model_cubic =  lm(quality ~ fixed.acidity + volatile.acidity + sulphates + citric.acid + residual.sugar + 
                         chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + alcohol + #linear Terms
                         I(fixed.acidity^2) +  I(volatile.acidity^2) + I(sulphates^2) +  I(citric.acid^2) + 
                         I(residual.sugar^2) +  I(chlorides^2) + I(free.sulfur.dioxide^2) +  I(total.sulfur.dioxide^2) + I(density^2) + 
                         I(pH^2) + I(alcohol^2) + #quadratic terms
                         I(fixed.acidity^3) +  I(volatile.acidity^3) + I(sulphates^3) +  I(citric.acid^3) + 
                         I(residual.sugar^3) +  I(chlorides^3) + I(free.sulfur.dioxide^3) +  I(total.sulfur.dioxide^3) + I(density^3) + 
                         I(pH^3) + I(alcohol^3), 
                       data = red_wine)

forward_cubic = step(empty_formula, 
                     scope=list(upper = full_model_cubic), 
                     direction = "forward", trace = FALSE)


backward_cubic = step(full_model_cubic, 
                      direction = "backward", trace = FALSE) 

stepwise_cubic = step(empty_formula, 
                      scope=list(upper = full_model_cubic), 
                      direction = "both", trace = FALSE)

summary(forward_cubic)
summary(backward_cubic)
summary(stepwise_cubic)


### Cubic Regression with Cross Terms

full_model_cubicX =  lm(quality ~ (fixed.acidity + volatile.acidity + sulphates + citric.acid + residual.sugar + 
                         chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + alcohol + #linear Terms
                         I(fixed.acidity^2) +  I(volatile.acidity^2) + I(sulphates^2) +  I(citric.acid^2) + 
                         I(residual.sugar^2) +  I(chlorides^2) + I(free.sulfur.dioxide^2) +  I(total.sulfur.dioxide^2) + I(density^2) + 
                         I(pH^2) + I(alcohol^2))^2 + #quadratic terms
                         I(fixed.acidity^3) +  I(volatile.acidity^3) + I(sulphates^3) +  I(citric.acid^3) + 
                         I(residual.sugar^3) +  I(chlorides^3) + I(free.sulfur.dioxide^3) +  I(total.sulfur.dioxide^3) + I(density^3) + 
                         I(pH^3) + I(alcohol^3), #cubic terms, all crossed
                       data = red_wine)

forward_cubicX = step(empty_formula, 
                     scope=list(upper = full_model_cubicX), 
                     direction = "forward", trace = FALSE)


#backward_cubicX = step(full_model_cubicX, 
                      # = "backward", trace = FALSE) 

stepwise_cubicX = step(empty_formula, 
                      scope=list(upper = full_model_cubicX), 
                      direction = "both", trace = FALSE)

summary(forward_cubicX)
summary(backward_cubicX)
summary(stepwise_cubicX)
