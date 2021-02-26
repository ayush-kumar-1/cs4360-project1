library(ggplot2)
library(reshape2)
library(glmnet)
source("regression_utils.R")

autoMPG = read.csv("../data/auto-mpg.csv", sep = ",")


head(autoMPG)

##Exploratory Data Analysis 
#correlation matrices to determine collinearity 

plot_cormat(autoMPG)
#reproducibility for both dataframes (red_wine, white_wine)

scatter(autoMPG, "model.year", col = 3, row = 4)

###feature selection
#feature selection using forward selection
full_formula = lm(model.year ~ ., data = autoMPG) 


empty_formula = lm(model.year ~ 1, data = autoMPG)




step(empty_formula, 
     scope=list(upper = full_formula), 
     direction = "forward")


step(lm(full_formula, data = autoMPG), 
     direction = "backward") 

step(lm(empty_formula, data = autoMPG), 
     scope=list(upper = (lm(full_formula, data = autoMPG))), 
     direction = "both")

#sink()

models = list(lm(model.year~1, data = autoMPG), 
              lm(model.year~mpg, data = autoMPG),                  #model1
              lm(model.year~mpg + weight, data = autoMPG),             #model2
              lm(model.year~mpg + weight + horsepower, data = autoMPG), #model3
              lm(model.year~mpg + weight + horsepower + 
                   displacement, data = autoMPG),                         #model4
              lm(model.year~mpg + weight + horsepower + 
                   displacement + acceleration, data = autoMPG),             #model5
              lm(model.year~mpg + weight + horsepower + 
                   displacement + acceleration + displacement, data = autoMPG))        #model6
        

aics = rep(NA, 6)
rsq = rep(NA, 6)
rsq.adj = rep(NA, 6)
rsq.cv = rep(NA, 6)

for (i in 1:6) { 
  current = models[[i]]
  call = formula(current)
  
  aics[i] = AIC(current)
  
  yhat = predict(current)
  y = autoMPG$model.year
  
  sse = sum((yhat-y)^2)
  sst = var(y)*length(y)
  
  rsq[i] = 1-(sse/sst)
  rsq.adj[i] = 1-((1-rsq[i])*(length(y)-1)/((length(y)-i-1)))
  rsq.cv[i] = rsq_cv(call, autoMPG, 10, "model.year")
} #for i 

par(mfrow=c(2,2))
plot(1:6, aics, main = "AIC vs. Model Complexity", 
     xlab = "Number of Variables", ylab = "AIC")
plot(1:6, rsq, main = "R^2 vs. Model Complexity", 
     xlab = "Number of Variables", ylab = "rsq")
plot(1:6, rsq.adj, main = "adj-R^2 vs. Model Complexity", 
     xlab = "Number of Variables", ylab = "adj-rsq")
plot(1:6, rsq.cv, main = "cv-R^2 (10-Fold) vs. Model Complexity", 
     xlab = "Number of Variables", ylab = "cv-rsq")


### Quadratic Model and Feature Selection 



full_model_quad = lm(model.year ~ mpg + I(mpg^2) + cylinders + I(cylinders^2) + 
                       displacement + I(displacement^2) + horsepower + I(horsepower^2) + 
                       weight + I(weight^2) + acceleration + I(acceleration^2), data = autoMPG)

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


full_model_quadX = lm(model.year ~ (mpg + cylinders + displacement + horsepower + weight + 
                                      acceleration)^2 + 
                        I(mpg^2) +  I(cylinders^2) + I(displacement^2) +  I(horsepower^2) + 
                        I(weight^2) +  I(acceleration^2),
                      data = autoMPG)


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
#????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????
### Cubic Regression 

full_model_cubic =  lm(model.year ~ mpg + cylinders + displacement + horsepower + weight + 
                         acceleration + #linear Terms
                         I(mpg^2) +  I(cylinders^2) + I(displacement^2) +  I(horsepower^2) + 
                         I(weight^2) +  I(acceleration^2)  + #quadratic terms
                         I(mpg^3) +  I(cylinders^3) + I(displacement^3) +  I(horsepower^3) + 
                         I(weight^3) +  I(acceleration^3), 
                       data = autoMPG)

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

full_model_cubicX =  lm(model.year ~ (mpg + cylinders + displacement + horsepower + weight + 
                                        acceleration + #linear Terms
                                     I(mpg^2) +  I(cylinders^2) + I(displacement^2) +  I(horsepower^2) + 
                                     I(weight^2) +  I(acceleration^2))^2 + #quadratic terms
                          I(mpg^3) +  I(cylinders^3) + I(displacement^3) +  I(horsepower^3) + 
                          I(weight^3) +  I(acceleration^3), #cubic terms, all crossed
                        data = autoMPG)

forward_cubicX = step(empty_formula, 
                      scope=list(upper = full_model_cubicX), 
                      direction = "forward", trace = FALSE)


backward_cubicX = step(full_model_cubicX, 
 direction = "backward", trace = FALSE) 

stepwise_cubicX = step(empty_formula, 
                       scope=list(upper = full_model_cubicX), 
                       direction = "both", trace = FALSE)

summary(forward_cubicX)
summary(backward_cubicX)
summary(stepwise_cubicX)
