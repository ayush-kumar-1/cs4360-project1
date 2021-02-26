library(ggplot2)
library(reshape2)
library(glmnet)
source("regression_utils.R")

setwd("../../../GitHub/cs4360-project1/data/")
list.files()

bike_Data = read.csv("SeoulBikeData.csv", sep = ",")

head(bike_Data)


##############################################################
#bike_Data = bike_Data[-1]
#head(bike_Data)

#bike_Data["Season.Winter"] = as.numeric((bike_Data["Seasons"] == "Winter"))
#bike_Data["Season.Spring"] = as.numeric((bike_Data["Seasons"] == "Spring"))
#bike_Data["Season.Summer"] = as.numeric((bike_Data["Seasons"] == "Summer"))
#bike_Data = bike_Data[-11]

#levels(as.factor(bike_Data["Holiday"]))
#bike_Data["Holiday"] = as.numeric(bike_Data["Holiday"] == "Holiday")

#levels(as.factor(bike_Data["Functioning.Day"]))
#bike_Data["Functioning.Day"] = as.numeric(bike_Data["Functioning.Day"] == "Yes")

#head(bike_Data)

#write.csv(bike_Data, "../data/SeoulBikeDataCleaned.csv", row.names = FALSE)
#############################################################

bike_Data1 = read.csv("SeoulBikeDataCleaned.csv", sep = ",")
#print(bike_Data1)





head(bike_Data1)

##Exploratory Data Analysis 
#correlation matrices to determine collinearity 

#make columns numeric
plot_cormat(bike_Data1)

  
#reproducibility for both dataframes (red_wine, white_wine)

scatter(bike_Data1, "Rented.Bike.Count", col = 4, row = 4)

###feature selection
#feature selection using forward selection
full_formula = lm(Rented.Bike.Count ~ ., data = bike_Data1)


empty_formula = lm(Rented.Bike.Count ~ 1, data = bike_Data1)




step(empty_formula, 
     scope=list(upper = full_formula), 
     direction = "forward")


step(lm(full_formula, data = bike_Data1), 
     direction = "backward") 

step(lm(empty_formula, data = bike_Data1), 
     scope=list(upper = (lm(full_formula, data = bike_Data1))), 
     direction = "both")

#sink()
head(bike_Data1)

models = list(lm(Rented.Bike.Count~1, data = bike_Data1), 
              lm(Rented.Bike.Count~Hour, data = bike_Data1),                  #model1
              lm(Rented.Bike.Count~Hour + Temperature..C., data = bike_Data1),             #model2
              lm(Rented.Bike.Count~Hour + Temperature..C. + Humidity..., data = bike_Data1), #model3
              lm(Rented.Bike.Count~Hour + Temperature..C. + Humidity... + 
                   Wind.speed..m.s., data = bike_Data1),                         #model4
              lm(Rented.Bike.Count~Hour + Temperature..C. + Humidity... + 
                   Wind.speed..m.s. + Visibility..10m., data = bike_Data1),             #model5
              lm(Rented.Bike.Count~Hour + Temperature..C. + Humidity... + 
                   Wind.speed..m.s. + Visibility..10m. + Dew.point.temperature..C., data = bike_Data1),        #model6
              lm(Rented.Bike.Count~Hour + Temperature..C. + Humidity... + 
                   Wind.speed..m.s. + Visibility..10m. + Dew.point.temperature..C. + 
                   Solar.Radiation..MJ.m2., data = bike_Data1),                          #model7
              lm(Rented.Bike.Count~Hour + Temperature..C. + Humidity... + 
                   Wind.speed..m.s. + Visibility..10m. + Dew.point.temperature..C. + 
                   Solar.Radiation..MJ.m2. + Rainfall.mm., data = bike_Data1),            #model8
              lm(Rented.Bike.Count~Hour + Temperature..C. + Humidity... + 
                   Wind.speed..m.s. + Visibility..10m. + Dew.point.temperature..C. + 
                   Solar.Radiation..MJ.m2. + Rainfall.mm. + 
                   Snowfall..cm., data = bike_Data1),                               #model9
              lm(Rented.Bike.Count~Hour + Temperature..C. + Humidity... + 
                   Wind.speed..m.s. + Visibility..10m. + Dew.point.temperature..C. + 
                   Solar.Radiation..MJ.m2. + Rainfall.mm. + 
                   Snowfall..cm. + Holiday, data = bike_Data1),                     #model10
              lm(Rented.Bike.Count~Hour + Temperature..C. + Humidity... + 
                   Wind.speed..m.s. + Visibility..10m. + Dew.point.temperature..C. + 
                   Solar.Radiation..MJ.m2. + Rainfall.mm. + 
                   Snowfall..cm. + Holiday + Functioning.Day, data = bike_Data1),     #model11
              lm(Rented.Bike.Count~Hour + Temperature..C. + Humidity... + 
                   Wind.speed..m.s. + Visibility..10m. + Dew.point.temperature..C. + 
                   Solar.Radiation..MJ.m2. + Rainfall.mm. + 
                   Snowfall..cm. + Holiday + Functioning.Day + Season.Winter + Season.Spring + Season.Summer, data = bike_Data1))     #model12

aics = rep(NA, 12)
rsq = rep(NA, 12)
rsq.adj = rep(NA, 12)
rsq.cv = rep(NA, 12)

for (i in 1:12) { 
  current = models[[i]]
  call = formula(current)
  
  aics[i] = AIC(current)
  
  yhat = predict(current)
  y = bike_Data1$Rented.Bike.Count
  
  sse = sum((yhat-y)^2)
  sst = var(y)*length(y)
  
  rsq[i] = 1-(sse/sst)
  rsq.adj[i] = 1-((1-rsq[i])*(length(y)-1)/((length(y)-i-1)))
  rsq.cv[i] = rsq_cv(call, bike_Data1, 10, "Rented.Bike.Count")
} #for i 

par(mfrow=c(2,2))
plot(1:12, aics, main = "AIC vs. Model Complexity", 
     xlab = "Number of Variables", ylab = "AIC")
plot(1:12, rsq, main = "R^2 vs. Model Complexity", 
     xlab = "Number of Variables", ylab = "rsq")
plot(1:12, rsq.adj, main = "adj-R^2 vs. Model Complexity", 
     xlab = "Number of Variables", ylab = "adj-rsq")
plot(1:12, rsq.cv, main = "cv-R^2 (10-Fold) vs. Model Complexity", 
     xlab = "Number of Variables", ylab = "cv-rsq")


### Quadratic Model and Feature Selection 



full_model_quad = lm(Rented.Bike.Count ~ Hour + I(Hour^2) + Temperature..C. + I(Temperature..C.^2) + 
                       Humidity... + I(Humidity...^2) + Wind.speed..m.s. + I(Wind.speed..m.s.^2) + 
                       Visibility..10m. + I(Visibility..10m.^2) + Dew.point.temperature..C. + I(Dew.point.temperature..C.^2) + 
                       Solar.Radiation..MJ.m2. + I(Solar.Radiation..MJ.m2.^2) + Rainfall.mm. + I(Rainfall.mm.^2) + 
                       Snowfall..cm. + I(Snowfall..cm.^2) + Holiday + I(Holiday^2) + Functioning.Day + I(Functioning.Day^2) 
                        + Season.Winter + I(Season.Winter^2) + Season.Spring + I(Season.Spring^2) + Season.Summer + I(Season.Summer^2), data = bike_Data1)

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

#???????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????
### Quadratic Model with Cross Terms 


full_model_quadX = lm(Rented.Bike.Count ~ (Hour + Temperature..C. + Humidity... + Wind.speed..m.s. + Visibility..10m. + 
                                             Dew.point.temperature..C. + Solar.Radiation..MJ.m2. + Rainfall.mm. + Snowfall..cm. + Holiday + Functioning.Day + Season.Winter + Season.Spring + Season.Summer)^2 + 
                        I(Hour^2) +  I(Temperature..C.^2) + I(Humidity...^2) +  I(Wind.speed..m.s.^2) + 
                        I(Visibility..10m.^2) +  I(Dew.point.temperature..C.^2) + I(Solar.Radiation..MJ.m2.^2) +  I(Rainfall.mm.^2) + I(Snowfall..cm.^2) + 
                        I(Holiday^2) + I(Functioning.Day^2) + I(Season.Winter^2) + I(Season.Spring^2) + I(Season.Summer^2),
                      data = bike_Data1)


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

full_model_cubic =  lm(Rented.Bike.Count ~ Hour + Temperature..C. + Humidity... + Wind.speed..m.s. + Visibility..10m. + 
                         Dew.point.temperature..C. + Solar.Radiation..MJ.m2. + Rainfall.mm. + Snowfall..cm. + Holiday + Functioning.Day + Season.Winter + Season.Spring + Season.Summer + #linear Terms
                         I(Hour^2) +  I(Temperature..C.^2) + I(Humidity...^2) +  I(Wind.speed..m.s.^2) + 
                         I(Visibility..10m.^2) +  I(Dew.point.temperature..C.^2) + I(Solar.Radiation..MJ.m2.^2) +  I(Rainfall.mm.^2) + I(Snowfall..cm.^2) + 
                         I(Holiday^2) + I(Functioning.Day^2) + I(Season.Winter^2) + I(Season.Spring^2) + I(Season.Summer^2) + #quadratic terms
                         I(Hour^3) +  I(Temperature..C.^3) + I(Humidity...^3) +  I(Wind.speed..m.s.^3) + 
                         I(Visibility..10m.^3) +  I(Dew.point.temperature..C.^3) + I(Solar.Radiation..MJ.m2.^3) +  I(Rainfall.mm.^3) + I(Snowfall..cm.^3) + 
                         I(Holiday^3) + I(Functioning.Day^3) + I(Season.Winter^3) + I(Season.Spring^3) + I(Season.Summer^3), 
                       data = bike_Data1)

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

full_model_cubicX =  lm(Rented.Bike.Count ~ (Hour + Temperature..C. + Humidity... + Wind.speed..m.s. + Visibility..10m. + 
                                    Dew.point.temperature..C. + Solar.Radiation..MJ.m2. + Rainfall.mm. + Snowfall..cm. + Holiday + Functioning.Day + Season.Winter + Season.Spring + Season.Summer + #linear Terms
                                     I(Hour^2) +  I(Temperature..C.^2) + I(Humidity...^2) +  I(Wind.speed..m.s.^2) + 
                                     I(Visibility..10m.^2) +  I(Dew.point.temperature..C.^2) + I(Solar.Radiation..MJ.m2.^2) +  I(Rainfall.mm.^2) + I(Snowfall..cm.^2) + 
                                     I(Holiday^2) + I(Functioning.Day^2) + I(Season.Winter^2) + I(Season.Spring^2) + (Season.Summer^2))^2 + #quadratic terms
                          I(Hour^3) +  I(Temperature..C.^3) + I(Humidity...^3) +  I(Wind.speed..m.s.^3) + 
                          I(Visibility..10m.^3) +  I(Dew.point.temperature..C.^3) + I(Solar.Radiation..MJ.m2.^3) +  I(Rainfall.mm.^3) + I(Snowfall..cm.^3) + 
                          I(Holiday^3) + I(Functioning.Day^3) + I(Season.Winter^3) + I(Season.Spring^3) + I(Season.Summer^3), #cubic terms, all crossed
                        data = bike_Data1)

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
