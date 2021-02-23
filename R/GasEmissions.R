library(ggplot2)
library(reshape2)
source("regression_utils.R")

emissions = read.csv("../data/emissions.csv")
dim(emissions)
head(emissions)

plot_cormat(emissions)

full_model = lm(NOX ~ AT + AP + AH + AFDP + GTEP + TIT + TAT +
                TEY + CDP + CO, data = emissions)

empty_model = lm(NOX ~ 1, data = emissions) 

forward = step(empty_model, 
     scope=list(upper = full_model), 
     direction = "forward")


backward = step(full_model, 
     direction = "backward") 

step = step(empty_model, 
     scope=list(upper = full_model), 
     direction = "both")


models = list(empty_model, 
              lm(NOX ~ AT, data = emissions),
              lm(NOX ~ AT + CO, data = emissions), 
              lm(NOX ~ AT + CO + AH, data = emissions), 
              lm(NOX ~ AT + CO + AH + GTEP, data = emissions), 
              lm(NOX ~ AT + CO + AH + GTEP + TIT, data = emissions), 
              lm(NOX ~ AT + CO + AH + GTEP + TIT + TAT, data = emissions), 
              lm(NOX ~ AT + CO + AH + GTEP + TIT + TAT + TEY, data = emissions), 
              lm(NOX ~ AT + CO + AH + GTEP + TIT + TAT + TEY + AP, data = emissions), 
              lm(NOX ~ AT + CO + AH + GTEP + TIT + TAT + TEY + AP + AFDP, data = emissions),
              full_model
) #list

aics = rep(NA, 11)
rsq = rep(NA, 11)
rsq.adj = rep(NA, 11)
rsq.cv = rep(NA, 11)

for (i in 1:11) { 
  current = models[[i]]
  call = formula(current)
  
  aics[i] = AIC(current)
  
  yhat = predict(current)
  y = emissions$NOX
  
  sse = sum((yhat-y)^2)
  sst = var(y)*length(y)
  
  rsq[i] = 1-(sse/sst)
  rsq.adj[i] = 1-((1-rsq[i])*(length(y)-1)/((length(y)-i-1)))
  rsq.cv[i] = rsq_cv(call, emissions, 10, "NOX")
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

###Quadratic Model 
###WARNING: Variable Selection is Computationally and Time Intensive 
full_model_quad  = lm(NOX ~ AT + AP + AH + AFDP + GTEP + TIT + TAT + TEY + CDP + CO +
                      I(AT^2) + I(AP^2) + I(AFDP^2) + I(GTEP^2) + I(TIT^2) + 
                        I(TAT^2) + I(TEY^2) + I(CDP^2) + I(CO^2), data = emissions)

forwardQuad = step(empty_model, 
     scope=list(upper = full_model_quad), 
     direction = "forward")


backwardQuad = step(full_model_quad, 
     direction = "backward") 

stepQuad = step(empty_model, 
     scope=list(upper = full_model_quad), 
     direction = "both")


summary(stepQuad)

###Quadratic Model with Cross Terms 

full_model_quadX  = lm(NOX ~ (AT + AP + AH + AFDP + GTEP + TIT + TAT + TEY + CDP + CO)^2 +
                        I(AT^2) + I(AP^2) + I(AFDP^2) + I(GTEP^2) + I(TIT^2) + 
                        I(TAT^2) + I(TEY^2) + I(CDP^2) + I(CO^2), data = emissions)

forwardQuadX = step(empty_model, 
     scope=list(upper = full_model_quad), 
     direction = "forward")


backwardQuadX = step(full_model_quad, 
     direction = "backward") 

stepQuadX = step(empty_model, 
                scope=list(upper = full_model_quadX), 
                direction = "both")

