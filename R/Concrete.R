## ---------------------------
## Script name: Concrete.R
## Purpose of script: CSCI 4360 Project 1
## Author: Faisal Hossain
## Date Created: 02-14-2021
## ---------------------------

## Import Libraries
library(olsrr)
source("regression_utils.R")

## Read Datasets
concrete = read.csv("../data/ConcreteCompressiveStrength/Concrete_Data.csv", sep = ",", header=TRUE)
colnames(concrete) <- c("Cement", "Blast_Furnace_Slag", "Fly_Ash", "Water", "Superplasticizer", "Coarse_Aggregate", "Fine_Aggregate", "Age", "Concrete_Compressive_Strength")
head(concrete)

## Exploratory Data Analysis 
# Correlation Matrix
plot_cormat(concrete)

# Scatter Plots
scatter(concrete, "Concrete Compressive Strength", col = 3, row = 3)

## Regression & Feature Selection

# -- Multiple Linear Regression
model <- lm(Concrete_Compressive_Strength ~ ., data = concrete)

# Stepwise Forward Selection
k <- ols_step_forward_p(model, details = TRUE)
k
plot(k)

# Stepwise Backward Elimination
k <- ols_step_backward_p(model, details = TRUE)
k
plot(k)

# Stepwise Regression
k <- ols_step_both_p(model, details = TRUE)
k
plot(k)

# Ridge Regression

#Lasso Regression 

# -- Quadratic Regression Model
full_model_quad <- lm(Concrete_Compressive_Strength ~ Cement + Blast_Furnace_Slag + Fly_Ash + Water + Superplasticizer + Coarse_Aggregate + Fine_Aggregate + Age + #linear terms
                      I(Cement^2) + I(Blast_Furnace_Slag^2) + I(Fly_Ash^2) + I(Water^2) + I(Superplasticizer^2) + I(Coarse_Aggregate^2) + I(Fine_Aggregate^2) + I(Age^2), #quadratic terms
                      data = concrete)
empty_formula <- lm(Concrete_Compressive_Strength ~ 1, data = concrete)

# Quadratic Forward Selection
forward_quad <- step(empty_formula, scope=list(upper = full_model_quad), direction = "forward", trace = FALSE)
summary(forward_quad)

# Quadratic Backward Elimination
backward_quad <- step(full_model_quad, direction = "backward", trace = FALSE)
summary(backward_quad)

# Quadratic Stepwise Regression
stepwise_quad <- step(empty_formula, scope=list(upper = full_model_quad), direction = "both", trace = FALSE)
summary(stepwise_quad)


# -- Quadratic Regression Model with Cross Terms (QuadX)
full_model_quadX <- lm(Concrete_Compressive_Strength ~ (Cement + Blast_Furnace_Slag + Fly_Ash + Water + Superplasticizer + Coarse_Aggregate + Fine_Aggregate + Age)^2 + #linear terms summed squared
                      I(Cement^2) + I(Blast_Furnace_Slag^2) + I(Fly_Ash^2) + I(Water^2) + I(Superplasticizer^2) + I(Coarse_Aggregate^2) + I(Fine_Aggregate^2) + I(Age^2), #quadratic terms
                      data = concrete)

# QuadX Forward Selection
forward_quadX <-step(empty_formula, scope=list(upper = full_model_quadX), direction = "forward", trace = FALSE)
summary(forward_quadX)

# QuadX Backward Elimination
backward_quadX <- step(full_model_quadX, direction = "backward", trace = FALSE)
summary(backward_quadX)

# QuadX Stepwise Regression
stepwise_quadX <- step(empty_formula, scope=list(upper = full_model_quadX), direction = "both", trace = FALSE)
summary(stepwise_quadX)

# -- Cubic Regression Model
full_model_cubic <- lm(Concrete_Compressive_Strength ~ Cement + Blast_Furnace_Slag + Fly_Ash + Water + Superplasticizer + Coarse_Aggregate + Fine_Aggregate + Age + #linear terms
                      I(Cement^2) + I(Blast_Furnace_Slag^2) + I(Fly_Ash^2) + I(Water^2) + I(Superplasticizer^2) + I(Coarse_Aggregate^2) + I(Fine_Aggregate^2) + I(Age^2) + #quadratic terms
                      I(Cement^3) + I(Blast_Furnace_Slag^3) + I(Fly_Ash^3) + I(Water^3) + I(Superplasticizer^3) + I(Coarse_Aggregate^3) + I(Fine_Aggregate^3) + I(Age^3), #cubic terms
                      data = concrete)

empty_formula <- lm(Concrete_Compressive_Strength ~ 1, data = concrete)

# Cubic Forward Selection
forward_cubic <- step(empty_formula, scope=list(upper = full_model_cubic), direction = "forward", trace = FALSE)
summary(forward_cubic)

# Cubic Backward Elimination
backward_cubic <- step(full_model_cubic, direction = "backward", trace = FALSE) 
summary(backward_cubic)

# Cubic Stepwise Regression
stepwise_cubic <- step(empty_formula, scope=list(upper = full_model_cubic), direction = "both", trace = FALSE)
summary(stepwise_cubic)

# -- Cubic Regression with Cross Terms (CubicX)
full_model_cubicX <- lm(Concrete_Compressive_Strength ~ (Cement + Blast_Furnace_Slag + Fly_Ash + Water + Superplasticizer + Coarse_Aggregate + Fine_Aggregate + Age +
                        I(Cement^2) + I(Blast_Furnace_Slag^2) + I(Fly_Ash^2) + I(Water^2) + I(Superplasticizer^2) + I(Coarse_Aggregate^2) + I(Fine_Aggregate^2) + I(Age^2))^2 + #linear & quadratic terms summed squared
                        I(Cement^3) + I(Blast_Furnace_Slag^3) + I(Fly_Ash^3) + I(Water^3) + I(Superplasticizer^3) + I(Coarse_Aggregate^3) + I(Fine_Aggregate^3) + I(Age^3), #cubic terms
                        data = concrete)

# CubicX Forward Selection
forward_cubicX <- step(empty_formula, scope=list(upper = full_model_cubicX), direction = "forward", trace = FALSE)
summary(forward_cubicX )

# CubicX Backward Elimination
backward_cubicX <- step(full_model_cubicX, direction = "backward", trace = FALSE) 
summary(backward_cubicX )

# CubicX Stepwise Regression
stepwise_cubicX <- step(empty_formula, scope=list(upper = full_model_cubicX), direction = "both", trace = FALSE)
summary(stepwise_cubicX )







