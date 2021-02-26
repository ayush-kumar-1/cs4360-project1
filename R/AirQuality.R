## ---------------------------
## Script name: AirQuality.R
## Purpose of script: CSCI 4360 Project 1
## Author: Faisal Hossain
## Date Created: 02-14-2021
## ---------------------------

## Import Libraries
library(glmnet)
library(ggplot)
library(olsrr)
source("regression_utils.R")

## Read Datasets
air_quality <- read.csv2("../data/AirQualityUCI/AirQualityUCI.csv", header = TRUE)

# Remove missing data
#tail(air_quality, n = 115) # Last 114 rows are null values
air_quality = air_quality[1:9357, 1:15] # Last two columns are null 
air_quality[air_quality == -200] <- NA # define Missing Value (NA)
air_quality <- na.omit(air_quality)

# Change and concatenate the date and time into a single column
air_quality = within(air_quality, {DateTime = format(as.POSIXct(paste(Date, Time), format = "%d/%m/%Y %H.%M.%S"))})
air_quality = air_quality[, c(16, 3:15)]
air_quality$DateTime = as.POSIXct(air_quality$DateTime)

colnames(air_quality) <- c("Time", "CO", "Tin_oxide", "NMHC", "Benzene", "Titania",
                           "NOx", "Tungsten_oxide_S2", "NO2", "Tungsten_oxide_S3", "Indium_oxide",
                           "Temperature", "Relative_Humidity", "Absolute_Humidity")
head(air_quality)

## Exploratory Data Analysis 
# Correlation Matrix
plot_cormat(air_quality)

# Scatter Plots
scatter(air_quality, "NOx", col = 3, row = 5)

## Regression & Feature Selection

# -- Multiple Linear Regression
model <- lm(NOx ~ ., data = air_quality)

# Stepwise Forward Selection
stepwise_forward <- ols_step_forward_p(model, details = TRUE)
stepwise_forward
plot(stepwise_forward)

# Stepwise Backward Elimination
stepwise_backward <- ols_step_backward_p(model, details = TRUE)
stepwise_backward
plot(stepwise_backward)

# Stepwise Regression
stepwise_backward <- ols_step_both_p(model, details = TRUE)
stepwise_backward
plot(stepwise_backward)

# -- Ridge & Lasso Regression
x = model.matrix(NOx ~ ., data = air_quality)
y = air_quality$NOx

# Ridge Regression 
ridge_lambda = cv.glmnet(x, y, alpha = 0)$lambda.min
ridge = glmnet(x, y, alpha = 0, lambda = ridge_lambda)
coef(ridge)

# Lasso Regression 
lasso_lambda = cv.glmnet(x, y, alpha = 1)$lambda.min
lasso = glmnet(x, y, alpha = 1, lamdda = lasso_lambda)
coef(lasso)

# -- Quadratic Regression Model
full_model_quad <- lm(NOx ~ Time + CO + Tin_oxide + NMHC + Benzene + Titania + Tungsten_oxide_S2 + NO2 + Tungsten_oxide_S3 + Indium_oxide + Temperature + Relative_Humidity + Absolute_Humidity + #linear terms
                    I(Time^2) + I(CO^2) + I(Tin_oxide^2) + I(NMHC^2) + I(Benzene^2) + I(Titania^2) + I(Tungsten_oxide_S2^2) + I(NO2^2) + I(Tungsten_oxide_S3^2) + I(Indium_oxide^2) + I(Temperature^2) + I(Relative_Humidity^2) + I(Absolute_Humidity^2), #quadratic terms
                    data = air_quality)
empty_formula <- lm(NOx ~ 1, data = air_quality)

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
full_model_quadX <- lm(NOx ~ (Time + CO + Tin_oxide + NMHC + Benzene + Titania + Tungsten_oxide_S2 + NO2 + Tungsten_oxide_S3 + Indium_oxide + Temperature + Relative_Humidity + Absolute_Humiditye)^2 + #linear terms summed squared
                      I(Time^2) + I(CO^2) + I(Tin_oxide^2) + I(NMHC^2) + I(Benzene^2) + I(Titania^2) + I(Tungsten_oxide_S2^2) + I(NO2^2) + I(Tungsten_oxide_S3^2) + I(Indium_oxide^2) + I(Temperature^2) + I(Relative_Humidity^2) + I(Absolute_Humidity^2), #quadratic terms
                      data = air_quality)

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
full_model_cubic <- lm(NOx ~ Time + CO + Tin_oxide + NMHC + Benzene + Titania + Tungsten_oxide_S2 + NO2 + Tungsten_oxide_S3 + Indium_oxide + Temperature + Relative_Humidity + Absolute_Humidity + #linear terms
                      I(Time^2) + I(CO^2) + I(Tin_oxide^2) + I(NMHC^2) + I(Benzene^2) + I(Titania^2) + I(Tungsten_oxide_S2^2) + I(NO2^2) + I(Tungsten_oxide_S3^2) + I(Indium_oxide^2) + I(Temperature^2) + I(Relative_Humidity^2) + I(Absolute_Humidity^2) + #quadratic terms
                      I(Time^3) + I(CO^3) + I(Tin_oxide^3) + I(NMHC^3) + I(Benzene ^3) + I(Titania^3) + I(Tungsten_oxide_S2^3) + I(NO2^3) + I(Tungsten_oxide_S3^3) + I(Indium_oxide^3) + I(Temperature^3) + I(Relative_Humidity^3) + I(Absolute_Humidity^3), #cubic terms
                      data = air_quality)

empty_formula <- lm(NOx ~ 1, data = air_quality)

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
full_model_cubicX <- lm(NOx ~ (Time + CO + Tin_oxide + NMHC + Benzene + Titania + Tungsten_oxide_S2 + NO2 + Tungsten_oxide_S3 + Indium_oxide + Temperature + Relative_Humidity + Absolute_Humidity +
                        I(Time^2) + I(CO^2) + I(Tin_oxide^2) + I(NMHC^2) + I(Benzene^2) + I(Titania^2) + I(Tungsten_oxide^2) + I(NO2^2) + I(Tungsten_oxide_S3^2) + I(Indium_oxide^2) + I(Temperature^2) + I(Relative_Humidity^2) + I(Absolute_Humidity^2))^2 + #linear & quadratic terms summed squared
                        I(Time^3) + I(CO^3) + I(Tin_oxide^3) + I(NMHC^3) + I(Benzene^3) + I(Titania^3) + I(Tungsten_oxide_S2^3) + I(NO2^3) + I(Tungsten_oxide_S3^3) + I(Indium_oxide^3) + I(Temperature^3) + I(Relative_Humidity^3) + I(Absolute_Humidity^3), #cubic terms
                        data = air_quality)

# CubicX Forward Selection
forward_cubicX <- step(empty_formula, scope=list(upper = full_model_cubicX), direction = "forward", trace = FALSE)
summary(forward_cubicX )

# CubicX Backward Elimination
backward_cubicX <- step(full_model_cubicX, direction = "backward", trace = FALSE) 
summary(backward_cubicX )

# CubicX Stepwise Regression
stepwise_cubicX <- step(empty_formula, scope=list(upper = full_model_cubicX), direction = "both", trace = FALSE)
summary(stepwise_cubicX )







