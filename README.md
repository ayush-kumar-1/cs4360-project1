# Project Problem 10 

​	Stepwise Regression. An improvement over Forward Selection and Backward Elimination is Stepwise
Regression. Start with no variables in the model and add one variable that improves the selection
criterion the most. Add the second best variable for step two. After the second step determine
whether it is better to add or remove a variable. Continue in this fashion until no improvement in the
selection criterion is found. For Forward Selection and Backward Elimination it may instructive to
continue all the way to the end (all variables for forward/no variables for backward).

​	Stepwise regression may lead to coincidental relationships being included in the model, particularly
if a t-test is the basis of inclusion or a penalty-free QoF measure such as $R^2$ is used. Typically, this
approach is used when there a penalty for having extra variables/parameters, e.g., $R^2$, adjusted-$R^2$ ($R^2_a$), cross-validation $R^2_cv$ or Akaike Information Criterion (AIC). Alternatives to Stepwise Regression
include Ridge Regression and Lasso Regression.

​	Write ScalaTion code for a stepRegression method and stepRegressionAll method and redo
exercise 6 using all three: Forward Selection, Backward Elimination, and Stepwise Regression with all
four criteria: $R^2$, $R^2_a$, $R^2_cv$, and AIC. Plot the curve for each criterion, determine the best number of
variables and what these variables are. Compare the four criteria.

​	As part of a larger project compare this form of feature selection with that provided by Ridge Regression
and Lasso Regression. See the next two sections.

​	Now add features including quadratric terms and dummy variables to the model using QuadRegression,
QuadXRegression, CubicRegression, CubicXRegression, and ANCOVA. See the subsequent sections.

​	In addition to the AutoMPG dataset, use the Concrete dataset and 4 more datasets from UCI Machine
Learning Repository. The UCI datasets should have more instances (m) and variables (n) than the
first two datasets. The testing should also be done in R or Python.

## Problem 6

​	For the AutoMPG dataset, repeatedly call the backwardElim method to remove the predictor variable
that contributes the least to the model. Show how the various quality of fit (QoF) measures change as
variables are eliminated. Do the same for the forwardSel method. Using $R^2$, select the best models
from the forward and backward approaches. Are they the same?