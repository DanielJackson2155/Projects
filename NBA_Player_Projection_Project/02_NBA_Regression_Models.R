# NBA Player Projection Project -------------------------------------------

# Libraries Used ----------------------------------------------------------
library(glmnet)
library(caret)
library(pls)
library(tree)
library(ipred)
library(randomForest)
library(gbm)

# Quantitative Regression Analysis ----------------------------------------

# Linear Regression -------------------------------------------------------
# Let's fit linear model using all of the variables in the data set
ws_lm = lm(ws ~., ws_df)
summary(ws_lm)
# We see that all of the p-values of our predictors are less than 0.05 besides
# ts_pct. age is barely less than 0.05, so we will keep it.
# Let us remove ts_pct
ws_lm = lm(ws ~. - ts_pct, ws_df)
summary(ws_lm)
# All of the p-values are less than 0.05 in this model.
# Model explains 95% of the variability in the data. Which is good!
# Let's use this model to predict the ws values within the model
pred_ws_lm = predict(ws_lm, newdata = ws_df)
mean((pred_ws_lm - ws_df$ws)^2)
# Produced mean squared error rate of 0.30

# Let's add the transformations we did earlier to see if we can reduce that mean
# squared error rate
# Transformations that made distributions more normal on each predictor:
# age:      N/A
# g:        Square root
# mp:       Log
# per:      Log
# ts_pct:   Log
# ftr:      Square root
# trb_pct:  Log
# ast_pct:  Square root
# blk_pct:  Square root
# usg_pct:  Sauare root
# vorp:     N/A

# Response variable
# ws:       N/A  
transform_ws_lm = lm(ws ~ age + g + log(mp) + log(per) + log(ts_pct) + sqrt(ftr) +
             log(trb_pct) + sqrt(ast_pct) + sqrt(blk_pct) + sqrt(usg_pct) +
             vorp, ws_df)
summary(transform_ws_lm)
# All predictors, besides age, are statistically significant with p-values less 
# than 0.05.
# We did not remove ts_pct in this model.
# Let us remove age.
transform_ws_lm = lm(ws ~ g + log(mp) + log(per) + log(ts_pct) + sqrt(ftr) +
                       log(trb_pct) + sqrt(ast_pct) + sqrt(blk_pct) + sqrt(usg_pct) +
                       vorp, ws_df)
summary(transform_ws_lm)
# All predictors have p-values less than 0.05.
# R^2 value is 94.5% in this model. Slightly less than previous linear model.
pred_transform_ws_lm = predict(transform_ws_lm, newdata = ws_df)
mean((pred_transform_ws_lm - ws_df$ws)^2)
# Our mean squared error was 0.34. Which is higher than previous linear model.
# Let's create or training and test data.

# Training and Test Data --------------------------------------------------
dim(ws_df)
# 786 observations
# Since we have a relatively smaller number of observations, let's do a 
# 70:30 split on our data where 70% of our data will be our training data,
# and 30% of our data will be our test data
set.seed(1)
index = createDataPartition(ws_df$ws, p = 0.7, list = FALSE)
train_df = ws_df[index,]
test_df = ws_df[-index,]

dim(train_df)
# 552 observations in training set
dim(test_df)
# 234 observations in test set

# Linear Regression Continued ---------------------------------------------
# Now train each model that we created earlier using training data set. And then
# used trained model to predict test data
ws_lm = lm(ws ~. - ts_pct, train_df)
summary(ws_lm)
# R^2 of 94.81%
# We see that age is not statistically significant in training data as the p-value
# is greater than 0.05. Let us remove it.
ws_lm = lm(ws ~. - age - ts_pct, train_df)
summary(ws_lm)
# R^2 still 94.81%
pred_ws_lm = predict(ws_lm, newdata = test_df)
mean((pred_ws_lm - test_df$ws)^2)
# Produced mean squared error of 0.29

# Try on transformed model
transform_ws_lm = lm(ws ~ age + g + log(mp) + log(per) + log(ts_pct) + sqrt(ftr) +
                       log(trb_pct) + sqrt(ast_pct) + sqrt(blk_pct) + sqrt(usg_pct) +
                       vorp, train_df)
summary(transform_ws_lm)
# R^2 of 94.3%
# We see that age and log(ts_pct) are not significant. Let us remove both.
transform_ws_lm = lm(ws ~ g + log(mp) + log(per) + sqrt(ftr) +log(trb_pct) + 
                       sqrt(ast_pct) + sqrt(blk_pct) + sqrt(usg_pct) +
                       vorp, train_df)
summary(transform_ws_lm)
# R^2 still 94.3%
# All predictors significant.
pred_transform_ws_lm = predict(transform_ws_lm, newdata = test_df)
mean((pred_transform_ws_lm - test_df$ws)^2)
# Produced mean squared error of 0.36
# Our model with no transformations produced a lower mean squared error rate for our
# predictions.
# Since R^2 values are almost identical, let's go with the model without transformations.


# Ridge Regression --------------------------------------------------------
# Try ridge regression model
train_matrix = model.matrix(ws ~., train_df)
test_matrix = model.matrix(ws ~., test_df)
# Now we need to select lambda using cross-validation
cv_out = cv.glmnet(train_matrix, train_df$ws, alpha = 0)
best_lam = cv_out$lambda.min
best_lam
# Lambda chosen by cross-validation is 0.2152
# Now we fit ridge regression model and make predictions:
ws_ridge = glmnet(train_matrix, train_df$ws, alpha = 0)
pred_ws_ridge = predict(ws_ridge, s = best_lam, newx = test_matrix)
# Find mean squared error:
mean((pred_ws_ridge - test_df$ws)^2)
# Produce MSE of 0.338
# This is not lower than the 0.29 that our linear model produced.


# Lasso Regression --------------------------------------------------------
# Try lasso regression model
# We need to select lambda using cross-validation
cv_out = cv.glmnet(train_matrix, train_df$ws, alpha = 1)
best_lam = cv_out$lambda.min
best_lam
# Lambda chosen by cross-validation is 0.0027
# Now we fit ridge regression model and make predictions:
ws_lasso = glmnet(train_matrix, train_df$ws, alpha = 1)
pred_ws_lasso = predict(ws_lasso, s = best_lam, newx = test_matrix)
# Find mean squared error:
mean((pred_ws_lasso - test_df$ws)^2)
# Produced MSE of 0.28. This is better than our linear model.
# We will keep this model in mind. However, the linear model is more
# interpretable.


# Principal Component Regression ------------------------------------------
# Try principal component regression model
ws_pcr = pcr(ws ~., data = train_df, 
                 scale = TRUE, validation = "CV")
summary(ws_pcr)
validationplot(ws_pcr, val.type = "MSEP")
# We see that M = 10 produces lowest mean squared error of prediction on training data
# The PCR model when M = 11, explains 99% of the variance of the training data.
pred_ws_pcr = predict(ws_pcr, test_df, ncomp = 11)
mean((pred_ws_pcr - test_df$ws)^2)
# Using all 11 predictors, we see that we get a MSE of 0.28.
# We want to try and reduce our dimensions of our model using principal component
# regression. 
# Our linear model has 9 predictors. We see that M = 7 produces a relatively low
# mean squared error on the training data. let's try M = 7 in our prediction model.
pred_ws_pcr = predict(ws_pcr, test_df, ncomp = 7)
mean((pred_ws_pcr - test_df$ws)^2)
# This produced a test MSE of 0.61. Much higher than what we have produced so far.


# Partial Least Squares ---------------------------------------------------
# Try partial least squares model
ws_pls = plsr(ws ~., data = train_df, 
                   scale = TRUE, validation = "CV")
summary(ws_pls)
validationplot(ws_pls, val.type = "MSEP")
# We see that M = 11 produces lowest mean squared error of prediction on training data.
# However, we are trying to reduce variables in the model, so let's look at when
# M = 6 as that has almost same MSEP as when M = 11.
# The partial least squares model when M = 6 explains about 94.5% of the variance of 
# training data.
pred_ws_pls = predict(ws_pls, test_df, ncomp = 6)
mean((pred_ws_pls - test_df$ws)^2)
# Produced a test MSE of 0.31.
# What predictors are being used in this model?
print(loadings(ws_pls))
# 7 predictors being used:
# age, g, ts_pct, ftr, trb_pct, ast_pct, usg_pct
# Only two less predictors than linear model.

# Find MSE when M = 5
pred_ws_pls = predict(ws_pls, test_df, ncomp = 5)
mean((pred_ws_pls - test_df$ws)^2)
# Produced MSE of 0.36. Not lower than what we have already produced.

# Regression Trees --------------------------------------------------------
# Try regression tree
ws_tree = tree(ws ~ ., train_df)
summary(ws_tree)
# 10 terminal nodes
# 5 variables used in tree model: vorp, ts_pct, mp, trb_pct, g
plot(ws_tree)
text(ws_tree, pretty = 0)

# Use tree model to predict test data
pred_ws_tree = predict(ws_tree, newdata = test_df)
mean((pred_ws_tree - test_df$ws)^2)
# Produced a MSE of over 1. This may mean that our tree overfitted our training data.

# Let us prune our tree using cross-validation to see if we can reduce our
# MSE.
cv_tree = cv.tree(ws_tree)
plot(cv_tree$size, cv_tree$dev, type = "b")
# We see that tree size 10 minimizes cross-validation error.
# However, we already used 10 terminal nodes in our first tree model.
# Based on the plot, we see that 6 terminal nodes relatively minimizes 
# cross-validation error. 
# Let's prune our tree to 6 terminal nodes and see if it has any effect
# on our MSE
ws_prune = prune.tree(ws_tree, best = 6)
summary(ws_prune)
# 6 terminal nodes
# 3 variables being used: vorp, ts_pct, mp
plot(ws_prune)
text(ws_prune, pretty = 0)
pred_ws_prune = predict(ws_prune, newdata = test_df)
mean((pred_ws_prune - test_df$ws)^2)
# Produced MSE over 1 again. We will not be using regression trees. 


# Bagging Model -----------------------------------------------------------
# Try bagging model
ws_bag = bagging(ws ~., data = train_df)
ws_bag
pred_ws_bag = predict(ws_bag, newdata = test_df)
mean((pred_ws_bag - test_df$ws)^2)
# Similar to regression tree, our bagging model did not perform well as it 
# produced a test MSE over one. We will not be using this model.


# Random Forest Model -----------------------------------------------------
# Try random forest model with 100 trees and using all predictors
set.seed(1)
ws_rf = randomForest(ws ~., ntree = 100,
                         mtry = 12, importance = TRUE, data = train_df)
ws_rf
# Explains 89.01% of the variance
varImpPlot(ws_rf)
# vorp, ts_pct and mp are three most important predictors in this random forest
# model.
pred_ws_rf = predict(ws_rf, newdata = test_df)
mean((pred_ws_rf - test_df$ws)^2)
# Produced 0.51 MSE. Not a better MSE than our previous models.


# Boosted Model -----------------------------------------------------------
# Our boosted models that we will fit will use lambda values of 0.001, 0.01, 
# 0.1, and 0.2 and 1000 trees.

# lambda = 0.001:
ws_boost = gbm(ws ~., data = train_df, distribution = "gaussian",
                   n.trees = 1000)
pred_ws_boost = predict(ws_boost, newdata = test_df, 
                     n.trees = 1000)
mean((pred_ws_boost - test_df$ws)^2)
# Test MSE of approx 0.45

# lambda = 0.01:
ws_boost = gbm(ws ~., data = train_df, distribution = "gaussian",
               n.trees = 1000, shrinkage = 0.01)
pred_ws_boost = predict(ws_boost, newdata = test_df, 
                        n.trees = 1000)
mean((pred_ws_boost - test_df$ws)^2)
# Test MSE of approx 0.65

# lambda = 0.1
ws_boost = gbm(ws ~., data = train_df, distribution = "gaussian",
               n.trees = 1000, shrinkage = 0.1)
pred_ws_boost = predict(ws_boost, newdata = test_df, 
                        n.trees = 1000)
mean((pred_ws_boost - test_df$ws)^2)
# Test MSE of approx 0.48

# lambda = 0.2
ws_boost = gbm(ws ~., data = train_df, distribution = "gaussian",
               n.trees = 1000, shrinkage = 0.2)
pred_ws_boost = predict(ws_boost, newdata = test_df, 
                        n.trees = 1000)
mean((pred_ws_boost - test_df$ws)^2)
# Test MSE of approx 0.49

# None of the boosted models performed better than our linear model.


# Regression Model Conclusion ---------------------------------------------
# We fit 9 models in our regression analysis. The model that produced the lowest
# test MSE of 0.28 was our lasso regression model. Our next closest model was our linear
# model, which produced a test MSE of 0.29. Since there is not a major statistical
# significant difference between thee two MSEs, I will choose the linear model
# as it is much more interpretable and easier to convey to a non-technical stake-
# holder. 

# Model in different syntax to see all variables used:
ws_lm = lm(ws ~ g + mp + per + ftr + trb_pct + ast_pct + blk_pct + 
             usg_pct + vorp, train_df)
summary(ws_lm)


# Our chosen model uses g, mp, per, ftr, vorp, trb_pct, ast_pct, blk_pct, and 
# usg_pct to predict ws.

# Linear model written out:
# Win Shares Estimate = -1.2347 + 0.0173(*Games*) + 0.0012(*Minutes Played*) +
# 0.3251(*Player Efficiency Rating*) + 1.9526(*Free Throw Attempt Rate*)  +
# 0.9932(*Wins Over Replacement*) - 0.0480(*Total Rebound Percentage*) - 
# 0.0382(*Assist Percentage*) - 0.1109(*Block Percentage*) - 0.1670(*usg_pct*)