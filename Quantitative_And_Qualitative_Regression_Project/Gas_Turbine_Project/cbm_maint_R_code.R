# Quantitative Linear Regression Final Project R Code -------------------------

# Data link: https://archive.ics.uci.edu/dataset/316/condition+based+maintenance+of+naval+propulsion+plants

# Citation:
# Coraddu,Andrea, Oneto,Luca, Ghio,Alessandro, Savio,Stefano, Anguita,Davide, and Figari,Massimo. (2014). 
# Condition Based Maintenance of Naval Propulsion Plants. UCI Machine Learning Repository.
# https://doi.org/10.24432/C5K31K.

# Use predictors to predict Gas Turbine (TB) decay state coefficient.

# Read in and clean up data -----------------------------------------------
cbm_df = read.table("Final_Project/uci_bm_dataset/data.txt", header = FALSE, colClasses = "numeric")
colnames(cbm_df) = tolower(colnames(cbm_df))
dim(cbm_df)
# 11934 observations
# 18 variables

# Variables names:
# v1 - Lever position (lp) [ ]
# v2 - Ship speed (v) [knots]
# v3 - Gas Turbine shaft torque (GTT) [kN m]
# v4 - Gas Turbine rate of revolutions (GTn) [rpm]
# v5 - Gas Generator rate of revolutions (GGn) [rpm]
# v6 - Starboard Propeller Torque (Ts) [kN]
# v7 - Port Propeller Torque (Tp) [kN]
# v8 - HP Turbine exit temperature (T48) [C]
# v9 - GT Compressor inlet air temperature (T1) [C]
# 10 - GT Compressor outlet air temperature (T2) [C]
# 11 - HP Turbine exit pressure (P48) [bar]
# 12 - GT Compressor inlet air pressure (P1) [bar]
# 13 - GT Compressor outlet air pressure (P2) [bar]
# 14 - Gas Turbine exhaust gas pressure (Pexh) [bar]
# 15 - Turbine Injecton Control (TIC) [%]
# 16 - Fuel flow (mf) [kg/s]
# 17 - GT Compressor decay state coefficient.
# 18 - GT Turbine decay state coefficient. 


names(cbm_df)
# V18 is our response variable
# V17 is also a response variable so we want to remove that column and just focus
## on V18.
cbm_df = cbm_df[, -which(names(cbm_df) == "v17")]
names(cbm_df)
dim(cbm_df)
# 11934 observations
# 17 variables. 16 predictors. 1 response.

table(cbm_df$v17)
# 459 occurrences of each response output in data set. 26 different responses
# for V18.

# Response Variable Summary -----------------------------------------------
# Using 16 predictors to predict Gas Turbine (TB) decay state coefficient extracted
# from experiments that have been carried out by means of a numerical simulator of 
# a naval vessel (Frigate) characterized by a Gas Turbine (GT) propulsion plant.
# Use training data and fit model that accurately predicts test data.

# Linear Regression -------------------------------------------------------
cbm_lm = lm(v18 ~., cbm_df)
summary(cbm_lm)
# v7, v9 and v12 are showing as NA coefficients. 
# This indicates muilticollinearity or perfect collinearity, meaning that one or 
## more predictors are linearly dependent on others. 
# Check for columns with near-zero or zero variance
near_zero_var = colSums(var(cbm_df)) < 1e-10
# Display columns with near-zero or zero variance
print(names(cbm_df)[near_zero_var])
# v9 and v12 were returned. Yet v7 also showing NA
cbm_df$v9
# All observations in v9 are the same at 288.
cbm_df$v12
# Same with v12 where all the same at 0.998.

# Let's remove v9 and v12.
cbm_df = cbm_df[, -which(names(cbm_df) == "v9")]
cbm_df = cbm_df[, -which(names(cbm_df) == "v12")]
# For v7, lets look at correlation matrix
cor(cbm_df)
install.packages("knitr")
install.packages("corrplot") 
library(knitr)
library(corrplot)
correlation_matrix = cor(cbm_df, use = "pairwise.complete.obs")
# v6 and v7 are perfectly correlated. 
are_same = all(cbm_df$v6 == cbm_df$v7)
if (are_same) {
  print("v6 and v7 are exactly the same.")
} else {
  print("v6 and v7 are different.")
}
# v6 = Starboard Propeller Torque (Ts) [kN] and v7 = Port Propeller Torque (Tp) [kN]
## These are the same values, so let's remove v7.
cbm_df = cbm_df[, -which(names(cbm_df) == "v7")]

# Training and Test Data --------------------------------------------------
# Here is training and test data that we will use. There is 11934 observations.
# Split data in half for training and test data.
11934 / 2
set.seed(1)
train = sample(1:nrow(cbm_df), 5967)
cbm_train = cbm_df[train,]
cbm_test = cbm_df[-train,]

# Linear Regression Cont. -------------------------------------------------

# Let's try best subset selection on entire data:
library(leaps)
best_cbm = regsubsets(v18 ~., cbm_df, nvmax = 13)
best_cbm_summary = summary(best_cbm)
names(best_cbm_summary)
# This returns R^2, RSS, adjusted R^2, Cp and BIC.
best_cbm_summary$rsq
# Plot RSS, adjusted R^2, Cp and BIC
par(mfrow = c(2, 2))
plot(best_cbm_summary$rss, xlab = "Number of Variables",
     ylab = "RSS", type = "l")
plot(best_cbm_summary$adjr2, xlab = "Number of Variables",
     ylab = "Adjusted RSq", type = "l")
# Plot red dot to indicate model with largest adjusted R^2
which.max(best_cbm_summary$adjr2)
# Returned 12
points(12, best_cbm_summary$adjr2[12], col = "red", cex = 2, pch = 20)
# Plot Cp and BIC stats and indicate models with smallest stat
plot(best_cbm_summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
which.min(best_cbm_summary$cp)
# Returns 12
points(12, best_cbm_summary$cp[12], col = "red", cex = 2,
       pch = 20)
which.min(best_cbm_summary$bic)
# Returns 12
plot(best_cbm_summary$bic, xlab = "Number of Variables",
     ylab = "BIC", type = "l")
points(12, best_cbm_summary$bic[12], col = "red", cex = 2,
       pch = 20)
# Best subset selection says to have 12 variables.

# Use forward selection:
fwd_cbm = regsubsets(v18 ~., cbm_df, nvmax = 13, method = "forward")
summary(fwd_cbm)
# Best 12 predictor model removes v1

# Use backward selection:
bwd_cbm = regsubsets(v18 ~., cbm_df, nvmax = 13, method = "backward")
summary(bwd_cbm)
# Best 12 predictor model only removes v1
# Since v1 was not included in both the forward and backward selection models, 
# I will keep that variable in mind as the first one removed in the models moving 
# forward. My approach for the multiple linear regression will combine both forward 
# and backward selection by using training data on test data while examining p-values 
# and absolute values of t to add and remove variables from models. My goal is to 
# try and make the model as simple as possible by maximizing the R^2 value and 
# minimizing the test mean squared error.

# Only 14 variables.
cbm_lm = lm(v18 ~., cbm_df)
summary(cbm_lm)
# All p-values are less than 0.05, so we can reject the null hypothesis and 
## say all values are statistically signicant.
# R-squared value is 91.1%
# v1 barely passes hyothesis test.
# Try model without v1
cbm_lm = lm(v18 ~. -v1, cbm_df)
summary(cbm_lm)
# All p-values are less than 0.05 once again.
# R-squared value still 91.1%
# Looking at coefficients, we see some ones with negative coefficients:
## v6, v8, v13, and v15
# Try running model with just positive coefficients
cbm_lm = lm(v18 ~. - v1 - v6 - v8 - v13 - v15, cbm_df)
summary(cbm_lm)
# This makes model way worse.
# Go back to model with all but v1 variable
cbm_lm = lm(v18 ~. -v1, cbm_df)
summary(cbm_lm)

# Let's try letting our train data predict our test model 
train_cbm_lm = lm(v18 ~. - v1, cbm_train)
summary(train_cbm_lm)
# All predictors p-values are less than 0.05
pred_cbm_lm = predict(train_cbm_lm, newdata = cbm_test)
mean((pred_cbm_lm - cbm_test$v18)^2)
# Means squared error is almost close to 0.
# This means that this model did a really good job at predicting response variable
## in test data.

# Can we lose some predictors on training data to make data more interpretable?
# Let's then remove each predictor, one at a time, if there p-values are less than
## 0.05 or if all p-values are less than, 0.05, remove predictor with smallest
### absolute t-value.

# Remove v1 from like we did in previous model.
train_cbm_lm = lm(v18 ~. - v1, cbm_train)
summary(train_cbm_lm)
# All predictors p-values are less than 0.05
pred_cbm_lm = predict(train_cbm_lm, newdata = cbm_test)
mean((pred_cbm_lm - cbm_test$v18)^2)
# All p-values pass hypothesis test.
# Test MSE is close to zero.
# R^2 is 91.25%
# Let's remove predictor with lowest absolute t-value which is v14

# Remove v14
train_cbm_lm = lm(v18 ~. - v1 - v14, cbm_train)
summary(train_cbm_lm)
# All predictors p-values are less than 0.05
pred_cbm_lm = predict(train_cbm_lm, newdata = cbm_test)
mean((pred_cbm_lm - cbm_test$v18)^2)
# All p-values besides pass hypothesis test.
# Test MSE is still close to 0.
# R^2 is 91.1%
# Let's remove predictor with lowest absolute t-value which is v10.

# Remove v10
train_cbm_lm = lm(v18 ~. - v1 - v14 - v10, cbm_train)
summary(train_cbm_lm)
# All predictors p-values are less than 0.05
pred_cbm_lm = predict(train_cbm_lm, newdata = cbm_test)
mean((pred_cbm_lm - cbm_test$v18)^2)
# All p-values less than 0.05.
# MSE still close to 0.
# R-squared is 90.81%%
# Let's remove predictor with lowest absolute t-value which is v11

# Remove v11
train_cbm_lm = lm(v18 ~. - v1 - v14 - v10 - v11, cbm_train)
summary(train_cbm_lm)
# All predictors p-values are less than 0.05
pred_cbm_lm = predict(train_cbm_lm, newdata = cbm_test)
mean((pred_cbm_lm - cbm_test$v18)^2)
# All p-values less than 0.05.
# MSE still close to 0.
# R-squared is 89.93%%
# Let's remove predictor with lowest absolute t-value which is v15

# Remove v15
train_cbm_lm = lm(v18 ~. - v1 - v14 - v10 - v11 - v15, cbm_train)
summary(train_cbm_lm)
# All predictors p-values are less than 0.05
pred_cbm_lm = predict(train_cbm_lm, newdata = cbm_test)
mean((pred_cbm_lm - cbm_test$v18)^2)
# All p-values less than 0.05.
# MSE still close to 0.
# R-squared is 88.43%%
# Let's remove predictor with lowest absolute t-value which is v16

# Remove v16
train_cbm_lm = lm(v18 ~. - v1 - v14 - v10 - v11 - v15 - v16, cbm_train)
summary(train_cbm_lm)
# All predictors p-values are less than 0.05
pred_cbm_lm = predict(train_cbm_lm, newdata = cbm_test)
mean((pred_cbm_lm - cbm_test$v18)^2)
# All p-values less than 0.05.
# MSE still close to 0.
# R-squared is 76.48%%
# R-squared value dropped dramatically. 
# I want to keep that value above 90%.

# So let's go back to most recent model that was above 90% for R-squared value:
train_cbm_lm = lm(v18 ~. - v1 - v14 - v10, cbm_train)
summary(train_cbm_lm)
# All predictors p-values are less than 0.05
pred_cbm_lm = predict(train_cbm_lm, newdata = cbm_test)
mean((pred_cbm_lm - cbm_test$v18)^2)
# All p-values less than 0.05.
# MSE still close to 0.
# R-squared is 90.81%%
# 10 predictors in this model. 2 less predictors than previous model that just 
## dropped v1 predictor.


# Create a scatter plot comparing actual vs. predicted values
par(mfrow = c(1, 1))
plot_df = data.frame(Actual = cbm_test$v18, Predicted = pred_cbm_lm)

ggplot(plot_df, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # Adds a line of perfect prediction
  labs(x = "Actual Gas Turbine Decay State Coefficient", 
       y = "Predicted Gast Turbine Decay State Coefficient", 
       title = "Actual vs. Predicted") +
  theme_minimal()

# Plot this model on entire data set:
library(ggplot2)
cbm_lm = lm(v18 ~. - v1 - v14 - v10, cbm_df)
par(mfrow = c(2, 2))
plot(cbm_lm)

# Try some transformations of predictors on model we have
train_cbm_lm = lm(v18 ~. - v1 - v14 - v10, cbm_train)
summary(train_cbm_lm)
# Same as this model:
train_cbm_lm = lm(v18 ~ v2 + v3 + v4 + v5 + v6 + v8 + v11 +
                    v13 + v15 + v16, cbm_train)
summary(train_cbm_lm)

# Try sqrt():
train_cbm_lm = lm(v18 ~ sqrt(v2) + sqrt(v3) + sqrt(v4) + sqrt(v5) + sqrt(v6) +
                  sqrt(v8) + sqrt(v11) + sqrt(v13) + v15 + sqrt(v16), cbm_train)
summary(train_cbm_lm)
pred_cbm_lm = predict(train_cbm_lm, newdata = cbm_test)
mean((pred_cbm_lm - cbm_test$v18)^2)
# All p-values less than 0.05 besides v15.
# Test MSE still close to 0.
# R-squared is 93.21%%
# Try model without square ro0ting v15.
# p-value still greater than 0.05.
# Remove v15 predictor
train_cbm_lm = lm(v18 ~ sqrt(v2) + sqrt(v3) + sqrt(v4) + sqrt(v5) + sqrt(v6) +
                    sqrt(v8) + sqrt(v11) + sqrt(v13)  + sqrt(v16), cbm_train)
summary(train_cbm_lm)
pred_cbm_lm = predict(train_cbm_lm, newdata = cbm_test)
mean((pred_cbm_lm - cbm_test$v18)^2)
# All p-values less than 0.05.
# Test MSE still close to 0.
# R-squared is 93.21%%

# Try squared:
train_cbm_lm = lm(v18 ~ I(v2)^2 + I(v3)^2 + I(v4)^2 + I(v5)^2 + I(v6)^2 +
                    I(v8)^2 + I(v11)^2 + I(v13)^2 + I(v15)^2 + I(v16)^2, cbm_train)
summary(train_cbm_lm)
pred_cbm_lm = predict(train_cbm_lm, newdata = cbm_test)
mean((pred_cbm_lm - cbm_test$v18)^2)
# All p-values less than 0.05.
# Test MSE still close to 0.
# R-squared is 90.81%%

# Try cubed:
train_cbm_lm = lm(v18 ~ I(v2)^3 + I(v3)^3 + I(v4)^3 + I(v5)^3 + I(v6)^3 +
                    I(v8)^3 + I(v11)^3 + I(v13)^3 + I(v15)^3 + I(v16)^3, cbm_train)
summary(train_cbm_lm)
pred_cbm_lm = predict(train_cbm_lm, newdata = cbm_test)
mean((pred_cbm_lm - cbm_test$v18)^2)
# All p-values less than 0.05.
# Test MSE still close to 0.
# R-squared is 90.81%%

# Sqrt() transformation is highest R-squared value so far with test MSE being
## 0.

# Try interactions of sqrt() model
# Look at most correlated variables using these predictors:
cor(cbm_df)
best_train_cbm_lm = lm(v18 ~ sqrt(v2)*sqrt(v5) + sqrt(v4)*sqrt(v11) + sqrt(v3)*sqrt(v6) +
                    sqrt(v8)*sqrt(v13) + sqrt(v16), cbm_train)
summary(best_train_cbm_lm)
best_pred_cbm_lm = predict(best_train_cbm_lm, newdata = cbm_test)
mean((best_pred_cbm_lm - cbm_test$v18)^2)
# This raised R-squared to 94.82%
# Tried other variations. This was the best R-squared value that I got.
# Test MSE = 0.00000295

# Plot this model:
par(mfrow = c(1, 1))
plot_df_1 = data.frame(Actual = cbm_test$v18, Predicted = pred_cbm_lm)

ggplot(plot_df, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # Adds a line of perfect prediction
  labs(x = "Actual Gas Turbine Decay State Coefficient", 
       y = "Predicted Gast Turbine Decay State Coefficient", 
       title = "Actual vs. Predicted") +
  theme_minimal()

# Ridge Regression Model --------------------------------------------------
library(glmnet)
# Perform ridge regression on data
set.seed(1)
train_matrix = model.matrix(v18~., cbm_train)
test_matrix = model.matrix(v18~., cbm_test)
# Now we need to select lambda using cross-validation
cv_out = cv.glmnet(train_matrix, cbm_train$v18, alpha = 0)
best_lam = cv_out$lambda.min
best_lam
# Lambda chosen by cross-validation is 0.00004
# Now we fit ridge regression model and make predictions:
cbm_ridge = glmnet(train_matrix, cbm_train$v18, alpha = 0)
pred_cbm_ridge = predict(cbm_ridge, s = best_lam, newx = test_matrix)
# Find test error:
mean((pred_cbm_ridge - cbm_test$v18)^2)
# Test Error is very close to 0
# Test error is 0.000048


# Lasso Regression Model --------------------------------------------------
# Perform lasso regression
set.seed(1)
# Select lambda using cross-validation
cv_lam = cv.glmnet(train_matrix, cbm_train$v18, alpha = 1)
best_lam = cv_lam$lambda.min
best_lam
# Lambda chosen by cross-validation is 0.00000505
# Now we fit lasso regression model and make predictions:
cbm_lasso = glmnet(train_matrix, cbm_train$v18, alpha = 1)
pred_cbm_lasso = predict(cbm_lasso, s = best_lam, newx = test_matrix)
# Find test error:
mean((pred_cbm_lasso - cbm_test$v18)^2)
# Test error is very close to 0
# Test error is 0.0000095.
# Ridge regression has lower test MSE.

# Principal Components Regression -----------------------------------------
# Perform principal components regression and try to reduce dimensions of data.
library(pls)
set.seed(1)
cbm_pcr = pcr(v18~., data = cbm_train, 
              scale = TRUE, validation = "CV")
summary(cbm_pcr)
validationplot(cbm_pcr, val.type = "MSEP")
# Where M = 11, 12, and 13 have lowest MSEP values. But we want to try and reduce
# dimensions.
# Let's try where M = 10.
pred_pcr = predict(cbm_pcr, cbm_test, ncomp = 10)
mean((pred_pcr - cbm_test$v18)^2)
# Test MSE = 0.0000223

# Try where M = 9
pred_pcr = predict(cbm_pcr, cbm_test, ncomp = 9)
mean((pred_pcr - cbm_test$v18)^2)
# Test MSE = 0.0000234

# And try where M = 11
pred_pcr = predict(cbm_pcr, cbm_test, ncomp = 11)
mean((pred_pcr - cbm_test$v18)^2)
# Test MSE = 0.00000605
# Even though we are trying to reduce dimensions. M = 11 provides the lowest
## test error.

# Partial Least Squares ---------------------------------------------------
set.seed(1)
cbm_pls = plsr(v18~., data = cbm_train, 
               scale = TRUE, validation = "CV")
summary(cbm_pls)
validationplot(cbm_pls, val.type = "MSEP")
# Lowest MSEP occurs when M = 12 but we are trying to reduce dimension
# Try with M = 8
pred_pls = predict(cbm_pls, cbm_test, ncomp = 8)
mean((pred_pls - cbm_test$v18)^2)
# Test MSE = 0.0000144

# Try M = 9
pred_pls = predict(cbm_pls, cbm_test, ncomp = 9)
mean((pred_pls - cbm_test$v18)^2)
# Test MSE = 0.0000113

# Try M = 10
pred_pls = predict(cbm_pls, cbm_test, ncomp = 10)
mean((pred_pls - cbm_test$v18)^2)
# Test MSE = 0.00000578
# M = 10 provides lowest test MSE of Partial Least Squares
# Lower than PCR with M = 11.

# Regression Trees --------------------------------------------------------
# Fit a regression tree on training data
library(tree)
cbm_tree = tree(v18 ~ ., cbm_train)
summary(cbm_tree)
plot(cbm_tree)
text(cbm_tree, pretty = 0)
# Number of terminal nodes: 21
# Only variables used in tree: v3, v13, v5, v4, v14, v10 and v11
# Find test MSE
yhat = predict(cbm_tree, newdata = cbm_test)
mean((yhat - cbm_test[, "v18"])^2)
# Test MSE = 0.0000191
cbm_tree

# Pruning Regression Trees
# Use cv.tree() function to prune tree to help make it more interpretable.
cv_tree_cbm = cv.tree(cbm_tree)
plot(cv_tree_cbm$size, cv_tree_cbm$dev, type = "b")
# Try with 15 terminal nodes instead of 21
cbm_prune = prune.tree(cbm_tree, best = 15)
plot(cbm_prune)
text(cbm_prune, pretty = 0)
summary(cbm_prune)
# Find test MSE on pruned tree:
yhat = predict(cbm_prune, newdata = cbm_test)
mean((yhat - cbm_test[, "v18"])^2)
# Test MSE = 0.0000231
# Close to test MSE of original tree and more interpretable.
# Still not very easy to interpret.

# Try with 11 terminal nodes
cbm_prune = prune.tree(cbm_tree, best = 11)
plot(cbm_prune)
text(cbm_prune, pretty = 0)
summary(cbm_prune)
# Find test MSE on pruned tree:
yhat = predict(cbm_prune, newdata = cbm_test)
mean((yhat - cbm_test[, "v18"])^2)
# Test MSE = 0.0000284
# Similar test MSE. More interpretable tree.

# Try with 9 terminal nodes
cbm_prune = prune.tree(cbm_tree, best = 9)
plot(cbm_prune)
text(cbm_prune, pretty = 0)
summary(cbm_prune)
# Find test MSE on pruned tree:
yhat = predict(cbm_prune, newdata = cbm_test)
mean((yhat - cbm_test[, "v18"])^2)
# Test MSE = 0.0000284
# Similar test MSE to tree with 11 terminal nodes

# Try with 8 terminal nodes
cbm_prune = prune.tree(cbm_tree, best = 8)
plot(cbm_prune)
text(cbm_prune, pretty = 0)
summary(cbm_prune)
# Find test MSE on pruned tree:
yhat = predict(cbm_prune, newdata = cbm_test)
mean((yhat - cbm_test[, "v18"])^2)
# Test MSE = 0.0000363
# Slightly higher test MSE to tree with 9 terminal nodes. More easy to read.

# Pruned tree preferred as it has small test MSE and it is easier to understand compared
## to tree with 21 terminal nodes.


# Bagging and Random Forests ---------------------------------------------------
# Perform bagging model and Random Forest. Find test MSE
library(randomForest)
set.seed(1)
cbm_bag = randomForest(v18 ~., data = cbm_train, mtry = 13, importance = TRUE)
cbm_bag
yhat_bag = predict(cbm_bag, newdata = cbm_test)
mean((yhat_bag - cbm_test$v18)^2)
# Using 500 trees and trying all 13 predictors at each split, 98.45%
## of variance explained. 
# Test MSE = 0.000000858
# Lowest test MSE so far.

# Try with number of trees as 25 to simplify model
set.seed(1)
cbm_rf = randomForest(v18 ~., data = cbm_train, ntree = 25,
                       mtry = 13, importance = TRUE)
cbm_rf
yhat_rf = predict(cbm_rf, newdata = cbm_test)
mean((yhat_rf - cbm_test$v18)^2)
# Using 25 trees and trying all 13 predictors at each split, 97.96%
## of variance explained. 
# Test MSE = 0.000000969
# Still very small MSE.

# Try with 10 variables instead of 13 and 25 trees
set.seed(1)
cbm_rf = randomForest(v18 ~., data = cbm_train, ntree = 25,
                       mtry = 10, importance = TRUE)
cbm_rf
yhat_rf = predict(cbm_rf, newdata = cbm_test)
mean((yhat_rf - cbm_test$v18)^2)
# Using 25 trees and trying 10 predictors at each split, 97.86%
## of variance explained. 
# Test MSE = 0.00000103
# Still very small MSE and variance barely dropped.

# Try with 10 variables instead of 13 and 10 trees
set.seed(1)
cbm_bag = randomForest(v18 ~., data = cbm_train, ntree = 10,
                       mtry = 10, importance = TRUE)
cbm_rf
yhat_rf = predict(cbm_rf, newdata = cbm_test)
mean((yhat_rf - cbm_test$v18)^2)
# Using 10 trees and trying 10 predictors at each split, 96.97%
## of variance explained. 
# Test MSE = 0.00000112
# Still very small MSE and variance barely dropped.

# Try with 8 variables and 5 trees
set.seed(1)
cbm_rf = randomForest(v18 ~., data = cbm_train, ntree = 5,
                       mtry = 8, importance = TRUE)
cbm_rf
yhat_rf = predict(cbm_rf, newdata = cbm_test)
mean((yhat_rf - cbm_test$v18)^2)
# Using 5 trees and trying 8 predictors at each split, 95.53%
## of variance explained. 
# Test MSE = 0.00000148
# Still very small MSE and variance barely dropped.
# As you bring M down, and cut down on tree sizes, Test MSE barely affected
## and variance explained by model does not change drastically.


# Boosting ----------------------------------------------------------------
# Use gbm() library
library(gbm)
# lambda = 0.001:
set.seed(1)
cbm_boost = gbm(v18 ~., data = cbm_train, distribution = "gaussian",
                      n.trees = 1000)
yhat_boost = predict(cbm_boost, newdata = cbm_test, 
                       n.trees = 1000)
mean((yhat_boost - cbm_test$v18)^2)
# Test MSE = 0.0000179

# lamda = 0.01
set.seed(1)
cbm_boost = gbm(v18 ~., data = cbm_train, distribution = "gaussian",
                n.trees = 1000, shrinkage = 0.01)
yhat_boost = predict(cbm_boost, newdata = cbm_test, 
                     n.trees = 1000)
mean((yhat_boost - cbm_test$v18)^2)
# Test MSE = 0.0000448
# Lambda = 0.001 produced smaller test MSE

# lambda = 0.1
set.seed(1)
cbm_boost = gbm(v18 ~., data = cbm_train, distribution = "gaussian",
                n.trees = 1000, shrinkage = 0.1)
yhat_boost = predict(cbm_boost, newdata = cbm_test, 
                     n.trees = 1000)
mean((yhat_boost - cbm_test$v18)^2)
# Test MSE = 0.0000179
# Lambda = 0.001 produced same test MSE as using lambda = 0.001

# lambda = 0.2
set.seed(1)
cbm_boost = gbm(v18 ~., data = cbm_train, distribution = "gaussian",
                n.trees = 1000, shrinkage = 0.2)
yhat_boost = predict(cbm_boost, newdata = cbm_test, 
                     n.trees = 1000)
mean((yhat_boost - cbm_test$v18)^2)
# Test MSE = 0.00001
# Lambda = 0.2 produced smallest test MSE of all boosted models.


# Bayesian Additive Regression Trees (BART) -------------------------------
# Reread in data for BART model
cbm_df = read.table("Final_Project/uci_bm_dataset/data.txt", header = FALSE, colClasses = "numeric")
colnames(cbm_df) = tolower(colnames(cbm_df))
cbm_df = cbm_df[, -which(names(cbm_df) == "v9")]
cbm_df = cbm_df[, -which(names(cbm_df) == "v12")]
cbm_df = cbm_df[, -which(names(cbm_df) == "v7")]

# Training data for BART
set.seed(1)
train = sample(1:nrow(cbm_df), 5967)

# Perform BART
library(BART)

x1 = cbm_df[, 1:13]
y1 = cbm_df[,"v18"]

xtrain1 = x1[train,]
ytrain1 = y1[train]

xtest1 = x1[-train,]
ytest1 = y1[-train]
set.seed(1)
cbm_bart = gbart(xtrain1, ytrain1, x.test = xtest1)
# Now compute test MSE
yhat_bart = cbm_bart$yhat.test.mean 
mean((ytest1 - yhat_bart)^2)
# Test MSE is 0.00000414

# Test MSE by Model -------------------------------------------------------
# Liner Model:
cor(cbm_df)
best_cbm_lm = lm(v18 ~ sqrt(v2)*sqrt(v5) + sqrt(v4)*sqrt(v11) + sqrt(v3)*sqrt(v6) +
                         sqrt(v8)*sqrt(v13) + sqrt(v16), cbm_train)
summary(best_cbm_lm)
best_pred_cbm_lm = predict(best_train_cbm_lm, newdata = cbm_test)
mean((best_pred_cbm_lm - cbm_test$v18)^2)
# This raised R-squared to 94.82%
# Tried other variations. This was the best R-squared value that I got.
# Test MSE = 0.00000295

# Ridge Regression Model
# Test MSE = 0.000048

# Lasso Regression Model
# Test MSE = 0.0000095

# Principal Component Regression Model
pred_pcr = predict(cbm_pcr, cbm_test, ncomp = 11)
mean((pred_pcr - cbm_test$v18)^2)
# Test MSE = 0.00000605

# Partial Least Squares Model
pred_pls = predict(cbm_pls, cbm_test, ncomp = 10)
mean((pred_pls - cbm_test$v18)^2)
# Test MSE = 0.00000578

# Regression Tree
cbm_tree = tree(v18 ~ ., cbm_train)
# Test MSE = 0.0000191

# Bagging
cbm_bag = randomForest(v18 ~., data = cbm_train, mtry = 13, importance = TRUE)
# Using 500 trees and trying all 13 predictors at each split, 98.45% of variance explained. 
# Test MSE = 0.000000858

# Random Forest
cbm_rf = randomForest(v18 ~., data = cbm_train, ntree = 25,
                      mtry = 13, importance = TRUE)
# Test MSE = 0.000000969.

# Boosting
cbm_boost = gbm(v18 ~., data = cbm_train, distribution = "gaussian",
                n.trees = 1000, shrinkage = 0.2)
# Test MSE = 0.00001


# BART Model
cbm_bart = gbart(xtrain1, ytrain1, x.test = xtest1)
# Test MSE = 0.00000414

# Best Model:
# Bagging using all 13 predictors at each split provided best Test MSE and explained
## 98.45% of the variance in the data set. 
# On that conclusion, we will say that this model best fits our training data to 
## predict our test data.