# Qualitative Regression Final Project R code ----------------------------------

# Data link: https://archive.ics.uci.edu/dataset/186/wine+quality

# Citation:
# Cortez,Paulo, Cerdeira,A., Almeida,F., Matos,T., and Reis,J.. (2009). Wine Quality. 
# UCI Machine Learning Repository. https://doi.org/10.24432/C56S3T.

# Use wine data to predict wine quality as bad and good.
# Bad wine quality will be ranked from 3-5.
# Good wine quality will be ranked from 6-9.

# Read in and clean up data -----------------------------------------------
red_wine_df = read.csv("Final_Project/wine_quality/wine_quality_red.csv")
white_wine_df = read.csv("Final_Project/wine_quality/wine_quality_white.csv")
wine_df = rbind(red_wine_df, white_wine_df)
# Change column names to snake case:
colnames(wine_df) = gsub("\\.", "_", colnames(wine_df))
lowercase_cols = tolower(colnames(wine_df))
colnames(wine_df) = lowercase_cols
dim(wine_df)
# 6497 observations and 12 variables.

# Response Variable Summary -----------------------------------------------
# quality will be our response variable.
# Can we use qualitative regression to find model to accurately predict quality response variable
## using 11 other predictors? Let's find out.
# First let's categorize the quality response variable into bad or good.
# First range of quality:
range(wine_df$quality)
# 3 to 9.
# Use for loop to categorize wine quality 3-5 as poor and 6-9 as good:
# 0 will equal bad. 
# 1 will equal good.
for (i in 1:length(wine_df$quality)) {
  if (wine_df$quality[i] >= 3 & wine_df$quality[i] <= 5) {
    wine_df$quality[i] = 0
  } else if (wine_df$quality[i] >= 6 & wine_df$quality[i] <= 9) {
    wine_df$quality[i] = 1
  } else {
    wine_df$quality[i] = NA  # Handle values outside the specified ranges
  }
}
# Count how many poors:
sum(wine_df$quality == 0)
# 2384
# Count how many goods:
sum(wine_df$quality == 1)
# 4113

#

# Training and Test Data --------------------------------------------------
# Here is training and test data that we will use. There is 6497 observations.
# Split data in half for training and test data.
set.seed(1)
train = sample(1:nrow(wine_df), 3248)
wine_train = wine_df[train,]
wine_test = wine_df[-train,]

# Check correlated variables
cor(wine_df[-12])

# Logistic Regression -----------------------------------------------------
glm_wine = glm(quality~., wine_df, family = binomial)
summary(glm_wine)
# Using null hypothesis where no predictor is significant, we see that that we can
## reject the null hypothesis for the following predictors as their p-values are all
### less than 0.05:
# volatile_acidity
# citric_acid
# residual_sugar
# free_sulfur_dioxide
# total_sulfur_dioxide
# ph
# sulphates
# alcohol

# Three of the predictors have p-values greater than 0.05, so we fail to reject
## the null hypothesis on those predictors. 
# With that being said, we remove them:
# fixed_acidity
# chlorides
# density
glm_wine = glm(quality ~. - fixed_acidity - chlorides - density, 
               wine_df, family = binomial)
summary(glm_wine)
# After creating this model, we fall to reject the null hypothesis for ph
## variable. Remove that variable from the model:
glm_wine = glm(quality ~. - fixed_acidity - chlorides - density - ph, 
               wine_df, family = binomial)
summary(glm_wine)
# All variables p-values are less than 0.

# Predict on the entire dataset using the logistic model
pred_probs = predict(glm_wine, newdata = wine_df, type = "response")

# Convert probabilities to binary predictions (0 or 1)
pred_classes = ifelse(pred_probs > 0.5, 1, 0)

# Construct confusion matrix:
conf_matrix = table(wine_df$quality, pred_classes)
print(conf_matrix)
accurate_pred = (1357 + 3464)/6497
# Returns 0.7420
# Results in prediction error of:
1 - accurate_pred
# 0.2579 or 25.8%

# This data is definitely overfitted using just entire data set.
# Let's use training and test data. Use training data to create model then
## test data to test the model:
glm_wine = glm(quality ~., wine_train, family = binomial)
summary(glm_wine)
# Looking at p-values, we will leave out following variables:
# chlorides
glm_wine = glm(quality~. - chlorides, wine_train, family = binomial)
summary(glm_wine)
# Remove predictors that are barely significant to see if that changes 
## prediction rate to simplify model:
# citric_acid
# density
# ph
glm_wine = glm(quality~. - chlorides - citric_acid - 
                 density - ph, wine_train, family = binomial)
summary(glm_wine)
# fixed_acidity no longer significant
glm_wine = glm(quality~. - chlorides - citric_acid - 
                 density - ph - fixed_acidity, wine_train, family = binomial)
# Same as:
glm_wine = glm(quality ~ volatile_acidity + residual_sugar +
                 free_sulfur_dioxide + total_sulfur_dioxide + sulphates +
                 alcohol, wine_train, family = binomial)
summary(glm_wine)


pred_probs = predict(glm_wine, newdata = wine_test, type = "response")
pred_classes = ifelse(pred_probs > 0.5, 1, 0)
conf_matrix = table(wine_test$quality, pred_classes)
print(conf_matrix)
accurate_pred = (671 + 1733) / 3249
# Returns 0.73992
1 - accurate_pred
# Prediction error of 0.26 or 26%. Higher than prediction error rate on
## over fitted data, but this one is less bias as we used training data and
# tested it on test data.


# Transformations and Interactions ----------------------------------------
# Interaction terms:
glm_wine = glm(quality ~ volatile_acidity + residual_sugar +
                 free_sulfur_dioxide + total_sulfur_dioxide + sulphates*alcohol, 
               wine_train, family = binomial)
summary(glm_wine)

pred_probs = predict(glm_wine, newdata = wine_test, type = "response")
pred_classes = ifelse(pred_probs > 0.5, 1, 0)
conf_matrix = table(wine_test$quality, pred_classes)
print(conf_matrix)
accurate_pred = (670 + 1733) / 3249
# This produced similar prediction rate of 74%

# After multiple tries at different interactions between different variables,
## I could not produce a significantly better test prediction percentage.

# Try sqrt:
glm_wine = glm(quality ~ sqrt(volatile_acidity) + sqrt(residual_sugar) +
                 sqrt(free_sulfur_dioxide) + sqrt(total_sulfur_dioxide) + 
                 sqrt(sulphates) + sqrt(alcohol), wine_train, family = binomial)
summary(glm_wine)

pred_probs = predict(glm_wine, newdata = wine_test, type = "response")
pred_classes = ifelse(pred_probs > 0.5, 1, 0)
conf_matrix = table(wine_test$quality, pred_classes)
print(conf_matrix)
accurate_pred = (691 + 1733) / 3249
# 74.6%. Highest prediction rate yet.

# Try squared:
glm_wine = glm(quality ~ I(volatile_acidity)^2 + I(residual_sugar)^2 +
                 I(free_sulfur_dioxide)^2 + I(total_sulfur_dioxide)^2 + 
                 I(sulphates)^2 + I(alcohol)^2, wine_train, family = binomial)
summary(glm_wine)

pred_probs = predict(glm_wine, newdata = wine_test, type = "response")
pred_classes = ifelse(pred_probs > 0.5, 1, 0)
conf_matrix = table(wine_test$quality, pred_classes)
print(conf_matrix)
accurate_pred = (671 + 1733) / 3249
# 73.99%.

# Try cubed:
glm_wine = glm(quality ~ I(volatile_acidity)^3 + I(residual_sugar)^3 +
                 I(free_sulfur_dioxide)^3 + I(total_sulfur_dioxide)^3 + 
                 I(sulphates)^3 + I(alcohol)^3, wine_train, family = binomial)
summary(glm_wine)

pred_probs = predict(glm_wine, newdata = wine_test, type = "response")
pred_classes = ifelse(pred_probs > 0.5, 1, 0)
conf_matrix = table(wine_test$quality, pred_classes)
print(conf_matrix)
accurate_pred = (671 + 1733) / 3249
# Sames as cubed. 73.99%

# Try log transformation:
glm_wine = glm(quality ~ log(volatile_acidity) + log(residual_sugar) +
                 log(free_sulfur_dioxide) + log(total_sulfur_dioxide) + 
                 log(sulphates) + log(alcohol), wine_train, family = binomial)
summary(glm_wine)

pred_probs = predict(glm_wine, newdata = wine_test, type = "response")
pred_classes = ifelse(pred_probs > 0.5, 1, 0)
conf_matrix = table(wine_test$quality, pred_classes)
print(conf_matrix)
accurate_pred = (710 + 1725) / 3249
# 74.96%. 

# Linear Discrimination Analysis ------------------------------------------
# Use lda() function on training set and predict test data.
library(MASS)
lda_wine = lda(quality ~., wine_train)
summary(lda_wine)
lda_pred = predict(lda_wine, wine_test)
lda_class = lda_pred$class
table(lda_class, wine_test$quality)
accurate_pred = mean(lda_class == wine_test$quality)
# Prediction rate = 0.7371 or 73.56%
# Prediction error rate of:
1 - accurate_pred
# 0.2629 or 26.29%
# Same prediction error rate from logistic regression.

# Using QDA ---------------------------------------------------------------
qda_wine = qda(quality ~., wine_train)
qda_wine

qda_class = predict(qda_wine, wine_test)$class
table(qda_class, wine_test$quality)
accurate_pred = mean(qda_class == wine_test$quality)
# Prediction rate of 0.7291 or 72.91%
# Prediction error rate of:
1 - accurate_pred
# 0.2709 or 27.09%

# Using KNN ---------------------------------------------------------------
# K = 1
library(class)
# Predictor variables used in glm_wine
predictor_vars = c('volatile_acidity', 'residual_sugar',
                         'free_sulfur_dioxide', 'total_sulfur_dioxide',
                         'sulphates', 'alcohol')

# Extract predictor variables from wine_train
train_data = wine_train[, predictor_vars]

# Define the test data
test_data = wine_test[, predictor_vars]

# Perform KNN prediction
set.seed(1)
knn_wine = knn(train_data, test_data, wine_train$quality, k = 1)
# Use confusion matrix to test predictions
conf_matrix = table(wine_test$quality, knn_wine)
print(conf_matrix)
accurate_pred = (692 + 1581) / 3249
# Prediction rate is 0.6995
# Prediction error rate is:
1 - accurate_pred
# 0.3004 or 34.04%.
# Worst prediction error rate so far.

# Try K = 2
set.seed(1)
knn_wine = knn(train_data, test_data, wine_train$quality, k = 2)
# Use confusion matrix to test predictions
conf_matrix = table(wine_test$quality, knn_wine)
print(conf_matrix)
accurate_pred = (603 + 1491) / 3249
# Prediction rate is 0.6445
# Prediction error rate is:
1 - accurate_pred
# 0.3555 or 35.55%.

# Try K = 3
set.seed(1)
knn_wine = knn(train_data, test_data, wine_train$quality, k = 3)
# Use confusion matrix to test predictions
conf_matrix = table(wine_test$quality, knn_wine)
print(conf_matrix)
accurate_pred = (555 + 1560) / 3249
# Prediction rate is 0.651
# Prediction error rate is:
1 - accurate_pred
# 0.349 or 34.9%.

# Try K = 4
set.seed(1)
knn_wine = knn(train_data, test_data, wine_train$quality, k = 4)
# Use confusion matrix to test predictions
conf_matrix = table(wine_test$quality, knn_wine)
print(conf_matrix)
accurate_pred = (548 + 1559) / 3249
# Prediction rate is 0.6485
# Prediction error rate is:
1 - accurate_pred
# 0.3515 or 35.15%.
# Not much better than K = 1

# Try K = 5
set.seed(1)
knn_wine = knn(train_data, test_data, wine_train$quality, k = 5)
# Use confusion matrix to test predictions
conf_matrix = table(wine_test$quality, knn_wine)
print(conf_matrix)
accurate_pred = (557 + 1606) / 3249
# Prediction rate is 0.6657
# Prediction error rate is:
1 - accurate_pred
# 0.3343 or 33.43%.
# Not much better than K = 1

# Try K = 6
set.seed(1)
knn_wine = knn(train_data, test_data, wine_train$quality, k = 6)
# Use confusion matrix to test predictions
conf_matrix = table(wine_test$quality, knn_wine)
print(conf_matrix)
accurate_pred = (531 + 1600) / 3249
# Prediction rate is 0.6559
# Prediction error rate is:
1 - accurate_pred
# 0.3441 or 34.41%.
# Not much better than K = 1

# Try K = 7
set.seed(1)
knn_wine = knn(train_data, test_data, wine_train$quality, k = 7)
# Use confusion matrix to test predictions
conf_matrix = table(wine_test$quality, knn_wine)
print(conf_matrix)
accurate_pred = (504 + 1687) / 3249
# Prediction rate is 0.6743
# Prediction error rate is:
1 - accurate_pred
# 0.3256 or 32.56%.
# Not much better than K = 1 or K = 5

# KNN has worst prediction rate of all models that we ran.

# Naive Bayes -----------------------------------------------------
library(e1071)
set.seed(1)
nb_wine = naiveBayes(quality ~ volatile_acidity + residual_sugar +
                       free_sulfur_dioxide + total_sulfur_dioxide + sulphates +
                       alcohol, wine_train)
nb_wine
nb_class = predict(nb_wine, wine_test)
table(nb_class, wine_test$quality)
accurate_pred = mean(nb_class == wine_test$quality)
# Prediction accuracy is 70.33%
# Prediction error rate is:
1 - accurate_pred
# 29.67%
# Low. but not as low as logistic regression, QDA or LDA.

# Poisson Regression ------------------------------------------------------
poiss_wine = glm(quality ~ volatile_acidity + residual_sugar +
                   free_sulfur_dioxide + total_sulfur_dioxide + sulphates +
                   alcohol, wine_train, family = poisson)
summary(poiss_wine)
# We can reject null hypothesis for all predictors.
pred_probs = predict(poiss_wine, newdata = wine_test, type = "response")
pred_classes = ifelse(pred_probs > 0.5, 1, 0)
conf_matrix = table(wine_test$quality, pred_classes)
print(conf_matrix)
accurate_pred = (698 + 1699) / (3249)
# Accurate prediction percentage is 73.78%
# Prediction error rate is:
1 - accurate_pred
# 26.22%.
# Similar to prediction error rates for logistic regression, LDA and QDA.

# Classification Tree -----------------------------------------------------
red_wine_df = read.csv("Final_Project/wine_quality/wine_quality_red.csv")
white_wine_df = read.csv("Final_Project/wine_quality/wine_quality_white.csv")
wine_df = rbind(red_wine_df, white_wine_df)
# Change column names to snake case:
colnames(wine_df) = gsub("\\.", "_", colnames(wine_df))
lowercase_cols = tolower(colnames(wine_df))
colnames(wine_df) = lowercase_cols

library(tree)
attach(wine_df)
good = factor(ifelse(quality <= 5, "No", "Yes"))
wine_df = data.frame(wine_df, good)
# New training and test data:
set.seed(1)
train = sample(1:nrow(wine_df), 3248)
wine_train = wine_df[train,]
wine_test = wine_df[-train,]

tree_wine = tree(good ~. - quality, wine_df)
summary(tree_wine)
# Terminal nodes: 5
# Training error rate = 26.43%.

# Plot tree
plot(tree_wine)
text(tree_wine, pretty = 0)
# Most important indicator of quality is alcohol.
# Shows up three times on tree.
tree_wine

# To properly evaluate performance of classification tree, we must estimate test 
## error:
tree_wine = tree(good ~ . - quality, wine_train)
pred_tree = predict(tree_wine, wine_test, type = "class")
table(pred_tree, wine_test$good)
accurate_pred = (794 + 1565) / (3249)
# Accurate prediction rate is 72.61%
# Prediction error rate is:
1 - accurate_pred
# 27.39%.

# Pruning tree:
# Now, let's prune the tree to see if that leads to improved results.
set.seed(1)
cv_wine = cv.tree(tree_wine, FUN = prune.misclass)
names(cv_wine)
cv_wine
# dev corresponds to number of cv errors. Same cv errors at five and 3 node tree
par(mfrow = c(1,2))
plot(cv_wine$size, cv_wine$dev, type = "b")
plot(cv_wine$k, cv_wine$dev, type = "b")
# Apply prune.misslcass function in order to purne tree to obtain 3 node tree
prune_wine = prune.misclass(tree_wine, best = 3)
plot(prune_wine)
text(prune_wine, pretty = 0)
# Test pruned tree performance on test data set.
pred_tree = predict(prune_wine, wine_test, type = "class")
table(pred_tree, wine_test$good)
accurate_pred = (794 + 1565) / (3249)
# Same prediction rate an prediction error rate as 5 node tree.
# Go with 3 node tree because it is more interpretable than 5 node tree.

# Bagging Approach:
library(randomForest)
set.seed(1)
bag_wine = randomForest(good ~. - quality, wine_train, mtry = 11,
                        importance = TRUE)
bag_wine
# 500 trees considering all 11 predictors 
# Out-of-bag estimate of error rate is: 19.98
# Accuracy rate is:
1 - 0.1998
# 80 %. Highest prediction rate so far. But this is only on training set.
# Try on test set
pred_bag = predict(bag_wine, wine_test, type = "class")
table(pred_bag, wine_test$good)
accurate_pred = (824 + 1788) / (3249)
# 80.39% accurate prediction rate.
# Highest so far!

# Try with number of trees as 25 instead of 500 to simplify model:
set.seed(1)
bag_wine = randomForest(good ~. - quality, wine_train, mtry = 11, ntree = 25,
                        importance = TRUE)
bag_wine
# Slightly larger OOB estimate of error rate:
pred_bag = predict(bag_wine, wine_test, type = "class")
table(pred_bag, wine_test$good)
accurate_pred = (808 + 1774) / (3249)
# 79.47%. Not as high as bagging model with 500 Trees

# Try 100 trees
set.seed(1)
bag_wine = randomForest(good ~. - quality, wine_train, mtry = 11, ntree = 100,
                        importance = TRUE)
bag_wine
# Slightly larger OOB estimate of error rate:
pred_bag = predict(bag_wine, wine_test, type = "class")
table(pred_bag, wine_test$good)
accurate_pred = (826 + 1783) / (3249)
# 80.30% 
# About same as 500 trees, and much more simpler model with only 100 trees.

# Try with 50 trees
set.seed(1)
bag_wine = randomForest(good ~. - quality, wine_train, mtry = 11, ntree = 50,
                        importance = TRUE)
bag_wine
# Slightly larger OOB estimate of error rate:
pred_bag = predict(bag_wine, wine_test, type = "class")
table(pred_bag, wine_test$good)
accurate_pred = (822 + 1761) / (3249)
# 79.50%.
# Slightly lower than model with 100 trees.
# So far 100 tree bagging model has highest prediction rate.

# Random Forests:
# Same as bagging but we use smaller mtry argument
# By default, random forest uses sqrt(p) when building a random forest of classification
## trees.
# Let's try mtry = 5
set.seed(1)
rf_wine = randomForest(good ~. - quality, wine_train, mtry = 5,
                        importance = TRUE)
rf_wine
# OOB Estimate of error rate: 19.83%
pred_rf = predict(rf_wine, wine_test, type = "class")
table(pred_rf, wine_test$good)
accurate_pred = (823 + 1792) / (3249)
# Prediction rate: 80.49%
# Slightly higher than bagging model with 100 trees

# Try random forest with 100 trees and mtry = 5.
set.seed(1)
rf_wine = randomForest(good ~. - quality, wine_train, mtry = 5, ntree = 100,
                       importance = TRUE)
rf_wine
# OOB Estimate of error rate: 20.26%
pred_rf = predict(rf_wine, wine_test, type = "class")
table(pred_rf, wine_test$good)
accurate_pred = (837 + 1796) / (3249)
# 81.04%
# Highest prediction rate so far!

# Try same model as above with mtry = 4
set.seed(1)
rf_wine = randomForest(good ~. - quality, wine_train, mtry = 4, ntree = 100,
                       importance = TRUE)
rf_wine
# OOB Estimate of error rate: 20.14%
pred_rf = predict(rf_wine, wine_test, type = "class")
table(pred_rf, wine_test$good)
accurate_pred = (827 + 1790) / (3249)
# 80.55%.
# Not as high when mtry = 5.

# Try when mtry = 6
set.seed(1)
rf_wine = randomForest(good ~. - quality, wine_train, mtry = 6, ntree = 100,
                       importance = TRUE)
rf_wine
# OOB Estimate of error rate: 19.92%
pred_rf = predict(rf_wine, wine_test, type = "class")
table(pred_rf, wine_test$good)
accurate_pred = (837 + 1778) / (3249)
# 80.49%

# Random forest model with m try = 5 and ntree = 100 has best prediction rate
## on test data: 81.04%
set.seed(1)
rf_wine = randomForest(good ~. - quality, wine_train, mtry = 5, ntree = 100,
                       importance = TRUE)
rf_wine
# OOB Estimate of error rate: 20.26%
pred_rf = predict(rf_wine, wine_test, type = "class")
table(pred_rf, wine_test$good)
accurate_pred = (837 + 1796) / (3249)
# importance() function shows importance of each variable
importance(rf_wine)
# Plot these importance measures:
varImpPlot(rf_wine)
# alcohol and volatile_acidity seem to be most important predictors in this model.

# Boosting: 
# Call gbm library for boosting.
# Reset data back to binary classification for quality variable:
red_wine_df = read.csv("Final_Project/wine_quality/wine_quality_red.csv")
white_wine_df = read.csv("Final_Project/wine_quality/wine_quality_white.csv")
wine_df = rbind(red_wine_df, white_wine_df)
# Change column names to snake case:
colnames(wine_df) = gsub("\\.", "_", colnames(wine_df))
lowercase_cols = tolower(colnames(wine_df))
colnames(wine_df) = lowercase_cols

for (i in 1:length(wine_df$quality)) {
  if (wine_df$quality[i] >= 3 & wine_df$quality[i] <= 5) {
    wine_df$quality[i] = 0
  } else if (wine_df$quality[i] >= 6 & wine_df$quality[i] <= 9) {
    wine_df$quality[i] = 1
  } else {
    wine_df$quality[i] = NA  # Handle values outside the specified ranges
  }
}
# Re-establish test and training data:
set.seed(1)
train = sample(1:nrow(wine_df), 3248)
wine_train = wine_df[train,]
wine_test = wine_df[-train,]

# Default lambda value in model is 0.001
library(gbm)
set.seed(1)
boost_wine = gbm(quality ~., wine_train, distribution = "bernoulli",
                 n.trees = 5000, interaction.depth = 4)
summary(boost_wine)
# We see that alcohol, total_sulfur dioxide, and volatile_acidity
# are most important variables in boosted model.
plot(boost_wine, i = "alcohol")
plot(boost_wine, i = "total_sulfur_dioxide")
plot(boost_wine, i = "volatile_acidity")

# Use model to predict test data
pred_boost = predict(boost_wine, wine_test, n.trees = 5000, type = "response")
pred_classes = ifelse(pred_boost > 0.5, 1, 0)
table(pred_classes, wine_test$quality)
accurate_pred = (828 + 1733) / (3249)
acurate_pred
# 78.82% prediction rate

# Try with n.trees = 100. Same shinking parameter.
set.seed(1)
boost_wine = gbm(quality ~., wine_train, distribution = "bernoulli",
                 n.trees = 100, interaction.depth = 4)
# Use model to predict test data
pred_boost = predict(boost_wine, wine_test, n.trees = 100, type = "response")
pred_classes = ifelse(pred_boost > 0.5, 1, 0)
table(pred_classes, wine_test$quality)
accurate_pred = (752 + 1745) / (3249)
# 76.85%. Smaller than previous model.

# Try with 5000 trees and shrinking parameter of 0.01
set.seed(1)
boost_wine = gbm(quality ~., wine_train, distribution = "bernoulli",
                 n.trees = 5000, interaction.depth = 4, shrinkage = 0.01,
                 verbose = FALSE)
# Use model to predict test data
pred_boost = predict(boost_wine, wine_test, n.trees = 5000, type = "response")
pred_classes = ifelse(pred_boost > 0.5, 1, 0)
table(pred_classes, wine_test$quality)
accurate_pred = (791 + 1761) / (3249)
# 78.54%

# Shrinking parameter of 0.1
set.seed(1)
boost_wine = gbm(quality ~., wine_train, distribution = "bernoulli",
                 n.trees = 5000, interaction.depth = 4, shrinkage = 0.1,
                 verbose = FALSE)
# Use model to predict test data
pred_boost = predict(boost_wine, wine_test, n.trees = 5000, type = "response")
pred_classes = ifelse(pred_boost > 0.5, 1, 0)
table(pred_classes, wine_test$quality)
accurate_pred = (828 + 1733) / (3249)
# 78.82

# Shrinking parameter of 0.2
set.seed(1)
boost_wine = gbm(quality ~., wine_train, distribution = "bernoulli",
                 n.trees = 5000, interaction.depth = 4, shrinkage = 0.2,
                 verbose = FALSE)
# Use model to predict test data
pred_boost = predict(boost_wine, wine_test, n.trees = 5000, type = "response")
pred_classes = ifelse(pred_boost > 0.5, 1, 0)
table(pred_classes, wine_test$quality)
accurate_pred = (812 + 1732) / (3249)
# 78.30

# Boosting model with 5000 trees and shrinking parameter of 0.1 produced
## best prediction rate.
# Still not best prediction rate we have seen.

# BART
# Use BART library
library(BART)
# Create matrices of predictors
xtrain = wine_df[wine_train,]
ytrain = wine_train[, "quality"]

xtest = wine_test[, 1:11]
ytest = wine_test[, "quality"]

# Create training model and test using test data
set.seed(1)
train = sample(1:nrow(wine_df), 3248)
x = wine_df[, 1:11]
y = wine_df[, "quality"]

xtrain = x[train,]
ytrain = y[train]

xtest = x[-train,]
ytest = y[-train]

bart_wine = lbart(xtrain, ytrain, x.test = xtest)

# Train and test model on test data:
bart_wine = lbart(xtrain, ytrain)
pred_bart = predict(bart_wine, newdata =  xtest)
# Use $prob.test.mean extraction that represents the mean probs across
## iterations or trees for each observation in test set
pred_classes = ifelse(pred_bart$prob.test.mean > 0.5, 1, 0)
table(pred_classes, ytest)
accurate_pred = (754 + 1732) / 3249
accurate_pred
# Prediction rate of 76.52%

# Model with Best Prediction Rate -----------------------------------------
# Random forest model with 100 trees and mtry = 5.
# 81.04% predictin rate on test data.
set.seed(1)
rf_wine = randomForest(good ~. - quality, wine_train, mtry = 5, ntree = 100,
                       importance = TRUE)
rf_wine
# OOB Estimate of error rate: 20.26%
pred_rf = predict(rf_wine, wine_test, type = "class")
table(pred_rf, wine_test$good)
accurate_pred = (837 + 1796) / (3249)