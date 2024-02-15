# Assignment 1 Running Data -----------------------------------------------
# Packages used
library(readxl)
library(lubridate)
library(tidyverse)
library(dplyr)
library(caret)
library(stats)
library(corrplot)
library(ggplot2)
library(glmnet)
library(pls)
library(tree)
library(randomForest)
library(fpc)
library(cluster)

setwd("/Users/doojerthekid/Documents/Merrimack Grad School Documents/DSE6620")
# Read in data
run_df = read_excel("Week_2/Running_Data_Project/Data/Run.xlsx")
dim(run_df)
head(run_df)
# 387 rows
# 15 columns
# Rows after row 361 are all NA
# Subset data frame
run_df = run_df[1:361,]

# Change date column to date structure
str(run_df$Date)
run_df$Date = as.Date(run_df$Date,format = '%Y-%m-%d')

# Create a year column
run_df = run_df %>%
  mutate(Year = year(Date))

# Convert Terrain column to a factor variable
run_df$Terrain = as.factor(run_df$Terrain)
head(run_df)

# View race data
race_df = run_df %>%
  filter(!Area %in% "Treadmill") %>%
  select(Date:Terrain, Pace:Elev_Per_Mile, PE:Year, Notes)
dim(race_df)
# 300 observations

# Check for missing values
colSums(is.na(race_df))

# Check for NA by year
result_df = race_df %>%
  group_by(Year) %>%
  summarize_all(list(~ sum(is.na(.))))
head(result_df)

# Remove PE, Notes, Sneaker, Area
race_df = race_df %>%
  select(-c(Notes, PE, Sneaker, Area)) %>%
  na.omit()

# Correlation between numeric variables
cor_df = race_df %>%
  select_if(is.numeric)
cor(cor_df)
c = round(cor(cor_df), digits = 2)
cor(cor_df)
# Correlation plot
corrplot(c)
pairs(cor_df)


# Question 1 --------------------------------------------------------------
# What is the relationship between Heartrate, Cadence and elevation?
# Looking at correlation plot:
# Heartrate and cadence have 0.31 correlation. This moderate positive
# correlation means has heart rate increases, cadence also increases.
# Heartrate and elevation have -0.10 correlation. This negative relationship
# means that as elevation increases, heartrate slightly decreases.
# Cadence and elevation have -0.55 correlation. As elevation increases,
# cadence decreases.
# Look at some plots
plot(race_df$Cadence, race_df$Heartrate)
plot(race_df$Elevation, race_df$Heartrate)
plot(race_df$Elevation, race_df$Cadence)

# Pace and speed seem to have highest absolute correlation value.
# Pace and speed have a correlation value of -0.95. That means they are 
# almost perfectly negatively correlated. As speed increases, pace decreases.
plot(race_df$Speed, race_df$Pace)

# Pace and cadence also have a high absolute correlation value. The value is
# -0.85. This Means that as cadence increases, pace decreases,
plot(race_df$Cadence, race_df$Pace)

# Pace and elevation per mile have high absolute correlation value. 
# The value is 0.85. This means that as elevation per mile increases,
# elevation per mile also increases.
plot(race_df$Elev_Per_Mile, race_df$Pace)

# Speed and elevation per mile have high absolute correlation value.
# The value is -0.81. As elevation per mile increases, speed decreases.
plot(race_df$Elev_Per_Mile, race_df$Speed)

# Question 2 --------------------------------------------------------------
# Improve the accuracy of the current model on pace:
fit_lm = lm(Pace~ Distance, log(Elev_Per_Mile):Terrain + Heartrate + Cadence)

# Linear Regression:
run_lm = lm(Pace ~ Distance + log(Elev_Per_Mile):Terrain + Heartrate + Cadence
            + Temp, race_df)
summary(run_lm)
# Using a null hypothesis that none of the predictors are significant
# of pace, we fail to reject that hypothesis for any predictor with
# a p-value less than 0.05. Let's remove the predictors with p-values
# greater than 0.05.
# Temp is the only predictor that does not have statistical significance
# in linear model.
run_lm = lm(Pace ~ Distance + log(Elev_Per_Mile):Terrain + Heartrate + Cadence, race_df)
summary(run_lm)
# Create training and test data from 262 observations. Train linear
# model using training data and predict test data.
262 / 2
# Returns 131
set.seed(1)
train = sample(1:nrow(race_df), 131)
race_train = race_df[train,]
race_test = race_df[-train,]

train_race_lm = lm(Pace ~ Distance + log(Elev_Per_Mile):Terrain + Heartrate + 
                     Cadence, race_train)
summary(train_race_lm)
# All p-values are less than 0.05.
# Let's predict test data using model
pred_race_lm = predict(train_race_lm, newdata = race_test)
mean((pred_race_lm - race_test$Pace)^2)
# Test error rate is 0.85. Prediction rate of approx 15%.
# Create a scatter plot comparing actual vs. predicted values
par(mfrow = c(1, 1))
plot_df = data.frame(Actual = race_test$Pace, Predicted = pred_race_lm)
ggplot(plot_df, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # Adds a line of perfect prediction
  labs(x = "Actual Pace", 
       y = "Predicted Pace", 
       title = "Actual vs. Predicted Linear Model") +
  theme_minimal()

# Try a few transformations:
# Try sqrt() of predictors
sqrt_race_lm = lm(Pace ~ sqrt(Distance) + log(Elev_Per_Mile):Terrain +
                    sqrt(Heartrate) + sqrt(Cadence), race_train)
summary(sqrt_race_lm)
# Predictors pass null hypothesis test
# Predict test data
pred_sqrt_race_lm = predict(sqrt_race_lm, newdata = race_test)
mean((pred_sqrt_race_lm - race_test$Pace)^2)
# Test error rate of 83%. Prediction rate of approx 17%.
# Create a scatter plot comparing actual vs. predicted values
par(mfrow = c(1, 1))
plot_df = data.frame(Actual = race_test$Pace, Predicted = pred_sqrt_race_lm)
ggplot(plot_df, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # Adds a line of perfect prediction
  labs(x = "Actual Pace", 
       y = "Predicted Pace", 
       title = "Actual vs. Predicted with Square Root Transformed Linear Model") +
  theme_minimal()

# Try squared transformation
sq_race_lm = lm(Pace ~ (Distance)^2 + log(Elev_Per_Mile:Terrain) +
                    (Heartrate)^2 + (Cadence)^2, race_train)
summary(sq_race_lm)
# All predictors pass null hypothesis test.
pred_sq_race_lm = predict(sq_race_lm, newdata = race_test)
mean((pred_sq_race_lm - race_test$Pace)^2)
# Test error rate of approx 85%.

# Try log transformation on predictors
log_race_lm = lm(Pace ~ log(Distance) + log(Elev_Per_Mile):Terrain +
                  log(Heartrate) + log(Cadence), race_train)
summary(log_race_lm)
# All predictors pass null hypothesis test.
pred_log_race_lm = predict(log_race_lm, newdata = race_test)
mean((pred_log_race_lm - race_test$Pace)^2)
# Test error rate of 0.82.
par(mfrow = c(1, 1))
plot_df = data.frame(Actual = race_test$Pace, Predicted = pred_log_race_lm)
ggplot(plot_df, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # Adds a line of perfect prediction
  labs(x = "Actual Pace", 
       y = "Predicted Pace", 
       title = "Actual vs. Predicted with Log Transformed Linear Model") +
  theme_minimal()
# Best model so far.

# Try Ridge Regression Model:
set.seed(1)
train_matrix = model.matrix(Pace ~ Distance + log(Elev_Per_Mile):Terrain + Heartrate + 
                              Cadence, race_train)
test_matrix = model.matrix(Pace ~ Distance + log(Elev_Per_Mile):Terrain + Heartrate + 
                             Cadence, race_test)
# Now we need to select lambda using cross-validation
cv_out = cv.glmnet(train_matrix, race_train$Pace, alpha = 0)
best_lam = cv_out$lambda.min
best_lam
# Lambda chosen by cross-validation is 0.15
# Now we fit ridge regression model and make predictions:
race_ridge = glmnet(train_matrix, race_train$Pace, alpha = 0)
pred_race_ridge = predict(race_ridge, s = best_lam, newx = test_matrix)
# Find test error
mean((pred_race_ridge - race_test$Pace)^2)
# 0.93 test error.

# Try Lasso Regression Model:
set.seed(1)
train_matrix = model.matrix(Pace ~ Distance + log(Elev_Per_Mile):Terrain + Heartrate + 
                              Cadence, race_train)
test_matrix = model.matrix(Pace ~ Distance + log(Elev_Per_Mile):Terrain + Heartrate + 
                             Cadence, race_test)
# Now we need to select lambda using cross-validation
cv_out = cv.glmnet(train_matrix, race_train$Pace, alpha = 1)
best_lam = cv_out$lambda.min
best_lam
# Lambda chosen by cross-validation is 0.00109
# Now we fit ridge regression model and make predictions:
race_lasso = glmnet(train_matrix, race_train$Pace, alpha = 1)
pred_race_lasso = predict(race_lasso, s = best_lam, newx = test_matrix)
# Find test error
mean((pred_race_lasso - race_test$Pace)^2)
# Test error rate is 0.85. Not better than log transformation.

# Fit a regression tree
race_tree = tree(Pace ~ Distance + log(Elev_Per_Mile) + Terrain + Heartrate + 
                   Cadence, race_train)
summary(race_tree)
plot(race_tree)
text(race_tree, pretty = 0)
# Number of terminal nodes: 9
yhat = predict(race_tree, newdata = race_test)
mean((yhat - race_test$Pace)^2)
# Test MSE = 1.42. This model performed poorly on data
race_tree

# Try bagging model:
set.seed(1)
race_bag = bagging(Pace ~ Distance + Elevation:Terrain + Heartrate + 
                     Cadence, data = race_train)
race_bag
yhat_bag = predict(race_bag, newdata = race_test)
mean((yhat_bag - race_test$Pace)^2)
# Test MSE of 1.62. This model also performed poorly on data

control = trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 3, 
                       savePredictions = TRUE)

# Try random forest model with 100 trees:
set.seed(1)
race_rf = randomForest(Pace ~ Distance + Elevation:Terrain + Heartrate + 
                         Cadence, data = race_train, ntree = 100,
                       mtry = 11, importance = TRUE)
race_rf
yhat_rf = predict(race_rf, newdata = race_test)
mean((yhat_rf - race_test$Pace)^2)
# Test MSE of 1.093.
# I believe that all of the models with test MSE's greater than 1, were overfitting
# to the training data.

# Best model we were able to make was the linear regression model 
# with log transformations
# 
log_race_lm = lm(Pace ~ log(Distance) + log(Elev_Per_Mile):Terrain +
                   log(Heartrate) + log(Cadence), race_train)
summary(log_race_lm)
pred_log_race_lm = predict(log_race_lm, newdata = race_test)
mean((pred_log_race_lm - race_test$Pace)^2)
# Test MSE of 82%, which is 0.3% less than liner regression model fit in the notes
# from class.

# Question 3 --------------------------------------------------------------
# Make a race prediction of minutes for a 5k race held on November 23, 2023. 
# Assume a distance of 3.1 miles, 95 feet of elevation and a PE (perceived exertion) 
# of 5, temperature of 47 degrees and terrain = 0. How about a 10k race (6.2 miles), 
# 200 feet of elevation with same temp and terrain? Be sure to account for expected 
# heart rate and cadence when solving this problem!

# Using log transformation model, we have distance, log(Elev_Per_Mile):Terrain,
# log(Heartrate) and log(Cadence).
log_race_lm = lm(Pace ~ log(Distance) + log(Elev_Per_Mile):Terrain +
                   log(Heartrate) + log(Cadence), race_train)
summary(log_race_lm)
pred_log_race_lm = predict(log_race_lm, newdata = race_test)
mean((pred_log_race_lm - race_test$Pace)^2)

# Create new data frame with information from question.
# We need to use expected heart rate and cadence, which will be the averages of 
# each predictor
expected_hr = mean(race_df$Heartrate)
expected_cadence = mean(race_df$Cadence)
race_projection_data = data.frame(
  Distance = 3.1,
  Elev_Per_Mile = 95/3.1,
  Terrain = 0,
  Heartrate = expected_hr,
  Cadence = expected_cadence
)
race_projection_data$Terrain = as.factor(race_projection_data$Terrain)
predictions = predict(log_race_lm, newdata = race_projection_data)
predictions
# Predicted pace = 10.55
# To get total minutes for the race, multiple predictions by 3.1
total_minutes = 10.55 * 3.1
total_minutes
# 32.71 total minutes for the 5k

# For the 10k
race_projection_data = data.frame(
  Distance = 6.1,
  Elev_Per_Mile = 200/6.2,
  Terrain = 0,
  Heartrate = expected_hr,
  Cadence = expected_cadence
)
race_projection_data$Terrain = as.factor(race_projection_data$Terrain)
predictions = predict(log_race_lm, newdata = race_projection_data)
predictions
# Predicted pace = 11.07
total_minutes = 11.07 * 6.2
total_minutes
# 68.63 total minutes for the 10k.


# Question 4 --------------------------------------------------------------
# Write a brief summary of your findings and what additional data you would 
# want to more accurately make a prediction.

# I found that some of the non-linear models that I fitted, like the regression tree 
# model, the random forest model and the bagging model, were all over fitting to the
# training data, as I was getting mean squared error values of over 1 when using the
# training models to predict the test data. On the other models that I fitted using the
# training data, I felt like I was not able to move the needle on the test error rates.
# If anything, most of my models had worse test rate than the original linear model that
# was fit in the class notes. I was only able to produce a smaller mean squared error value
# in the linear model where I log transformed Distance, Heartrate, and Cadence along with the
# interaction term of log(Elev_Per_Mile):Terrain.
# I think the only thing I could ask for at this point is more data. The more data would 
# probably have some influence on the models that I fit. If I could have a bigger overall
# data set, I could then have more data to train the models, which would hopefully minimize
# any over fitting occurring, which could then produce better prediction rates.

# Question 5 --------------------------------------------------------------
# With unfiltered data (i.e., don’t remove treadmill data), create a clustering model to backfill 
# perceived exertion. Write a short summary explaining how and why it works to a non-technical 
# audience. Be sure to test the performance of your model if you can.

# Clustering is used to impute missing values

# Reset original run data frame
run_df = read_excel("Week_2/Run.xlsx")
# Subset data frame
run_df = run_df[1:361,]

# Change date column to date structure
str(run_df$Date)
run_df$Date = as.Date(run_df$Date,format = '%Y-%m-%d')

# Create a year column
run_df = run_df %>%
  mutate(Year = year(Date))

# Convert Terrain column to a factor variable
run_df$Terrain = as.factor(run_df$Terrain)
head(run_df)

# Keep treadmill data
run_df = run_df %>%
  select(Date:Terrain, Pace:Elev_Per_Mile, PE, Year)
run_df

# Flip terrain back into numeric column
run_df$Terrain = as.numeric(run_df$Terrain)

# Before using cluster model to backfill missing PE values, we need to handle
# NA values in other columns
# Let's remove all rows in data frame that have NA besides rows that have NA in
# PE values, since that is what we are looking to backfill
colnames(run_df)
columns_to_check = c("Date", "Distance", "Elevation", "Minutes", "Heartrate", "Cadence",
                     "Temp", "Terrain", "Pace", "Speed", "Elev_Per_Mile", "Year")
run_df = run_df[complete.cases(run_df[, columns_to_check]), ]

# Now, let us use clustering to backfill PE missing values
columns = run_df[, c("Distance", "Elevation", "Minutes", "Heartrate", 
                      "Cadence", "Temp", "Terrain", "Pace", "Speed", "Elev_Per_Mile")]

# Handle missing values in columns
columns[is.na(columns)] = 0  
# Replace NA with 0

# Normalize features
normalized_columns = scale(columns)

# Perform hierarchical clustering:
hclust = hclust(dist(normalized_columns), method = "complete")

# Perform k-means clustering
k = 3  # Set the number of clusters
clusters = cutree(hclust, k)
run_df$cluster = clusters

# Backfill PE column based on cluster means
run_df = run_df %>%
  group_by(cluster) %>%
  mutate(PE = ifelse(is.na(PE), mean(PE, na.rm = TRUE), PE))

# Remove the 'cluster' column if you don't need it anymore
run_df$cluster = NULL

# Since PE is a whole number, let's adjust the new data to be rounded to nearest
# whole number:
run_df$PE = round(run_df$PE)

# Clustering is a way to group certain events, in this case, similar runs. What we did
# is group similar runs together based on distance, elevation, minutes,
# heart rate, cadence, temp, terrain, pace, speed and elevation per mile. The clustering
# algorithm automatically does this sorting and takes the average of those similar runs to
# help replace the NA values in our data set.