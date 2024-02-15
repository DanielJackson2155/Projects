# Franchise Investment Project --------------------------------------------
# Libraries used
library(dplyr)
library(ggplot2)
library(scales)
library(corrplot)
library(glmnet)
library(pls)
library(tree)
library(randomForest)

# Read in and clean data --------------------------------------------------
# Read in data
value_df = read.csv("Week_3/franchise_values.csv")

# Change name of columns for ease of coding
colnames(value_df)[colnames(value_df) == "Year.over.Year.Value.Increase"] = "YOY_Val_Inc"
# Do the same for Debt.Value column
colnames(value_df)[colnames(value_df) == "Debt.Value"] = "Debt_Value"
# Convert all column names to lowercase

# Check column classes
colnames(value_df) = tolower(colnames(value_df))
column_classes = sapply(value_df, class)
column_classes
# Will need to change revenue, value, yoy_val_inc, debt value, character and population
# to numeric vector

# Year span
range(value_df$Year)
# 2020 to 2023. No 2022 year.
# In the data set. We see that 2020 does not have yoy_val_inc, debt_val or income.
# Just has population.
# Let's subset that data frame to be the population data frame and remove those observations
# from value data frame
pop_df = value_df[248:393,]
value_df = value_df[-c(248:393),]
# Since we do not have the 2020 values of yoy_val_inc, debt_val or income,
# I am going to do away with those observations and use years 2021 and 2023. I 
# just want the most complete data for my observations. In doing so, I will be
# removing the MLS. There is only 2020 data on the MLS franchises, meaning there is
# no data to compare the 2020 data to anyways.

# I still want to use population for my analysis, as I think that will play a part.
# Using data I found from the US Pop Growth Rate from Macrotrends https://www.macrotrends.
# net/countries/USA/united-states/population-growth-rate#:~:text=The%20current%20population
# %20of%20U.S.,a%200.38%25%20increase%20from%202021.:
# Population growth of 0.31% from 2020 to 2021
# Population growth of 0.38% from 2021 to 2022
# Population growth of 0.5% from 2022 to 2023

# Let's multiply our populations from 2020 population data by 0.0031 to get estimated
# population data for 2021
# Before we can do that, let's remove revenue, value, yoy_val_inc, debt_value, and
# income in 2020 population data frame since we are only working with population 
# in that data frame

columns_to_remove = c("revenue", "value", "yoy_val_inc", "debt_value", "income")
pop_df = pop_df[, !(names(pop_df) %in% columns_to_remove)]

# Convert population column into numeric column
pop_df$population = as.numeric(gsub(",", "", pop_df$population))
class(pop_df$population)

# Create new population columns for 2021, 2022 and 2023
pop_df$twenty_one_pop = (pop_df$population)*1.0031
pop_df$twenty_two_pop = (pop_df$twenty_one_pop)*1.0038
pop_df$twenty_three_pop = (pop_df$twenty_two_pop)*1.005

# Create new data frames with new populations and update the years
twenty_one_pop_df = pop_df[, c("year", "league", "team", "twenty_one_pop")]
# Update year
twenty_one_pop_df$year = 2021

twenty_three_pop_df = pop_df[, c("year", "league", "team", "twenty_three_pop")]
# Update year
twenty_three_pop_df$year = 2023

# Now let us merge data frames to get populations for 
# First, remove population column in value_df
value_df = value_df[, -which(names(value_df) == "population")]
# Merge 2021 population data frame
value_df = merge(value_df, twenty_one_pop_df[, c("year", "league", 
                                                  "team", "twenty_one_pop")], 
                  by = c("year", "league", "team"), all.x = TRUE)

# Merge 2023 population data frame
value_df = merge(value_df, twenty_three_pop_df[, c("year", "league", 
                                                 "team", "twenty_three_pop")], 
                 by = c("year", "league", "team"), all.x = TRUE)

# Let's make all NA values equal to 0 and then combine the two columns to make
# one population column
value_df$twenty_one_pop[is.na(value_df$twenty_one_pop)] = 0
value_df$twenty_three_pop[is.na(value_df$twenty_three_pop)] = 0
# Now let's add them together into one column
value_df$population = value_df$twenty_one_pop + value_df$twenty_three_pop

# Check to see if there are any zeros in population column:
zero_population_rows = value_df[value_df$population == 0, ]
zero_population_rows
# We can see we ran into some issues with three teams in 2023:
# Cleveland Indians because they became the Cleveland Guardians
# Washington Commanders because they were once the Washington Football Team
# Seattle Kraken because they were not around in 2020. 
# For the populations, other teams play in those cities so we can just use those
# estimates for those teams.

# For Cleveland, let's pull the population value from the 2023 Browns
cleveland_pop = value_df[value_df$year == 2023 & 
                           value_df$team == "Cleveland Browns", "population"]
cleveland_pop
# For Washington, let's pull the population value from the 2023 Wizards
wash_pop = value_df[value_df$year == 2023 & 
                           value_df$team == "Washington Wizards", "population"]
wash_pop
# For Seattle, let's pull the population value from the 2023 Mariners
seattle_pop = value_df[value_df$year == 2023 & 
                           value_df$team == "Seattle Mariners", "population"]
seattle_pop
# Now let's get these values in for our missing data
zero_population_rows
value_df[131,"population"] = cleveland_pop
value_df[215,"population"] = wash_pop
value_df[240,"population"] = seattle_pop
zero_population_rows
# No observations have 0 in the population
# Let's delete the 2021 and 2023 population columns
value_df = value_df[, -which(names(value_df) == "twenty_one_pop")]
value_df = value_df[, -which(names(value_df) == "twenty_three_pop")]

# Now we have our educated estimated populations set for the 2021 and 2023
# years. Let's make sure all of our numers are in numeric class
column_classes
# Need to convert revenue, value, yoy_val_inc, debt_value, income and 
# population to numeric
# First revenue:
# Remove commas and convert to numeric
value_df$revenue = as.numeric(gsub(",", "", value_df$revenue))

# Now, value:
value_df$value = as.numeric(gsub(",", "", value_df$value))

# Now, yoy_val_inc:
# Convert year over year column to all decimals. Some observations are already in 
# decimal format. Use ifelse() statement to help with taht
value_df$yoy_val_inc = ifelse(grepl("%", value_df$yoy_val_inc),
                              as.numeric(sub("%", "", value_df$yoy_val_inc))/100,
                              as.numeric(value_df$yoy_val_inc))

# Do the same thing for the debt_value
value_df$debt_value = ifelse(grepl("%", value_df$debt_value),
                              as.numeric(sub("%", "", value_df$debt_value))/100,
                              as.numeric(value_df$debt_value))

# Now, income:
value_df$income = as.numeric(gsub(",", "", value_df$income))

# Finally, population:
value_df$population = as.numeric(gsub(",", "", value_df$population))

# Check classes
column_classes = sapply(value_df, class)
column_classes

# One last thing we need to do is convert the Washington Football Team to the
# Commanders and the Cleveland Indians to the Guardians.
value_df$team = ifelse(value_df$team == "Washington Football Team", 
                        "Washington Commanders", 
                        value_df$team)
value_df$team <- ifelse(value_df$team == "Cleveland Indians", 
                        "Cleveland Guardians", 
                        value_df$team)

# check for NA values in each column:
colSums(is.na(value_df))
# One NA in income column.
team_with_na_income = subset(value_df, is.na(income))$team
# It is for the Calgary Flames. Let's find out what year
team_with_na_income = subset(value_df, is.na(income))$year
# 2021
# Let's take average income of NHL in 2021 and replace it as NA value
filtered_df = value_df %>%
  filter(year == 2021)
average_income = mean(filtered_df$income, na.rm = TRUE)
value_df$income[is.na(filtered_df$income)] = average_income

# Now that the data is cleaned, we can now start our analysis

# Exploratory Analysis ----------------------------------------------------
# Look at value-revenue for each team in each league in data set
# Create new predictor of that value-revenue ratio
value_df$rev_to_val_ratio = value_df$revenue / value_df$value
# Look at average revenue to value ratio of all of the leagues
avg_rev_to_val_ratio = value_df %>%
  group_by(league) %>%
  summarise(average_rev_to_val = mean(rev_to_val_ratio, na.rm = TRUE))
print(avg_rev_to_val_ratio)
# league        average_rev_to_val
# MLB           0.120
# NBA           0.112
# NFL           0.118
# NHL           0.216
# The NHL, on average, as the highest revenue to value ratio of all the leagues.

# Let's look at box plots for each predictor by league, just to get an idea
# of how the data is distributed
selected_predictors = c("revenue", "value", "income", "population", 
                        "rev_to_value_ratio")

# Create box plots using ggplot2
# Revenue
ggplot(value_df, aes(x = league, y = value_df[[selected_predictors[1]]]/1e6)) +
  geom_boxplot() +
  labs(title = paste("Revenue by League"),
       x = "League",
       y = "Revenue (in USD Millions)")
# Value
ggplot(value_df, aes(x = league, y = value_df[[selected_predictors[2]]]/1e9)) +
  geom_boxplot() +
  labs(title = paste("Value by League"),
       x = "League",
       y = "Value (in USD Billions)")
# Income
ggplot(value_df, aes(x = league, y = value_df[[selected_predictors[3]]]/1e6)) +
  geom_boxplot() +
  labs(title = paste("Income by League"),
       x = "League",
       y = " Income (in USD Millions)")
# Population
ggplot(value_df, aes(x = league, y = value_df[[selected_predictors[4]]]/1e6)) +
  geom_boxplot() +
  labs(title = paste("Population by League"),
       x = "League",
       y = " Population (Millions)")

# Looking at big picture. Let's look at and average revenue and value for each 
# of the major sports in both 2021 and 2023. Let's then graph it
average_revenue = value_df %>%
  group_by(year, league) %>%
  summarise(avg_revenue = mean(revenue, na.rm = TRUE))
print(average_revenue)
# Plot it
ggplot(average_revenue, aes(x = as.factor(year), y = avg_revenue / 1e6, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge", color = "black", alpha = 0.7) +
  facet_grid(. ~ league) +
  labs(title = "Average Revenue by League and Year",
       x = "Year",
       y = "Average Revenue (Millions)",
       fill = "Year") +
  scale_y_continuous(labels = scales::comma) +  # Use comma as the thousand separator
  theme_minimal()

# Average value
average_value = value_df %>%
  group_by(year, league) %>%
  summarise(avg_value = mean(value, na.rm = TRUE))
print(average_revenue)

# Plot it
ggplot(average_value, aes(x = as.factor(year), y = avg_value / 1e9, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge", color = "black", alpha = 0.7) +
  facet_grid(. ~ league) +
  labs(title = "Average Value by League and Year",
       x = "Year",
       y = "Average Value (Billions)",
       fill = "Year") +
  scale_y_continuous(labels = scales::comma) +  # Use comma as the thousand separator
  theme_minimal()

# Look at top 5 teams from 2021 with highest revenue:
filtered_df = value_df %>%
  filter(year == 2021)
sorted_df = filtered_df %>%
  arrange(desc(revenue))
top_5_teams = head(sorted_df, 5)
print(top_5_teams)
# Cowboys
# Patriots
# Rams
# Texans
# Raiders
# All NFL Teams

# Look at top 5 teams from 2023 with highest revenue:
filtered_df = value_df %>%
  filter(year == 2023)
sorted_df = filtered_df %>%
  arrange(desc(revenue))
top_5_teams = head(sorted_df, 5)
print(top_5_teams)
# Cowboys
# Warriors
# Yankees
# Patriots
# Rams
# Now, there is one basketball and one baseball team in there.

# Look at top 5 teams from 2021 with highest values:
filtered_df = value_df %>%
  filter(year == 2021)
sorted_df = filtered_df %>%
  arrange(desc(value))
top_5_teams = head(sorted_df, 5)
print(top_5_teams)
# Cowboys
# Knicks
# Warriors
# Lakers
# Yankees

# # Look at top 5 teams from 2023 with highest values:
filtered_df = value_df %>%
  filter(year == 2023)
sorted_df = filtered_df %>%
  arrange(desc(value))
top_5_teams = head(sorted_df, 5)
print(top_5_teams)
# Cowboys
# Yankees
# Warriors
# Patriots
# Rams

# Overall, Cowboys are highest valued team in our data set.They also have the highest
# revenue in 2021 and 2023, so it makes sense that they stay at that highest valued 
# team.

# Let's look at highest average year over year value increase for each league for 
# 2021 and 2023
# For 2021:
filtered_df = value_df %>%
  filter(year == 2021)

avg_yoy_increase = filtered_df %>%
  group_by(league) %>%
  summarise(avg_yoy_increase = mean(yoy_val_inc, na.rm = TRUE))

print(avg_yoy_increase)
# league        avg_yoy_increase
# 1 MLB              0.0233
# 2 NBA              0.113 
# 3 NFL              0.145 
# 4 NHL             -0.0271
# On average, NFL has biggest year over year value increase

# For 2023:
filtered_df = value_df %>%
  filter(year == 2023)

avg_yoy_increase = filtered_df %>%
  group_by(league) %>%
  summarise(avg_yoy_increase = mean(yoy_val_inc, na.rm = TRUE))

print(avg_yoy_increase)
# league        avg_yoy_increase
# 1 MLB               0.106
# 2 NBA               0.158
# 3 NFL               0.286
# 4 NHL               0.218
# NHL has biggest year over year value increase.
# NHL went from being last in 2021 to second behind NFL in 2023. Hockey
# seems to be having success as a league if average year over year value 
# increase is greater than both NBA and MLB

# Let's look at average debt value for each league for 2021 and 2023
# For 2021:
filtered_df = value_df %>%
  filter(year == 2021)

avg_debt_value = filtered_df %>%
  group_by(league) %>%
  summarise(debt_value = mean(debt_value, na.rm = TRUE))

print(avg_debt_value)
# league           debt_value
# 1 MLB               0.134
# 2 NBA               0.120
# 3 NFL               0.122
# 4 NHL               0.276
# NBA has lowest debt value of all the leagues in 2021
# For 2023:
filtered_df = value_df %>%
  filter(year == 2023)

avg_debt_value = filtered_df %>%
  group_by(league) %>%
  summarise(debt_value = mean(debt_value, na.rm = TRUE))

print(avg_debt_value)
# league           debt_value
# 1 MLB               0.120
# 2 NBA               0.104
# 3 NFL               0.0931
# 4 NHL               0.191
# In 2023, NFL has lowest debt value of all four sports.

# Let's look at average income for each league for 2021 and 2023
# For 2021:
filtered_df = value_df %>%
  filter(year == 2021)
avg_income = filtered_df %>%
  group_by(league) %>%
  summarise(avgerage_income = mean(income, na.rm = TRUE))

print(avg_income)
# league        average_income
# MLB           -60066667
# NBA           24606667
# NFL           6990625
# NHL           790183
# NFL with the highest average income value.
# For 2023:
filtered_df = value_df %>%
  filter(year == 2023)
avg_income = filtered_df %>%
  group_by(league) %>%
  summarise(average_income = mean(income, na.rm = TRUE))

print(avg_income)
# league        average_ income
# MLB           17653333
# NBA           90800000
# NFL           145968750
# NHL           49162500
# Once again, NFL with highest average income value in 2023.


# Regression to Predict Team Value ----------------------------------------
# Let's look at correlation between numeric values in dataset
cor_df = value_df %>%
  select_if(is.numeric)
cor(cor_df)
c = round(cor(cor_df), digits = 2)
cor(cor_df)
# Correlation plot
corrplot(c)
pairs(cor_df)
# Let's remove rev_to_val_ratio as it is could create mutlicolliearity issues
# Assuming your_data is your dataset
cor_df = subset(cor_df, select = -rev_to_val_ratio)
c = round(cor(cor_df), digits = 2)
corrplot(c)

# Let's create training and test set
dim(value_df)
247 / 2
set.seed(1)
train = sample(1:nrow(cor_df), 123)
value_train = cor_df[train,]
value_test = cor_df[-train,]

# Try linear regression to predict value in data set
value_lm = lm(value ~., value_train)
summary(value_lm)
# Using null hypothesis testing, we see that all predictors pass null hypothesis test,
# since p-values are less than 0.05.
# R^2 value of 84.15
# Let's use trained model to predict test data
pred_value_lm = predict(value_lm, newdata = value_test)
mean((pred_value_lm - value_test$value)^2)
# Very high mean squared error value due to high value of response variable.

# Try log transformation on response variable re-fit linear model
# Log transformation
value_train$log_value = log(value_train$value)
value_test$log_value = log(value_test$value)
log_value_lm = lm(log_value ~. - value, value_train)
summary(log_value_lm)
# All predictors pass null hypothesis test.
# R^2 value of 0.79.
pred_value_lm = predict(log_value_lm, newdata = value_test)
mean((pred_value_lm - value_test$log_value)^2)
# Test MSE of 0.139.

# Plot linear model
par(mfrow = c(2, 2))
plot(log_value_lm)

# Plot actual values of log of response variable value versus predicted values
par(mfrow = c(1, 1))
plot_df = data.frame(Actual = value_test$log_value, Predicted = pred_value_lm)

ggplot(plot_df, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  
  # Adds a line of perfect prediction
  labs(x = "Actual log(Value)", 
       y = "Predicted log(Value)", 
       title = "Actual vs. Predicted") +
  theme_minimal()

# Try some transformations on predictors
# Square Root
sqrt_value_lm = lm(log_value ~ - value + year + sqrt(revenue) +
                     sqrt(yoy_val_inc) + sqrt(debt_value) + 
                     sqrt(income) + sqrt(population), value_train)
summary(sqrt_value_lm)
# yoy_val_inc and income have p-values greater than 0.05 so we will remove
# them from the model.
sqrt_value_lm = lm(log_value ~ - value + year + sqrt(revenue) +
                      sqrt(debt_value) + sqrt(population), value_train)
summary(sqrt_value_lm)
# Predict test data
pred_value_lm_sqrt = predict(sqrt_value_lm, newdata = value_test)
mean((pred_value_lm_sqrt - value_test$log_value)^2)
# 0.13 MSE. Barely better than previous model.

# Squared
sqrd_value_lm = lm(log_value ~ - value + year + (revenue)^2 +
                     (yoy_val_inc)^2 + (debt_value)^2 + 
                     (income)^2 + (population)^2, value_train)
summary(sqrd_value_lm)
# All predictors have p-values less than 0.05.
# Predict test data
pred_value_lm_sqrd = predict(sqrd_value_lm, newdata = value_test)
mean((pred_value_lm_sqrd - value_test$log_value)^2)
# 0.139 MSE.
# Not much improvement

# Try Ridge Regression
set.seed(1)
train_matrix = model.matrix(log_value ~. - value, value_train)
test_matrix = model.matrix(log_value ~. - value, value_test)
# Now we need to select lambda using cross-validation
cv_out = cv.glmnet(train_matrix, value_train$log_value, alpha = 0)
best_lam = cv_out$lambda.min
best_lam
# Lambda chosen by cross-validation is 0.0579
# Now we fit ridge regression model and make predictions:
value_ridge = glmnet(train_matrix, value_train$log_value, alpha = 0)
pred_value_ridge = predict(value_ridge, s = best_lam, newx = test_matrix)
# Find test error:
mean((pred_value_ridge - value_test$log_value)^2)
# 0.117 MSE.

# Try Lasso Regression
set.seed(1)
train_matrix = model.matrix(log_value ~. - value, value_train)
test_matrix = model.matrix(log_value ~. - value, value_test)
# Now we need to select lambda using cross-validation
cv_out = cv.glmnet(train_matrix, value_train$log_value, alpha = 1)
best_lam = cv_out$lambda.min
best_lam
# Lambda chosen by cross-validation is 0.0014
# Now we fit ridge regression model and make predictions:
value_lasso = glmnet(train_matrix, value_train$log_value, alpha = 1)
pred_value_lasso = predict(value_lasso, s = best_lam, newx = test_matrix)
# Find test error:
mean((pred_value_lasso - value_test$log_value)^2)
# 0.12 MSE so far.

# Principal Components Regression
set.seed(1)
value_pcr = pcr(log_value ~. - value, data = value_train, 
              scale = TRUE, validation = "CV")
summary(value_pcr)
par(mfrow = c(1, 1))
validationplot(value_pcr, val.type = "MSEP")
# We see that where M = 6 and 5, our model produces the lowest mean squared error 
# prediction value. However, we are trying our best to reduce the dimensions 
# of the model.  
pred_pcr = predict(value_pcr, value_test, ncomp = 4)
mean((pred_pcr - value_test$log_value)^2)
# When we fit a principal component regression using M = 4,
# we get a mean squared test error of 0.18.

# Now try using 3 components
pred_pcr = predict(value_pcr, value_test, ncomp = 3)
mean((pred_pcr - value_test$log_value)^2)
# 0.20
# Neither of these MSEs are less than our lowest one that we have produced so far.

# Try regression tree
value_tree = tree(log_value ~. - value, value_train)
summary(value_tree)
plot(value_tree)
text(value_tree, pretty = 0)
# There are 11 terminal nodes
yhat = predict(value_tree, newdata = value_test)
mean((yhat - value_test[, "log_value"])^2)
# MSE of 0.14. Best MSE so far!

# Try random forest model
value_rf = randomForest(log_value ~. - value, data = value_train, ntree = 500)
value_rf
# 85.9% of variance explained
yhat_rf = predict(value_rf, newdata = value_test)
mean((yhat_rf - value_test$log_value)^2)
# 0.06 MSE. This is the best MSE so far!

# Let's look at most important variables in random forest model
varImpPlot(value_rf)
# Most important variables in random forest model are revenue and yoy_val_inc.

# Of all of the models that we fit, our trained random forest model using 500 trees
# produced mean squared error of 0.058 on our test data. This was the most accurate
# model and it also explains 85.89% of the data's variance.

# Conclusions Drawn from Analysis -----------------------------------------
# Based on my exploratory analysis, my recommendation would be to at least invest
# in one NFL franchise. The league as a whole seems to generating the most revenue which
# is driving the values up year over year. The NFL is also showing the lowest
# debt value of all sports in 2023. I think it is important to diversify the portfolio
# of our investors, so I would like to recommend investing in another franchise in another
# league as well. 

# Let's figure out what NFL franchise we will recommend to our investors.
# Let's filter our data to only have NFL franchises in 2023 since that is the most
# recent year
nfl_value = value_df %>%
  filter(year == 2023 & league == "NFL")
# Since we only have 4 billion to spend, let's filter data to show teams only
# under 4 billion in value
nfl_value = nfl_value %>%
  filter(value < 4e9)
# All NFL values under $4 billion are over $3 billion
min(nfl_value$value)
# Min value of NFL franchise is $3 billion
# Let's invest in the NFL team that is under $4 billion with the highest year
# over year value increase in 2023
max_yoy_index = which(nfl_value$yoy_val_inc == max(nfl_value$yoy_val_inc))
nfl_value$team[max_yoy_index]
# This returns the Buffalo Bills who had a yoy value increase of 0.5 in 2023
nfl_value$value[nfl_value$team == "Buffalo Bills"]
# Assuming we purchase the Bills for their 2023 value, that would cost us
# $3.4 billion.

# This leave us $600 million to invest in another franchise.
# Let's look at all of the other non-NFL franchises and see who had the largest
# year over year value increase that is less than or equal to $600 million.

non_nfl_value_df = value_df %>%
  filter(year == 2023 & league != "NFL")
non_nfl_value_df = non_nfl_value_df %>%
  filter(value < (4e9 - 3.4e9))
head(non_nfl_value_df)
# There are only two teams that fit this criteria and they are both NHL teams:
# the Arizona Coyotes and the Florida Panthers.
# As we saw earlier, next to the NFL, the NHL had the next highest average year over year
# value. Therefore, I think that is a good financial move to invest in an NHL franchise,
# as the league seems to be growing in popularity and in value.
# Between the Panthers and the Coyotes, the Panthers have a higher year over year
# value increase and a lower debt value. Therefore, we should invest in the Panthers
non_nfl_value_df$value[non_nfl_value_df$team == "Florida Panthers"]
# Assuming we can purchase the Panthers for the value of $550 million, we would be able to
# do so with $40 million leftover from our $4 billion investment fund.

# Conclusion Summary ------------------------------------------------------
# We were able to use some regression tactics to fit models to our training data and predict
# the test data and check our mean squared error rates of those predictions. In doing so,
# we found that fitting a random forest model with 11 terminal nodes produced the lowest
# mean squared error rate of all of our models with revenue and the year-over-year
# value increase predictors being the most important ones of them all.
# Using some exploratory analysis of the data set, we decided that the NFL is where
# all of the money is, based on value, year over year value increases, low debt values
# and high incomes. Therefore, I think it is smart to invest in a NFL franchise as it
# is the most profitable league. However, I think it is important to diversify our
# portfolio and invest in another team as well. After looking at the NFL teams that are 
# valued at less than $4 billion, I suggested that they invest in the NFL franchise that
# had the highest year over year value increase, which happened to be the Buffalo Bills.
# With a new stadium coming in 2026 and a franchise quarterback located in a city that loves
# their football, I think that it is a sound investment to purchase the Buffalo Bills for
# $3.4 billion.
# After deciding to invest in the Buffalo Bills, we were left with $600 million to invest
# in another franchise. The only other two franchises to invest in at that price point
# were the Florida Panthers and the Arizona Coyotes. Based on what we saw earlier in our
# analysis, we saw that the NHL had the second highest average year over year value
# increase next to the NFL. This shows that the NHL is growing in popularity which will
# drive revenue and increase the value of the league and individual franchises
# over time. Between the two franchises, the Panthers had a higher year over year 
# value increase compared to the Coyotes. Plus, the Panthers have about half as less debt
# compared to the Coyotes. With a trip to the NHL Stanley Cup Finals last year as an
# eight seed going into the playoffs, I think this investment is a wise one and allows
# us to enter the NHL market.