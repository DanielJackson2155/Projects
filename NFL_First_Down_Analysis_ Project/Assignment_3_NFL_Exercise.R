# NFL Analysis R code -----------------------------------------------------
# Libraries used
library(nflfastR)
library(tidyverse)
library(nflreadr)
library(nflplotR)
library(ggrepel)
library(dplyr)
library(corrplot)
library(glmnet)
library(caret)
library(pROC)
library(PRROC)
library(MASS)
library(class)
library(tree)
library(randomForest)
library(ipred)
library(e1071)

# Read in and clean data --------------------------------------------------
# Years 2018 to 2023
nfl_df = load_pbp(2018:2023)
# Look at unique observations in data frame
unique(nfl_df$season_type)
# Filter only regular season games
nfl_df = subset(nfl_df, season_type == "REG")
dim(nfl_df)
# 281783    372
# Let's look at plays per season
nfl_df %>%
  group_by(season) %>%
  summarize(n = n())

# Exploratory Analysis ----------------------------------------------------
# Find completion percentage for quarterbacks (passers) by season. Who had the 
# highest, the lowest? (Min 100 passes)
# Add completion percentage column
colnames(nfl_df)
unique(nfl_df$passer)
selected_columns = c("season", "passer", "passer_player_id", "pass_attempt", "complete_pass", 
                     "incomplete_pass")
completion_df = nfl_df[,selected_columns]
completion_df = na.omit(completion_df)

# Group by passer, season, and player ID, filter by pass attempts being greater
# than 100, and find completion percentage
complete_percentage_df = completion_df %>%
  group_by(passer, season, passer_player_id) %>%
  filter(sum(pass_attempt) > 100) %>%
  summarise(completion_percentage = sum(complete_pass) / sum(pass_attempt) * 100)

# Find best completion percentage by passer for each year
complete_percentage_df %>%
  arrange(desc(completion_percentage)) %>%
  group_by(season) %>%
  slice(1)
# 2018  Brees   71.1
# 2019  Brees   72.8
# 2020  Rodgers 68.5
# 2021  McCoy   70.5
# 2022  Herbert 64.8
# 2023  Cousins 66.3

# Find worst completion percentage by passer for each year
complete_percentage_df %>%
  arrange(completion_percentage) %>%
  group_by(season) %>%
  slice(1)
# 2018  Allen   48.4
# 2019  Rosen   46.4
# 2020  Hurts   47.8
# 2021  Wilson  49.7
# 2022  Wilson  49.8 
# 2023  Cousins 44.6


# Find total rushing yards and average yards per attempt for all rushers by season. 
# Who had the highest and lowest rushing yards per attempt (min 100 rushes)?
colnames(nfl_df)
selected_columns = c("season", "rusher", "rusher_id", "rush_attempt", "rushing_yards")
rushing_df = nfl_df[,selected_columns]
rushing_df = na.omit(rushing_df)

total_rush_yards_df = rushing_df %>%
  group_by(rusher, season, rusher_id) %>%
  filter(sum(rush_attempt) > 100) %>%
  summarise(total_yards = sum(rushing_yards),
            yards_per_attempt = sum(rushing_yards) / sum(rush_attempt))

# Find highest rushing yards per attempt per year
total_rush_yards_df %>%
  arrange(desc(yards_per_attempt)) %>%
  group_by(season) %>%
  slice(1)
#                 Total Yards     YPA
# 2018  A. Jones      728         5.47 
# 2019  L. Jackson    834         5.96       
# 2020  J.Dobbins     805         6.01
# 2021  R. Penny      749         6.29
# 2022  K. Herbert    731         5.67
# 2023  D. Achane     800         7.77

# Find lowest rushing yards per attempt per year
total_rush_yards_df %>%
  arrange(yards_per_attempt) %>%
  group_by(season) %>%
  slice(1)
#                   Total Yards     YPA
# 2018  L. Blount       418         2.71 
# 2019  P. Barber       470         3.05       
# 2020  J. Kelley       354         3.19
# 2021  M. Ingram       554         3.46
# 2022  Mi. Carter      402         3.53
# 2023  J. Hurts        283         2.60


# Explore first down probability when a team is leading, trailing or tied
# in specific quarters
colnames(nfl_df)
# Create a new variable to represent score outcome (leading, trailing, tied) for
# each play
first_down_df = nfl_df %>%
  mutate(score_outcome = case_when(
    total_home_score > total_away_score ~ "Leading",
    total_home_score < total_away_score ~ "Trailing",
    total_home_score == total_away_score ~ "Tied"
  ))
first_down_df = first_down_df %>% 
  filter(!is.na(first_down))

# Group by team, quarter, and game outcome, then calculate first down probability
first_down_prob_df = first_down_df %>%
  group_by(qtr, score_outcome) %>%
  summarise((first_down_prob = sum(first_down) / n())*100)
first_down_prob_df
# First down probabilities when Home team is either leading, trailing or
# tied per quarter with 5 equaling overtime
#   qtr   game_outcome        first_down_prob
#   1     Leading                25.3
#   1     Tied                   23.2
#   1     Trailing               24.7
#   2     Leading                23.5
#   2     Tied                   22.6
#   2     Trailing               23.3
#   3     Leading                24.5
#   3     Tied                   22.6
#   3     Trailing               24.5
#   4     Leading                22.6
#   4     Tied                   20.7
#   4     Trailing               23.1
#   5     Leading                22.0
#   5     Tied                   19.7
#   5     Trailing               33.7


# For the 2023 season, examine how often the Super Bowl quarterbacks, 
# Brock Purdy and Patrick Mahomes, converted 1st downs on their passes, on 
# 1st, 2nd, 3rd and 4th down.
twenty_three_nfl_df = load_pbp(2023)
twenty_three_nfl_df = subset(twenty_three_nfl_df, season_type == "REG")
mahomes_purdy_df = twenty_three_nfl_df %>%
  filter(passer %in% c("P.Mahomes", "B.Purdy"))
# Remove NAs from first_down_pass, pass_attempt and complete_pass
mahomes_purdy_df = mahomes_purdy_df %>%
  drop_na(first_down_pass, pass_attempt, complete_pass)

# Conversions by downs on pass attempts
mahomes_purdy_df %>%
  group_by(passer, down) %>%
  summarise(first_down_percentage_per_attempt = 
              (sum(first_down_pass) / sum(pass_attempt))*100)
#   Passer      Down    first_down_percentage_per_attempt
#   B.Purdy       1      40.7
#   B.Purdy       2      38.6
#   B.Purdy       3      43.9
#   B.Purdy       4      66.7
#   P.Mahomes     1      29.0
#   P.Mahomes     2      33.3
#   P.Mahomes     3      39.4
#   P.Mahomes     4      27.3

# Conversions by downs on pass completions
mahomes_purdy_df %>%
  group_by(passer, down) %>%
  summarise(first_down_percentage_per_completion = 
              sum((first_down_pass) / sum(complete_pass))*100)
#   Passer      Down    first_down_percentage_per_completion
#   B.Purdy       1      58.7
#   B.Purdy       2      57.3
#   B.Purdy       3      78.1
#   B.Purdy       4      66.7
#   P.Mahomes     1      45.4
#   P.Mahomes     2      46.7
#   P.Mahomes     3      67.0
#   P.Mahomes     4      100.0


# Examine the leading rushers for both teams, Isiah Pacheco and Christian McCaffrey
# and how often they converted runs into 1st downs on 1st, 2nd, 3rd, and 4th.
twenty_three_nfl_df = load_pbp(2023)
twenty_three_nfl_df = subset(twenty_three_nfl_df, season_type == "REG")
pacheco_cmc_df = twenty_three_nfl_df %>%
  filter(rusher %in% c("I.Pacheco", "C.McCaffrey"))
unique(pacheco_cmc_df$rusher_player_name)
# Drop NA values in rushing_attempt first_down_run and down
pacheco_cmc_df = pacheco_cmc_df %>%
  drop_na(first_down_rush, rush_attempt, down)

pacheco_cmc_df %>%
  group_by(rusher, down) %>%
  summarise(first_down_percentage_per_rush = 
              (sum(first_down_rush) / sum(rush_attempt))*100)
# Rusher        Down    first_down_percentage_per_rush
# C.McCaffrey     1       17.2
# C.McCaffrey     2       46.2
# C.McCaffrey     3       57.1
# C.McCaffrey     4       66.7
# I.Pacheco       1       13.1
# I.Pacheco       2       41.0
# I.Pacheco       3       75.0  
# I.Pacheco       4       100.0

# Predictive Model / Probabilistic Machine Learning -----------------------
# Build a predictive model incorporating factors such as field position, down and 
# distance, and time remaining to forecast the probability of a first down. Be sure to 
# filter your data for Regular Season games only (i.e., season_type = “REG”) and
# play_type = “run” or “pass”.
# nfl_df already filtered  from 2018 to 2023
# Now let's filter play_type to run and pass
unique(nfl_df$play_type)
nfl_df = subset(nfl_df, play_type %in% c("pass", "run"))
# Response variable is going to be first_down
unique(nfl_df$first_down)
# Binary response variable:
# 0 for no first down
# 1 for first down.
# This is a classification problem, we will need to run some classification
# regression models

# Let's figure out what predictors we want to use in our models
selected_columns = c("season", "first_down", "play_type", "down", "ydstogo", "yardline_100", 
                     "yards_gained", "pass_location", "air_yards", "yards_after_catch", 
                     "run_location","run_gap", "temp", "wind", "passer",
                     "rusher", "receiver_player_name")
first_down_df = nfl_df[,selected_columns]
colSums(is.na(first_down_df))
# Let's make two data frames, one for play and one for pass 
pass_first_down_df = subset(first_down_df, play_type == "pass")
run_first_down_df = subset(first_down_df, play_type == "run")

# Let's focus on the pass data frame first. Will come back to run later on
# Let's remove rush predictors: run_location, run_gap, rusher
pass_first_down_df = pass_first_down_df[, -which(names(pass_first_down_df) == "run_gap")]
pass_first_down_df = pass_first_down_df[, -which(names(pass_first_down_df) == "run_location")]
pass_first_down_df = pass_first_down_df[, -which(names(pass_first_down_df) == "rusher")]

# Check NAs in columns
colSums(is.na(pass_first_down_df))
# Lot of NAs in receiver player name. Going to remove that column
pass_first_down_df = pass_first_down_df[, -which(names(pass_first_down_df) 
                                                 == "receiver_player_name")]
# I am going to remove pass_location as well
pass_first_down_df = pass_first_down_df[, -which(names(pass_first_down_df)
                                                 == "pass_location")]
colSums(is.na(pass_first_down_df))

# Let's look at values of each column
unique(pass_first_down_df$down)
# Remove 542 obs where first down was NA
pass_first_down_df = pass_first_down_df[complete.cases(pass_first_down_df$down), ]
colSums(is.na(pass_first_down_df))

# Let's look at air_yards values and yards after catch
colSums(is.na(pass_first_down_df))
# There are 7600 NAs in air_yards and 45000 NAs in yards_after_catch.
# I do not want to lose all of those data points. 
unique(first_down_df$air_yards)

hist(pass_first_down_df$air_yards, main = "Distribution of Air Yards")
# Distribution of air yards seems to be right skewed
# Since the median is less sensitive to extreme values in a right-skewed distribution, 
# filling NAs with the median can be a robust choice.
pass_first_down_df$air_yards = ifelse(is.na(pass_first_down_df$air_yards), 
                                 median(pass_first_down_df$air_yards, na.rm = TRUE), 
                                 pass_first_down_df$air_yards)

# Let's look at distribution for yards_after_catch
hist(pass_first_down_df$yards_after_catch, main = "Distribution of Yards After Catch")
# Also right skewed. Let's use median for NA values
pass_first_down_df$yards_after_catch = ifelse(is.na(pass_first_down_df$yards_after_catch), 
                                      median(pass_first_down_df$yards_after_catch, na.rm = TRUE), 
                                      pass_first_down_df$yards_after_catch)

# Let's look at temp
hist(pass_first_down_df$temp, main = "Distribution of Temp")
# temp seems to be someone normal. So let's use average for NA values
pass_first_down_df$temp = ifelse(is.na(pass_first_down_df$temp), 
                                              mean(pass_first_down_df$temp, na.rm = TRUE), 
                                              pass_first_down_df$temp)

# And now wind
hist(pass_first_down_df$wind, main = "Distribution of Wind")
# Wind is right skewed so let's use median for NA values
pass_first_down_df$wind = ifelse(is.na(pass_first_down_df$wind), 
                                      median(pass_first_down_df$wind, na.rm = TRUE), 
                                      pass_first_down_df$wind)

colSums(is.na(pass_first_down_df))

# So now we have no NAs in our pass data frame
# Let's look at correlation matrix of numeric values
corrplot(cor(pass_first_down_df[sapply(pass_first_down_df, is.numeric)], 
             use = "pairwise.complete.obs"), method = "color")
# Yards gained and first down seem to be positively correlated. Which makes sense.

# Let's now make our training and test data
unique(pass_first_down_df$season)
# We have 6 seasons of data. Rather than using 2018-2020 for training data,
# let's use every other year for training data: 2018, 2020 and 2022 and then we
# can use the 2019, 2021, and 2023 data to test our trained models.
# Before we do that, let's remove yards_after_catch, air_yards and yards_gained
# because those are details we will not know when deciding if we will get a first
# down if we pass or run
# Let's also remove play_type because these are all pass plays and we can remove
# passer as well
pass_first_down_df = pass_first_down_df[, -which(names(pass_first_down_df)
                                                 == "play_type")]
pass_first_down_df = pass_first_down_df[, -which(names(pass_first_down_df)
                                                 == "yards_after_catch")]
pass_first_down_df = pass_first_down_df[, -which(names(pass_first_down_df)
                                                 == "air_yards")]
pass_first_down_df = pass_first_down_df[, -which(names(pass_first_down_df)
                                                 == "yards_gained")]
pass_first_down_df = pass_first_down_df[, -which(names(pass_first_down_df)
                                                 == "passer")]
# Look at class of first_down response variable
class(pass_first_down_df$first_down)
# Let's change it from numeric to factor
pass_first_down_df$first_down = as.factor(pass_first_down_df$first_down)
class(pass_first_down_df$first_down)
# Now, it is a factor

# Filter data for training years (2018, 2020, 2022)
train_data = pass_first_down_df[pass_first_down_df$season %in% c(2018, 2020, 2022), ]
test_data = pass_first_down_df[pass_first_down_df$season %in% c(2019, 2021, 2023), ]

# Now that we have our training data and test data, we can fit our models

# First let's change first_down back to numeric to check correlation plot
pass_first_down_df$first_down = as.numeric(as.character(pass_first_down_df$first_down))
class(pass_first_down_df$first_down)

corrplot(cor(pass_first_down_df[sapply(pass_first_down_df, is.numeric)], 
             use = "pairwise.complete.obs"), method = "color")
# ydstogo has slight negative correlation with first_down
# We also see that ydstogo and down have strong negative correlation

# Change first_down back to a factor
pass_first_down_df$first_down = as.factor(pass_first_down_df$first_down)
class(pass_first_down_df$first_down)


# Passing Classification --------------------------------------------------
# First, let's try logistic regression
glm_pass = glm(first_down ~., train_data, family = binomial)
summary(glm_pass)
# We can see that temp is the only predictor that is not statistically
# significant to the model as it's p-value is not less than 0.05.
# Let's remove temp
glm_pass = glm(first_down ~. - temp, train_data, family = binomial)
summary(glm_pass)
# Now all of our predictors are statistically significant
# Let's try our trained model on our test data
# First with probability threshold of 0.5
pred_probs = predict(glm_pass, newdata = test_data, type = "response")
pred_classes = ifelse(pred_probs > 0.5, 1, 0)
pred_classes = as.factor(pred_classes)
conf_matrix = confusionMatrix(data = pred_classes, reference = test_data$first_down)
print(conf_matrix)
# Accuracy: 0.6881
# Sensitivity: 0.9372
# Specificity: 0.1662
# 68.81% accuracy rate.

# Now, with optimal threshold
pred_probs = predict(glm_pass, newdata = test_data, type = "response")
# Create ROC curve
roc_curve = roc(test_data$first_down, pred_probs)
# Find the threshold that maximizes the F1-score
optimal_threshold = coords(roc_curve, "best", maximize = "f1", 
                           ret = "threshold")$threshold
# Classify predictions based on the optimal threshold
pred_classes = ifelse(pred_probs > optimal_threshold, 1, 0)
pred_classes = as.factor(pred_classes)
conf_matrix = confusionMatrix(data = pred_classes, reference = test_data$first_down)
print(conf_matrix)
# Accuracy: 0.6418
# Sensitivity: 0.7161
# Specificity: 0.4863
# Less prediction accuracy but better trade off between sensitivity and specificity
# Let's see if we can improve this model. Let's remove wind, because it is the
# least significant and see what that does to model
glm_pass = glm(first_down ~. - temp - wind, train_data, family = binomial)
summary(glm_pass)
# With optimal threshold
pred_probs = predict(glm_pass, newdata = test_data, type = "response")
# Create ROC curve
roc_curve = roc(test_data$first_down, pred_probs)
# Find the threshold that maximizes the F1-score
optimal_threshold = coords(roc_curve, "best", maximize = "f1", 
                           ret = "threshold")$threshold
# Classify predictions based on the optimal threshold
pred_classes = ifelse(pred_probs > optimal_threshold, 1, 0)
pred_classes = as.factor(pred_classes)
conf_matrix = confusionMatrix(data = pred_classes, reference = test_data$first_down)
print(conf_matrix)
# Accuracy: 0.643
# No change in accuracy rate. Let's try to improve on this model.

# We saw earlier that ydstogo and down were negatively correlated so let's create
# an interaction term between the two to see if that improves our model
glm_pass = glm(first_down ~. - down - ydstogo + down*ydstogo, 
               train_data, family = binomial)
summary(glm_pass)
# temp and down are not statistically significant. Neither is down:ydstogo.
# Let's remove temp and try with just down:ydstogo
glm_pass = glm(first_down ~. - temp - down - ydstogo + down:ydstogo, 
               train_data, family = binomial)
summary(glm_pass)
# yardline_100 is no longer significant
# Going back to prevous model, let's just remove down and temp
glm_pass = glm(first_down ~. - down - temp + down:ydstogo, 
               train_data, family = binomial)
summary(glm_pass)
# Now, all predictors are statistically significant. 
# Let's try test data on it
pred_probs = predict(glm_pass, newdata = test_data, type = "response")
# Create ROC curve
roc_curve = roc(test_data$first_down, pred_probs)
# Find the threshold that maximizes the F1-score
optimal_threshold = coords(roc_curve, "best", maximize = "f1", 
                           ret = "threshold")$threshold
# Classify predictions based on the optimal threshold
pred_classes = ifelse(pred_probs > optimal_threshold, 1, 0)
pred_classes = as.factor(pred_classes)
conf_matrix = confusionMatrix(data = pred_classes, reference = test_data$first_down)
print(conf_matrix)
# Accuracy: 0.6416
# Sensitivity: 0.7157
# Specificity: 0.4864

# We did see a slight positive correlation between yardline_100 and ydstogo
# Let's try an interaction term there to see if it betters are model
glm_pass = glm(first_down ~. - down - temp + down:ydstogo + ydstogo:yardline_100, 
               train_data, family = binomial)
summary(glm_pass)
# All p-values in this model are less than 0.05. Let's try it on test set
pred_probs = predict(glm_pass, newdata = test_data, type = "response")
# Create ROC curve
roc_curve = roc(test_data$first_down, pred_probs)
# Find the threshold that maximizes the F1-score
optimal_threshold = coords(roc_curve, "best", maximize = "f1", 
                           ret = "threshold")$threshold
# Classify predictions based on the optimal threshold
pred_classes = ifelse(pred_probs > optimal_threshold, 1, 0)
pred_classes = as.factor(pred_classes)
conf_matrix = confusionMatrix(data = pred_classes, reference = test_data$first_down)
print(conf_matrix)
print(conf_matrix)
# Accuracy: 0.6417
# Sensitivity: 0.7155
# Specificity: 0.487
# No difference between the last two models.

# Try removing down:ydstogo interaction term and see what happens
glm_pass = glm(first_down ~. - temp + ydstogo:yardline_100, 
               train_data, family = binomial)
summary(glm_pass)
# All predictors are statistically significant
pred_probs = predict(glm_pass, newdata = test_data, type = "response")
# Create ROC curve
roc_curve = roc(test_data$first_down, pred_probs)
# Find the threshold that maximizes the F1-score
optimal_threshold = coords(roc_curve, "best", maximize = "f1", 
                           ret = "threshold")$threshold
# Classify predictions based on the optimal threshold
pred_classes = ifelse(pred_probs > optimal_threshold, 1, 0)
pred_classes = as.factor(pred_classes)
conf_matrix = confusionMatrix(data = pred_classes, reference = test_data$first_down)
print(conf_matrix)
print(conf_matrix)
# Accuracy: 0.6412
# Sensitivity: 0.7139
# Specificity: 0.4888
# No difference either.

# Let's go back to this model 
glm_pass = glm(first_down ~. - down - temp + down:ydstogo, 
               train_data, family = binomial)
summary(glm_pass)
# And let this be our basis of comparison for our other models that we will fit

# Let's revisit pass data distributions
hist(pass_first_down_df$down, main = "Distribution of Down")

hist(pass_first_down_df$ydstogo, main = "Distribution of Yards to Go")
# Looks skewed left. Let's try square root transformation to see if that
# helps normalize distribution
hist(sqrt(pass_first_down_df$ydstogo), main = "Distribution of sqrt(Yards to Go)")
# Definitely helps normalize it

hist(pass_first_down_df$yardline_100, main = "Distribution of Yardline")
# Looks skewed left again. Let's try square root transformation again
hist(sqrt(pass_first_down_df$yardline_100), main = "Distribution of sqrt(Yardline)")
# That made it skewed right. Let's try log transformation
hist(log(pass_first_down_df$yardline_100), main = "Distribution of log(Yardline)")
# That also made it more skewed right.
# Try squared transformation
hist((pass_first_down_df$yardline_100)^2, main = "Distribution of Yardline^2")
# Let's leave it as it was

hist(pass_first_down_df$wind, main = "Distribution of Wind")
# Looks relatively normal

# Let's try our logistic regression model one more time with the
# transformed Yards to Go predictor to see if it helps
# We will go off of glm_pass_2 model and sqrt the yds to go 
glm_pass = glm(first_down ~. - down - temp + down:sqrt(ydstogo)^2, 
               train_data, family = binomial)
summary(glm_pass)
# All p-values are less than 0.05
# Let's test it out on test data
pred_probs = predict(glm_pass, newdata = test_data, type = "response")
# Create ROC curve
roc_curve = roc(test_data$first_down, pred_probs)
# Find the threshold that maximizes the F1-score
optimal_threshold = coords(roc_curve, "best", maximize = "f1", 
                           ret = "threshold")$threshold
# Classify predictions based on the optimal threshold
pred_classes = ifelse(pred_probs > optimal_threshold, 1, 0)
pred_classes = as.factor(pred_classes)
conf_matrix = confusionMatrix(data = pred_classes, reference = test_data$first_down)
print(conf_matrix)
# Accuracy: 0.6421
# Sensitivity: 0.7169
# Specificity: 0.4855
# No major statistical change in prediction rate. We will stick with our current
# logistic regression model:
glm_pass = glm(first_down ~. - down - temp + down:ydstogo, 
               train_data, family = binomial)
summary(glm_pass)


# Now let's try Linear Discriminant Analysis 
# With all predictors
lda_pass = lda(first_down ~., train_data)
summary(lda_pass)
lda_pred = predict(lda_pass, test_data)
lda_class = lda_pred$class
lda_class = as.factor(lda_class)
conf_matrix = confusionMatrix(data = lda_class, reference = test_data$first_down)
print(conf_matrix)
# Accuracy: 0.6879
# Sensitivity: 0.9461
# Specificity: 0.1467
# This model performed better than our logistic regression model
lda_pass$scaling
#               LD1
# season       -0.040657211
# down          0.026826779
# ydstogo      -0.263048772
# yardline_100  0.006854927
# temp          0.003182821
# wind         -0.013190579

# Now try with predictors we chose in our logistic regression model
lda_pass = lda(first_down ~. - down - temp + down:ydstogo, train_data)
summary(lda_pass)
lda_pred = predict(lda_pass, test_data)
lda_class = lda_pred$class
lda_class = as.factor(lda_class)
conf_matrix = confusionMatrix(data = lda_class, reference = test_data$first_down)
print(conf_matrix)
# Accuracy: 0.6878
# Sensitivity: 0.9474
# Specificity: 0.1438
# No major statistical change.


# Let's try a Quadratic Descriptive Analysis
qda_pass = qda(first_down ~., train_data)
summary(qda_pass)
qda_pred = predict(qda_pass, test_data)
qda_class = qda_pred$class
qda_class = as.factor(qda_class)
conf_matrix = confusionMatrix(data = qda_class, reference = test_data$first_down)
print(conf_matrix)
# Accuracy: 0.6765
# Sensitivity: 0.9136
# Specificity: 0.1797
# No big change in accuracy. Performed slightly worse than LDA

# Now try with predictors we chose in our logistic regression model
qda_pass = qda(first_down ~.- down - temp + down:ydstogo, train_data)
summary(qda_pass)
qda_pred = predict(qda_pass, test_data)
qda_class = qda_pred$class
qda_class = as.factor(qda_class)
conf_matrix = confusionMatrix(data = qda_class, reference = test_data$first_down)
print(conf_matrix)
# These models are having a hard time predicting first downs compared to non-
# first downs


# Try K-Nearest Neighbor model.
# Since we have a binary classification variable, we will use K = 3, 5 and 7
# to avoid ties.
# Predictor variables used in glm_pass

# K = 3
set.seed(1)
knn_pass = knn(train_data, test_data, train_data$first_down, k = 3)
# Use confusion matrix to test predictions
conf_matrix = confusionMatrix(data = knn_pass, reference = test_data$first_down)
print(conf_matrix)

# Accuracy: 0.8753
# Sensitivity: 0.9351
# Specificity: 0.7499
# Best model so far!

# K = 5
set.seed(1)
knn_pass = knn(train_data, test_data, train_data$first_down, k = 5)
# Use confusion matrix to test predictions
conf_matrix = confusionMatrix(data = knn_pass, reference = test_data$first_down)
print(conf_matrix)
# Accuracy: 0.8735
# Sensitivity: 0.9457
# Specificity: 0.7222
# Same prediction rate as K = 1

# K = 7
set.seed(1)
knn_pass = knn(train_data, test_data, train_data$first_down, k = 7)
# Use confusion matrix to test predictions
conf_matrix = confusionMatrix(data = knn_pass, reference = test_data$first_down)
print(conf_matrix)
# Accuracy: 0.8715
# Sensitivity: 0.9527
# Specificity: 0.7013
# All K-nearest neighbors models performed better than any previous model that
# we fit. Since K = 3, 5, and 7 all produced similar prediction rates, we will
# use K = 3 as our best model so far since it is more interpretable than K = 5
# or 7.


# Let's try a classification tree to see if that will perform better
tree_pass = tree(first_down ~., train_data)
summary(tree_pass)
# Only used ydstogo variables
# 2 terminal nodes
plot(tree_pass)
text(tree_pass, pretty = 0)
pred_tree = predict(tree_pass, test_data, type = "class")
conf_matrix = confusionMatrix(data = pred_tree, reference = test_data$first_down)
print(conf_matrix)
# Accuracy: 0.6769
# Sensitivity: 1
# Specificity: 0
# This is our worst model so far. Which makes sense, it is only using one
# predictor.

# Try Naive Bayes model 
nb_pass = naiveBayes(first_down ~., train_data)
nb_pass
pred_nb = predict(nb_pass, test_data)
conf_matrix = confusionMatrix(data = pred_nb, reference = test_data$first_down)
print(conf_matrix)
# Accuracy: 0.6791
# Sensitivity: 0.9282
# Specificity: 0.1571
# Not as good as KNN model.

# Try randomForest model with 25 trees
set.seed(1)
rf_pass = randomForest(first_down ~., train_data, mtry = 6, ntree = 25,
                      importance = TRUE)
rf_pass
pred_rf = predict(rf_pass, test_data, type = "class")
pred_rf = as.factor(pred_rf)
conf_matrix = confusionMatrix(data = pred_rf, reference = test_data$first_down)
print(conf_matrix)
# Accuracy: 0.6407
# Sensitivity: 0.8219
# Specificity: 0.2611
# Not as good as KNN model. 


# Try Bagging model
set.seed(1)
bag_pass = bagging(first_down ~., data = train_data)
bag_pass
pred_bag = predict(bag_pass, test_data, type = "class")
pred_bag = as.factor(pred_bag)
conf_matrix = confusionMatrix(data = pred_bag, reference = test_data$first_down)
print(conf_matrix)
# Accuracy: 0.6467
# Sensitivity: 0.8342
# Specificity: 0.2539
# Not as good as KNN model


# After fitting several models, I do not think that we will get a better model
# than 87.53% prediction rate that we got from our KNN model when K = 3.
set.seed(1)
knn_pass = knn(train_data, test_data, train_data$first_down, k = 3)
# Use confusion matrix to test predictions
conf_matrix = confusionMatrix(data = knn_pass, reference = test_data$first_down)
print(conf_matrix)

# Let's call training and test data train_pass and test_pass for later
train_pass = pass_first_down_df[pass_first_down_df$season %in% c(2018, 2020, 2022), ]
test_pass = pass_first_down_df[pass_first_down_df$season %in% c(2019, 2021, 2023), ]



# Running Classification --------------------------------------------------
# Next, we will work on model for running data
# Select predictors we want to use based on our previous insights we gained from
# our passing first down analysis
selected_columns = c("season", "first_down", "play_type", "down", "ydstogo", "yardline_100", 
                     "yards_gained", "run_location","run_gap", "temp", "wind")
first_down_df = nfl_df[,selected_columns]
# Subset to all plays being a run
run_first_down_df = subset(first_down_df, play_type == "run")
# Change response variable first_down to a factor
run_first_down_df$first_down = as.factor(run_first_down_df$first_down)
# Check for NA values
colSums(is.na(run_first_down_df))
# Let's remove run_location and run_gap predictors
run_first_down_df = run_first_down_df[, -which(names(run_first_down_df) == "run_gap")]
run_first_down_df = run_first_down_df[, -which(names(run_first_down_df) == "run_location")]
# Let's also remove the observations that have NA for down. Only 217 of them
run_first_down_df = run_first_down_df[complete.cases(run_first_down_df$down), ]
# Check for NA values
colSums(is.na(run_first_down_df))
# We have 41k NA values in temp and wind. Let's do the same thing we did in the
# passing data frame to fill in those NA values. We may not even use the predictors
# in our data frame, but let's fill in those NA values and see if they have more 
# statistical significance in predicting first downs for run plays compared to pass
# plays. You would not think so but we will see
run_first_down_df$temp = ifelse(is.na(run_first_down_df$temp), 
                                 mean(run_first_down_df$temp, na.rm = TRUE), 
                                 run_first_down_df$temp)
run_first_down_df$wind = ifelse(is.na(run_first_down_df$wind), 
                                mean(run_first_down_df$wind, na.rm = TRUE), 
                                run_first_down_df$wind)
# Check for NA values
colSums(is.na(run_first_down_df))
# No more NA values in run data frame

# Let's look at correlation between predictors in data frame
# Change first down back to numeric
# We can remove play_type as well since all are running plays
run_first_down_df = run_first_down_df[, -which(names(run_first_down_df) == "play_type")]
# Let's also remove yards_gained
run_first_down_df = run_first_down_df[, -which(names(run_first_down_df) == "yards_gained")]
# check to see if first_down is factor
class(run_first_down_df$first_down)
# Turn first down into numeric class for correlation plot
run_first_down_df$first_down = as.numeric(as.character(run_first_down_df$first_down))
corrplot(cor(run_first_down_df[sapply(run_first_down_df, is.numeric)], 
             use = "pairwise.complete.obs"), method = "color")
# down and first_down are positively correlated
# ytdstogo and first down are negatively correlated
# ydstogo and down are very much negatively correlated

# Let's look at distributions for each predictor
# Down
hist(run_first_down_df$down, main = "Distribution of Down")
# Way more first downs than fourth downs, which makes sense

# Yards to go
hist(run_first_down_df$ydstogo, main = "Distribution of Yards to Go")
# Seems to be skewed left
# Lot of frequency at 10 yards. Which makes sense. Every new set of downs starts
# with 10 yards to go
# Try sqrt transformation to it
hist(sqrt(run_first_down_df$ydstogo), main = "Distribution of sqrt(Yards to Go)")
# Not much of a difference
# Try log transformation
hist(log(run_first_down_df$ydstogo), main = "Distribution of log(Yards to Go)")
# Not much better either
# Let's just leave this predictor as it is

# yardline_100
hist(run_first_down_df$yardline_100, main = "Distribution of Yard Line")
# Skewed a little left
# Try sqrt transformation
hist(sqrt(run_first_down_df$yardline_100), main = "Distribution of sqrt(Yard Line)")
# No improvement. Skewed more right now
# Try log transformation
hist(log(run_first_down_df$yardline_100), main = "Distribution of log(Yard Line)")
# Not much better either.
# Try squared
hist((run_first_down_df$yardline_100)^2, main = "Distribution of squared Yard Line)")
# Not much better.
# Let's also not transform this predictor

# temp
hist(run_first_down_df$temp, main = "Distribution of Temperature")
# Looks normal

# wind
hist(run_first_down_df$wind, main = "Distribution of Wind")
# Looks relatively normal

# Now, let's set our training and test data. We will do the same thing that we did
# for our passing data
# Let's make first_down a factor again
run_first_down_df$first_down = as.factor(run_first_down_df$first_down)
train_data = run_first_down_df[run_first_down_df$season %in% c(2018, 2020, 2022), ]
test_data = run_first_down_df[run_first_down_df$season %in% c(2019, 2021, 2023), ]

# Let's try logistic regression on model
glm_run = glm(first_down ~., train_data, family = "binomial")
summary(glm_run)
# The intercept, season, temp and wind are not statistically significant. Let's
# create the interaction term down:ydstogo and see how that changes our model,
# since they are heavilty correlated
glm_run = glm(first_down ~. - down - ydstogo + ydstogo:down, 
              train_data, family = "binomial")
summary(glm_run)
# Now, season, yardline_100 and wind are not statistically significant
# Remove wind
glm_run = glm(first_down ~. - wind - down - ydstogo + ydstogo:down, 
              train_data, family = "binomial")
summary(glm_run)
# Now, just season and intercept are not significant
# Let's remove season
glm_run = glm(first_down ~. - season - wind - down - ydstogo + ydstogo:down, 
              train_data, family = "binomial")
summary(glm_run)
# Now all predictors are significant
# Let's try model on test data
pred_probs = predict(glm_run, newdata = test_data, type = "response")
# Create ROC curve
roc_curve = roc(test_data$first_down, pred_probs)
# Find the threshold that maximizes the F1-score
optimal_threshold = coords(roc_curve, "best", maximize = "f1", 
                           ret = "threshold")$threshold
# Classify predictions based on the optimal threshold
pred_classes = ifelse(pred_probs > optimal_threshold, 1, 0)
pred_classes = as.factor(pred_classes)
conf_matrix = confusionMatrix(data = pred_classes, reference = test_data$first_down)
print(conf_matrix)
# Accuracy: 0.7776
# Sensitivity: 0.8654
# Specificity: 0.5220

# Since we did not want to transform any predictors earlier, let's see if any
# more interaction terms can be created
# Current model:
glm_run = glm(first_down ~. - season - wind - down - ydstogo + ydstogo:down, 
              train_data, family = "binomial")
summary(glm_run)
# Let's remove temp
glm_run = glm(first_down ~. - temp - season - wind - down - ydstogo + ydstogo:down, 
              train_data, family = "binomial")
summary(glm_run)
# Predictors is still statistically significant
# Try predicting test data again
pred_probs = predict(glm_run, newdata = test_data, type = "response")
# Create ROC curve
roc_curve = roc(test_data$first_down, pred_probs)
# Find the threshold that maximizes the F1-score
optimal_threshold = coords(roc_curve, "best", maximize = "f1", 
                           ret = "threshold")$threshold
# Classify predictions based on the optimal threshold
pred_classes = ifelse(pred_probs > optimal_threshold, 1, 0)
pred_classes = as.factor(pred_classes)
conf_matrix = confusionMatrix(data = pred_classes, reference = test_data$first_down)
print(conf_matrix)
# Accuracy: 0.777
# Sensitivity: 0.8615
# Specificity: 0.5313
# More simple model and no difference in prediction accuracy.

# Let's clean up syntax of current model
glm_run = glm(first_down ~ yardline_100 + down:ydstogo, train_data, family = "binomial")
summary(glm_run)

# Let's let this model be our basis of comparison to the other models that we fit.


# Let's try LDA
lda_run = lda(first_down ~., train_data)
summary(lda_run)
lda_pred = predict(lda_run, test_data)
lda_class = lda_pred$class
lda_class = as.factor(lda_class)
conf_matrix = confusionMatrix(data = lda_class, reference = test_data$first_down)
print(conf_matrix)
# Accuracy: 0.8023
# Sensitivity: 0.9154
# Specificity: 0.4735
lda_run$scaling
#                 LD1
# season       -0.013112426
# down          0.502048279
# ydstogo      -0.226701840
# yardline_100  0.002244006
# temp         -0.001751457
# wind          0.002141318
# Better prediction rate than our logistic regression model.

# Try LDA with predictors we used in our logistic regression model
lda_run = lda(first_down ~ yardline_100 + down:ydstogo, train_data)
summary(lda_run)
lda_pred = predict(lda_run, test_data)
lda_class = lda_pred$class
lda_class = as.factor(lda_class)
conf_matrix = confusionMatrix(data = lda_class, reference = test_data$first_down)
print(conf_matrix)
# Accuracy: 0.7441
# Sensitivity: 1
# Specificity: 0
# This model did not fit well. We did not predict any first downs. Not good.


# Let's try QDA
qda_run = qda(first_down ~., train_data)
summary(qda_run)
lda_pred = predict(qda_run, test_data)
lda_class = lda_pred$class
lda_class = as.factor(lda_class)
conf_matrix = confusionMatrix(data = lda_class, reference = test_data$first_down)
print(conf_matrix)
# Accuracy: 0.8015
# Sensitivity: 0.9211
# Specificity: 0.4538

# Try QDA with predictors we used in our logistic regression model
qda_run = qda(first_down ~ yardline_100 + down:ydstogo, train_data)
summary(qda_run)
lda_pred = predict(qda_run, test_data)
lda_class = lda_pred$class
lda_class = as.factor(lda_class)
conf_matrix = confusionMatrix(data = lda_class, reference = test_data$first_down)
print(conf_matrix)
# Accuracy: 0.7441
# Sensitivity: 1
# Specificity: 0
# Same as LDA, we did not predict a single first down. Not good!


# Try K-nearest neighbors model
# We will use K = 3, 5 and 7 just like we did in our passing analysis
set.seed(1)
knn_run = knn(train_data, test_data, train_data$first_down, k = 3)
# Use confusion matrix to test predictions
conf_matrix = confusionMatrix(data = knn_run, reference = test_data$first_down)
print(conf_matrix)
# Accuracy: 0.9063
# Sensitivity: 0.9580
# Specificity: 0.7561
# Best model so far for our running analysis!

# Try K = 5
set.seed(1)
knn_run = knn(train_data, test_data, train_data$first_down, k = 5)
# Use confusion matrix to test predictions
conf_matrix = confusionMatrix(data = knn_run, reference = test_data$first_down)
print(conf_matrix)
# Accuracy: 0.9041
# Sensitivity: 0.9620
# Specificity: 0.7357
# Very similar results to our K = 3 model.

# Try K = 7
set.seed(1)
knn_run = knn(train_data, test_data, train_data$first_down, k = 7)
# Use confusion matrix to test predictions
conf_matrix = confusionMatrix(data = knn_run, reference = test_data$first_down)
print(conf_matrix)
# Accuracy: 0.8948
# Sensitivity: 0.9617
# Specificity: 0.7002
# Very similar results to our K = 3 and K = 5 models.

# Since the prediction rates are all similar, we will go with simpler model where
# K = 3.
# This is the best model so far.
set.seed(1)
knn_run = knn(train_data, test_data, train_data$first_down, k = 3)
# Use confusion matrix to test predictions
conf_matrix = confusionMatrix(data = knn_run, reference = test_data$first_down)
print(conf_matrix)


# Let's try a classification tree to see how well that model performs
tree_run = tree(first_down ~., train_data)
summary(tree_run)
# Only one predictors used: yds_to_go
# 3 terminal nodes
plot(tree_run)
text(tree_run, pretty = 0)
# Since this model is only using one predictor, we probably will not use it, but let's
# predict test data to see how well the prediction rate is
pred_tree = predict(tree_run, test_data, type = "class")
conf_matrix = confusionMatrix(data = pred_tree, reference = test_data$first_down)
print(conf_matrix)
# Accuracy: 0.8005
# Sensitivity: 0.9409
# Specificity: 0.3920
# Best prediction rate so far, but we will not use this model as it is only factoring in
# one predictor in it's tree.

# Let's try a randomForest model
# Try 25 trees
set.seed(1)
rf_run = randomForest(first_down ~., train_data, mtry = 6, ntree = 25,
                        importance = TRUE)
rf_run
pred_rf = predict(rf_run, test_data, type = "class")
pred_rf = as.factor(pred_rf)
conf_matrix = confusionMatrix(data = pred_rf, reference = test_data$first_down)
print(conf_matrix)
# Accuracy: 0.7727
# Sensitivity: 0.8936
# Specificity: 0.4213
# Not better than K = 3 model.

# Try Bagging model
set.seed(1)
bag_run = bagging(first_down ~., data = train_data)
bag_run
pred_bag = predict(bag_run, test_data, type = "class")
pred_bag = as.factor(pred_bag)
conf_matrix = confusionMatrix(data = pred_bag, reference = test_data$first_down)
print(conf_matrix)
# Accuracy: 0.7822
# Sensitivity: 0.8994
# Specificity: 0.4415

# After fitting several models, the K = 1 model had the best prediction percentage 
# of 90.63%.
# Comparing this to our passing analysis, this is less than the the passing data
# prediction percentage that we got doing logistic regression.

# Now that we have our two chose models, we can now assess some different situations
# to decide if we should pass or throw.

# We should mention that our KNN models with K = 3 produced the best prediction
# percentages!

# Let's call our training data train_run and test data test_run for the next analysis
train_run = run_first_down_df[run_first_down_df$season %in% c(2018, 2020, 2022), ]
test_run = run_first_down_df[run_first_down_df$season %in% c(2019, 2021, 2023), ]

# Outside of Red Zone Area -------------------------------------------------
# Outside of the red zone area (more than 20 yards from the goal line), 
# do you prefer to run or pass?
# Outside of red zone area means that are starting yardline for the play
# is less than 80 
# Let's look at different scenarios under this assumption.
# Here are the models we will be using:
# For pass:
train_pass = pass_first_down_df[pass_first_down_df$season %in% c(2018, 2020, 2022), ]
test_pass = pass_first_down_df[pass_first_down_df$season %in% c(2019, 2021, 2023), ]
knn_pass = knn(train_pass, test_pass, train_pass$first_down, k = 3)

# For run:
train_run = run_first_down_df[run_first_down_df$season %in% c(2018, 2020, 2022), ]
test_run = run_first_down_df[run_first_down_df$season %in% c(2019, 2021, 2023), ]
knn_run = knn(train_run, test_run, train_run$first_down, k = 3)


# 3rd and short (3 or less yards to go)
# Let's create new training/test data that is outside of red zone area, on third down,
# with 3 or less yards to go.
# Pass:
subset_train_pass = subset(train_pass, down == 3 & ydstogo <= 3 & yardline_100 < 80)
subset_test_pass = subset(test_pass, down == 3 & ydstogo <= 3 & yardline_100 < 80)
subset_knn_pass = knn(subset_train_pass, subset_test_pass, 
                      subset_train_pass$first_down, k = 3)
conf_matrix = confusionMatrix(data = subset_knn_pass, 
                              reference = subset_test_pass$first_down)
print(conf_matrix)

# Run:
subset_train_run = subset(train_run, down == 3 & ydstogo <= 3 & yardline_100 < 80)
subset_test_run = subset(test_run, down == 3 & ydstogo <= 3 & yardline_100 < 80)
subset_knn_run = knn(subset_train_run, subset_test_run, 
                      subset_train_run$first_down, k = 3)
conf_matrix = confusionMatrix(data = subset_knn_run, 
                              reference = subset_test_run$first_down)
print(conf_matrix)

# Each confusion matrix produces a accuracy rate of approximately 81%
# With that being said, the sensitivity for predicting first downs when passing the ball
# is 81.5% and the sensitivity for predicting first downs when running the ball is 
# 81.4%. I think you can choose to either pass or run the ball confidently knowing it
# is the same odds either way, based on our model.


# 3rd and long (7 or more yards to go)
# Let's create new training/test data that is outside of red zone area, on third down,
# with 7 or more yards to go.
# Pass:
subset_train_pass = subset(train_pass, down == 3 & ydstogo > 7 & yardline_100 < 80)
subset_test_pass = subset(test_pass, down == 3 & ydstogo > 7 & yardline_100 < 80)
subset_knn_pass = knn(subset_train_pass, subset_test_pass, 
                      subset_train_pass$first_down, k = 3)
conf_matrix = confusionMatrix(data = subset_knn_pass, 
                              reference = subset_test_pass$first_down)
print(conf_matrix)

# Run:
subset_train_run = subset(train_run, down == 3 & ydstogo > 7 & yardline_100 < 80)
subset_test_run = subset(test_run, down == 3 & ydstogo > 7 & yardline_100 < 80)
subset_knn_run = knn(subset_train_run, subset_test_run, 
                     subset_train_run$first_down, k = 3)
conf_matrix = confusionMatrix(data = subset_knn_run, 
                              reference = subset_test_run$first_down)
print(conf_matrix)

# The passing model has a prediction rate of 83% while the running model has a
# prediction rate of 73%.
# The sensitivity for predicting a first down when passing the ball is 75% while
# the sensitivity for predicting a first down when running the ball is 34%. Therefore,
# I would pass the ball in this situation.


# 4th and short (3 or less yards to go)
# Let's create new training/test data that is outside of red zone area, on fourth down,
# with 3 or less yards to go.
# Pass:
subset_train_pass = subset(train_pass, down == 4 & ydstogo <= 3 & yardline_100 < 80)
subset_test_pass = subset(test_pass, down == 4 & ydstogo <= 3 & yardline_100 < 80)
subset_knn_pass = knn(subset_train_pass, subset_test_pass, 
                      subset_train_pass$first_down, k = 3)
conf_matrix = confusionMatrix(data = subset_knn_pass, 
                              reference = subset_test_pass$first_down)
print(conf_matrix)

# Run:
subset_train_run = subset(train_run, down == 4 & ydstogo <= 3 & yardline_100 < 80)
subset_test_run = subset(test_run, down == 4 & ydstogo <= 3 & yardline_100 < 80)
subset_knn_run = knn(subset_train_run, subset_test_run, 
                     subset_train_run$first_down, k = 3)
conf_matrix = confusionMatrix(data = subset_knn_run, 
                              reference = subset_test_run$first_down)
print(conf_matrix)

# The accuracy rate of the passing model is 67% while the accuracy rate of the running
# model is 75%.
# The passing models sensitivity is 69% when predicting first downs compared to the
# running models sensitivity of 78%. Since the running model is more accurate in this
# down and distance scenarios and has a higher sensitivity when predicting first downs,
# I would run the ball here.


# 4th and long (7 or more yards to go)
# Let's create new training/test data that is outside of red zone area, on fourth down,
# with 7 or more yards to go.
# Pass:
subset_train_pass = subset(train_pass, down == 4 & ydstogo > 7 & yardline_100 < 80)
subset_test_pass = subset(test_pass, down == 4 & ydstogo > 7 & yardline_100 < 80)
subset_knn_pass = knn(subset_train_pass, subset_test_pass, 
                      subset_train_pass$first_down, k = 3)
conf_matrix = confusionMatrix(data = subset_knn_pass, 
                              reference = subset_test_pass$first_down)
print(conf_matrix)

# Run:
subset_train_run = subset(train_run, down == 4 & ydstogo > 7 & yardline_100 < 80)
subset_test_run = subset(test_run, down == 4 & ydstogo > 7 & yardline_100 < 80)
subset_knn_run = knn(subset_train_run, subset_test_run, 
                     subset_train_run$first_down, k = 3)
conf_matrix = confusionMatrix(data = subset_knn_run, 
                              reference = subset_test_run$first_down)
print(conf_matrix)

# Our passing model here has an accuracy percentage of 69% while our running model
# has an accuracy percentage of 52%. Our passing models sensitivity when predicting
# first downs is 33% while the running models is 17%. If we had to go for it here,
# I would pass the ball over running the ball. But, I would maybe think about punting.
# I did not factor punts into my model.


# Red Zone ----------------------------------------------------------------
# For the four situations above, how does your answer change in the Red Zone 
# (Between 4 and 20 yards from the goal line)?

# 3rd and short (3 or less yards to go)
#Pass:
subset_train_pass = subset(train_pass, down == 3 & ydstogo <= 3 &
                             yardline_100 >= 80 & yardline_100 <= 96)
subset_test_pass = subset(test_pass, down == 3 & ydstogo <= 3 
                          & yardline_100 >= 80 & yardline_100 <= 96)
subset_knn_pass = knn(subset_train_pass, subset_test_pass, 
                      subset_train_pass$first_down, k = 3)
conf_matrix = confusionMatrix(data = subset_knn_pass, 
                              reference = subset_test_pass$first_down)
print(conf_matrix)

# Run:
subset_train_run = subset(train_run, down == 3 & ydstogo <= 3 & 
                            yardline_100 >= 80 & yardline_100 <= 96)
subset_test_run = subset(test_run, down == 3 & ydstogo <= 3 & 
                           yardline_100 >= 80 & yardline_100 <= 96)
subset_knn_run = knn(subset_train_run, subset_test_run, 
                     subset_train_run$first_down, k = 3)
conf_matrix = confusionMatrix(data = subset_knn_run, 
                              reference = subset_test_run$first_down)
print(conf_matrix)

# Passing model has accuracy rate of 72% and running model has accuracy rate of
# 80%. The passing models sensitivity when predicting first downs is 73% while the
# running models sensitivity when predicting first downs is 80%. Based on those
# percentages, I would run the ball in this situation.


# 3rd and long (7 or more yards to go)
# Pass:
subset_train_pass = subset(train_pass, down == 3 & ydstogo > 7 &
                             yardline_100 >= 80 & yardline_100 <= 96)
subset_test_pass = subset(test_pass, down == 3 & ydstogo > 7 & 
                            yardline_100 >= 80 & yardline_100 <= 96)
subset_knn_pass = knn(subset_train_pass, subset_test_pass, 
                      subset_train_pass$first_down, k = 3)
conf_matrix = confusionMatrix(data = subset_knn_pass, 
                              reference = subset_test_pass$first_down)
print(conf_matrix)

# Run:
subset_train_run = subset(train_run, down == 3 & ydstogo > 7 & 
                            yardline_100 >= 80 & yardline_100 <= 96)
subset_test_run = subset(test_run, down == 3 & ydstogo > 7 & 
                           yardline_100 >= 80 & yardline_100 <= 96)
subset_knn_run = knn(subset_train_run, subset_test_run, 
                     subset_train_run$first_down, k = 3)
conf_matrix = confusionMatrix(data = subset_knn_run, 
                              reference = subset_test_run$first_down)
print(conf_matrix)
# The passing model had a prediction rate of 81% and the running model had a prediction
# rate of 87%.
# The sensitivity of the passing model when predicting first downs was 63% while the
# running model had a sensitivity of 0%.
# That means the running model had a tough time predicting first downs.
# In this case, I would pass the ball.


# 4th and short (3 or less yards to go)
# Pass:
subset_train_pass = subset(train_pass, down == 4 & ydstogo <= 3 &
                             yardline_100 >= 80 & yardline_100 <= 96)
subset_test_pass = subset(test_pass, down == 4 & ydstogo <= 3 & 
                            yardline_100 >= 80 & yardline_100 <= 96)
subset_knn_pass = knn(subset_train_pass, subset_test_pass, 
                      subset_train_pass$first_down, k = 3)
conf_matrix = confusionMatrix(data = subset_knn_pass, 
                              reference = subset_test_pass$first_down)
print(conf_matrix)

# Run:
subset_train_run = subset(train_run, down == 4 & ydstogo <= 3 & 
                            yardline_100 >= 80 & yardline_100 <= 96)
subset_test_run = subset(test_run, down == 4 & ydstogo <= 3 & 
                           yardline_100 >= 80 & yardline_100 <= 96)
subset_knn_run = knn(subset_train_run, subset_test_run, 
                     subset_train_run$first_down, k = 3)
conf_matrix = confusionMatrix(data = subset_knn_run, 
                              reference = subset_test_run$first_down)
print(conf_matrix)
# The passing model had a prediction rate of 0.6% while the running model
# had a prediction rate of 82%.
# The passing model had also had a sensitivity of 0.6% when predicting first downs
# while the running model also had a sensitivity of 82% when predicting first downs.
# Not many sample sizes at all for these models. 
# However, based on our training and test data, I would say to run it.


# 4th and long (7 or more yards to go)
# Pass:
subset_train_pass = subset(train_pass, down == 4 & ydstogo > 7 &
                             yardline_100 >= 80 & yardline_100 <= 96)
subset_test_pass = subset(test_pass, down == 4 & ydstogo > 7 & 
                            yardline_100 >= 80 & yardline_100 <= 96)
subset_knn_pass = knn(subset_train_pass, subset_test_pass, 
                      subset_train_pass$first_down, k = 3)
conf_matrix = confusionMatrix(data = subset_knn_pass, 
                              reference = subset_test_pass$first_down)
print(conf_matrix)

# Run:
subset_train_run = subset(train_run, down == 4 & ydstogo > 7 & 
                            yardline_100 >= 80 & yardline_100 <= 96)
subset_test_run = subset(test_run, down == 4 & ydstogo > 7 & 
                           yardline_100 >= 80 & yardline_100 <= 96)
subset_knn_run = knn(subset_train_run, subset_test_run, 
                     subset_train_run$first_down, k = 3)
conf_matrix = confusionMatrix(data = subset_knn_run, 
                              reference = subset_test_run$first_down)
print(conf_matrix)
# Passing model had prediction rate of 55% while the running model had prediction rate
# of 1. However, there were only 5 observations in our training data and 4 in our test
# data for our running model. There was more data, although not a lot, in our passing
# training and test data. Neither were able ot predict a first down. 
# Therefore, I would say to pass the ball in this situation.


# Goal Line ---------------------------------------------------------------
# How about 3rd and short at the goal line (three or less yards from the goal line)?
#Pass:
subset_train_pass = subset(train_pass, down == 3 & ydstogo <= 3 &
                             yardline_100 >= 97)
subset_test_pass = subset(test_pass, down == 3 & ydstogo <= 3 
                          & yardline_100 >= 97)
subset_knn_pass = knn(subset_train_pass, subset_test_pass, 
                      subset_train_pass$first_down, k = 3)
conf_matrix = confusionMatrix(data = subset_knn_pass, 
                              reference = subset_test_pass$first_down)
print(conf_matrix)

# Run:
subset_train_run = subset(train_run, down == 3 & ydstogo <= 3 & 
                            yardline_100 >= 97)
subset_test_run = subset(test_run, down == 3 & ydstogo <= 3 & 
                           yardline_100 >= 97)
subset_knn_run = knn(subset_train_run, subset_test_run, 
                     subset_train_run$first_down, k = 3)
conf_matrix = confusionMatrix(data = subset_knn_run, 
                              reference = subset_test_run$first_down)
print(conf_matrix)
# Not enough observations in our models to train and test the data in both the
# passing and running model at the goal line when it is third and short.
# Not enough observations in data to make a prediction.
# Therefore, I will go off of what we said in the third and short between the
# 4 yard line and the 20, and run the ball.


# Does your answer change for 4th and short at the goal line?
# Assuming we are not kicking a field goal, we can try to predict whether to
# run or pass.
# Pass:
subset_train_pass = subset(train_pass, down == 4 & ydstogo <= 3 &
                             yardline_100 >= 97)
subset_test_pass = subset(test_pass, down == 4 & ydstogo <= 3 
                          & yardline_100 >= 97)
subset_knn_pass = knn(subset_train_pass, subset_test_pass, 
                      subset_train_pass$first_down, k = 3)
conf_matrix = confusionMatrix(data = subset_knn_pass, 
                              reference = subset_test_pass$first_down)
print(conf_matrix)

# Run:
subset_train_run = subset(train_run, down == 4 & ydstogo <= 3 & 
                            yardline_100 >= 97)
subset_test_run = subset(test_run, down == 4 & ydstogo <= 3 & 
                           yardline_100 >= 97)
subset_knn_run = knn(subset_train_run, subset_test_run, 
                     subset_train_run$first_down, k = 3)
conf_matrix = confusionMatrix(data = subset_knn_run, 
                              reference = subset_test_run$first_down)
print(conf_matrix)

# Same thing with this data. Not enough observations in my data set to make a 
# prediction. Therefore, I will fall back to my previous recommendation on what
# we did in the redzone in these instances and run the ball.








# Throwaway Code ----------------------------------------------------------
glm_pass = glm(first_down ~. - down - ydstogo + down:ydstogo, train_data, family = binomial)
summary(glm_pass)
# We now see that the interactive term of downs and yard to go is statistically
# significant in this model
# We see that the intercept, season, temp,  and wind are not statistically significant
# Let's remove everything by the intercept and run the model again
glm_pass_1 = glm(first_down ~. - down - ydstogo  + down:ydstogo
                 - season - temp - wind, train_data, family = binomial)
summary(glm_pass_1)

# Let's try with the interaction down and yards to go and each individual predictor of the
# two
glm_pass_2 = glm(first_down ~. - down - ydstogo + down*ydstogo - season - temp - wind, 
                 train_data, family = binomial)
summary(glm_pass_2)
# All predictors are significantly in both of these glm models.
# Let's test both of them out on our data and see which model performs better

# Let's now use our trained
# model on our test data using probability threshold of 0.5
# On glm_pass_1
pred_probs = predict(glm_pass_1, newdata = test_data, type = "response")
pred_classes = ifelse(pred_probs > 0.5, 1, 0)
pred_classes = as.factor(pred_classes)
conf_matrix = confusionMatrix(data = pred_classes, reference = test_data$first_down)
print(conf_matrix)
# Accuracy: 0.899  
# Sensitivity: 0.938         
# Specificity: 0.818 

# On glm_pass_2
pred_probs = predict(glm_pass_2, newdata = test_data, type = "response")
pred_classes = ifelse(pred_probs > 0.5, 1, 0)
pred_classes = as.factor(pred_classes)
conf_matrix = confusionMatrix(data = pred_classes, reference = test_data$first_down)
print(conf_matrix)
# Accuracy: 0.993  
# Sensitivity: 0.997         
# Specificity: 0.985 
# glm_pass_2 performed much better

# Now, let's do the same thing using the optimal threshold that maximizes the F1-Score
# rather than using 0.5 and then we can analyze prediction rate of both glm_pass_1
# and glm_pass_2

# glm_pass_1:
pred_probs = predict(glm_pass_1, newdata = test_data, type = "response")
# Create ROC curve
roc_curve = roc(test_data$first_down, pred_probs)
# Find the threshold that maximizes the F1-score
optimal_threshold = coords(roc_curve, "best", maximize = "f1", 
                           ret = "threshold")$threshold
# Classify predictions based on the optimal threshold
pred_classes = ifelse(pred_probs > optimal_threshold, 1, 0)
pred_classes = as.factor(pred_classes)
conf_matrix = confusionMatrix(data = pred_classes, reference = test_data$first_down)
print(conf_matrix)
# Accuracy: 0.887
# Sensitivity: 0.855
# Specificity: 0.953

# glm_pass_2:
pred_probs = predict(glm_pass_2, newdata = test_data, type = "response")
# Create ROC curve
roc_curve = roc(test_data$first_down, pred_probs)
# Find the threshold that maximizes the F1-score
optimal_threshold = coords(roc_curve, "best", maximize = "f1", 
                           ret = "threshold")$threshold
# Classify predictions based on the optimal threshold
pred_classes = ifelse(pred_probs > optimal_threshold, 1, 0)
pred_classes = as.factor(pred_classes)
conf_matrix = confusionMatrix(data = pred_classes, reference = test_data$first_down)
print(conf_matrix)
# Accuracy: 0.994
# Sensitivity: 0.996
# Specificity: 0.988
# This model barely performed better than the 0.5 threshold. But, glm_pass_2
# outperformed glm_pass_2


# Reset pass_first_down_data_df
pass_first_down_df = subset(first_down_df, play_type == "pass")
pass_first_down_df = pass_first_down_df[, -which(names(pass_first_down_df) == "run_gap")]
pass_first_down_df = pass_first_down_df[, -which(names(pass_first_down_df) == "run_location")]
pass_first_down_df = pass_first_down_df[, -which(names(pass_first_down_df) == "rusher")]
pass_first_down_df = pass_first_down_df[, -which(names(pass_first_down_df) 
                                                 == "receiver_player_name")]
pass_first_down_df = pass_first_down_df[, -which(names(pass_first_down_df)
                                                 == "pass_location")]
pass_first_down_df = pass_first_down_df[complete.cases(pass_first_down_df$down), ]
pass_first_down_df$air_yards = ifelse(is.na(pass_first_down_df$air_yards), 
                                      median(pass_first_down_df$air_yards, na.rm = TRUE), 
                                      pass_first_down_df$air_yards)
pass_first_down_df$yards_after_catch = ifelse(is.na(pass_first_down_df$yards_after_catch), 
                                              median(pass_first_down_df$yards_after_catch, na.rm = TRUE), 
                                              pass_first_down_df$yards_after_catch)
pass_first_down_df$temp = ifelse(is.na(pass_first_down_df$temp), 
                                 mean(pass_first_down_df$temp, na.rm = TRUE), 
                                 pass_first_down_df$temp)
pass_first_down_df$wind = ifelse(is.na(pass_first_down_df$wind), 
                                 median(pass_first_down_df$wind, na.rm = TRUE), 
                                 pass_first_down_df$wind)
train_data = pass_first_down_df[pass_first_down_df$season %in% c(2018, 2020, 2022), ]
test_data = pass_first_down_df[pass_first_down_df$season %in% c(2019, 2021, 2023), ]
train_data$first_down = as.factor(train_data$first_down)
test_data$first_down = as.factor(test_data$first_down)
sapply(train_data, function(x) length(levels(as.factor(x))))
# play_type has one level. Let's remove it
pass_first_down_df = pass_first_down_df[, -which(names(pass_first_down_df) 
                                                 == "play_type")]
pass_first_down_df = pass_first_down_df[, -which(names(pass_first_down_df) 
                                                 == "yards_after_catch")]
train_data = pass_first_down_df[pass_first_down_df$season %in% c(2018, 2020, 2022), ]
test_data = pass_first_down_df[pass_first_down_df$season %in% c(2019, 2021, 2023), ]


# Let's turn the down predictor into a factor and dummy variable for each down
pass_first_down_df$down = as.factor(pass_first_down_df$down)
model_matrix = model.matrix(~ down - 1, data = pass_first_down_df)
pass_first_down_df = cbind(pass_first_down_df, model_matrix)
replacement_vector <- c("down_1", "down_2", "down_3", "down_4")
colnames(pass_first_down_df)[grep("down", colnames(pass_first_down_df))] = 
  replacement_vector


# Let's flip flop training and test data
# Original for reference
# train_data = pass_first_down_df[pass_first_down_df$season %in% c(2018, 2020, 2022), ]
# test_data = pass_first_down_df[pass_first_down_df$season %in% c(2019, 2021, 2023), ]
# Now let's swap:
test_data = pass_first_down_df[pass_first_down_df$season %in% c(2018, 2020, 2022), ]
train_data = pass_first_down_df[pass_first_down_df$season %in% c(2019, 2021, 2023), ]
# let's test new model
glm_pass = glm(first_down ~. - down - ydstogo + down*ydstogo - season - temp - wind, 
               train_data, family = binomial)
summary(glm_pass)
# All predictors are still statistically significant
test_data$first_down = as.factor(test_data$first_down)
train_data$first_down = as.factor(train_data$first_down)
pred_probs = predict(glm_pass, newdata = test_data, type = "response")
# Create ROC curve
roc_curve = roc(test_data$first_down, pred_probs)
# Find the threshold that maximizes the F1-score
optimal_threshold = coords(roc_curve, "best", maximize = "f1", 
                           ret = "threshold")$threshold
# Classify predictions based on the optimal threshold
pred_classes = ifelse(pred_probs > optimal_threshold, 1, 0)
pred_classes = as.factor(pred_classes)
conf_matrix = confusionMatrix(data = pred_classes, reference = test_data$first_down)
print(conf_matrix)
# Accuracy: 0.994
# Sensitivity: 0.988
# Specificity: 0.994
# Still 99% prediction rate when you swapped the training and data set.


glm_run = glm(first_down ~. - down - ydstogo + down*ydstogo, train_data, family = "binomial")
summary(glm_run)
# Let's do what we did in our passing analysis and remove yards_gained because in this
# case, we only care about yds to go to get our first down 
glm_run = glm(first_down ~. - yards_gained - down - ydstogo + down*ydstogo, train_data, family = "binomial")
summary(glm_run)
# Let's try down:ydstogo interaction term instead and see how that affects our model
glm_run = glm(first_down ~. - down - ydstogo + down:ydstogo, train_data, family = "binomial")
summary(glm_run)
# In this model, intercept, season, and wind are not statistically significant
# Let's remove season and see if that affects the p-value of wind and the intercept
glm_run = glm(first_down ~. - season - down - ydstogo + down:ydstogo, train_data, family = "binomial")
summary(glm_run)
# Wind still not statistically significant. Let's remove wind
glm_run = glm(first_down ~. - wind - season - down - ydstogo + down:ydstogo, train_data, family = "binomial")
summary(glm_run)
glm_run = glm(first_down ~ yardline_100 + yards_gained + temp + down:ydstogo, 
              train_data, family = "binomial")
summary(glm_run)
# The p-values of each predictor in this model are less than 0.05
# based on the questions that we will get in this project, what happens if we 
# remove yards_gained and temp
glm_run = glm(first_down ~ yardline_100 + down:ydstogo, 
              train_data, family = "binomial")
summary(glm_run)
# Still statistically significant
# Let's go back to the model that has temp, but we will remove yards_gained as that
# is a stat we will not know when predicting whether we should run or pass
glm_run = glm(first_down ~ yardline_100 + temp + down:ydstogo, 
              train_data, family = "binomial")
summary(glm_run)

# Let's remove yards_gained from data set and training/test set
run_first_down_df = run_first_down_df[, -which(names(run_first_down_df) == "yards_gained")]
train_data = run_first_down_df[run_first_down_df$season %in% c(2018, 2020, 2022), ]
test_data = run_first_down_df[run_first_down_df$season %in% c(2019, 2021, 2023), ]

# glm_run model again
glm_run = glm(first_down ~ yardline_100 + temp + down:ydstogo, 
              train_data, family = "binomial")
summary(glm_run)

# Using 0.5 threshold
pred_probs = predict(glm_run, newdata = test_data, type = "response")
pred_classes = ifelse(pred_probs > 0.5, 1, 0)
pred_classes = as.factor(pred_classes)
conf_matrix = confusionMatrix(data = pred_classes, reference = test_data$first_down)
print(conf_matrix)
# Accuracy: 0.7508
# Sensitivity: 0.9876
# Specificity: 0.059

# Using optimal threshold
pred_probs = predict(glm_run, newdata = test_data, type = "response")
# Create ROC curve
roc_curve = roc(test_data$first_down, pred_probs)
# Find the threshold that maximizes the F1-score
optimal_threshold = coords(roc_curve, "best", maximize = "f1", 
                           ret = "threshold")$threshold
# Classify predictions based on the optimal threshold
pred_classes = ifelse(pred_probs > optimal_threshold, 1, 0)
pred_classes = as.factor(pred_classes)
conf_matrix = confusionMatrix(data = pred_classes, reference = test_data$first_down)
print(conf_matrix)
# Accuracy: 0.7781
# Sensitivity: 0.8651
# Specificity: 0.5241
# Slightly more accurate model