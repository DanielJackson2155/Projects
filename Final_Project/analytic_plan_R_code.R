# Machine Learning and AI Final Project -----------------------------------
# Part 1
# Analytic Planning Code --------------------------------------------------

# Customer: ABC Hotels.

# Business Need: ABC Hotels would like to identify bookings that have a high 
# risk of cancellation. The risk of cancellation should be a value between 0 and 1, 
# so it can be interpreted as the probability of cancellation. With this capability, 
# hotel management can target bookings that have a high risk (i.e., probability) of
# cancellation with additional advertisements and/or offers in an effort to prevent 
# them from being cancelled.

# Data: ABC Hotels has provided a data set containing over 35,000 bookings for
# which it is known whether or not the booking was cancelled. Students are required 
# to use this data set provided in the zipped folder below.

# Questions to Answer ------------------------------------------------------
# The first step in the machine learning process is to carefully consider the 
# objectives (i.e., the business needs) in the context of the available data,
# appropriate and applicable machine learning methods, as well as the expected 
# analytic and informational outcomes. Consequently, the Analytic Plan is a detailed
# and specific outline addressing at least the following:

# What is the label (i.e., the target or dependent variable) for the supervised
# classification problem?

# What data processing is needed and how will it be performed? 
# Note: all variables should be included in the analysis unless a reason is given for exclusion.

# What features will be initially included?

# What are the expected analytic and informational outcomes to be produced? 

# How will the model be used in practice?


# Libraries Used ----------------------------------------------------------
library(dplyr)

# Read in Data ------------------------------------------------------------

getwd()
setwd("/Users/doojerthekid/Documents/Merrimack Grad School Documents/DSE6211")

# Read in CSV
hotel_df = read.csv("Final_Project/project_data.csv")

# Check for NA values
colSums(is.na(hotel_df))
# No NA values in data frame. This is great!

# Check unique values of booking status. This will be our response variable for
# our analysis.
unique(hotel_df$booking_status)
# canceled and not_canceled

# We want our response variable to be a binary variable.
# Let us have 0 represent not canceled and 1 represent canceled
# Let us convert booking_status to binary response variable.
hotel_df = hotel_df %>%
  mutate(booking_status = ifelse(booking_status == "canceled", 1, 0))

# We will not need the booking ID code for each observation. Let us remove that
# variable
hotel_df = hotel_df[, -which(names(hotel_df) == "Booking_ID")]

# Now we have 16 total variables.
# With 1 response variable, we have 15 predictors to use in our modeling.


# Variable Summary --------------------------------------------------------
# Let us run through each variable to see what we are working with.

# booking_status
# This is our response variable. It is a binary variable with values 0 and 1
unique(hotel_df$booking_status)
# This is a classification variable, meaning we will be constructing qualitative 
# predictive models in our analysis.

# no_of_adults
# no_of_adults represents the number of adults in the reservation. 
# This is a discrete quantitative variable.
# Let us change the name of the variable from no_of_adults to just adults
# to help with syntax during modeling.
# We will have a snippet of code below for all of the variables that get a column 
# name change.

# no_of_children
# This represents the number of children in the reservation.
# This is a discrete quantitative variable.
# We will change name to children for easier syntax.

# no_of_weekend_nights
# This represents the number of weekend nights in the reservation.
# This is a discrete quantitative variable.
# We will change name to weekend_nights for easier syntax.

# no_of_week_nights
# This represents the number of week nights in the reservation.
# This is a discrete quantitative variable.
# We will change name to week_nights for easier syntax.

# type_of_meal_plan
# This variable represents type of meal plan for each reservation
# Let us look at the values
unique(hotel_df$type_of_meal_plan)
# There is four options: not selected, 1, 2, or 3
# Check counts of each value
hotel_df %>%
  count(type_of_meal_plan)
#  type_of_meal_plan     n
# 1       meal_plan_1 27802
# 2       meal_plan_2  3302
# 3       meal_plan_3     5
# 4       not_selected  5129
# Let us change values to none, one, two or three
hotel_df = hotel_df %>%
  mutate(type_of_meal_plan = ifelse(type_of_meal_plan == 
                                      "not_selected", "none", type_of_meal_plan)) %>%
  mutate(type_of_meal_plan = ifelse(type_of_meal_plan == 
                                      "meal_plan_1", "one", type_of_meal_plan)) %>%
  mutate(type_of_meal_plan = ifelse(type_of_meal_plan == 
                                      "meal_plan_2", "two", type_of_meal_plan)) %>%
  mutate(type_of_meal_plan = ifelse(type_of_meal_plan == 
                                      "meal_plan_3", "three", type_of_meal_plan))
# Check count again to make sure code worked
hotel_df %>%
  count(type_of_meal_plan)
# type_of_meal_plan     n
# 1              none  5129
# 2               one 27802
# 3             three     5
# 4               two  3302

# The code worked.
# This variable is a qualitative variable with four unique values.
# Let us change variable name to meal_plan

# required_car_parking_space
# This represents number of parking spaces needed.
# This is a discrete quantitative variable.
# Let us change the variable name to parking_space.

# room_type_reserved
# This represents type of room reserved.
# Let us check values
unique(hotel_df$room_type_reserved)
# Check counts of each
hotel_df %>%
  count(room_type_reserved)
# room_type_reserved     n
# 1         room_type1 28105
# 2         room_type2   692
# 3         room_type3     7
# 4         room_type4  6049
# 5         room_type5   263
# 6         room_type6   964
# 7         room_type7   158

# There are 7 unique values. Let us change the names of unique values
hotel_df = hotel_df %>%
  mutate(room_type_reserved = ifelse(room_type_reserved == 
                                      "room_type1", "one", room_type_reserved)) %>%
  mutate(room_type_reserved = ifelse(room_type_reserved == 
                                      "room_type2", "two", room_type_reserved)) %>%
  mutate(room_type_reserved = ifelse(room_type_reserved == 
                                      "room_type3", "three", room_type_reserved)) %>%
  mutate(room_type_reserved = ifelse(room_type_reserved == 
                                      "room_type4", "four", room_type_reserved)) %>%
  mutate(room_type_reserved = ifelse(room_type_reserved == 
                                      "room_type5", "five", room_type_reserved)) %>%
  mutate(room_type_reserved = ifelse(room_type_reserved == 
                                      "room_type6", "six", room_type_reserved)) %>%
  mutate(room_type_reserved = ifelse(room_type_reserved == 
                                      "room_type7", "seven", room_type_reserved))
hotel_df %>%
  count(room_type_reserved)

# Code worked.
# This is a qualitative variable with seven different unique values.
# We will change the variable name to room_type

# lead_time
# We will be removing the lead_time predictor. We will treat this as we did with the booking ID variable
# This is more of a time stamp observation on the reservation. Therefore we will remove it.

# arrival_date
# This represents arrival date of each customer
# I do not see us using arrival dates as a variable in our modeling.
# Therefore, we will be removing this variable
hotel_df = hotel_df[, -which(names(hotel_df) == "arrival_date")]

# market_segment_type
# This is a qualitative variable with five unique values that represents how
# the engaged customer booked.
unique(hotel_df$market_segment_type)
# The customer either booked offline (phone call), online, corporate, aviation
# or complementary
# Let us change the variable to market_type

# repeated_guest
# This is a binary qualitative variable with 0 representing not a repeat guest
# and 1 representing repeat guest
unique(hotel_df$repeated_guest)
# Let us change variable name to repeat_guest

# no_of_previous_cancellations
# This represents the number of previous cancellations by customer.
unique(hotel_df$no_of_previous_cancellations)
# This is a continuous quantitative variable.
# Let us change name to previous_cancellations

# no_of_previous_bookings_not_canceled
# This represents number of previous bookings that were not canceled.
unique(hotel_df$no_of_previous_bookings_not_canceled)
# This is a continuous quantitative variable.
# Let us change name to prev_not_cancel

# avg_price_per_room
# This represents average booking price per room.
# This is a continuous quantitative variable.
# We will not change this variable for now.

# no_of_special_requests
# This represents number of special requests made by each customer.
# This is a continuous quantitative variable.
# We will change this variable to spec_requests

# Let uc change variable names
hotel_df = hotel_df %>%
  rename(adults = no_of_adults,
         children = no_of_children,
         weekend_nights = no_of_weekend_nights,
         week_nights = no_of_week_nights,
         meal_plan = type_of_meal_plan,
         parking_spaces = required_car_parking_space,
         room_type = room_type_reserved,
         market_type = market_segment_type,
         repeat_guest = repeated_guest,
         prev_cancel = no_of_previous_cancellations,
         prev_not_cancel = no_of_previous_bookings_not_canceled,
         spec_requests = no_of_special_requests)

# We have now summarized and cleaned up our data.


# Variable Selection and Feature Engineering -------------------------------
# What variables do we want to include in our analysis?
# We have already removed the customer ID variable and the arrival date variable.

# Let us look at the average price per room variable.
# Looking at all of the other continuous variables, this specific continuous variable 
# has the most unique values.
# Let us look at the distribution of it
hist(hotel_df$avg_price_per_room, main = "Distribution of Avg Price Per Room")
boxplot(hotel_df$avg_price_per_room, main = "Boxplot of Avg Price Per Room")
# We see that the boxplot is heavily skewed right with a lot of outliers.
# Let us try some transformations to make the distribution more normal and
# minimize the number of outliers.

# Sqrt
hist(sqrt(hotel_df$avg_price_per_room), main = "Distribution of sqrt(Avg Price Per Room)")
boxplot(sqrt(hotel_df$avg_price_per_room), main = "Boxplot of sqrt(Avg Price Per Room)")
# Made distribution slightly more normal. Still a lot of outliers.

# Log
hist(log(hotel_df$avg_price_per_room), main = "Distribution of log(Avg Price Per Room)")
boxplot(log(hotel_df$avg_price_per_room), main = "Boxplot of log(Avg Price Per Room)")
# This did not make distribution more normal.

# Squared
hist((hotel_df$avg_price_per_room)^2, main = "Distribution of (Avg Price Per Room)^2")
boxplot((hotel_df$avg_price_per_room)^2, main = "Boxplot of (Avg Price Per Room)^2")
# This did not make distribution more normal.


# Analytic Plan Set Up -----------------------------------------------------------
# This will be a qualitative regression analysis. Our response variable is a binary
# qualitative variable with two values: 0 for not-cancelled and 1 for cancelled reservation.
# We have 14 predictor variables and the 1 response variable for our analysis.

# What is our goal? 
# Our goal is to fit a predictive model to help the ABC Hotels to identify bookings
# that have a high risk of cancellation using the data set given
# The risk of cancellation will be a value between 0 and 1. The closer the probability is
# to 1, the higher risk of cancellation.

# Since we are trying to predict a qualitative response variable, we will fit 
# at least one dense neural network model.
# In this portion of the project, we will specify the following aspects of the 
# neural network(s) and discuss why/how they were chosen: 
# number of layers
# number of units for each layer
# activation functions for each layer 
# loss function
# and optimization algorithm.


# We will create a training and test data set. We will train the model using the
# training data set and will use that trained model to predict the response variable
# in the test set.
# We will then be using confidence matrices to measure our prediction rate.

# We will evaluate the neural network(s) using learning curves on training and 
# validation sets. Is the model(s) underfitting or overfitting? Based on this,
# what changes will be made to the architecture of the dense neural network(s).
# Based on the evaluation of the preliminary model(s), provide further data processing 
# and feature engineering steps that will be implemented and investigated for the Final Report.


# Training/Test Data
# Let us look at the dimension of our data set
dim(hotel_df)
# 36238 observations
# 15 variables

# Let us define our training and test data.
# Since we have a good amount of observations, will be using 70% of our data to 
# be the training data and 30% to be our test data
set.seed(1)
train = sample(nrow(hotel_df), 0.7 * nrow(hotel_df))
train_df = hotel_df[train, ]
test_df = hotel_df[-train, ]

# We have 25366 observations in our training data
# We have 10872 observations in our test data


# Questions for Professor -------------------------------------------------
# 1.) What is the lead_time variable?
# 2.) What sort of visuals are expected in Analytic Planning (if any)?
# 3.) Do you want us to include all R code in submission? Or include just in appendix?
