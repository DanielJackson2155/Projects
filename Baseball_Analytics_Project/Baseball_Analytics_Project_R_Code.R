# Baseball Analytics Project R Code ---------------------------------------
# Predict each player’s on-base percentage at the end of the 2019 season given his 
# batting statistics in March/April 2019. You are to use the first 28 columns to 
# create your prediction and the final column, FullSeason_OBP to test your prediction 
# accuracy.

# Response variable: FullSeason_OBP (Full season On-Base Percentage)

# Libraries Used ----------------------------------------------------------
library(googlesheets4)
library(corrplot)
library(dplyr)
library(glmnet)
library(caret)
library(pls)
library(ggplot2)
library(tree)
library(ipred)
library(randomForest)
library(gbm)


# Read in Google Sheet Data -----------------------------------------------
# https://drive.google.com/file/d/1jXGfMCmPkutX7sXgyk1YZIryD_iFPa6J/edit?pli=1
sheet_url = "https://docs.google.com/spreadsheets/d/1U_QsSv6-68VLI0-5YrSPp2Bo-QhzZftiMU10b1Ajk0k/edit#gid=1381900348"
# Use gs4_auth() and sign into gmail to access Google Sheet
mlb_df = read_sheet(sheet_url)
head(mlb_df)
dim(mlb_df)
# 320 observations
# 29 predictors


# Clean Data --------------------------------------------------------------
# Look for NAs in any columns
colSums(is.na(mlb_df))
# No NAs. That means we have no missing values. Awesome!

# Let's remove playerid column as they will not have impact on predicting player's
# FSOBP (full season obp)
mlb_df = mlb_df[, -which(names(mlb_df) == "playerid")]

# Now since we now all of the data is between March and April, I want to clean up
# our predictor names to make coding easier.
# Let's remove "MarApr_" from each predictor
colnames(mlb_df)
predictor_names = colnames(mlb_df)
# Remove "MarApr_" from each predictor name
new_names = sub("^MarApr_", "", predictor_names)
colnames(mlb_df) = new_names

# Now let's swap the "%" with "pct"
predictor_names = colnames(mlb_df)
new_names = sub("%", "_pct", predictor_names)
colnames(mlb_df) = new_names

# Now let's swap any "-" with "_"
predictor_names = colnames(mlb_df)
new_names = sub("-", "_", predictor_names)
colnames(mlb_df) = new_names

# Change HR/FB predictor to HR_FB
colnames(mlb_df)[colnames(mlb_df) == "HR/FB"] = "HR_FB"

# Change response variable to FS_OBP to make code easier
colnames(mlb_df)[colnames(mlb_df) == "FullSeason_OBP"] = "FS_OBP"

# Change all predictors to lower case
colnames(mlb_df) = tolower(colnames(mlb_df))

# All predictors (all from March and April 2019, besides response variable)
# pa = player’s plate appearances
# ab = player’s at bats
# h = player’s hits
# hr = player’s home runs
# r = player’s runs scored
# rbi = player’s RBI
# sb = player’s stolen bases
# bb_pct = player’s walk percentage
# k_pct = player’s strikeout percentage
# iso = player’s isolated power
# babip = player’s BABIP
# avg = player’s batting average
# obp = player’s on-base percentage
# slg = player’s slugging percentage
# ld_pct = player’s line drive percentage
# gb_pct = player’s ground ball percentage
# fb_pct = player’s fly ball percentage
# iffb_pct = player’s infield fly ball percentage
# hr_fb = player’s home run per fly ball rate
# o_swing_pct = player’s out-of-zone swing-per-pitch percentage
# z_swing_pct = player’s in-zone swing-per-pitch percentage
# swing_pct = player’s total swing-per-pitch percentage
# o_contact_pct = player’s out-of-zone contact-per-swing percentage
# z_contact_pct = player’s in-zone contact-per-swing percentage
# contact_pct = player’s total contact-per-swing percentage

# Response variable
# fs_obp = player’s on-base percentage for his entire 2019 season

# Data all cleaned up. Ready to start exploratory analysis!


# Exploratory Analysis ----------------------------------------------------
# Before we remove name and team, since they are not quantitative predictors that 
# we will be using in our analysis, let's look at how many observations are for 
# each team
table(mlb_df$team)
# There are two players not assigned to a team. I am not too worried about this
# as we will not be looking at team name in our regression analysis.

# Let's look at the top 10 players in full season obp
top_ten_players_fs_obp = mlb_df[order(-mlb_df$fs_obp), ][1:10, c("name", "fs_obp")]
print(top_ten_players_fs_obp)
# name                fs_obp
# 1 Mike Trout        0.438
# 2 Christian Yelich  0.429
# 3 Alex Bregman      0.423
# 4 Anthony Rendon    0.412
# 5 Matt Joyce        0.408
# 6 Cody Bellinger    0.406
# 7 Anthony Rizzo     0.405
# 8 David Freese      0.403
# 9 Juan Soto         0.401
# 10 Carlos Santana   0.397

# Let's look at top ten players in March-April obp
top_ten_players_obp = mlb_df[order(-mlb_df$obp), ][1:10, c("name", "obp")]
print(top_ten_players_obp)
# name               obp
# 1 Cody Bellinger   0.508
# 2 Dominic Smith    0.5  
# 3 Mike Trout       0.487
# 4 Christian Yelich 0.46 
# 5 Eric Sogard      0.458
# 6 Scott Kingery    0.457
# 7 Daniel Vogelbach 0.457
# 8 Jeff McNeil      0.457
# 9 Hunter Dozier    0.447
# 10 Anthony Rendon  0.442

# Check to see what players are found in each data set
common_names = intersect(top_ten_players_fs_obp$name, top_ten_players_obp$name)
print(common_names)
# Four players are found in each top 10 list:
# Mike Trout
# Christian Yelich
# Anthony Rendon
# Cody Bellinger

# Let's look at top 10 list for hits, homeruns, runs, RBIs, and average.
# Just want to see who performed the best in the league during this stretch
# in these stats
# Hits
top_ten_players_hits = mlb_df[order(-mlb_df$h), ][1:10, c("name", "h")]
print(top_ten_players_hits)
# name                  h
# 1 Cody Bellinger      47
# 2 Paul DeJong         40
# 3 Elvis Andrus        39
# 4 Trey Mancini        39
# 5 Michael Brantley    39
# 6 David Peralta       39
# 7 Jeff McNeil         37
# 8 Christian Yelich    36
# 9 Tim Anderson        36
# 10 Domingo Santana    36

# HRs
top_ten_players_homeruns = mlb_df[order(-mlb_df$hr), ][1:10, c("name", "hr")]
print(top_ten_players_homeruns)
# name                  hr
# 1 Cody Bellinger      14
# 2 Christian Yelich    14
# 3 Eddie Rosario       11
# 4 Joey Gallo          10
# 5 Joc Pederson        10
# 6 Marcell Ozuna       10
# 7 Pete Alonso          9
# 8 Javier Baez          9
# 9 George Springer      9
# 10 Paul Goldschmidt    9

# Runs
top_ten_players_runs = mlb_df[order(-mlb_df$r), ][1:10, c("name", "r")]
print(top_ten_players_runs)
# name                  r
# 1 Cody Bellinger      32
# 2 Mitch Haniger       27
# 3 Christian Yelich    26
# 4 Paul DeJong         26
# 5 Marcell Ozuna       24
# 6 Andrew McCutchen    24
# 7 Ozzie Albies        24
# 8 Trey Mancini        23
# 9 Freddie Freeman     23
# 10 Javier Baez        23

# RBIs
top_ten_players_rbi = mlb_df[order(-mlb_df$rbi), ][1:10, c("name", "rbi")]
print(top_ten_players_rbi)
# name                 rbi
# 1 Cody Bellinger      37
# 2 Christian Yelich    34
# 3 Marcell Ozuna       28
# 4 Pete Alonso         26
# 5 Joey Gallo          25
# 6 Luke Voit           25
# 7 Domingo Santana     25
# 8 Rhys Hoskins        24
# 9 George Springer     24
# 10 Eddie Rosario      24

# Avg
top_ten_players_avg = mlb_df[order(-mlb_df$avg), ][1:10, c("name", "avg")]
print(top_ten_players_avg)
#    name           avg
# 1 Cody Bellinger 0.431
# 2 Scott Kingery  0.406
# 3 Eric Sogard    0.395
# 4 Tim Anderson   0.375
# 5 Tom Murphy     0.37 
# 6 Dominic Smith  0.37 
# 7 Jeff McNeil    0.37 
# 8 Jose Martinez  0.364
# 9 Elvis Andrus   0.361
# 10 James McCann  0.357

# In the March/April time frame, Cody Bellinger lead the entire league in 
# on base percentage, hits, homeruns, runs, RBIs, and average.
# Looks like he was on an offensive tear during this stretch.

# Now, we want to start looking at the numeric values and see how they are correlated
# to each other and our response variable of fs_obp.
# We can remove name and team for this analysis
mlb_df = mlb_df[, -which(names(mlb_df) == "name")]
mlb_df = mlb_df[, -which(names(mlb_df) == "team")]

# When it comes to on_base_percentage, here is the formula:
# (Hits + HBP + Walks) / (At-Bats + HBP + Walks + Sacrifice Flies)

# Let's look at correlation plot of our variables to see if we can remove any of our 
# predictors.We are looking for instances of multicollinearity.
# We want to try and remove some variables to help simplify model during our
# regression analysis.
corrplot::corrplot(cor(mlb_df[sapply(mlb_df, is.numeric)], 
             use = "pairwise.complete.obs"), method = "color")
# We see that fb_pct (flyball percentage) has no correlation to fs_obp so we can remove
# this predictor.
mlb_df = mlb_df[, -which(names(mlb_df) == "fb_pct")]
# We see that sb also has little correlation with fs_obp.
# Let's remove sb as well
mlb_df = mlb_df[, -which(names(mlb_df) == "sb")]

# Positively correlated predictors:
# We see that pa and ab are highly correlated.
# Knowing what we know about our formula to predict OBP, let's keep at-bats, and
# remove pa
mlb_df = mlb_df[, -which(names(mlb_df) == "pa")]
# We see that homeruns and hits are not that correlated, but we know that when you
# hit a homerun, it is considered a hit and based on our calculation for
# OBP, we only care about hits, not homeruns.
# Let's remove homeruns
mlb_df = mlb_df[, -which(names(mlb_df) == "hr")]
# Since we removed hr, let's also reomve hr_fb
mlb_df = mlb_df[, -which(names(mlb_df) == "hr_fb")]
# We see that avg and babip highly correlated. Since we know BABIP is batting
# average on balls put in play, we want to see average for balls also 
# not put in play, so let's remove BABIP
mlb_df = mlb_df[, -which(names(mlb_df) == "babip")]
# We see that o_swing_pct and z_swing_pct is highly correlated with total
# swing_pct. Since both o_swing_pct and z_swing_pct are used to calculate
# swing_pct, let's remove o_swing_pct and z_swing_pct.
mlb_df = mlb_df[, -which(names(mlb_df) == "o_swing_pct")]
mlb_df = mlb_df[, -which(names(mlb_df) == "z_swing_pct")]
# We see the same thing with o_contact_pct and z_contact_pct being
# highly correlated with contact_pct. Let's remove o_contact_pct and
# z_contact_pct
mlb_df = mlb_df[, -which(names(mlb_df) == "o_contact_pct")]
mlb_df = mlb_df[, -which(names(mlb_df) == "z_contact_pct")]
# We see that slg and iso, are highly correlated. iso is difference
# between slugging and average to get isolated power. In this case,
# let's just remove iso because we already have slg and avg
# predictors.
mlb_df = mlb_df[, -which(names(mlb_df) == "iso")]

# Run correlation plot again
corrplot::corrplot(cor(mlb_df[sapply(mlb_df, is.numeric)], 
             use = "pairwise.complete.obs"), method = "color")

# Negatively correlated predictors
# We see that contact_pct and k_pct are negatively correlated and 
# could create multicollinearity issues. 
# Based on our knowledge of what we are looking for in full season OBP,
# let's keep contact_pct and remove k_pct.
mlb_df = mlb_df[, -which(names(mlb_df) == "k_pct")]
# We also see that swing_pct and bb_pct are highly negatively correlated.
# Let's remove swing_pct as it is positively correlated with fs_obp
mlb_df = mlb_df[, -which(names(mlb_df) == "swing_pct")]

# Run correlation plot again
corrplot::corrplot(cor(mlb_df[sapply(mlb_df, is.numeric)], 
             use = "pairwise.complete.obs"), method = "color")
# We have now cleaned up data to have 12 predictors after removing
# highly correlated predictors.

# Let's look at correlation of each predictor in regards to fs_obp
# Let's calculate correlation coefficients
correlations = cor(mlb_df)
print(correlations)
ordered_fs_obp_correlations = correlations["fs_obp", order(abs(correlations["fs_obp", ]), 
                                                           decreasing = TRUE)]
print(ordered_fs_obp_correlations)
# obp, r, avg, h, slg, and bb_pct are most correlated variables with fs_obp.
# gb_pct, iffb_pct, ld_pct, and contact_pct are least correlated
# variables with fs_obp.

# We will most likely not be using some of these predictors that are not heavily
# correlated with fs_obp. We will see this when we run regression models.

# Let's look at histogram and boxplot of each predictor to see how each predictor
# is distributed.
# We can also see if we want to transform any of the predictors in our linear
# regression analysis.

# ab
hist(mlb_df$ab, main = "Distribution of At-Bats")
boxplot(mlb_df$ab, main = "Boxplot of At-Bats",
        ylab = "Frequency")
# Distribution slightly skewed left
# Try log transformation
hist(log(mlb_df$ab), main = "Distribution of log(At-Bats)")
boxplot(log(mlb_df$pa), main = "Boxplot of log(At-Bats)",
        ylab = "Frequency")
# More skewed left
# Try sqrt transformation
hist(sqrt(mlb_df$ab), main = "Distribution of sqrt(At-Bats)")
boxplot(sqrt(mlb_df$pa), main = "Boxplot of sqrt(At-Bats)",
        ylab = "Frequency")
# More skewed left. Better not to transform predictor.


# h
hist(mlb_df$h, main = "Distribution of Hits")
boxplot(mlb_df$h, main = "Boxplot of Hits",
        ylab = "Frequency")
# Data seems relatively normal. One outlier.


# hr
hist(mlb_df$hr, main = "Distribution of Homeruns")
boxplot(mlb_df$hr, main = "Boxplot of Homeruns",
        ylab = "Frequency")
# Heavily skewed right. One outlier.
# Try log transformation
hist(log(mlb_df$hr), main = "Distribution of log(Homeruns)")
boxplot(log(mlb_df$hr), main = "Boxplot of log(Homeruns)",
        ylab = "Frequency")
# Did not make distribution more normal
# Try sqrt transformation
hist(sqrt(mlb_df$hr), main = "Distribution of sqrt(Homeruns)")
boxplot(sqrt(mlb_df$hr), main = "Boxplot of sqrt(Homeruns)",
        ylab = "Frequency")
# More normal distribution with square root transformation


# r
hist(mlb_df$r, main = "Distribution of Runs")
boxplot(mlb_df$r, main = "Boxplot of Runs",
        ylab = "Frequency")
# Slightly skewed right. One outlier
# Try log transformation
hist(log(mlb_df$r), main = "Distribution of log(Runs)")
boxplot(log(mlb_df$r), main = "Boxplot of log(Runs)",
        ylab = "Frequency")
# This made data heavily skewed left
# Try sqrt transformation
hist(sqrt(mlb_df$r), main = "Distribution of sqrt(Runs)")
boxplot(sqrt(mlb_df$r), main = "Boxplot of sqrt(Runs)",
        ylab = "Frequency")
# More normal distribution with square root transformation


# rbi
hist(mlb_df$rbi, main = "Distribution of RBIs")
boxplot(mlb_df$rbi, main = "Boxplot of RBIs",
        ylab = "Frequency")
# Slightly skewed right. Two outliers.
# Try log transformation
hist(log(mlb_df$rbi), main = "Distribution of log(RBIs)")
boxplot(log(mlb_df$rbi), main = "Boxplot of log(RBIs)",
        ylab = "Frequency")
# Looks more normal. One outlier.
# Try sqrt transformation
hist(sqrt(mlb_df$rbi), main = "Distribution of sqrt(RBIs)")
boxplot(sqrt(mlb_df$rbi), main = "Boxplot of sqrt(RBIs)",
        ylab = "Frequency")
# Most normal distribution. One outlier still.


# sb
hist(mlb_df$sb, main = "Distribution of Stolen Bases")
boxplot(mlb_df$sb, main = "Boxplot of Stolen Bases",
        ylab = "Frequency")
# Heavily skewed right. 5 outliers. Lot of players with 0 stolen bases.
# Transformations will not perform well on stolen bases because of those
# values of 0.


# bb_pct
hist(mlb_df$bb_pct, main = "Distribution of Walk Percentages")
boxplot(mlb_df$bb_pct, main = "Boxplot of Walk Percentages",
        ylab = "Frequency")
# Relatively normal distribution. Few outliers here.
# Try log transformation
hist(log(mlb_df$bb_pct), main = "Distribution of log(Walk Percentages)")
boxplot(log(mlb_df$bb_pct), main = "Boxplot of log(Walk Percentages)",
        ylab = "Frequency")
# Way more outliers with log transformation
# Try sqrt transformation
hist(sqrt(mlb_df$bb_pct), main = "Distribution of sqrt(Walk Percentages)")
boxplot(sqrt(mlb_df$bb_pct), main = "Boxplot of sqrt(Walk Percentages)",
        ylab = "Frequency")
# Normal distribution and only three outliers here. Let's go with this
# transformation.


# k_pct
hist(mlb_df$k_pct, main = "Distribution of Strikeout Percentage")
boxplot(mlb_df$k_pct, main = "Boxplot of Strikeout Percentage",
        ylab = "Frequency")
# Normally distributed. Few outliers.
# Try log transformation
hist(log(mlb_df$k_pct), main = "Distribution of log(Strikeout Percentage)")
boxplot(log(mlb_df$k_pct), main = "Boxplot of log(Strikeout Percentage)",
        ylab = "Frequency")
# This transformation did not help with the outliers.
# Try sqrt transformation
hist(sqrt(mlb_df$k_pct), main = "Distribution of sqrt(Strikeout Percentage)")
boxplot(sqrt(mlb_df$k_pct), main = "Boxplot of sqrt(Strikeout Percentage)",
        ylab = "Frequency")
# Normal distribution. Still a few outliers. 
# Let's not transform this variable.


# iso
hist(mlb_df$iso, main = "Distribution of Isolated Power")
boxplot(mlb_df$iso, main = "Boxplot of Isolated Power",
        ylab = "Frequency")
# Relatively normal. Few outliers here.
# Try log transformation
hist(log(mlb_df$iso), main = "Distribution of log(Isolated Power)")
boxplot(log(mlb_df$iso), main = "Boxplot of log(Isolated Power)",
        ylab = "Frequency")
# Made distribution much less normal.
# Try sqrt transformation
hist(sqrt(mlb_df$iso), main = "Distribution of sqrt(Isolated Power)")
boxplot(sqrt(mlb_df$iso), main = "Boxplot of sqrt(Isolated Power)",
        ylab = "Frequency")
# Normal distribution, but still a few outliers. 
# We will not transform this variable.


# babip
hist(mlb_df$babip, main = "Distribution of Batting Average on Balls in Play")
boxplot(mlb_df$babip, main = "Boxplot of Batting Average on Balls in Play",
        ylab = "Frequency")
# Normally distributed. Couple of outliers. Try transformations to see if anything
# happens to two outliers.
hist(log(mlb_df$babip), main = "Distribution of log(Batting Average on Balls in Play)")
boxplot(log(mlb_df$babip), main = "Boxplot of log(Batting Average on Balls in Play)",
        ylab = "Frequency")
# More distributed normally. Still a few outliers.
# Try sqrt transformation
hist(sqrt(mlb_df$babip), main = "Distribution of sqrt(Batting Average on Balls in Play)")
boxplot(sqrt(mlb_df$babip), main = "Boxplot of sqrt(Batting Average on Balls in Play)",
        ylab = "Frequency")
# Sqrt transformation is distrubuted normally with two outliers. We will go with
# this transformation for our model.


# avg
hist(mlb_df$avg, main = "Distribution of Batting Average")
boxplot(mlb_df$avg, main = "Boxplot of Batting Average",
        ylab = "Frequency")
# Relatively normal. Few outliers.
# Log transformation
hist(log(mlb_df$avg), main = "Distribution of log(Batting Average)")
boxplot(log(mlb_df$avg), main = "Boxplot of log(Batting Average)",
        ylab = "Frequency")
# Try sqrt transformation
hist(sqrt(mlb_df$avg), main = "Distribution of sqrt(Batting Average)")
boxplot(sqrt(mlb_df$avg), main = "Boxplot of sqrt(Batting Average)",
        ylab = "Frequency")
# Sqrt root transformation most normally distrubutied. Two outliers.


# obp
hist(mlb_df$obp, main = "Distribution of On Base Percentage")
boxplot(mlb_df$obp, main = "Boxplot of On Base Percentage",
        ylab = "Frequency")
# Relatively normal. Few outliers.
# Try log transformation
hist(log(mlb_df$obp), main = "Distribution of log(On Base Percentage)")
boxplot(log(mlb_df$obp), main = "Boxplot of On log(Base Percentage)",
        ylab = "Frequency")
# Not as normally distributed as variable without transformation
# Try sqrt transformation
hist(sqrt(mlb_df$obp), main = "Distribution of sqrt(On Base Percentage)")
boxplot(sqrt(mlb_df$obp), main = "Boxplot of On sqrt(Base Percentage)",
        ylab = "Frequency")
# More normally distributed than predictor without transformation.
# Go with square root transformation


# slg
hist(mlb_df$slg, main = "Distribution of Slugging Percentage")
boxplot(mlb_df$slg, main = "Boxplot of Slugging Percentage",
        ylab = "Frequency")
# Relativley normal. Few outliers.
# Try log transformation
hist(log(mlb_df$slg), main = "Distribution of log(Slugging Percentage)")
boxplot(log(mlb_df$slg), main = "Boxplot of log(Slugging Percentage)",
        ylab = "Frequency")
# Made data more skewed left.
# Try sqrt transformation
hist(sqrt(mlb_df$slg), main = "Distribution of sqrt(Slugging Percentage)")
boxplot(sqrt(mlb_df$slg), main = "Boxplot of sqrt(Slugging Percentage)",
        ylab = "Frequency")
# Similar distribution to slugging percentage without transformation. 
# Deciding not to transform this variable.


# ld_pct
hist(mlb_df$ld_pct, main = "Distribution of Line Drive Percentage")
boxplot(mlb_df$ld_pct, main = "Boxplot of Line Drive Percentage",
        ylab = "Frequency")
# Relatively normal distribution. Two outliers.
# Try log transformation
hist(log(mlb_df$ld_pct), main = "Distribution of log(Line Drive Percentage)")
boxplot(log(mlb_df$ld_pct), main = "Boxplot of log(Line Drive Percentage)",
        ylab = "Frequency")
# Not much improvement on distribution. More outliers.
# Try sqrt
hist(sqrt(mlb_df$ld_pct), main = "Distribution of sqrt(Line Drive Percentage)")
boxplot(sqrt(mlb_df$ld_pct), main = "Boxplot of sqrt(Line Drive Percentage)",
        ylab = "Frequency")
# Looks slightly more normal. More outliers however.
# Deciding not to transform variable.


# gb_pct
hist(mlb_df$gb_pct, main = "Distribution of Ground Ball Percentage")
boxplot(mlb_df$gb_pct, main = "Boxplot of Ground Ball Percentage",
        ylab = "Frequency")
# Relatively normal. Three outliers.
# Try log transformation
hist(log(mlb_df$gb_pct), main = "Distribution of log(Ground Ball Percentage)")
boxplot(log(mlb_df$gb_pct), main = "Boxplot of log(Ground Ball Percentage)",
        ylab = "Frequency")
# Made distribution more skewed left.
# Try sqrt
hist(sqrt(mlb_df$gb_pct), main = "Distribution of sqrt(Ground Ball Percentage)")
boxplot(sqrt(mlb_df$gb_pct), main = "Boxplot of sqrt(Ground Ball Percentage)",
        ylab = "Frequency")
# Made distribution more normal. But created more outliers. Deciding not to 
# transform this variable


# iffb_pct
hist(mlb_df$iffb_pct, main = "Distribution of Infield Fly Ball Percentage")
boxplot(mlb_df$iffb_pct, main = "Boxplot of Infield Fly Ball Percentage",
        ylab = "Frequency")
# Skewed heavily to the right. Few outliers.
# Try log transformation
hist(log(mlb_df$iffb_pct), main = "Distribution of log(Infield Fly Ball Percentage)")
boxplot(log(mlb_df$iffb_pct), main = "Boxplot of log(Infield Fly Ball Percentage)",
        ylab = "Frequency")
# More normally distributed than no transformation
# Try sqrt transformation
hist(sqrt(mlb_df$iffb_pct), main = "Distribution of sqrt(Infield Fly Ball Percentage)")
boxplot(sqrt(mlb_df$iffb_pct), main = "Boxplot of sqrt(Infield Fly Ball Percentage)",
        ylab = "Frequency")
# sqrt transformation had best effect on distribution.


# hr_fb
hist(mlb_df$hr_fb, main = "Distribution of Homerun per Fly Ball Rate")
boxplot(mlb_df$hr_fb, main = "Boxplot of Homerun per Fly Ball Rate",
        ylab = "Frequency")
# Skewed slightly right
# Try log transformation
hist(log(mlb_df$hr_fb), main = "Distribution of log(Homerun per Fly Ball Rate)")
boxplot(log(mlb_df$hr_fb), main = "Boxplot of log(Homerun per Fly Ball Rate)",
        ylab = "Frequency")
# Slightly more normal
# Try sqrt
hist(sqrt(mlb_df$hr_fb), main = "Distribution of sqrt(Homerun per Fly Ball Rate)")
boxplot(sqrt(mlb_df$hr_fb), main = "Boxplot of sqrt(Homerun per Fly Ball Rate)",
        ylab = "Frequency")
# Most normal distribution


# o_swing_pct
hist(mlb_df$o_swing_pct, main = "Distribution of Out-of-Strike-Zone Swing-Per-Pitch %")
boxplot(mlb_df$o_swing_pct, main = "Boxplot of Out-of-Strike-Zone Swing-Per-Pitch %",
        ylab = "Frequency")
# Relatively normal. Two outliers.
hist(log(mlb_df$o_swing_pct), main = "Distribution of log(Out-of-Strike-Zone Swing-Per-Pitch %)")
boxplot(log(mlb_df$o_swing_pct), main = "Boxplot of log(Out-of-Strike-Zone Swing-Per-Pitch %)",
        ylab = "Frequency")
# Relatively normal. More outliers
# Try sqrt
hist(sqrt(mlb_df$o_swing_pct), main = "Distribution of sqrt(Out-of-Strike-Zone Swing-Per-Pitch %)")
boxplot(sqrt(mlb_df$o_swing_pct), main = "Boxplot of sqrt(Out-of-Strike-Zone Swing-Per-Pitch %)",
        ylab = "Frequency")
# Normal distribution. One outlier.
# We will go with this transformation


# z_swing_pct
hist(mlb_df$z_swing_pct, main = "Distribution of In-Zone Swing-Per-Pitch %")
boxplot(mlb_df$z_swing_pct, main = "Boxplot of In-Zone Swing-Per-Pitch %",
        ylab = "Frequency")
# Relatively normal. Three outliers.
# Try log transformation
hist(log(mlb_df$z_swing_pct), main = "Distribution of log(In-Zone Swing-Per-Pitch %)")
boxplot(log(mlb_df$z_swing_pct), main = "Boxplot of log(In-Zone Swing-Per-Pitch %)",
        ylab = "Frequency")
# Made distribution more skewed left
# Try sqrt
hist(sqrt(mlb_df$z_swing_pct), main = "Distribution of sqrt(In-Zone Swing-Per-Pitch %)")
boxplot(sqrt(mlb_df$z_swing_pct), main = "Boxplot of sqrt(In-Zone Swing-Per-Pitch %)",
        ylab = "Frequency")
# Transformations did not help make distribution more normal.


# swing_pct
hist(mlb_df$swing_pct, main = "Distribution of Total Swing-Per-Pitch %")
boxplot(mlb_df$swing_pct, main = "Boxplot of Total Swing-Per-Pitch %",
        ylab = "Frequency")
# Normal distribution. One outlier
# Try log
hist(log(mlb_df$swing_pct), main = "Distribution of log(Total Swing-Per-Pitch %)")
boxplot(log(mlb_df$swing_pct), main = "Boxplot of log(Total Swing-Per-Pitch %)",
        ylab = "Frequency")
# Normal. Two outliers.
# Try sqrt
hist(sqrt(mlb_df$swing_pct), main = "Distribution of sqrt(Total Swing-Per-Pitch %)")
boxplot(sqrt(mlb_df$swing_pct), main = "Boxplot of sqrt(Total Swing-Per-Pitch %)",
        ylab = "Frequency")
# Normal. One outlier. 
# Deciding against transforming this variable


# o_contact_pct
hist(mlb_df$o_contact_pct, main = "Distribution of Out-of-Strike-Zone Contact-Per-Swing %")
boxplot(mlb_df$o_contact_pct, main = "Boxplot of Out-of-Strike-Zone Contact-Per-Swing %",
        ylab = "Frequency")
# Relatively normal. Four outliers.
# Try log
hist(log(mlb_df$o_contact_pct), main = "Distribution of log(Out-of-Strike-Zone Contact-Per-Swing %)")
boxplot(log(mlb_df$o_contact_pct), main = "Boxplot of log(Out-of-Strike-Zone Contact %)",
        ylab = "Frequency")
# More skewed left
# Try sqrt
hist(sqrt(mlb_df$o_contact_pct), main = "Distribution of sqrt(Out-of-Strike-Zone Contact-Per-Swing %)")
boxplot(sqrt(mlb_df$o_contact_pct), main = "Boxplot of sqrt(Out-of-Strike-Zone Contact-Per-Swing %)",
        ylab = "Frequency")
# Relatively normal. Few outliers. Deciding not to transform this variable


# z_contact_pct
hist(mlb_df$z_contact_pct, main = "Distribution of In-Zone Contact-Per-Swing %")
boxplot(mlb_df$z_contact_pct, main = "Boxplot of In-Zone Contact-Per-Swing %",
        ylab = "Frequency")
# Relatively normal. Three outliers.
# Try log
hist(log(mlb_df$z_contact_pct), main = "Distribution of log(In-Zone Contact-Per-Swing %)")
boxplot(log(mlb_df$z_contact_pct), main = "Boxplot of log(In-Zone Contact-Per-Swing %)",
        ylab = "Frequency")
# Did not make distribution more normal
# Try sqrt
hist(sqrt(mlb_df$z_contact_pct), main = "Distribution of sqrt(In-Zone Contact-Per-Swing %)")
boxplot(sqrt(mlb_df$z_contact_pct), main = "Boxplot of sqrt(In-Zone Contact-Per-Swing %)",
        ylab = "Frequency")
# Relatively normal. Four outliers.
# Deciding not to transform variable.


# contact_pct
hist(mlb_df$contact_pct, main = "Distribution of Total Contact-Per-Swing %")
boxplot(mlb_df$contact_pct, main = "Boxplot of Total Contact-Per-Swing %",
        ylab = "Frequency")
# Relatively normal. Few outliers.
# Try log
hist(log(mlb_df$contact_pct), main = "Distribution of log(Total Contact-Per-Swing %)")
boxplot(log(mlb_df$contact_pct), main = "Boxplot of log(Total Contact-Per-Swing %)",
        ylab = "Frequency")
# Relatively normal. Three outliers.
# Try sqrt
hist(sqrt(mlb_df$contact_pct), main = "Distribution of sqrt(Total Contact-Per-Swing %)")
boxplot(sqrt(mlb_df$contact_pct), main = "Boxplot of sqrt(Total Contact-Per-Swing %)",
        ylab = "Frequency")
# Relatively normal. Few outliers.
# Deciding not to transform variable.


# fs_obp
hist(mlb_df$fs_obp, main = "Distribution of Full Season OBP")
boxplot(mlb_df$fs_obp, main = "Boxplot of Full Season OBP",
        ylab = "Frequency")
# Normal distribution. 6 outliers.

# Did transformations make predictors more normally distrubuted?
# pa = N/A/ REMOVED VARIABLE DUE TO COLLINEARITY.
# ab = NO
# h = NO
# hr = YES. SQUARE ROOT
# r = YES. SQUARE ROOT
# rbi = YES. SQUARE ROOT
# sb = NO
# bb_pct = YES. SQUARE ROOT
# k_pct = NO
# iso = NO
# babip = YES. SQUARE ROOT
# avg = YES. SQUARE ROOT
# obp = YES. SQUARE ROOT
# slg = NO
# ld_pct = NO
# gb_pct = NO
# fb_pct = N/A. REMOVED VARIABLE AS IT HAD NO CORRELATION TO FS_OBP
# iffb_pct = YES. SQUARE ROOT
# hr_fb = YES. SQUARE ROOT
# o_swing_pct = YES. SQUARE ROOT
# z_swing_pct = NO
# swing_pct = NO
# o_contact_pct = NO
# z_contact_pct = NO
# contact_pct = NO
# Response Variable Distribution
# fs_obp: Normal Distribution


# Quantitative Regression Analysis ----------------------------------------

# Linear Regression -------------------------------------------------------
# Let's run linear regression model on entire data set
fs_obp_lm = lm(fs_obp ~., mlb_df)
summary(fs_obp_lm)
# We can see that many of the variables have p-values that are greater than 
# 0.05: ab, r, rbi, bb_pct, slg, ld_pct, gb_pct, iffb_pct.
# We see variables with p-values less than 0.05: h, avg, obp, contact_pct
# Let's run model using h, avg, obp, contact_pct
fs_obp_lm = lm(fs_obp ~ h + avg + obp + contact_pct, mlb_df)
summary(fs_obp_lm)
# All predictors p-values are now less than 0.05. 
# Of the predictors that we have in this model, let's add our transformations
# that we went with in our exploratory analysis to help make distributions of
# predictors more normal. 

# Earlier, we decided to take square root of both avg and obp to 
# help make predictors more normal
fs_obp_lm = lm(fs_obp ~ h + sqrt(avg) + sqrt(obp) + contact_pct, mlb_df)
summary(fs_obp_lm)
# No major model improvements.
# Let's not transform either predictor
fs_obp_lm = lm(fs_obp ~ h + avg + obp + contact_pct, mlb_df)
summary(fs_obp_lm)

# Let's add in the variables that we initially removed, one by one to see
# if their p-values pass the hypothesis test: ab, r, rbi, bb_pct, slg, ld_pct,
# gb_pct, iffb_pct.
# If we transformed data in our earlier analysis, do the same here
# ab
fs_obp_lm = lm(fs_obp ~ h + avg + obp + contact_pct + ab, mlb_df)
summary(fs_obp_lm)
# Let's remove ab

# sqrt(r)
fs_obp_lm = lm(fs_obp ~ h + avg + obp + contact_pct + sqrt(r), mlb_df)
summary(fs_obp_lm)
# Let's remove sqrt(r)

# sqrt(rbi)
fs_obp_lm = lm(fs_obp ~ h + avg + obp + contact_pct + sqrt(rbi), mlb_df)
summary(fs_obp_lm)
# Adding sqrt(rbi) makes h not pass hypothesis test. Let's just try rbi
fs_obp_lm = lm(fs_obp ~ h + avg + obp + contact_pct + rbi, mlb_df)
summary(fs_obp_lm)
# Let's keep rbi

# sqrt(bb_pct)
fs_obp_lm = lm(fs_obp ~ h + avg + obp + contact_pct + rbi + sqrt(bb_pct), mlb_df)
summary(fs_obp_lm)
# Let's remove sqrt(bb_pct)

# slg
fs_obp_lm = lm(fs_obp ~ h + avg + obp + contact_pct + rbi + slg, mlb_df)
summary(fs_obp_lm)
# Let's remove slg

# ld_pct
fs_obp_lm = lm(fs_obp ~ h + avg + obp + contact_pct + rbi + ld_pct, mlb_df)
summary(fs_obp_lm)
# Let's remove ld_pct

# gb_pct
fs_obp_lm = lm(fs_obp ~ h + avg + obp + contact_pct + rbi + gb_pct, mlb_df)
summary(fs_obp_lm)
# Let's remove gb_pct

# sqrt(iffb_pct)
fs_obp_lm = lm(fs_obp ~ h + avg + obp + contact_pct + rbi + sqrt(iffb_pct), mlb_df)
summary(fs_obp_lm)
# Let's remove sqrt(iffb_pct)

# Let's have the fitted model on entire data set predict fs_obp and see how well
# it performs by checking mean squared error
fs_obp_lm = lm(fs_obp ~ h + avg + obp + contact_pct + rbi, mlb_df)
summary(fs_obp_lm)
pred_fs_obp_lm = predict(fs_obp_lm, newdata = mlb_df)
mean((pred_fs_obp_lm - mlb_df$fs_obp)^2)
# Mean squared error of 0.0008. However, this was using the model fitted on the
# entire data set to predict fs_obp.
# R^2 = 44.81

# Training and Test Data
# Now, let's create training and test data from mlb_df.
# We can then use fitted model on training data to predict test data.
dim(mlb_df)
# 320 observations.
# Since we only have 320 observations, our parameter estimates will have higher 
# variance. Since we have a smaller number of observations, let's do a 
# 70:30 split on our data where 70% of our data will be our training data,
# and 30% of our data will be our test data
set.seed(1)
index = createDataPartition(mlb_df$fs_obp, p = 0.7, list = FALSE)
mlb_train = mlb_df[index,]
mlb_test = mlb_df[-index,]

dim(mlb_train)
# 226 observations
dim(mlb_test)
# 94 observations

# Fit model that we made earlier to training data
fs_obp_lm = lm(fs_obp ~ h + avg + obp + contact_pct + rbi, mlb_train)
summary(fs_obp_lm)
# All predictors have p-values less than 0.05, besides rbi. Since we 
# initially removed rbi, then added it back in. Let's remove it again
fs_obp_lm = lm(fs_obp ~ h + avg + obp + contact_pct, mlb_train)
summary(fs_obp_lm)
# All predictors have p-values less than 0.05. contact_pct just barely
# passing the hypothesis test here.
# R^2 value is 0.4911 meaning our trained model explains 49% of the variability
# in our training data.

# Let's use this model to predict test data
pred_fs_obp_lm = predict(fs_obp_lm, newdata = mlb_test)
mean((pred_fs_obp_lm - mlb_test$fs_obp)^2)
# Mean squared error of 0.00099
# This is our first fitted model on the training data that was used to predict
# test data. We will use this to compare other regression models that we fit.

# Look at predicted values vs actual values
par(mfrow = c(1, 1))
lm_plot = data.frame(Actual = mlb_test$fs_obp, Predicted = pred_fs_obp_lm)
ggplot(lm_plot, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # Adds a line of perfect prediction
  labs(x = "Actual Full Season OBP", 
       y = "Predicted Full Season OBP", 
       title = "Actual vs. Predicted") +
  theme_minimal()


# Ridge Regression --------------------------------------------------------
# Try ridge regression model
train_matrix = model.matrix(fs_obp ~., mlb_train)
test_matrix = model.matrix(fs_obp ~., mlb_test)
# Now we need to select lambda using cross-validation
cv_out = cv.glmnet(train_matrix, mlb_train$fs_obp, alpha = 0)
best_lam = cv_out$lambda.min
best_lam
# Lambda chosen by cross-validation is 0.00247
# Now we fit ridge regression model and make predictions:
fs_obp_ridge = glmnet(train_matrix, mlb_train$fs_obp, alpha = 0)
pred_fs_obp_ridge = predict(fs_obp_ridge, s = best_lam, newx = test_matrix)
# Find mean squared error:
mean((pred_fs_obp_ridge - mlb_test$fs_obp)^2)
# Produce MSE of 0.001
# This is the same MSE of our linear regression model.

# Lasso Regression --------------------------------------------------------
# Try lasso model
train_matrix = model.matrix(fs_obp ~., mlb_train)
test_matrix = model.matrix(fs_obp ~., mlb_test)
# Now we need to select lambda using cross-validation
cv_out = cv.glmnet(train_matrix, mlb_train$fs_obp, alpha = 1)
best_lam = cv_out$lambda.min
best_lam
# Lambda chosen by cross-validation is 0.0000091
# Now we fit lasso regression model and make predictions:
fs_obp_lasso = glmnet(train_matrix, mlb_train$fs_obp, alpha = 1)
pred_fs_obp_lasso = predict(fs_obp_lasso, s = best_lam, newx = test_matrix)
# Find mean squared error:
mean((pred_fs_obp_lasso - mlb_test$fs_obp)^2)
# Produced MSE of 0.001. Same as MSE of ridge model and linear regression model.


# Principal Components Regression -----------------------------------------
fs_obp_pcr = pcr(fs_obp ~., data = mlb_train, 
              scale = TRUE, validation = "CV")
summary(fs_obp_pcr)
validationplot(fs_obp_pcr, val.type = "MSEP")
# We see that M = 6 produces lowest mean squared error of prediction on training data
# The PCR model when M = 6, explains 51% of the variance of the training data.
pred_fs_obp_pcr = predict(fs_obp_pcr, mlb_test, ncomp = 6)
mean((pred_fs_obp_pcr - mlb_test$fs_obp)^2)
# Produces mean squared error rate of 0.001. Same as previous models that we fit.


# Partial Least Squares ---------------------------------------------------
# Try partial least squares model
fs_obp_plsr = plsr(fs_obp ~., data = mlb_train, 
                 scale = TRUE, validation = "CV")
summary(fs_obp_plsr)
validationplot(fs_obp_plsr, val.type = "MSEP")
# We see that M = 10 produces lowest mean squared error of prediction on training data.
# However, we are trying to reduce variables in the model, so let's look at when
# M = 3 as that has almost same MSEP as when M = 10.
# The partial least squares model when M = 3 explains about 51% of the variance of 
# training data.
pred_fs_obp_plsr = predict(fs_obp_plsr, mlb_test, ncomp = 3)
mean((pred_fs_obp_plsr - mlb_test$fs_obp)^2)
# Produces mean squared error rate of 0.001. Same as previous models fitted.

# Regression Trees --------------------------------------------------------
# Try regression tree
fs_obp_tree = tree(fs_obp ~ ., mlb_train)
summary(fs_obp_tree)
# 16 terminal nodes
# 11 variables used in tree model
plot(fs_obp_tree)
text(fs_obp_tree, pretty = 0)

yhat = predict(fs_obp_tree, newdata = mlb_test)
mean((yhat - mlb_test$fs_obp)^2)
# Produced MSE of 0.001. Same as previous models.
# Since this tree is quite confusing to understand, we want to try and prune the 
# tree using cross-validation.
cv_tree = cv.tree(fs_obp_tree)
plot(cv_tree$size, cv_tree$dev, type = "b")
# We see that tree size 5 minimizes cross-validation error
# Let's prune our tree to make it more interpretable and see if it has any effect
# on our MSE
fs_obp_prune = prune.tree(fs_obp_tree, best = 5)
summary(fs_obp_prune)
# 3 Variables used: obp, contact_pct, r
# Terminal nodes: 5
plot(fs_obp_prune)
text(fs_obp_prune, pretty = 0)
yhat = predict(fs_obp_prune, newdata = mlb_test)
mean((yhat - mlb_test$fs_obp^2))
# Test MSE of 0.21. Made tree more interpretable, but test MSE was not better
# the previous models fit.


# Bagging Model -----------------------------------------------------------
# Try bagging model
fs_obp_bag = bagging(fs_obp ~., data = mlb_train)
fs_obp_bag
yhat_bag = predict(fs_obp_bag, newdata = mlb_test)
mean((yhat_bag - mlb_test$fs_obp)^2)
# Produced test MSE of 0.001. No better than what we already have found. 


# Random Forest -----------------------------------------------------------
# Try random forest model with 100 trees and using all predictors
set.seed(1)
fs_obp_rf = randomForest(fs_obp ~., ntree = 100,
                       mtry = 12, importance = TRUE, data = mlb_train)
fs_obp_rf
# Explains 44.55% of the variance
varImpPlot(fs_obp_rf)
yhat_rf = predict(fs_obp_rf, newdata = mlb_test)
mean((yhat_rf - mlb_test$fs_obp)^2)
# Produced 0.001 MSE. No better than models fit before.


# Boosted Model -----------------------------------------------------------
# Our boosted models that we will fit will use lambda values of 0.001, 0.01, 
# 0.1, and 0.2 and 1000 trees.

# lambda = 0.001:
fs_obp_boost = gbm(fs_obp ~., data = mlb_train, distribution = "gaussian",
                n.trees = 1000)
yhat_boost = predict(fs_obp_boost, newdata = mlb_test, 
                     n.trees = 1000)
mean((yhat_boost - mlb_test$fs_obp)^2)
# Test MSE of 0.001.

# lambda = 0.01:
fs_obp_boost = gbm(fs_obp ~., data = mlb_train, distribution = "gaussian",
                   n.trees = 1000, shrinkage = 0.01)
yhat_boost = predict(fs_obp_boost, newdata = mlb_test, 
                     n.trees = 1000)
mean((yhat_boost - mlb_test$fs_obp)^2)
# Test MSE of 0.001.

# lambda = 0.1:
fs_obp_boost = gbm(fs_obp ~., data = mlb_train, distribution = "gaussian",
                   n.trees = 1000, shrinkage = 0.1)
yhat_boost = predict(fs_obp_boost, newdata = mlb_test, 
                     n.trees = 1000)
mean((yhat_boost - mlb_test$fs_obp)^2)
# Test MSE of 0.001.

# lambda = 0.2
fs_obp_boost = gbm(fs_obp ~., data = mlb_train, distribution = "gaussian",
                   n.trees = 1000, shrinkage = 0.2)
yhat_boost = predict(fs_obp_boost, newdata = mlb_test, 
                     n.trees = 1000)
mean((yhat_boost - mlb_test$fs_obp)^2)
# Test MSE of 0.001.
# No improvement of test MSE with boosted model.


# Regression Models Conclusion --------------------------------------------
# We fit 9 models in our analysis.
# We were able to produce a test MSE of 0.001 for almost all models that
# we fit to the training data. Of all the models, the most interpretable is our
# simple linear model below:
fs_obp_lm = lm(fs_obp ~ h + avg + obp + contact_pct, mlb_train)
summary(fs_obp_lm)
pred_fs_obp_lm = predict(fs_obp_lm, newdata = mlb_test)
mean((pred_fs_obp_lm - mlb_test$fs_obp)^2)

# This linear model takes hits, average, obp and contact percentage to
# predict the full season obp of a player.
# Here is the model below with it the coefficients:
# Full Season OBP estimate = 0.1598 + 0.0011(*Hits*) - 0.2537(*Average*) + 
# 0.5084(*On-Base Percentage*) + 0.0523(*Contact Percentage*)


# Sources --------------------------------------------------------------
# https://docs.google.com/spreadsheets/d/1U_QsSv6-68VLI0-5YrSPp2Bo-QhzZftiMU10b1Ajk0k/edit#gid=1381900348. 

# https://www.mlb.com/glossary/standard-stats/on-base-percentage#







# Throwaway Code ----------------------------------------------------------

# Just adding sqrt transformation to o_swing_pct
fs_obp_lm = lm(fs_obp ~ pa + ab + h + sqrt(o_swing_pct) + z_swing_pct + swing_pct
               + o_contact_pct + z_contact_pct + contact_pct, mlb_df)
summary(fs_obp_lm)
# No major statistical difference in model. 
# Let's go back to model without transformation
fs_obp_lm = lm(fs_obp ~ pa + ab + h + o_swing_pct + z_swing_pct + swing_pct
               + o_contact_pct + z_contact_pct + contact_pct, mlb_df)
summary(fs_obp_lm)
# Now let's add in each of the predictors we removed and see how the p-values 
# are affected. If we transformed them in our analysis earlier, transform them
# for the model. If p-value for variable (or transformed variable is less than
# 0.05, keep the predictor).


# sqrt(hr)
fs_obp_lm = lm(fs_obp ~ pa + ab + h + o_swing_pct + z_swing_pct + swing_pct
               + o_contact_pct + z_contact_pct + contact_pct + sqrt(hr), mlb_df)
summary(fs_obp_lm)
# Let's not add sqrt(hr)

# sqrt(r)
fs_obp_lm = lm(fs_obp ~ pa + ab + h + o_swing_pct + z_swing_pct + swing_pct
               + o_contact_pct + z_contact_pct + contact_pct + sqrt(r), mlb_df)
summary(fs_obp_lm)
# Let's not add sqrt(r)

# sqrt(rbi)
fs_obp_lm = lm(fs_obp ~ pa + ab + h + o_swing_pct + z_swing_pct + swing_pct
               + o_contact_pct + z_contact_pct + contact_pct + sqrt(rbi), mlb_df)
summary(fs_obp_lm)
# Let's not add sqrt(rbi)

# sb
fs_obp_lm = lm(fs_obp ~ pa + ab + h + o_swing_pct + z_swing_pct + swing_pct
               + o_contact_pct + z_contact_pct + contact_pct + sb, mlb_df)
summary(fs_obp_lm)
# Let's not add sb

# sqrt(bb_pct)
fs_obp_lm = lm(fs_obp ~ pa + ab + h + o_swing_pct + z_swing_pct + swing_pct
               + o_contact_pct + z_contact_pct + contact_pct + sqrt(bb_pct), mlb_df)
summary(fs_obp_lm)
# Let's not add sqrt(bb_pct)

# k_pct
fs_obp_lm = lm(fs_obp ~ pa + ab + h + o_swing_pct + z_swing_pct + swing_pct
               + o_contact_pct + z_contact_pct + contact_pct + k_pct, mlb_df)
summary(fs_obp_lm)
# Let's not add k_pct

# iso
fs_obp_lm = lm(fs_obp ~ pa + ab + h + o_swing_pct + z_swing_pct + swing_pct
               + o_contact_pct + z_contact_pct + contact_pct + iso, mlb_df)
summary(fs_obp_lm)
# Keep iso

# sqrt(babip)
fs_obp_lm = lm(fs_obp ~ pa + ab + h + o_swing_pct + z_swing_pct + swing_pct
               + o_contact_pct + z_contact_pct + contact_pct + iso
               + sqrt(babip), mlb_df)
summary(fs_obp_lm)
# Let's not add sqrt(babip)

# sqrt(avg)
fs_obp_lm = lm(fs_obp ~ pa + ab + h + o_swing_pct + z_swing_pct + swing_pct
               + o_contact_pct + z_contact_pct + contact_pct + iso
               + sqrt(avg), mlb_df)
summary(fs_obp_lm)
# Let's not add sqrt(avg)

# sqrt(obp)
fs_obp_lm = lm(fs_obp ~ pa + ab + h + o_swing_pct + z_swing_pct + swing_pct
               + o_contact_pct + z_contact_pct + contact_pct + iso
               + sqrt(obp), mlb_df)
summary(fs_obp_lm)
# Let's not add sqrt(obp)

# slg
fs_obp_lm = lm(fs_obp ~ pa + ab + h + o_swing_pct + z_swing_pct + swing_pct
               + o_contact_pct + z_contact_pct + contact_pct + iso
               + slg, mlb_df)
summary(fs_obp_lm)
# Let's not add slg

# ld_pct
fs_obp_lm = lm(fs_obp ~ pa + ab + h + o_swing_pct + z_swing_pct + swing_pct
               + o_contact_pct + z_contact_pct + contact_pct + iso
               + ld_pct, mlb_df)
summary(fs_obp_lm)
# Let's not add ld_pct

# gb_pct
fs_obp_lm = lm(fs_obp ~ pa + ab + h + o_swing_pct + z_swing_pct + swing_pct
               + o_contact_pct + z_contact_pct + contact_pct + iso
               + gb_pct, mlb_df)
summary(fs_obp_lm)
# Let's not add gb_pct

# sqrt(iffb_pct)
fs_obp_lm = lm(fs_obp ~ pa + ab + h + o_swing_pct + z_swing_pct + swing_pct
               + o_contact_pct + z_contact_pct + contact_pct + iso
               + sqrt(iffb_pct), mlb_df)
summary(fs_obp_lm)
# Let's not add iffb_pct

# sqrt(hr_fb)
fs_obp_lm = lm(fs_obp ~ pa + ab + h + o_swing_pct + z_swing_pct + swing_pct
               + o_contact_pct + z_contact_pct + contact_pct + iso
               + sqrt(hr_fb), mlb_df)
summary(fs_obp_lm)
# Let's not add sqrt(hr_fb)

# From the data set, we found this model to best predict fs_obp:
fs_obp_lm = lm(fs_obp ~ pa + ab + h + o_swing_pct + z_swing_pct + swing_pct
               + o_contact_pct + z_contact_pct + contact_pct + iso, mlb_df)
summary(fs_obp_lm)
# All predictors p-values are less than 0.05
# R^2 = 0.4971


# Earlier, we added iso in to our model. Let's remove that first and
# see what that does to our model
fs_obp_lm = lm(fs_obp ~ pa + ab + h + o_swing_pct + z_swing_pct + swing_pct
               + o_contact_pct + z_contact_pct + contact_pct, mlb_train)
summary(fs_obp_lm)
# o_contact_pct still does not have p-value less than 0.05.
# Let's remove it
fs_obp_lm = lm(fs_obp ~ pa + ab + h + o_swing_pct + z_swing_pct + swing_pct
               + z_contact_pct + contact_pct, mlb_train)
summary(fs_obp_lm)
# Now, z_contact_pct does not have p-value less than 0.05. Let's remove it
fs_obp_lm = lm(fs_obp ~ pa + ab + h + o_swing_pct + z_swing_pct + swing_pct
               + contact_pct, mlb_train)
summary(fs_obp_lm)
# Now, o_contact_pct does not have p-value less than 0.05.
# Let's remove it
fs_obp_lm = lm(fs_obp ~ pa + ab + h + z_swing_pct + swing_pct + contact_pct, mlb_train)
summary(fs_obp_lm)
# contact_pct does not have p-value less than 0.05. Let's remove it.
fs_obp_lm = lm(fs_obp ~ pa + ab + h + z_swing_pct + swing_pct, mlb_train)
summary(fs_obp_lm)
# All predictors in this model have p-values less than 0.05
# Our model explains 51% of variability in model.







