# NBA Player Projection Project -------------------------------------------
# Assignment:

# 1.) Create a model using at least three seasons of NBA data (2020-21 Links to an
#     external site., 2021-22 Links to an external site., 2022-23 Links to an external 
#     site.)from Basketball-Reference.com or the R package (hoopR), to predict player
#     Win Shares for the 2023-24 season. Review the slides from Basketball Analytics for 
#     a full breakdown of how win shares is created.

# Use player data where players have a minimum of 1000 minutes played.

# 2.) Translate Win Shares to a dollar value with your own methodology or Seth 
#     Partnow’s from the readings.

# 3.) Use your valuations to make the decisions assigned to your team in the class 
#     7 slides and create a write-up for non-technical stakeholders. For free agency 
#     decisions, use the market value assigned to each player as how much you will
#     need to spend on them.

# Analytics Director for:
# Orlando Magic

# Cap Space Projection: $26-63.8 million
# The Magic need to make decisions on Jonathan Isaac, Markelle Fultz, and Gary
# Harris.
# Isaac has a partial guarantee for $7.6 million next season and a potential salary
# of $17.4 million.
# Fultz has a partial guarantee of $2 million next season and a potential salary of
# $17 million.
# Harris has a potential salary of $13 million, which is not guaranteed.
# Assume the Magic will waive Bol Bol, Goga Bitazde and any players with a club option.

# Decisions that need to be made:
# 1.) Analyze the players whose options were declined:
#     Bol Bol: $2.2 million
#     Bitazde: $2.1 million

# 2.) Decide whether to retain Harris

# 3.) Decide whether you want to guarantee Isaac and/or Fultz's contracts.

# 4.) Create a Free agency Plan A and Plan B using your remaining cap space.
#     You can try to use all of your money on one player or spread it out.

# Magic Offseason Outlook     Potential 23-23 Salary        Buyout  
# Isaac                           $17.4 mil                 $7.6 mil
# Fultz                           $17 mil                   $2 mil
# Harris                          $13 mil                   N/A
# $$ Available                    $26-$63.8 mil


# Working Directory -------------------------------------------------------
getwd()

# Libraries Used ----------------------------------------------------------
library(corrplot)
library(dplyr)

# Read In and Clean Data --------------------------------------------------
# Looking for regular season data from three seasons: 2020-2021, 2021-2022, 2022-2023
# Created three separate CSV files by exporting data from Basketball Reference. 
# We will be using advanced NBA metrics to predict win share for each player.

# Link to 2020-2021 season from Basketball Reference:
# https://www.basketball-reference.com/leagues/NBA_2021_advanced.html#advanced_stats
twenty_one_nba_df = read.csv("Week_7/NBA_Player_Projection_Project/2021_NBA_Data.csv")
# Remove Player.additional and Rk (rank) in data frame. No use to us in this analysis
twenty_one_nba_df = twenty_one_nba_df[, -which(names(twenty_one_nba_df) ==
                                                 "Player.additional")]
twenty_one_nba_df = twenty_one_nba_df[, -which(names(twenty_one_nba_df) ==
                                                 "Rk")]
# Two empty columns in data frame: X and X.1
twenty_one_nba_df = twenty_one_nba_df[, -which(names(twenty_one_nba_df) ==
                                                 "X")]
twenty_one_nba_df = twenty_one_nba_df[, -which(names(twenty_one_nba_df) ==
                                                 "X.1")]

# Let's remove Win Share per 48 minutes variable, as we will only be focusing on
# win shares response variable
twenty_one_nba_df = twenty_one_nba_df[, -which(names(twenty_one_nba_df) ==
                                                 "WS.48")]

dim(twenty_one_nba_df)
# 705 observations
# 25 variables

# Link to 2021-2022 season from Basketball Reference:
# https://www.basketball-reference.com/leagues/NBA_2022_advanced.html
twenty_two_nba_df = read.csv("Week_7/NBA_Player_Projection_Project/2022_NBA_Data.csv")
# Remove Player.additional and Rk (rank) in data frame. No use to us in this analysis
twenty_two_nba_df = twenty_two_nba_df[, -which(names(twenty_two_nba_df) ==
                                                 "Player.additional")]
twenty_two_nba_df = twenty_two_nba_df[, -which(names(twenty_two_nba_df) ==
                                                 "Rk")]
# Two empty columns in data frame: X and X.1
twenty_two_nba_df = twenty_two_nba_df[, -which(names(twenty_two_nba_df) ==
                                                 "X")]
twenty_two_nba_df = twenty_two_nba_df[, -which(names(twenty_two_nba_df) ==
                                                 "X.1")]

# Let's remove Win Share per 48 minutes variable, as we will only be focusing on
# win shares response variable
twenty_two_nba_df = twenty_two_nba_df[, -which(names(twenty_two_nba_df) ==
                                                 "WS.48")]
dim(twenty_two_nba_df)
# 812 observations
# 25 variables

# Link to 2022-2023 season from Basketball Reference:
# https://www.basketball-reference.com/leagues/NBA_2023_advanced.html
twenty_three_nba_df = read.csv("Week_7/NBA_Player_Projection_Project/2023_NBA_Data.csv")
# Remove Player.additional and Rk (rank) in data frame. No use to us in this analysis
twenty_three_nba_df = twenty_three_nba_df[, -which(names(twenty_three_nba_df) ==
                                                 "Player.additional")]
twenty_three_nba_df = twenty_three_nba_df[, -which(names(twenty_three_nba_df) ==
                                                     "Rk")]
# Two empty columns in dataframe: X and X.1
twenty_three_nba_df = twenty_three_nba_df[, -which(names(twenty_three_nba_df) ==
                                                     "X")]
twenty_three_nba_df = twenty_three_nba_df[, -which(names(twenty_three_nba_df) ==
                                                     "X.1")]

# Let's remove Win Share per 48 minutes variable, as we will only be focusing 
# on win shares response variable
twenty_three_nba_df = twenty_three_nba_df[, -which(names(twenty_three_nba_df) ==
                                                     "WS.48")]

dim(twenty_three_nba_df)
# 679 observations
# 26 variables

# Let's tidy up variable names to help make our coding easier
colnames(twenty_one_nba_df)

# Player: Player name
# Pos:    Position
# Age:    Age
# Tm:     Team
# G:      Games Played
# MP:     Minutes Played
# PER:    Player Efficiency Rating
# TS.:    True Shooting Percentage
# X3PAr:  3-Point Attempt Rate
# FTr:    Free Throw Attempt Rate
# ORB.:   Offensive Rebound Percentage
# DRB.:   Defensive Rebound Percentage
# TRB.:   Total Rebound Percentage
# AST.:   Assist Percentage
# STL.:   Steal Percentage
# BLK.:   Block Percentage
# TOV.:   Turnover Percentage
# USG.:   Usage Percentage
# OWS:    Offensive Win Shares
# DWS:    Defensive Win Shares
# WS:     Win Shares (This will be our response variable)
# OBPM:   Offensive Box Plus/Minus
# DBPM:   Defensive Box Plus/Minus
# BPM:    Total Box Plus/Minus
# VORP:   Wins Over Replacement Player (Multiply by 2.7 to get Wins Over Replacement)

# Let's make all variables lower case in each data frame.
colnames(twenty_one_nba_df) = tolower(colnames(twenty_one_nba_df))
colnames(twenty_two_nba_df) = tolower(colnames(twenty_two_nba_df))
colnames(twenty_three_nba_df) = tolower(colnames(twenty_three_nba_df))

# Let's make all periods "." into "_per"
# 2020-2021
predictor_names = colnames(twenty_one_nba_df)
new_names = gsub("\\.", "_pct", predictor_names)
colnames(twenty_one_nba_df) = new_names

# 2021-2022
predictor_names = colnames(twenty_two_nba_df)
new_names = gsub("\\.", "_pct", predictor_names)
colnames(twenty_two_nba_df) = new_names

# 2022-2023
predictor_names = colnames(twenty_three_nba_df)
new_names = gsub("\\.", "_pct", predictor_names)
colnames(twenty_three_nba_df) = new_names


# Check to see if there are any duplicate names in each data frame.
# Looking at the data, you can see that there are players that played for multiple teams.
# Those players stats are broken up by each team and their totals.
# We only want their totals for their entire year. 
# We only want to look at players that have played at least 1000 minutes played.
# Let's filter out players that played at least 1000 minutes. Then we can check if there are 
# still duplicate names in each data frame. 
# Once we do that, we can then merge the three data frames together.

# 2020-2021
twenty_one_nba_df = twenty_one_nba_df[twenty_one_nba_df$mp >= 1000, ]
# Observations went from 705 to 260
# Check for duplicate player names
# Create a table of player frequencies
player_frequencies = table(twenty_one_nba_df[["player"]])
# Extract player names that have counts greater than 1 (indicating duplicates)
duplicate_players = names(player_frequencies[player_frequencies > 1])
# Print or view the duplicate player names
print(duplicate_players)
# [1] "Caris LeVert"   "Daniel Theis"   "Delon Wright"   "Gary Trent Jr." "James Harden"  
# [6] "Jarrett Allen"  "Kelly Olynyk"   "Nikola Vučević" "Norman Powell" 

# We have 9 duplicate players in this data frame.
# These 9 players played on multiple teams in the 2020-2021 season and also played
# over 1000 minutes with at least one of those teams.
# Subset data to only keep duplicate players season totals
twenty_one_nba_df = twenty_one_nba_df[!(twenty_one_nba_df[["player"]] %in% 
                                          duplicate_players & 
                                          twenty_one_nba_df[["tm"]] != "TOT"), ]
# 251 total observations


# 2021-2022
twenty_two_nba_df = twenty_two_nba_df[twenty_two_nba_df$mp >= 1000, ]
# Observations went from 812 to 291
# Check for duplicate player names
player_frequencies = table(twenty_two_nba_df[["player"]])
# Extract player names that have counts greater than 1 (indicating duplicates)
duplicate_players = names(player_frequencies[player_frequencies > 1])
# Print or view the duplicate player names
print(duplicate_players)
# [1] "Buddy Hield"              "Caris LeVert"             "CJ McCollum"             
# [4] "Dennis Schröder"          "Derrick White"            "Domantas Sabonis"        
# [7] "James Harden"             "Josh Hart"                "Josh Richardson"         
# [10] "Justin Holiday"           "Kristaps Porziņģis"       "Montrezl Harrell"        
# [13] "Nickeil Alexander-Walker" "Norman Powell"            "Robert Covington"        
# [16] "Seth Curry"               "Spencer Dinwiddie"        "Torrey Craig"            
# [19] "Tyrese Haliburton" 

# 19 duplicate players in 2021-2022 data.
# Subset data to only keep duplicate players season totals
twenty_two_nba_df = twenty_two_nba_df[!(twenty_two_nba_df[["player"]] %in% 
                                          duplicate_players & 
                                          twenty_two_nba_df[["tm"]] != "TOT"), ]
# 272 total observations


# 2022-2023
twenty_three_nba_df = twenty_three_nba_df[twenty_three_nba_df$mp >= 1000, ]
# Observations went from 679 to 281
# Check for duplicate player names
player_frequencies = table(twenty_three_nba_df[["player"]])
# Extract player names that have counts greater than 1 (indicating duplicates)
duplicate_players = names(player_frequencies[player_frequencies > 1])
# Print or view the duplicate player names
print(duplicate_players)
# [1] "D'Angelo Russell"    "Dorian Finney-Smith" "Eric Gordon"        
# [4] "Jakob Poeltl"        "Jalen McDaniels"     "Jarred Vanderbilt"  
# [7] "Josh Hart"           "Kevin Durant"        "Kyrie Irving"       
# [10] "Malik Beasley"       "Mason Plumlee"       "Mikal Bridges"      
# [13] "Mike Conley"         "Patrick Beverley"    "Reggie Jackson"     
# [16] "Russell Westbrook"   "Saddiq Bey"          "Spencer Dinwiddie"  

# 18 duplicate players in 2022-2023 data
# Subset data to only keep duplicate players season totals
twenty_three_nba_df = twenty_three_nba_df[!(twenty_three_nba_df[["player"]] %in% 
                                          duplicate_players & 
                                          twenty_three_nba_df[["tm"]] != "TOT"), ]
# 263 total observations


# Now that we removed duplicate players. We can now combine our three data sets into
# one data set using rbind function
# Before we do so, let's add a season column in each data frame for our exploratory
# analysis
twenty_one_nba_df$season = "2020-2021"
twenty_two_nba_df$season = "2021-2022"
twenty_three_nba_df$season = "2022-2023"
nba_df = rbind(twenty_one_nba_df, twenty_two_nba_df, twenty_three_nba_df)

# Now that we have our cleaned up data, we can now start to explore data!
dim(nba_df)
# 26 variables
# 786 observations

# Check for missing values in data set
colSums(is.na(nba_df))
# No missing values. Awesome!

# Exploratory Analysis ----------------------------------------------------
# What is a win share?
# A “Win Share” reflects the offensive (points added) and defensive (points prevented) 
# contributions of a player that led to one season win while on the floor.

# Offensive win share and defensive win share calculated separately. You add the
# two win shares together to get total win share for each player.

# Win share will be our response variable in our analysis.

# Look at top 10 players in WS over this three season stretch
top_ten_players_ws = nba_df[order(-nba_df$ws), ][1:10, c("season", "player", "ws")]
print(top_ten_players_ws)
# season          player                ws
# 2020-2021     Nikola Jokić            15.6
# 2021-2022     Nikola Jokić            15.2
# 2022-2023     Nikola Jokić            14.9
# 2021-2022     Giannis Antetokounmpo   12.9
# 2022-2023     Domantas Sabonis        12.6
# 2022-2023     Jimmy Butler            12.3
# 2022-2023     Joel Embiid             12.3
# 2021-2022     Joel Embiid             12.0
# 2021-2022     Rudy Gobert             11.7
# 2022-2023     Shai Gilgeous-Alexander 11.4

# Jokic has led the league in win shares the past three years. Only other player to
# show up in the top ten twice over that stretch is Joel Embiid in the 2021 and 
# 2022 season. Giannis is the next player behind Jokic. It makes sense that these
# three are in the top ten as Jokic won back to back MVPs in the 2021 and 2020 and 2021 
# seasons with Embiid winning MVP in the 2022 Season. And Giannis won back to back MVPs in
# 2018 and 2019

# Look at correlation plot of all predictors
corrplot::corrplot(cor(nba_df[sapply(nba_df, is.numeric)], 
                       use = "pairwise.complete.obs"), method = "color")

# From our correlation plot, we see that only one predictor has a negative correlation
# with win shares. That is the x3par variable. Let's go ahead and remove that predictor
nba_df = nba_df[, -which(names(nba_df) == "x3par")]

# We also know that win shares is the sum of offensive win shares and defensive win
# shares. For our analysis, we will focus solely on the total win shares as our
# response variable. We see the multicollinearity between the three variables in our
# correlation plot. Therefore, we will remove both the offensive and defensive
# win share variables.
nba_df = nba_df[, -which(names(nba_df) == "ows")]
nba_df = nba_df[, -which(names(nba_df) == "dws")]

# Let's look at correlation plot again
corrplot::corrplot(cor(nba_df[sapply(nba_df, is.numeric)], 
                       use = "pairwise.complete.obs"), method = "color")
# Let's create a new data frame called ws_df (win shares data frame) which removes 
# the descriptive variables in our nba data set: player, position, season, tm
ws_df = nba_df[, -which(names(nba_df) == "player")]
ws_df = ws_df[, -which(names(ws_df) == "pos")]
ws_df = ws_df[, -which(names(ws_df) == "season")]
ws_df = ws_df[, -which(names(ws_df) == "tm")]

# Let's move ws response variable to end of data set to help with correlation plot 
# visual using dplyr package
ws_df = ws_df %>% 
  select(-"ws", everything(), "ws")

# Let's look at correlation plot of ws_df
corrplot::corrplot(cor(ws_df[sapply(ws_df, is.numeric)], 
                       use = "pairwise.complete.obs"), method = "color")
# Look at correlations of each predictor
correlations = cor(ws_df)
ordered_ws_df_corr = correlations["ws", order(abs(correlations["ws",]), 
                                              decreasing = TRUE)]
print(ordered_ws_df_corr)
# We see that tov_pct and stl_pct are the lowest correlated predictors
# in regards to ws. 
# Let's remove those predictors
ws_df = ws_df[, -which(names(ws_df) == "tov_pct")]
ws_df = ws_df[, -which(names(ws_df) == "stl_pct")]

# Look at updated correlation plot
corrplot::corrplot(cor(ws_df[sapply(ws_df, is.numeric)], 
                       use = "pairwise.complete.obs"), method = "color")
# Looking for multicollinearity issues, we see that orb_pct and drb_pct are 
# highly correlated with trb_pct. Which makes sense as trb_pct is the sum of
# orb_pct and drb_pct. Let's remove orb_pct and drb_pct
ws_df = ws_df[, -which(names(ws_df) == "drb_pct")]
ws_df = ws_df[, -which(names(ws_df) == "orb_pct")]

# We also see the same thing with bpm and obpm/dbpm. Since bmp is total box plus/minus,
# let's remove obpm and dbpm
ws_df = ws_df[, -which(names(ws_df) == "obpm")]
ws_df = ws_df[, -which(names(ws_df) == "dbpm")]

# We see that bpm and vorp are highly correlated as well. Between the two,
# vorp is slightly more correlated with ws than bpm is. To avoid multicollinearity issues
# let us remove bpm.
ws_df = ws_df[, -which(names(ws_df) == "bpm")]

# Look at correlation plot again
corrplot::corrplot(cor(ws_df[sapply(ws_df, is.numeric)], 
                       use = "pairwise.complete.obs"), method = "color")
dim(ws_df)
# We now have 11 predictors to our ws response variable.

# Let us look at the distributions of each predictor and see if we need to do
# any transformations to help make the distributions more normal
# age
hist(ws_df$age, main = "Distribution of Age")
boxplot(ws_df$age, main = "Boxplot of Age",
        ylab = "Frequency")
# Age is relatively normal distribution. Slightly skewed to the right.
# Will not transform variable.

# g
hist(ws_df$g, main = "Distribution of Games Played")
boxplot(ws_df$g, main = "Boxplot of Games Played",
        ylab = "Frequency")
# g is skewed to the left with about 5 outliers.
# Try square root transformation.
hist(sqrt(ws_df$g), main = "Distribution of sqrt(Games Played)")
boxplot(sqrt(ws_df$g), main = "Boxplot of sqrt(Games Played)",
        ylab = "Frequency")
# Square root transformation did not make distribution more normal.
# Try squared transformation.
hist((ws_df$g)^2, main = "Distribution of (Games Played)^2")
boxplot((ws_df$g)^2, main = "Boxplot of (Games Played)^2",
        ylab = "Frequency")
# Squared transformation made distribution more normal.
# Try log transformation.
hist(log(ws_df$g), main = "Distribution of log(Games Played)")
boxplot(log(ws_df$g), main = "Boxplot of log(Games Played)",
        ylab = "Frequency")
# Log transformation did not make distribution more normal.
# Decided not to transform variable.


# mp:
hist(ws_df$mp, main = "Distribution of Minutes Played")
boxplot(ws_df$mp, main = "Boxplot of Minutes Played",
        ylab = "Frequency")
# Not much of a normal distribution. Slightly skewed right. No outliers.
# Square root
hist(sqrt(ws_df$mp), main = "Distribution of sqrt(Minutes Played)")
boxplot(sqrt(ws_df$mp), main = "Boxplot of sqrt(Minutes Played)",
        ylab = "Frequency")
# Made distribution slightly more normal. 
# Try squared
hist((ws_df$mp)^2, main = "Distribution of (Minutes Played)^2")
boxplot((ws_df$mp)^2, main = "Boxplot of (Minutes Played)^2",
        ylab = "Frequency")
# Made distribution very right skewed. One outlier.
# Try log
hist(log(ws_df$mp), main = "Distribution of log(Minutes Played)")
boxplot(log(ws_df$mp), main = "Boxplot of log(Minutes Played)",
        ylab = "Frequency")
# Made distribution slightly more normal than no transformation and the square
# root distribution. 
# Go with log transformation.


# per:
hist(ws_df$per, main = "Distribution of PER")
boxplot(ws_df$per, main = "Boxplot of PER",
        ylab = "Frequency")
# Slightly skewed right distribution. Several outliers.
# Try square root
hist(sqrt(ws_df$per), main = "Distribution of sqrt(PER)")
boxplot(sqrt(ws_df$per), main = "Boxplot of sqrt(PER)",
        ylab = "Frequency")
# Help make distribution more normal. Still several outliers.
# Try squared
hist((ws_df$per)^2, main = "Distribution of (PER)^2")
boxplot((ws_df$per)^2, main = "Boxplot of (PER)^2",
        ylab = "Frequency")
# Make distribution way more right skewed. More outliers.
# Try log
hist(log(ws_df$per), main = "Distribution of log(PER)")
boxplot(log(ws_df$per), main = "Boxplot of log(PER)",
        ylab = "Frequency")
# Still a few outliers. But most normal distribution of all transformations.
# Go with log transform


# ts_pct:
hist(ws_df$ts_pct, main = "Distribution of TS %")
boxplot(ws_df$ts_pct, main = "Boxplot of TS %",
        ylab = "Frequency")
# Relatively normal distribution. Several outliers
# Square root
hist(sqrt(ws_df$ts_pct), main = "Distribution of sart(TS %)")
boxplot(sqrt(ws_df$ts_pct), main = "Boxplot of sqrt(TS %)",
        ylab = "Frequency")
# Kept distribution relatively normal. Still several outliers.
# Squared
hist((ws_df$ts_pct)^2, main = "Distribution of (TS %)^2")
boxplot((ws_df$ts_pct)^2, main = "Boxplot of (TS %)^2",
        ylab = "Frequency")
# Did not make distribution any more normal. Still several outliers
# Log
hist(log(ws_df$ts_pct), main = "Distribution of log(TS %)")
boxplot(log(ws_df$ts_pct), main = "Boxplot of log(TS %)",
        ylab = "Frequency")
# Created negative values. Do not transform variable.
# Go with log transformation


# ftr:
hist(ws_df$ftr, main = "Distribution of FTR")
boxplot(ws_df$ftr, main = "Boxplot of FTR",
        ylab = "Frequency")
# Skewed right. Several outliers.
# Square root
hist(sqrt(ws_df$ftr), main = "Distribution of sqrt(FTR)")
boxplot(sqrt(ws_df$ftr), main = "Boxplot of sqrt(FTR)",
        ylab = "Frequency")
# Made distribution more normal. Few outliers
# Squared
hist((ws_df$ftr)^2, main = "Distribution of (FTR)^2")
boxplot((ws_df$ftr)^2, main = "Boxplot of (FTR)^2",
        ylab = "Frequency")
# Made distribution incredibly right skewed. A ton of outliers.
# Log
hist(log(ws_df$ftr), main = "Distribution of log(FTR)")
boxplot(log(ws_df$ftr), main = "Boxplot of log(FTR)",
        ylab = "Frequency")
# Made distribution normal. More outliers than square root transformation
# Go with square root transformation


# trb_pct
hist(ws_df$trb_pct, main = "Distribution of TRB %")
boxplot(ws_df$trb_pct, main = "Boxplot of TRB %",
        ylab = "Frequency")
# Skewed right. Ton of outliers.
# Square root
hist(sqrt(ws_df$trb_pct), main = "Distribution of sqrt(TRB %)")
boxplot(sqrt(ws_df$trb_pct), main = "Boxplot of sqrt(TRB %)",
        ylab = "Frequency")
# Slightly more normal. Still slightly skewed right. Few outliers.
# Squared
hist((ws_df$trb_pct)^2, main = "Distribution of (TRB %)^2")
boxplot((ws_df$trb_pct)^2, main = "Boxplot of (TRB %)^2",
        ylab = "Frequency")
# Made distribution way more right skewed. Ton of outliers.
# Log
hist(log(ws_df$trb_pct), main = "Distribution of log(TRB %)")
boxplot(log(ws_df$trb_pct), main = "Boxplot of log(TRB %)",
        ylab = "Frequency")
# Most normal distribution. No outliers.
# Go with log transformation


# ast_pt:
hist(ws_df$ast_pct, main = "Distribution of Ast %")
boxplot(ws_df$ast_pct, main = "Boxplot of Ast %",
        ylab = "Frequency")
# Skewed to the right. Ton of outliers
# Square root
hist(sqrt(ws_df$ast_pct), main = "Distribution of sqrt(Ast %)")
boxplot(sqrt(ws_df$ast_pct), main = "Boxplot of sqrt(Ast %)",
        ylab = "Frequency")
# More normal distribution. No outliers.
# Squared
hist((ws_df$ast_pct)^2, main = "Distribution of (Ast %)^2")
boxplot((ws_df$ast_pct)^2, main = "Boxplot of (Ast %)^2",
        ylab = "Frequency")
# Made distribution way more right skewed. Ton of outliers.
# Log
hist(log(ws_df$ast_pct), main = "Distribution of log(Ast %)")
boxplot(log(ws_df$ast_pct), main = "Boxplot of log(Ast %)",
        ylab = "Frequency")
# Similar effect to distribution that square root transformation had. No outliers.
# Go with square root. Slightly more evenly distributed.


# blk_pct:
hist(ws_df$blk_pct, main = "Distribution of Blk %")
boxplot(ws_df$blk_pct, main = "Boxplot of Blk %",
        ylab = "Frequency")
# Heavily skewed right. Ton of outliers.
# Square root
hist(sqrt(ws_df$blk_pct), main = "Distribution of sqrt(Blk %)")
boxplot(sqrt(ws_df$blk_pct), main = "Boxplot of sqrt(Blk %)",
        ylab = "Frequency")
# Slightly more normal. Still a lot of outliers.
# Squared
hist((ws_df$blk_pct)^2, main = "Distribution of (Blk %)^2")
boxplot((ws_df$blk_pct)^2, main = "Boxplot of (Blk %)^2",
        ylab = "Frequency")
# Made distribution way more right skewed. Ton of outliers.
# Log
hist(log(ws_df$blk_pct), main = "Distribution of log(Blk %)")
boxplot(log(ws_df$blk_pct), main = "Boxplot of log(Blk %)",
        ylab = "Frequency")
# Log transformation creates negative frequencies. Do not use this transformation.
# Go with square root transformation as it is the most normal of the transformations.


# usg_pct
hist(ws_df$usg_pct, main = "Distribution of Usg %")
boxplot(ws_df$usg_pct, main = "Boxplot of Usg %",
        ylab = "Frequency")
# Very normal distribution. Few outliers.
# Square root
hist(sqrt(ws_df$usg_pct), main = "Distribution of sqrt(Usg %)")
boxplot(sqrt(ws_df$usg_pct), main = "Boxplot of sqrt(Usg %)",
        ylab = "Frequency")
# Made distribution more normal. Only two outliers.
# Squared
hist((ws_df$usg_pct)^2, main = "Distribution of (Usg %)^2")
boxplot((ws_df$usg_pct)^2, main = "Boxplot of (Usg %)^2",
        ylab = "Frequency")
# Made distribution more right skewed. Ton of outliers.
# Log
hist(log(ws_df$usg_pct), main = "Distribution of log(Usg %)")
boxplot(log(ws_df$usg_pct), main = "Boxplot of log(Usg %)",
        ylab = "Frequency")
# Normal distribution. Two outliers. 
# Go with square root distribution. Most normal distribution of the transformations.


# vorp:
hist(ws_df$vorp, main = "Distribution of VORP")
boxplot(ws_df$vorp, main = "Boxplot of VORP",
        ylab = "Frequency")
# Skewed right. Ton of outliers.
# Square root
hist(sqrt(ws_df$vorp), main = "Distribution of sqrt(VORP)")
boxplot(sqrt(ws_df$vorp), main = "Boxplot of sqrt(VORP)",
        ylab = "Frequency")
# NAs were produced in square root distribution. Probably from the negative values.
# Squared
hist((ws_df$vorp)^2, main = "Distribution of (VORP)^2")
boxplot((ws_df$vorp)^2, main = "Boxplot of (VORP)^2",
        ylab = "Frequency")
# Made distribution more right skewed. Bunch of outliers.
# Log
hist(log(ws_df$vorp), main = "Distribution of log(VORP)")
boxplot(log(ws_df$vorp), main = "Boxplot of log(VORP)",
        ylab = "Frequency")
# NA values were produced due to negative VORP values.
# Do not do any transformations on this variable.


# Response Variable Distribution
# ws:
# Skewed to the right. Ton of outliers.
# Square root
hist(sqrt(ws_df$ws), main = "Distribution of sqrt(WS)")
boxplot(sqrt(ws_df$ws), main = "Boxplot of sqrt(WS)",
        ylab = "Frequency")
# NA values produced from negative ws values. 
# Squared:
hist((ws_df$ws)^2, main = "Distribution of (WS)^2")
boxplot((ws_df$ws)^2, main = "Boxplot of (WS)^2",
        ylab = "Frequency")
# Made distribution way more right skewed. Ton of outliers.
# Log
hist(log(ws_df$ws), main = "Distribution of log(WS)")
boxplot(log(ws_df$ws), main = "Boxplot of log(WS)",
        ylab = "Frequency")
# NA values produced as well.
# Do not transform response variable.


# Transformations that made distributions more normal on each predictor:
# age:      N/A
# g:        N/A
# mp:       Log
# per:      Log
# ts_pct:   N/A
# ftr:      Square root
# trb_pct:  Log
# ast_pct:  Square root
# blk_pct:  Square root
# usg_pct:  Sauare root
# vorp:     N/A

# Response variable
# ws:       N/A    


# Let us now fit our regression models to see which model will be the best at
# predicting win share.