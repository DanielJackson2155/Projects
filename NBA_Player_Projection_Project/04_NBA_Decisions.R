# NBA Player Projection Project -------------------------------------------

# Libraries Used ----------------------------------------------------------
library(readxl)
library(dplyr)

# Make Decisions ----------------------------------------------------------

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

# 1. Analyze Bol Bol and Bitazde ------------------------------------------
# Analyze the players whose options were declined:
#     Bol Bol: $2.2 million
#     Bitazde: $2.1 million

# Cap Space Projection: $26-63.8 million

# Since both Bol Bol and Bitazde declined their options, we now have 
# their combined $2.2 million and $2.1 million added to our cap space.
# Using our predicted win share values, let's take a look a and see
# what we estimated each player's win shares dollar value to be.

# Let's re-read 2022-2023 NBA Data because Goga Bitazde did not play over 1,000 minutes
# last year.
twenty_three_nba_df = read.csv("Week_7/NBA_Player_Projection_Project/2023_NBA_Data.csv")
twenty_three_nba_df = twenty_three_nba_df[, -which(names(twenty_three_nba_df) ==
                                                     "Player.additional")]
twenty_three_nba_df = twenty_three_nba_df[, -which(names(twenty_three_nba_df) ==
                                                     "Rk")]
twenty_three_nba_df = twenty_three_nba_df[, -which(names(twenty_three_nba_df) ==
                                                     "X")]
twenty_three_nba_df = twenty_three_nba_df[, -which(names(twenty_three_nba_df) ==
                                                     "X.1")]
twenty_three_nba_df = twenty_three_nba_df[, -which(names(twenty_three_nba_df) ==
                                                     "WS.48")]
colnames(twenty_three_nba_df) = tolower(colnames(twenty_three_nba_df))
predictor_names = colnames(twenty_three_nba_df)
new_names = gsub("\\.", "_pct", predictor_names)
colnames(twenty_three_nba_df) = new_names

player_frequencies = table(twenty_three_nba_df[["player"]])
# Extract player names that have counts greater than 1 (indicating duplicates)
duplicate_players = names(player_frequencies[player_frequencies > 1])

twenty_three_nba_df = twenty_three_nba_df[!(twenty_three_nba_df[["player"]] %in% 
                                              duplicate_players & 
                                              twenty_three_nba_df[["tm"]] != "TOT"), ]


# Now let's add our estimated win share and win share dollar value columns into
# data frame
twenty_three_nba_df$ws_est = predict(ws_lm, newdata = twenty_three_nba_df)
twenty_three_nba_df$ws_dollar_val = ((twenty_three_nba_df$ws_est) * 3.44)

# Now let's pull Boll Boll and Bitazde information
players_to_search = c("Bol Bol", "Goga Bitadze")
selected_players = twenty_three_nba_df[twenty_three_nba_df$player %in% 
                                         players_to_search, ]
print(selected_players[c("player", "pos", "ws_est", "ws_dollar_val")])

#   player          pos       ws_est          ws_dollar_val
#   Goga Bitadze    C         2.058863        7.082489
#   Bol Bol         PF        2.666870        9.174034

# We see that we were anticipating Bitadze to be worth approx $7.1 million in the
# 2023-2024 season.
# We were anticipating Bol Bol to be worth approx $9.2 million in the 2023-2024
# season.
# Based on our predicted win share dollar values for each player, we would have liked
# to sign them both at their player options values.
# However, they declined their options and will be testing the free agency market.

# With both players declining their option, we know that our cap space currently
# sits at $26 million.


# 2. Decide whether to retain Harris --------------------------------------
# Pull Harris' estimated win share stats
selected_players = twenty_three_nba_df[twenty_three_nba_df$player == "Gary Harris", ]
print(selected_players[c("player", "pos", "ws_est", "ws_dollar_val")])

# player        pos   ws_est        ws_dollar_val
# Gary Harris   SG    2.320093      7.981121

# Gary Harris' potential salary will be $13 million. Based on our predictions,
# we see that is estimated win share dollar value for the 2023-2024 season will be
# approx $8 million. Therefore, I do not think that we should resign Gary Harris.
# The $13 million that he will make next year does not match the $8 million that we
# are expecting him to be worth.
# Since his $13 million his not guaranteed, we should waive Gary Harris.
# This brings our cap space number from $26 mill to $39 million.


# 3. Do we want to guarantee Isaac and/or Fultz's contracts ----------------
# Let's pull Isaac's and Fultz's win share stats
players_to_search = c("Markelle Fultz", "Jonathan Isaac")
selected_players = twenty_three_nba_df[twenty_three_nba_df$player %in% 
                                         players_to_search, ]
print(selected_players[c("player", "pos", "ws_est", "ws_dollar_val")])

# player            pos   ws_est        ws_dollar_val
# Markelle Fultz    PG    3.740426      12.867067
# Jonathan Isaac    PF    1.123787      3.865828

# We are expecting Fultz to be worth approx $12.9 million in the 2023-2024 
# season. We are planning to pay Fultz $17 million next year. If we waive him,
# we will have to pay a $2 million buyout. If we add that $2 million dollar buyout
# to what we think he will be worth next year, that puts his adjusted worth at 
# $14.9 million. We would still be netting out $2.1 million by waiving Fultz.
# Therefore, we will not re-sign Fultz.
# This brings our salary cap from $39 million to $54 million after we buy
# Fultz out of his contract.

# We are expecting Isaac to be worth approx $3.9 million in the 2023-2024 season.
# We are planning to pay Isaac $7.4 million next year. If we waive him,
# we will have to pay a $7.6 million buyout. Let's add that $7.6 to what we think
# he will be worth next year. That puts his adjusted worth at $11.5 million.
# We would still be netting out $5.9 million by not resigning Isaac.
# Therefore, we will not re-sign Isaac.
# This brings our salary cap from $54 million to $63.8 million.

# 4. Create a Free agency Plan A and Plan B -------------------------------
# We decided to waive Harris, Fultz and Isaac going into the 2023-2024 season.
# That means we are down a SG, PG, PF.
# Bol Bol and Bitadze also declined their player options, which means we are down
# a C and another PF.
# In this analysis, we assume that we will not be making any trades.
# We also assume that we will be signing any free agent for one year and that 
# they are worth their market value.

# What we need:
# PG
# SG
# 2 PFs
# C

# Let's read in our free agent data
getwd()
fa_df = readxl::read_xlsx("Week_7/NBA_Player_Projection_Project/NBA_Free_Agency_Data.xlsx")

# Linear model we will be using:
ws_lm = lm(ws ~ g + mp + per + ftr + trb_pct + ast_pct + blk_pct + 
             usg_pct + vorp, train_df)

# Let's clean up data frame so our predictors match previous data frames
fa_df = fa_df[, -which(names(fa_df) == "Player-additional")]
colnames(fa_df) = tolower(colnames(fa_df))

# Remove age variable. There are two in data frame
fa_df = fa_df[, -which(names(fa_df) == "age")]

# Remove pos column
fa_df = fa_df[, -which(names(fa_df) == "pos")]

# Rename pos. column
colnames(fa_df)[colnames(fa_df) == "pos."] = "pos"

# Change % to _pct
predictor_names = colnames(fa_df)
new_names = gsub("\\%", "_pct", predictor_names)
colnames(fa_df) = new_names

# change " " to "_"
predictor_names = colnames(fa_df)
new_names = gsub(" ", "_", predictor_names)
colnames(fa_df) = new_names

# Let's use trained linear model to predict each players win share value and win 
# share dollar value
fa_df$ws_est = predict(ws_lm, newdata = fa_df)
fa_df$ws_dollar_val = ((fa_df$ws_est) * 3.44)

# Let's convert market_value column into market value by millions
fa_df$market_value = (fa_df$market_value) / 1000000

# Check for NA values by column
colSums(is.na(fa_df))
# 102 missing values in the market_value column
# Let us look at what the min market_value is in the data set:
min(fa_df$market_value, na.rm = TRUE)
# $1.5 Million is the minimum market value
# Let's assume that all players with no market value are worth the minimum
# $1.5 million.
# Change all NAs from NA to 1.5
fa_df$market_value = ifelse(is.na(fa_df$market_value), 1.5, fa_df$market_value)
# Check for NA values by column
colSums(is.na(fa_df))
# No more NAs

# As mentioned earlier, we mentioned that we will not be making any trades.
# Therefore, we need to replace the five players that we just lost.
# We need:
# 1 PG
# 1 SG
# 2 PFs
# 1 C

# We see that we do not need a SF based on what we just waived.
# Let's filter out all of the Small Forwards in our data set
no_sf_fa_df = subset(fa_df, pos != "SF")

# Since we need 5 players and our cap space is $63.8 million,
# that puts our average salary per player at $12.76 per player.
63.8 / 5
# That means we need to be strategic in maximizing our free agency plan.

# For plan A, we will play it more on the safe side and maximize the players
# we get based on what we need. Meaning we will not over spend to get anyone
# based on our win share dollar value model. We will look at what players, by
# position, who have the biggest delta between what we think they are worth and what
# the market value is.

# For plan B, we will be a little more aggressive by overpaying and potentially
# making a big free agent splash.


# Plan A ------------------------------------------------------------------
# Let's filter out any players whose market values are worth more than what we
# think their win share dollar value will be.
plan_a_df = no_sf_fa_df[!fa_df$market_value > 
                          fa_df$ws_dollar_val, ]
plan_a_df = no_sf_fa_df[!no_sf_fa_df$market_value > no_sf_fa_df$ws_dollar_val, ]

# Create column that measures delta between ws_dollar_val and market_value
plan_a_df$delta = (plan_a_df$ws_dollar_val - plan_a_df$market_value)

# Write code that maximizes both the market_value of a player and maximizes
# the delta that we created.
# This will code will recommend what players to sign for the best value based on
# what we think they are worth and what their market value is.
# This code will also choose two PFs.
cap_space = 63.8
selected_players = plan_a_df %>%
  filter(pos %in% c("PF", "PG", "SG", "C")) %>% # filter by positions
  arrange(desc(market_value), desc(delta)) %>% # arrange by market_value and delta
  group_by(pos) %>% # Group by positions
  top_n(ifelse(first(pos) == "PF", 2, 1), wt = delta + market_value) %>% # Returns 2 PFs
  ungroup() %>%
  filter(sum(market_value) <= cap_space) %>%  # Filter to ensure total market_value is within cap_space
  select(player, pos, market_value, ws_dollar_val, delta)
print(selected_players)
# player           pos   market_value       ws_dollar_val     delta
# Josh Hart        SG            20         22.7              2.71
# Harrison Barnes  PF            15         21.3              6.26
# Mason Plumlee    C             12.5       27.9              15.4 
# Jevon Carter     PG             5         10.6              5.62
# Keita Bates-Diop PF             1.5       13.5              12.0 

# This code produced these five players. These five maximize the market value
# and delta (what we think they are worth next year and what the market says
# they are worth) that we created. The sum of these players market values add up to
sum(selected_players$market_value)
# $54 million
# Our cap space is $63.8
63.8 - 54
# This leaves us with 9.8 in cap space.
# Let us assume we can sign one more player with the remaining cap space that we have. 
# Since we initially did not sign an SF, let us look at top 5 SFs by delta to see who
# we can sign for the best value. 
sf_plan_a_df = subset(fa_df, pos == "SF")
sf_plan_a_df = sf_plan_a_df[!sf_plan_a_df$market_value > sf_plan_a_df$ws_dollar_val, ]
sf_plan_a_df$delta = (sf_plan_a_df$ws_dollar_val - sf_plan_a_df$market_value)

top_5_SFs = sf_plan_a_df %>%
  filter(pos == "SF" & market_value < rem_cap_space) %>%
  arrange(desc(delta)) %>%
  slice_head(n = 5) %>%
  select(player, pos, market_value, ws_dollar_val, delta)
print(top_5_SFs)
#   player          pos   market_value  ws_dollar_val delta
# Yuta Watanabe     SF    1.5           8.51          7.01
# Troy Brown Jr.    SF    5             11.4          6.44
# Torrey Craig      SF    7             12.9          5.88
# Anthony Lamb      SF    1.5           7.38          5.88
# Otto Porter Jr.   SF    1.5           6.14          4.64

# Let's grab Yuta Watanabe, since he is the free agent with the highest delta of 
# remaining SF free agents we can sign. This does leave us with surplus cap space, 
# but we are operating under the impression that we can only sign one more player and he is the
# best value for our approach. 
# Meaning we can get him for a good price based on what we expect of him.

# Plan B ------------------------------------------------------------------
# For Plan B, let's be a little more aggressive and go after players without
# ensuring that we are getting them at an expected value price.
# Add delta column to fa_df set
fa_df$delta = (fa_df$ws_dollar_val - fa_df$market_value)

#Let us take a bigger chance in this plan and use a majority of our salary cap 
# to sign one player.
# Let's look at top 5 free agents in each position by market value:
cap_space = 63.8
selected_players = fa_df %>%
  filter(market_value <= cap_space) %>%  # Filter by market value
  arrange(desc(market_value)) %>%  # Arrange by market value
  group_by(pos) %>%  # Group by positions
  slice_head(n = 5) %>%  # Select top 5 players within each position
  ungroup() %>%  # Ungroup the data
  select(player, pos, market_value, ws_dollar_val, delta)
print(selected_players)

# We are taking a chance and going after a player who may be worth way more than
# what we think we want to pay them. This is our aggressive approach though.
# Looking at the available players in Free Agency by position, let's go ahead and
# sign Fred VanFleet at market value of $40 million. This fills our PG spot.
rem_cap_space = 63.8 - 40
print(rem_cap_space)
# Now we only have $23.8 million to fill other four roster spots: 2 PFs, 1 C and
# 1 SG.

# Since we are taking a chance on VanFleet. Let's be slightly more conservative
# selecting our remaining four players by selecting them from the Plan A
# data set to ensure we are maximizing value of remaining four Free Agent spots.
selected_players = plan_a_df %>%
  filter(pos %in% c("PF", "SG", "C")) %>% # filter by positions
  arrange(desc(delta)) %>% # arrange by market_value and delta
  group_by(pos) %>% # Group by positions
  top_n(ifelse(first(pos) == "PF", 2, 1), wt = delta) %>% # Returns 2 PFs
  ungroup() %>%
  filter(sum(market_value) <= rem_cap_space) %>%  # Filter to ensure total market_value is within cap_space
  select(player, pos, market_value, ws_dollar_val, delta)
print(selected_players)
#   player            pos     market_value    ws_dollar_val   delta
# Mason Plumlee       C       12.5            27.9            15.4 
# Keita Bates-Diop    PF      1.5             13.5            12.0 
# Derrick Jones Jr.   PF      1.5             10.0            8.55
# Josh Okogie         SG      5               11.6            6.57

cap_space - sum(selected_players$market_value)
# This means we have $3.3 million remaining cap space
# Like we did in Plan A, let's look at top 5 players and pick player
# with highest delta
top_5_SFs = sf_plan_a_df %>%
  filter(pos == "SF" & market_value < rem_cap_space) %>%
  arrange(desc(delta)) %>%
  slice_head(n = 5) %>%
  select(player, pos, market_value, ws_dollar_val, delta)
print(top_5_SFs)
# Once again, Yuta Watanabe is the player that we can sign for the best value with
# the remaining salary cap that we have. We are left with a small surplus of $1.8 
# million in our cap.

# Conclusion --------------------------------------------------------------
# Plan A 
# Conservative approach:
# player           pos 
# Josh Hart        SG 
# Harrison Barnes  PF           
# Mason Plumlee    C            
# Jevon Carter     PG            
# Keita Bates-Diop PF            
# Yuta Watanabe    SF    

# Plan B
# More aggressive:
#   player            pos     
# Fred VanFleet       PG       
# Mason Plumlee       C      
# Keita Bates-Diop    PF    
# Derrick Jones Jr.   PF   
# Josh Okogie         SG  
# Yuta Watanabe       SF       