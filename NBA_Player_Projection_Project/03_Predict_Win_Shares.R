# NBA Player Projection Project -------------------------------------------

# Now let us predict the win shares for each player in our data set for the 
# 2023-24 data set.

# We used our data from three NBA seasons to help us create a linear model that
# helps us predict win shares for each player. Let's use the most recent
# NBA season (2022-2023) to predict the Win Shares for players in the 2023-2024 season

# Linear model being used for estimates:
ws_lm = lm(ws ~ g + mp + per + ftr + trb_pct + ast_pct + blk_pct + 
             usg_pct + vorp, train_df)

# Use 2022-2023 data, create 2023-2024 ws_est (win share estimate) column 
# to estimate win shares
twenty_three_nba_df$ws_est = predict(ws_lm, newdata = twenty_three_nba_df)

# This code took in the 2022-2023 data and predicted our 2023-2024 win share estimate
# for each player that played more than 1,000 minutes during the 2022-2023 season.

# Let's look at the top ten players in estimated win share value
top_ten_players_ws_est = twenty_three_nba_df[order(-twenty_three_nba_df$ws_est), ][1:10, c("player", "ws_est")]
print(top_ten_players_ws_est)
# player                       ws_est
# Nikola Jokić                15.040304
# Jimmy Butler                12.486686
# Joel Embiid                 11.963378
# Domantas Sabonis            11.517184
# Shai Gilgeous-Alexander     11.214848
# Luka Dončić                 10.918253
# Jayson Tatum                9.824837
# Giannis Antetokounmpo       9.366693
# Damian Lillard              9.325284
# Anthony Davis               9.189665

# Using the 2022-2023 data to predict win share for the 2023-2024 season,
# these are the top ten players.

# Let's translate our estimated win share values to a dollar value
# using Seth Partnow's formula from the Midrange Theory:
# Production value = Win Share * League Value Per Win
# Seth Partnow used the league value per win of $2.8 million/win from the 2018-
# 2019 season.
# From this Athletic article:
# https://theathletic.com/3517502/2022/08/23/nba-analytics-win-cost/#
# I found that the league value per win in 2022-2023 was $3.44 million

# Now we can use that league value per win to predict a players production value
# based on our estimated win share value
twenty_three_nba_df$ws_dollar_val = ((twenty_three_nba_df$ws_est) * 3.44)

# Let's look at the top ten players in estimated win share dollar value
top_ten_players_ws_dollar_val = twenty_three_nba_df[order(-twenty_three_nba_df$ws_dollar_val), ][1:10, c("player", "ws_dollar_val")]
print(top_ten_players_ws_dollar_val)
# player                        ws_dollar_val
# Nikola Jokić                  51.73865
# Jimmy Butler                  42.95420
# Joel Embiid                   41.15402
# Domantas Sabonis              39.61911
# Shai Gilgeous-Alexander       38.57908
# Luka Dončić                   37.55879
# Jayson Tatum                  33.79744
# Giannis Antetokounmpo         32.22142
# Damian Lillard                32.07898
# Anthony Davis                 31.61245

# Look at players present in both top ten in predicted win shares and top ten in
# predicted win share dollar value
common_names = intersect(top_ten_players_est_ws$player, top_ten_players_ws_dollar_val$player)
print(common_names)
# 10 Players:
# [1] "Nikola Jokić"            "Jimmy Butler"            "Joel Embiid"            
# [4] "Domantas Sabonis"        "Shai Gilgeous-Alexander" "Luka Dončić"            
# [7] "Jayson Tatum"            "Giannis Antetokounmpo"   "Damian Lillard"         
# [10] "Anthony Davis" 