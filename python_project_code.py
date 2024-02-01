#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Aug 18 14:52:55 2023

@author: doojerthekid
"""

# Import libraries we will be using
import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
from matplotlib.ticker import StrMethodFormatter
import pycountry

cost_of_living_df = pd.read_csv('cost_of_living.csv')
ds_salaries_df = pd.read_csv('ds_salaries.csv')
levels_salary_df = pd.read_csv('Levels_Fyi_Salary_Data.csv')

# Used pip install pip install openpyxl to read in excel files into Python
country_codes_df = pd.read_excel('country_codes.xlsx')


# What do we want to do with this code??
# Problem:
    # You are a data scientist and would like to know where the top 5 places 
    # in the world (country or city) where your salary will go the farthest 
    # with respect to each individual index within the cost_of_living.csv file.
    # Provide a simple statistical analysis in a Jupyter Notebook file and provide 
    # visualizations to support your analysis (I am looking for data wrangling more
    # than anything). 

# What does each index mean?
# Cost of living index = The Cost of Living Index (COLI) is a measure used to 
#                        compare the relative cost of living between different 
#                        geographic locations or cities. Cities with COLI of 100
#                        will be reference point for other cities close to them


# Run a code to find where our reference point cities will be. Need to figure out
coli_city_ref = cost_of_living_df[cost_of_living_df['Cost of Living Index'] == 100]
print(coli_city_ref['City'])
# Let's say I am a intermediate level Data Scientist making $100,000 living in New York 
# City (our reference point city). For example: If cost of living index is 120 somewhere, 
# then cost of living is 20% higher than in  New York. If cost of living index is 80, 
# then cost of living is 20% lower than in New York. Where is my money going the furthest?

# Let's look at cost_of_living_df
cost_of_living_df = cost_of_living_df.sort_values(by='City')
# Drop Rank column
cost_of_living_df.drop('Rank', axis=1, inplace=True)
cost_of_living_df.head()
off_cost_of_living_df.head()

# Find means of each column for each data frame
us_avg = us_cost_of_living_df.mean(numeric_only = True)
print(us_avg)
off_avg = off_cost_of_living_df.mean(numeric_only = True)
print(off_avg)

# Find standard deviation of each column for each data frame
us_stand_dev = us_cost_of_living_df.std(numeric_only = True)
print(us_stand_dev)
off_stand_dev = off_cost_of_living_df.std(numeric_only = True)
print(off_stand_dev)

# Plot mean and standard deviation of both US and Offshore data sets
plt.bar(us_avg.index, us_avg.values, yerr = us_stand_dev, capsize = 5, label = 'US Data')
plt.bar(off_avg.index, off_avg.values, yerr = off_stand_dev, capsize = 5, label = 'Offshore Data')
plt.xlabel('Indexes')
plt.xticks(rotation = 90)
plt.ylabel('Values')
plt.title('US vs. Offshore Mean and Standard Deviation')
plt.legend()
plt.show()

# The averages of each US index is higher than the averages of each Offshore index. The 
# long bar in Local Purchasing power Index represents high variability compared to the shorter
# bars in the other indexes. The error bar only overlaps in Local Purchasing Power index, 
 # which indicates a very large difference between the means of the US and Offshore data.

### Let's merge the cost_of_living_df and the country_codes_df
def split_city(city):
    parts = city.split(',')
    if len(parts) == 3:
        return pd.Series(parts, index=['city', 'State', 'Country'])
    elif len(parts) == 2:
        return pd.Series(parts, index=['city', 'Country'])

# Apply the function to each row of the 'City' column
split_columns = cost_of_living_df['City'].apply(split_city)

# Concatenate the split columns with the original DataFrame
cost_of_living_df = pd.concat([cost_of_living_df, split_columns], axis=1)

# Drop the original 'City' column
cost_of_living_df.drop('City', axis=1, inplace=True)
cost_of_living_df.head()

# Change Country name to Country codes to make merge easier
def map_to_country_code(country_name):
    try:
        country = pycountry.countries.search_fuzzy(country_name)[0]
        return country.alpha_2
    except LookupError:
        return country_name  # Return original name if country code not found
# Make full country names to country codes using pycountry method we created
cost_of_living_df['Country'] = cost_of_living_df['Country'].apply(map_to_country_code)
cost_of_living_df.head()

# Rename 'Country' to 'country_code' to merge after
new_col_name_1 = 'Alpha-2 code'
cost_of_living_df.rename(columns = {'Country': new_col_name_1}, inplace = True)
cost_of_living_df.head()
country_codes_df.head()
coli_codes_df = pd.merge(cost_of_living_df, country_codes_df, on = 'Alpha-2 code')
coli_codes_df.head()

# Merge coli_codes_pdf with ds_salaries_df
# First, need to filter out ds_salaries_df to only show data scientists, since that is what we are looking for in our analysis
ds_salaries_df = ds_salaries_df[ds_salaries_df['job_title'] == 'Data Scientist']
# 143 Observations
ds_salaries_df = ds_salaries_df.drop('Unnamed: 0', axis = 1)
ds_salaries_df = ds_salaries_df.reset_index(drop = True)
ds_salaries_df.head()

# Change 'employee_residence' to 'country_code' so we can merge
new_col_name = 'Alpha-2 code'
ds_salaries_df.rename(columns = {'employee_residence': new_col_name}, inplace = True)
ds_salaries_df.head()

coli_codes_sal_df = pd.merge(coli_codes_df, ds_salaries_df, on = 'Alpha-2 code')
coli_codes_sal_df.head()

# Add new column, 'weighted_sal_usd' which is the weighted salary in USD dollars based on the cost of living index
coli_codes_sal_df['weighted_sal_usd'] = ((coli_codes_sal_df['Cost of Living Index']*coli_codes_sal_df['salary_in_usd'])/100).round()
coli_codes_sal_df.head()

# Plot the weighted income by each country
us_count = coli_codes_sal_df['Alpha-2 code'].value_counts()['US']
print('US count:', us_count)
off_count = len(coli_codes_sal_df['Alpha-2 code']) - us_count
print('Offshore count:', off_count)
# More US data than any other country

# Create barplot of Mean Weighted Salaries by country
country_mean_salary = coli_codes_sal_df.groupby('Alpha-2 code')['weighted_sal_usd'].mean()

plt.figure(figsize=(12, 6))
sns.barplot(x = 'Alpha-2 code', y = 'weighted_sal_usd', data = coli_codes_sal_df)
plt.xlabel('Country')
plt.ylabel('Mean Weighted Salary')
plt.title('Mean Weighted Salary by Country')
y_formatter = StrMethodFormatter("${x:.2f}")
plt.gca().yaxis.set_major_formatter(y_formatter)
plt.xticks(rotation = 90)  # Rotate x-axis labels for better readability
plt.tight_layout()

# Display the plot
plt.show()

### China has the highest mean weighted salary in USD, the US is second, and Singapore 
### is third. These countries have relatively high salaries.
### Based on this bar plot, I will look at China, US and Singapore data to see what the
### top 5 places to live where my income will go the farthest.

# Filter the data so it only has China, US and Singapore Intermediate level data science jobs for Medium sized companies
condition1 = coli_codes_sal_df['Alpha-2 code'].isin(['CH', 'US', 'SG'])
condition2 = coli_codes_sal_df['experience_level'] == 'MI'
condition3 = coli_codes_sal_df['company_size'] == 'M'
subset_df = coli_codes_sal_df[condition1 & condition2 & condition3]
subset_df.head()

print('There are',len(subset_df), 'observations in subset dataframe.')

# Create barplot of Mean Weighted Salaries by country for Intermediate DS at Medium sized companies
country_mean_salary_subset = subset_df.groupby('Alpha-2 code')['weighted_sal_usd'].mean()

plt.figure(figsize=(12, 6))
sns.barplot(x='Alpha-2 code', y='weighted_sal_usd', data = subset_df)
plt.xlabel('Country')
plt.ylabel('Mean Weighted Salary')
plt.title('Mean Weighted Salary by Country for Intermediate DS at Medium sized Co.')
y_formatter = StrMethodFormatter("${x:.2f}")
plt.gca().yaxis.set_major_formatter(y_formatter)
plt.tight_layout()
# Display the plot
plt.show()

# Since New York COLI is 100, I do not want to be at or over 100. However, we do not want 
# to be too low on the COLI, becuase quality of life could be affected. Let's subset data 
# again to where COLI is lower than 100 but purchasing power is still over 100. And is not 
# in New York
condition_1 = (subset_df['Cost of Living Index'] >= 70) & (subset_df['Cost of Living Index'] <= 80)
condition_2 = subset_df['Local Purchasing Power Index'] > 100
condition_3 = subset_df['State'].str.strip() != 'NY'

subset_df = subset_df[condition_1 & condition_2 & condition_3]
subset_df.head()

print('There are',len(subset_df), 'observations in subset dataframe.')

# Filter out duplicate cities to see what we have left then we can rank those based COLI
filtered_df = subset_df.drop_duplicates(subset='city', keep='first')

filtered_df.head()

# Reset the index first
filtered_df = filtered_df.reset_index(drop = True)
filtered_df.head()

# Find top 5 cities with higest COLI. We already set parameters on COLI and purchasing power index in this data frame and filitred properly based on our data scientist parameters
top_cities = filtered_df.nlargest(5, 'Cost of Living Index')[['city','State','Cost of Living Index', 'Local Purchasing Power Index']]
top_cities.head()