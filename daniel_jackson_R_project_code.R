library(ggplot2)
library(dplyr)

data_csv <- read.csv("R Project/data.csv"
                      ,stringsAsFactors=FALSE
                     )
# Read in csv data. 607 observations in file.

# Look at how many counts of each job title there are
table(data_csv$job_title)
# 3D Computer Vision Researcher          AI Scientist 
# 1                                        7 
# Analytics Engineer                      Applied Data Scientist 
# 4                                        5 
# Applied Machine Learning Scientist      BI Data Analyst 
# 4                                        6 
# Big Data Architect                      Big Data Engineer 
# 1                                        8 
# Business Data Analyst                   Cloud Data Engineer 
# 5                                        2 
# Computer Vision Engineer                Computer Vision Software Engineer 
# 6                                        3 
# Data Analyst                            Data Analytics Engineer 
# 97                                        4 
# Data Analytics Lead                     Data Analytics Manager 
# 1                                        7 
# Data Architect                          Data Engineer 
# 11                                      132 
# Data Engineering Manager                Data Science Consultant 
# 5                                        7 
# Data Science Engineer                   Data Science Manager 
# 3                                       12 
# Data Scientist                          Data Specialist 
# 143                                     1 
# Director of Data Engineering            Director of Data Science 
# 2                                        7 
# ETL Developer                           Finance Data Analyst 
# 2                                        1 
# Financial Data Analyst                  Head of Data 
# 2                                        5 
# Head of Data Science                   Head of Machine Learning 
# 4                                        1 
# Lead Data Analyst                       Lead Data Engineer 
# 3                                        6 
# Lead Data Scientist                     Lead Machine Learning Engineer 
# 3                                        1 
# Machine Learning Developer              Machine Learning Engineer 
# 3                                       41 
# Machine Learning Infrastructure Eng     Machine Learning Manager 
# 3                                        1 
# Machine Learning Scientist              Marketing Data Analyst 
# 8                                        1 
# ML Engineer                             NLP Engineer 
# 6                                        1 
# Principal Data Analyst                  Principal Data Engineer 
# 2                                        3 
# Principal Data Scientist                Product Data Analyst 
# 7                                        2 
# Research Scientist                      Staff Data Scientist 
# 16                                       1 

# Filter only full time workers. We are only looking for full time employees. This
## brought total observations to 588.
data_csv = data_csv[data_csv$employment_type == "FT",]

# CEO is looking for top talent so let's filter out the entry level so we can 
## look at both junior mid-level and up. 509 observations.
data_csv = data_csv[data_csv$experience_level != "EN",]

# Let's remove small companies as well since we are looking to grow. 457
## observations.
data_csv = data_csv[data_csv$company_size != "S",]

# Let's now create two data frames where we will be working within. One within
## the US and one offshore. We then can start running some analysis on each 
### data frame (avg salaries of senior, avg salaries intermediate, what size
#### company, etc).
us_df = data_csv[data_csv$company_location == "US",]
# There are 301 observations in this data frame.
  
off_df = data_csv[data_csv$company_location != "US",]
# There are 156 observations in this data frame.

# DATA SCIENTISTS

# US
# Filter only US data scientists by subsetting the data_csv data by job title. 73
## observations.
us_data_scientists_df = us_df[us_df$job_title == "Data Scientist",]

# Let's find average salary in US dollar
us_avg_salary = mean(us_data_scientists_df$salary_in_usd)
print(us_avg_salary)
# This returns $153,517.51


# Let's find average salaries by year
us_avg_salary_by_year = aggregate(us_data_scientists_df$salary_in_usd ~
                                    us_data_scientists_df$work_year, FUN = mean)
# 2020: $160,295.30
# 2021: $131,555.60
# 2022: $156,224.10
# Rename columns to clean up data then create column bar plot
us_avg_salary_by_year = us_avg_salary_by_year %>%
  rename(year = 'us_data_scientists_df$work_year') %>%
  rename(avg_salary = 'us_data_scientists_df$salary_in_usd')
ggplot(us_avg_salary_by_year) +
  geom_col(aes(x = year, 
               y = avg_salary,
               fill = year)) +
  labs(x = 'Year', 
       y = 'Average Salary in USD', 
       title = 'US Average Salary by Year',
       fill = 'Year') +
  scale_fill_continuous(breaks = c(2020, 2021, 2022)) +
  scale_y_continuous(labels = scales::dollar_format())
  


# Let's look at average, min, and max salary in US dollar by company size.
us_avg_salary_by_size = aggregate(us_data_scientists_df$salary_in_usd ~
                                    us_data_scientists_df$company_size, FUN = mean)
print(us_avg_salary_by_size)
# Large is $154,729.90
# Medium is $153,027.36
# Rename columns to clean up data
us_avg_salary_by_size = us_avg_salary_by_size %>%
  rename(size = 'us_data_scientists_df$company_size') %>%
  rename(salary = 'us_data_scientists_df$salary_in_usd')


us_min_salary = min(us_data_scientists_df$salary_in_usd)
print(us_min_salary)
# This returns $68,428

us_max_salary = max(us_data_scientists_df$salary_in_usd)
print(us_max_salary)
# This returns $412,000


# Let's look at average salary by experience level
us_avg_salary_by_exp = aggregate(us_data_scientists_df$salary_in_usd ~
                                   us_data_scientists_df$experience_level, FUN = mean)
print(us_avg_salary_by_exp)
# MI: $126,587.50
# SE: $163,679.8
# Rename column names to clean up data
us_avg_salary_by_exp = us_avg_salary_by_exp %>%
  rename(experience = "us_data_scientists_df$experience_level") %>%
  rename(salary = "us_data_scientists_df$salary_in_usd")

mean(us_avg_salary_by_exp[,2])
# Average of those two experiences is $145,133!

# Let's look at average salary by remote, hybrid, full time in office
us_avg_salary_by_remote = aggregate(us_data_scientists_df$salary_in_usd ~
                                      us_data_scientists_df$remote_ratio, FUN = mean)
# Remote?       Salary
# No            $140,225
# Hybrid        $124,200
# Yes           $161,234
# Rename columns to clean up data
us_avg_salary_by_remote = us_avg_salary_by_remote %>%
  rename(remote_ratio = "us_data_scientists_df$remote_ratio") %>%
  rename(salary = "us_data_scientists_df$salary_in_usd")

# Offshore
# Filter only offshore data scientists
off_data_scientists_df = off_df[off_df$job_title == "Data Scientist",]
# There are 33 observations in data frame

# Now, let's look at offshore data and run the same code on that
off_avg_salary = mean(off_data_scientists_df$salary_in_usd)
print(off_avg_salary)
# This returns $70,798.33

# Average salary by year
off_avg_salary_by_year = aggregate(off_data_scientists_df$salary_in_usd ~
                                    off_data_scientists_df$work_year, FUN = mean)
# 2020: $48,706.25
# 2021: $68,400.59
# 2022: $81,559.17
# Rename columns to clean up data and create column bar plot
off_avg_salary_by_year = off_avg_salary_by_year %>%
  rename(off_year = 'off_data_scientists_df$work_year') %>%
  rename(off_avg_salary = 'off_data_scientists_df$salary_in_usd')

ggplot(off_avg_salary_by_year) +
  geom_col(aes(x = off_year, 
               y = off_avg_salary,
               fill = off_year)) +
  labs(x = 'Year', 
       y = 'Average Salary in USD', 
       title = 'Offshore Average Salary by Year',
       fill = 'Year') +
  scale_fill_continuous(breaks = c(2020, 2021, 2022)) +
  scale_y_continuous(labels = scales::dollar_format())

# Percentage difference US vs Offshore avg salary by year
((us_avg_salary_by_year / off_avg_salary_by_year) * 100)
# 2020: 329%
# 2021: 192%
# 2022: 191%

# Let's look at average salary in US dollar by company size for offshore data
off_avg_salary_by_size = aggregate(off_data_scientists_df$salary_in_usd ~
                                     off_data_scientists_df$company_size, FUN = mean)
print(off_avg_salary_by_size)
# Large is $64,976.83
# Medium is $77,784.13
# Rename column names to clean up data
off_avg_salary_by_size = off_avg_salary_by_size %>%
  rename(size = 'off_data_scientists_df$company_size') %>%
  rename(salary = 'off_data_scientists_df$salary_in_usd')

ggplot(off_avg_salary_by_size) +
  geom_col(aes(x = off_size, 
               y = off_avg_salary_size,
               fill = off_size)) +
  labs(x = 'Company Size', 
       y = 'Average Salary in USD', 
       title = 'Offshore Average Salary by Size',
       fill = "Number of Employees") +
  scale_x_discrete(labels = c("Large", "Medium")) +
  scale_fill_discrete(labels = c("250+", "50-250"))

off_min_salary = min(off_data_scientists_df$salary_in_usd)
print(off_min_salary)
# This returned $20,171

off_max_salary = max(off_data_scientists_df$salary_in_usd)
print(off_max_salary)
# This returned $183,228

# Average salary by experience level
off_avg_salary_by_exp = aggregate(off_data_scientists_df$salary_in_usd ~ 
                                  off_data_scientists_df$experience_level, FUN = mean)
print(off_avg_salary_by_exp)
# MI: $68856.15
# SE: $79538.17
# Rename column names to clean up data
off_avg_salary_by_exp = off_avg_salary_by_exp %>%
  rename(experience = "off_data_scientists_df$experience_level") %>%
  rename(salary = "off_data_scientists_df$salary_in_usd")

# Let's look at average salary by remote, hybrid, full time in office
off_avg_salary_by_remote = aggregate(off_data_scientists_df$salary_in_usd ~
                                      off_data_scientists_df$remote_ratio, FUN = mean)
# Remote?       Salary
# No            $83,024
# Hybrid        $69,154
# Yes           $58,955
# Rename columns to clean up date
off_avg_salary_by_remote = off_avg_salary_by_remote %>%
  rename(remote_ratio = "off_data_scientists_df$remote_ratio") %>%
  rename(salary = "off_data_scientists_df$salary_in_usd")

# COMPARISON GRAPHS USING GGPLOT() AND GEOM_LINE()
# Create line graph to compare differences in income: US vs. Offshore
ggplot() +
  geom_line(data = us_avg_salary_by_year, aes(x = year, 
                                       y = avg_salary, 
                                       color = "US")) +
  geom_line(data = off_avg_salary_by_year, aes(x = off_year, 
                                        y = off_avg_salary,
                                        color = "Offshore")) +
  labs(x = "Year", 
       y = "Average Salary in USD", 
       color = "Data",
       title = "US vs. Offshore Average Salaries per Year") +
  scale_x_continuous(breaks = c(2020, 2021, 2022)) +
  scale_y_continuous(labels = scales::dollar_format())

# Create column graph to compare differences in average income by company size
## US vs. Offshore. Add new column in each data frame then create combined data 
### frame then plot
us_avg_salary_by_size$source = "US"
off_avg_salary_by_size$source = "Offshore"
us_combined_off_size = rbind(us_avg_salary_by_size, off_avg_salary_by_size)

ggplot(us_combined_off_size, aes(x = size,
                                 y = salary,
                                 fill = source)) +
  geom_col(position = position_dodge(width = 0.9)) + 
  labs(x = "Company Size (by Employees)", 
       y = "Average Salary in USD", 
       fill = "Data",
       title = "US vs. Offshore Average Salaries by Company size") +
  scale_x_discrete(labels = c("Large (250 +)", "Medium (50-250)")) +
  scale_y_continuous(labels = scales::dollar_format())


# Create column graph to compare average salaries by experience level US vs. 
## Offshore. Add new column in each data frame then combine then plot
us_avg_salary_by_exp$source = "US"
off_avg_salary_by_exp$source = "Offshore"
us_combined_off_exp = rbind(us_avg_salary_by_exp, off_avg_salary_by_exp)

ggplot(us_combined_off_exp, aes(x = experience,
                                 y = salary,
                                 fill = source)) +
  geom_col(position = position_dodge(width = 0.9)) + 
  labs(x = "Employee Experience Level", 
       y = "Average Salary in USD", 
       fill = "Data",
       title = "US vs. Offshore Average Salaries by Employee Experience Level") +
  scale_x_discrete(labels = c("Mid-Level", "Senior")) +
  scale_y_continuous(labels = scales::dollar_format())


# Create column graph to compare average salaries by remote ratio US vs. 
## Offshore. Add new column in each data frame then combine then plot
us_avg_salary_by_remote$source = "US"
off_avg_salary_by_remote$source = "Offshore"
us_combined_off_remote = rbind(us_avg_salary_by_remote, off_avg_salary_by_remote)
us_combined_off_remote$remote_ratio = as.character(us_combined_off_remote$remote_ratio)


ggplot(us_combined_off_remote, aes(x = remote_ratio,
                                y = salary,
                                fill = source)) +
  geom_col(position = position_dodge(width = 0.9)) + 
  labs(x = "Work Environment", 
       y = "Average Salary in USD", 
       fill = "Data",
       title = "US vs. Offshore Average Salaries by Employee Work Environment") +
  scale_x_discrete(labels = c("In Office","Remote", "Hybrid")) +
  scale_y_continuous(labels = scales::dollar_format())


# RECOMMENDATION: HIRE OFFSHORE MID-LEVEL DATA SCIENTIST FROM MEDIUM-SIZED COMPANY
## How to pay them: Take average salary of offshore Medium-sized employees avg salary and
### offshore senior-level avg salary. I will not focus on remote salary because they will be
#### remote to us anyways.

# average medium-sized salary = $77,784
# average senior-level salary = $79,538
# average of both $78,601. Multiply by 120% based off of growth between 2022 and 2021 
## offshore average salaries to account for market. That equals $94,393

# Do the same for US to compare savings
# average medium-sized salary = $153,027
# average senior-level salary = $164,679
# average in-office salary = $140,225
# average of three is $152,443. Multiply by 120% based off of growth between 2022 and 2021 
## offshore average salaries to account for market. That equals $182,931

# Total approx. savings is $88,500. Use this to put towards Head of Data Science 
## position. Below, we only have one offshore Head of Data Science position to look at
### and their salary is 2021 was $85,000. With the 120% market growth you can assume
#### we could pay them approx $102,000. 

# DATA SCIENCE MANAGER - 12 to look at
table(us_df$job_title == "Data Science Manager")
# 10 Data Science Managers in US
table(off_df$job_title == "Data Science Manager")
# 2 Data Science Managers offshore

# DIRECTOR OF DATA SCIENCE - 6 to look at
table(us_df$job_title == "Director of Data Science")
# 2 Directors of Data Science in US
table(off_df$job_title == "Director of Data Science")
# 4 Directors of Data Science Offshore

# HEAD OF DATA SCIENCE - 3 to look at 
table(us_df$job_title == "Head of Data Science")
# 2 Head of Data Science in US
table(off_df$job_title == "Head of Data Science")
# 1 Head of Data Science in Offshore

# HEAD OF MACHINE LEARNING - 1 to look at
table(us_df$job_title == "Head of Machine Learning")
# 0 Head of Machine Learning in US
table(off_df$job_title == "Head of Machine Learning")
# 1 Head of Machine Learning Offshore

# LEAD DATA SCIENTIST - 2 to look at
table(us_df$job_title == "Lead Data Scientist")
# 0 Lead Data Scientists in US
table(off_df$job_title == "Lead Data Scientist")
# 2 Lead Data Scientists in Offshore

# PRINCIPAL DATA SCIENTIST - 6 to look at 
table(us_df$job_title == "Principal Data Scientist")
# 3 Principal Data Scientists in US
table(off_df$job_title == "Principal Data Scientist")
# 3 Principal Data Scientists Offshore

# STAFF DATA SCIENTIST - no data too look at
table(us_df$job_title == "Staff Data Scientist")
# 0 Staff Data Scientist in US
table(off_df$job_title == "Staff Data Scientist")
# 0 Staff Data Scientists Offshore