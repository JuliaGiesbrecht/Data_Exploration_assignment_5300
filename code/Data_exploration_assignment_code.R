# Data exploration assignment - Julia Giesbrecht

# Load libraries
library(tidyverse)
library(vtable)
library(dplyr)
library(fs)
library(purrr)
library(lubridate)

### Data Preparation ##

# Reading in Google trends data
file_names <- list.files('../Data/Lab3_Rawdata', pattern='trends_up_to_', full.names = TRUE) 
google_trends <- map_df(file_names, read_csv)

# Creating new variable 'Month'
google_trends$date <- str_sub(google_trends$monthorweek,1,10)
google_trends$date <- ymd(google_trends$date)
google_trends$date <- floor_date(google_trends$date, unit = "month")

# Standardizing Index by school name and key word
google_trends <- google_trends %>% 
  group_by(schname, keyword) %>% 
  mutate(Index_standard = (index - mean(index)) / sd(index))

# remove na values for index 
google_trends <- google_trends %>% 
  filter(!is.na(Index_standard)) 

# Group by school name
google_trends <- google_trends %>% 
  group_by(schname, date) %>% 
  summarise(mean(Index_standard))

# create variable for before and after the score card is introduced
google_trends$before_score_card <- ifelse(google_trends$date < '2015-09-01', 0, 1) 
google_trends$before_score_card <- as.factor(processed_data$before_score_card)

# Reading in Score Card Data 
score_card <- read_csv('../Data/Lab3_Rawdata/Most+Recent+Cohorts+(Scorecard+Elements).csv')
id_name_link <- read_csv('../Data/Lab3_Rawdata/id_name_link.csv')

# Removing school names that show up more than once
id_name_link <- id_name_link %>% 
  group_by(schname) %>% 
  mutate(n = n()) %>% 
  filter(n == 1)
  
#Joining Data together 
processed_data <- google_trends %>% 
    inner_join(id_name_link, by = "schname") 
    
processed_data <- processed_data %>% 
  inner_join(score_card, by = c("unitid" = "UNITID"))


# filtering for colleges that predominantly grant bachelor’s degrees (PREDDEG == 3 )
processed_data <- processed_data %>% 
  filter(PREDDEG == 3 )

# Median earnings - Median earnings of students working and not enrolled 10 years after entry
# Converting median earnings to numeric
processed_data$median_earings <- as.numeric(processed_data$`md_earn_wne_p10-REPORTED-EARNINGS`)
# removing n/a values
processed_data <- na.omit(processed_data)

# N/A and class checks
sum(is.na(processed_data$median_earings))
colSums(is.na(processed_data))
class(processed_data$median_earings)

# Creating new column grouping median earnings by quartile
quantile(processed_data$median_earings)
processed_data$quartile_earnings<-cut(processed_data$median_earings,quantile(processed_data$median_earings),include.lowest=TRUE,labels=FALSE)
processed_data$quartile_earnings <- as.factor(processed_data$quartile_earnings)

# Creating low-earning/high-earning variables from quartile earnings. 
# 4 being high-earnings. 1,2,3 low-earnings
processed_data$binary_high_earnings <- ifelse(processed_data$quartile_earnings != '4', 0, 1)
processed_data$binary_high_earnings <- as.factor(processed_data$binary_high_earnings)


# selecting variables and renaming
processed_data <- processed_data %>% 
  select(unitid, opeid, 'school name' = schname, date, before_score_card, index_standard = 'mean(Index_standard)', median_earings, binary_high_earnings)


# Exporting processed data
write.csv(processed_data,"../Processed_data/processed_data.csv", row.names = FALSE)




