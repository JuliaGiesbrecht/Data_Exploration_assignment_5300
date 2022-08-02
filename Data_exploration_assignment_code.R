# Data exploration assignment - Julia Giesbrecht

# Load libraries
library(tidyverse)
library(vtable)
library(fixest)
library(wooldridge)
library(dplyr)
library(fs)
library(purrr)
library(lubridate)
library(dplyr)
library(ggplot2)




### Reading in Google trends data ###

file_names <- list.files('Data/Lab3_Rawdata', pattern='trends_up_to_', full.names = TRUE) 
google_trends <- map_df(file_names, read_csv)

# Creating new variable 'Month and 'year'
google_trends$date <- str_sub(google_trends$monthorweek,1,10)
google_trends$date <- ymd(google_trends$date)
google_trends$date <- floor_date(google_trends$date, unit = "month")

# Standardizing Index by school name and key word
google_trends <- google_trends %>% 
  group_by(schname, keyword) %>% 
  mutate(Index_standard = (index - mean(index)) / sd(index))

  
### Reading in Score Card Data ###

score_card <- read_csv('Data/Lab3_Rawdata/Most+Recent+Cohorts+(Scorecard+Elements).csv')
id_name_link <- read_csv('Data/Lab3_Rawdata/id_name_link.csv')

# Removing school names that show up more than once
id_name_link <- id_name_link %>% 
  group_by(schname) %>% 
  mutate(n = n()) %>% 
  filter(n == 1)
  
### Joining Data together ###

processed_data <- google_trends %>% 
    inner_join(id_name_link, by = "schname") 
    
processed_data <- processed_data %>% 
  inner_join(score_card, by = c("unitid" = "UNITID", "opeid" = "OPEID"))


### Filtering and cleaning the data ###

# filtering for colleges that predominantly grant bachelor’s degrees (PREDDEG == 3 )
# and selecting/ renaming variables
processed_data <- processed_data %>% 
  filter(PREDDEG == 3 ) %>% 
  select(unit_id = 'unitid', 'opeid', school_name = 'schname', key_word = 'keyword', 
         key_num = 'keynum', median_earings = 'md_earn_wne_p10-REPORTED-EARNINGS', 
           "Index_standard", "date")

## Median earnings - Median earnings of students working and not enrolled 10 years after entry ##
processed_data$median_earings <- as.numeric(processed_data$median_earings)
processed_data <- na.omit(processed_data)
# N/A and class checks
sum(is.na(processed_data$median_earings))
colSums(is.na(processed_data))
class(processed_data$median_earings)

# Creating new column grouping median earnings by quartile
# first quartile at 25%, second quartile at 50% and third quartile at 75% hence 
# there will be four quarters to represent first 25%, second 25%, third 25% and 
# the last 25% in a set of data.

quantile(processed_data$median_earings)
processed_data$quartile_earnings<-cut(processed_data$median_earings,quantile(processed_data$median_earings),include.lowest=TRUE,labels=FALSE)
processed_data$quartile_earnings <- as.factor(processed_data$quartile_earnings)
class(processed_data$quartile_earnings) 

# Creating low-earning/high-earning variables from quartile earnings. 
# 4 being high-earnings. 1,2,3 low-earnings
processed_data$binary_high_earnings <- ifelse(processed_data$quartile_earnings != '4', 0, 1)
processed_data$binary_high_earnings <- as.factor(processed_data$binary_high_earnings)
class(processed_data$binary_high_earnings)

# group by date and quartile and to get mean of the standardized index score.
processed_data <- processed_data %>% 
  group_by(date, binary_high_earnings)%>% 
  summarise(mean_index_by_date = mean(Index_standard)) 

# Adding in month variable
processed_data$Month <- month(processed_data$date)
processed_data$Month <- as.factor(processed_data$Month)

# before and after the score card is introduced
processed_data$before_score_card <- ifelse(processed_data$date < '2015-09-01', 0, 1) 
processed_data$before_score_card <- as.factor(processed_data$before_score_card)

# Exporting processed data
write.csv(processed_data,"processed_data.csv", row.names = FALSE)







