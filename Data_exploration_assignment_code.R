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

## Reading in Google trends data ##

file_names <- list.files('Data/Lab3_Rawdata', pattern='trends_up_to_', full.names = TRUE) 
google_trends <- map_df(file_names, read_csv)


google_trends$Month <- str_sub(google_trends$monthorweek,1,10)
google_trends$Month <- ymd(google_trends$Month)

google_trends <- google_trends %>% 
  group_by(schname, keyword) %>% 
  mutate(Index_standard = (index - mean(index)) / sd(index))


  
## Reading in Score Card Data ##

score_card <- read_csv('Data/Lab3_Rawdata/Most+Recent+Cohorts+(Scorecard+Elements).csv')
id_name_link <- read_csv('Data/Lab3_Rawdata/id_name_link.csv')

id_name_link <- id_name_link %>% 
  group_by(schname) %>% 
  mutate(n = n()) %>% 
  filter(n == 1)
  
## Joining Data together ##

processed_data <- google_trends %>% 
    inner_join(id_name_link, by = "schname") 
    
processed_data <- processed_data %>% 
  inner_join(score_card, by = c("unitid" = "UNITID", "opeid" = "OPEID"))

## Filtering and cleaning the data ##

# filtering for colleges that predominantly grant bachelor’s degrees
processed_data <- processed_data %>% 
  filter(PREDDEG == 3 ) %>% 
  select(unit_id = 'unitid', 'opeid', school_name = 'schname', key_word = 'keyword', 
         key_num = 'keynum', median_earings = 'md_earn_wne_p10-REPORTED-EARNINGS', 
          'Month', "Index_standard", bach_deg = 'PREDDEG')


## Median earnings ##
processed_data$median_earings <- as.numeric(processed_data$median_earings)
processed_data <- na.omit(processed_data)


sum(is.na(processed_data$median_earings))
colSums(is.na(processed_data))

class(processed_data$median_earings)

head(processed_data)
hist(processed_data$median_earings, main = 'Histogram of Median Earnings',
     xlab= "Median Earings", ylab = "Count")
median(processed_data$median_earings)
mean(processed_data$median_earings)

  
