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
library(viridis)
library(dygraphs)
library(xts)
library(hrbrthemes)



### Reading in Google trends data ###

file_names <- list.files('Data/Lab3_Rawdata', pattern='trends_up_to_', full.names = TRUE) 
google_trends <- map_df(file_names, read_csv)

# Creating new variable 'Month and 'year'
google_trends$date <- str_sub(google_trends$monthorweek,1,10)
google_trends$date <- ymd(google_trends$date)
google_trends$date <- floor_date(google_trends$date, unit = "month")
google_trends$year <- year(google_trends$date)
google_trends$Month <- month(google_trends$date)



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
          'Month', 'year', "Index_standard", "date")

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




# Creating low-earning/high-earning variables
processed_data$low_earnings <- ifelse(processed_data$quartile_earnings == '1', 1, 0) 
processed_data$low_med_earnings  <- ifelse(processed_data$quartile_earnings == '2', 1, 0)
processed_data$med_high_earnings  <- ifelse(processed_data$quartile_earnings == '3', 1, 0) 
processed_data$high_earnings <- ifelse(processed_data$quartile_earnings == '4', 1, 0)
processed_data$binary_high_earnings <- ifelse(processed_data$quartile_earnings != '4', 0, 1)



# group by date and quartile and the mean of in the index standard
processed_data <- processed_data %>% 
  group_by(date, binary_high_earnings)%>% 
  summarise(mean_index_by_date = mean(Index_standard)) 

processed_data$binary_high_earnings <- as.factor(processed_data$binary_high_earnings)

# month variable
processed_data$Month <- month(processed_data$date)
processed_data$Month <- as.factor(processed_data$Month)

#before and after the score card is introduced
processed_data$before_score_card <- ifelse(processed_data$date <= '2015-09-01', 1, 0) 
processed_data$before_score_card <- as.factor(processed_data$before_score_card)

class(processed_data$binary_high_earnings)

write.csv(processed_data,"processed_data.csv", row.names = FALSE)






# graphing the popularity (mean index by month) of quartile earnings over time
processed_data %>% 
ggplot( aes(x = date, y = mean_index_by_date, color = binary_high_earnings)) +
  geom_line() + geom_point() +   theme_test()




x <- processed_data %>% 
  select(date, mean_index_by_date, binary_high_earnings) %>% 
  filter(quartile_earnings== 4 | quartile_earnings ==1)

processed_data %>% 
  ggplot( aes(x = date, y = mean_index_by_date, color = binary_high_earnings)) +
  geom_line() + geom_point() +   theme_test()


ggplot(x, aes(x = quartile_earnings, y = mean_index_by_date)) + 
  geom_point()

feols(mean_index_by_date~ binary_high_earnings*before_score_card, data = processed_data)

library(tidyverse) # This loads ggplot2 as well
ggplot(processed_data, aes(x = binary_high_earnings, y = mean_index_by_date)) + 
  geom_point() + # Draw points
  geom_smooth(method = 'lm') # add OLS line

#hist(google_trends$Index_standard, main = 'Histogram of Median Earnings',
     #xlab= "x", ylab = "Count")  


# dep - popularity(index)- high or low earning schools popularity
# in - score card

# 
