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
all_csvs <- map_df(file_names, read_csv)
head(all_csvs)


all_csvs$Month <- str_sub(all_csvs$monthorweek,1,10)

floor_date(all_csvs$Month, unit = "month")


## Reading in Score Card Data ##

score_Card <- read_csv('Data/Lab3_Rawdata/Most+Recent+Cohorts+(Scorecard+Elements).csv')
id_name_link <- read_csv('Data/Lab3_Rawdata/id_name_link.csv')




