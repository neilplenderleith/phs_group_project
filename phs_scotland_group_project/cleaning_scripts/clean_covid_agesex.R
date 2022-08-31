library(tidyverse)
library(janitor)
library(lubridate)

#read in necessary datasets
hb_agesex <- read.csv(
  "raw_data/covid_raw_data/hospital_admissions_hb_agesex_20220302.csv") %>% 
  clean_names()
hb_names <- read_csv(
  "raw_data/covid_raw_data/health_board_names.csv") %>% 
  clean_names()

#select only the columns with health board names
hb_names <- hb_names %>% 
  select(hb, hb_name)

#join to get health board names
hb_agesex <- hb_agesex %>% 
  left_join(hb_names, by = "hb")

#Fill in NAs in hb_name - these are NA because the code is the country code for NHS Scotland
hb_agesex <- hb_agesex %>% 
  mutate(hb_name = if_else(
    is.na(hb_name),
    "All Scotland",
    hb_name
  )) 

#change week_ending into date format
hb_agesex <- hb_agesex %>% 
  mutate(week_ending = ymd(week_ending))

#create separate variables for month and year
hb_agesex <- hb_agesex %>% 
  mutate(month = month(week_ending, label = TRUE),
         year = year(week_ending), .after = week_ending)

#create winter and non winter groups
hb_agesex <- hb_agesex %>% 
  mutate(is_winter = if_else(
    month %in% c("Dec", "Jan", "Feb"), TRUE, FALSE
  ), .after = month)

#write clean covid_agesex.csv
write_csv(hb_agesex, file = "clean_data/covid_agesex.csv")
