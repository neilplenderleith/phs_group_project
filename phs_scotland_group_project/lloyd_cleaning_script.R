library(tidyverse)
library(janitor)
library(lubridate)


waiting_times <- read_csv("raw_data/non_covid_raw_data/monthly_ae_waitingtimes_202206.csv") %>% clean_names()


waiting_times <- waiting_times %>%
  mutate(date = ym(month),
         quarter = quarter(date))

waiting_times <- waiting_times %>%
  mutate(
    prop_admission_to_same = (discharge_destination_admission_to_same/number_of_attendances_aggregate)
  ) %>%
  mutate(prop_other_speciality = (discharge_destination_other_specialty/number_of_attendances_aggregate)) %>%
  mutate(prop_residence = (discharge_destination_residence/number_of_attendances_aggregate)) %>%
  mutate(prop_transfer = (discharge_destination_transfer/number_of_attendances_aggregate)) %>%
  mutate(prop_unknown = (discharge_destination_unknown/number_of_attendances_aggregate))

waiting_times <- waiting_times %>%
  pivot_longer(cols = prop_admission_to_same:prop_unknown, names_to = "discharge_destination", values_to = "discharge_proportion")

waiting_times <- waiting_times %>%
  mutate(discharge_destination = case_when(
    str_detect(discharge_destination, "prop_admission_to_same") ~ "Admission to Same Facility",
    str_detect(discharge_destination, "prop_other_speciality") ~ "Transfer to Other Facility",
    str_detect(discharge_destination, "prop_residence") ~ "Transfer to Residence",
    str_detect(discharge_destination, "prop_transfer") ~ "Transfer to Private Facility",
    str_detect(discharge_destination, "prop_unknown") ~ "Unknown"
  ))


write_csv(waiting_times, "clean_data/waiting_times.csv")
