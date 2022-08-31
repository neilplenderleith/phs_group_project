library(tidyverse)
library(janitor)
library(lubridate)


ae_wait_times <- read_csv(
  "raw_data/non_covid_raw_data/monthly_ae_waitingtimes_202206.csv") %>% 
  clean_names()

hb_names <- read_csv("raw_data/covid_raw_data/health_board_names.csv") %>% 
  clean_names()

#select only variables needed for summary table
ae_attendance_summary <- ae_wait_times %>% 
  select(month, hbt, number_of_attendances_aggregate, 
         discharge_destination_admission_to_same,
         discharge_destination_other_specialty,
         discharge_destination_residence,
         discharge_destination_transfer,
         discharge_destination_unknown)

#create separate month and year columns
ae_attendance_summary <- ae_attendance_summary %>% 
  mutate(month_year = month, .before = month)

ae_attendance_summary <- ae_attendance_summary %>% 
  mutate(month = month(ym(month_year), label = TRUE),
         year = year(ym(month_year)), .after = month_year)

ae_attendance_summary <- ae_attendance_summary %>% 
  left_join(hb_names, by = c("hbt" = "hb"))

#make all scotland attendance table
all_scotland_attendance <- ae_attendance_summary %>% 
  group_by(year, month) %>% 
  filter(year >= 2017) %>% 
  summarise(num_attendances = sum(number_of_attendances_aggregate),
            admission_to_same = 
              sum(discharge_destination_admission_to_same, na.rm = TRUE),
            other_specialty = 
              sum(discharge_destination_other_specialty, na.rm = TRUE),
            residence = sum(discharge_destination_residence, na.rm = TRUE),
            transfer = sum(discharge_destination_transfer, na.rm = TRUE),
            unknown = sum(discharge_destination_unknown, na.rm = TRUE))

#get proportions for destinations for all scotland
all_scotland_attendance <- all_scotland_attendance %>% 
  rowwise() %>% 
  mutate(
    total_avg_discharge = sum(c(admission_to_same, other_specialty,
                                residence, transfer, unknown)),
    prop_admission = admission_to_same/total_avg_discharge,
    prop_other_specialty = other_specialty/total_avg_discharge,
    prop_residence = residence/total_avg_discharge,
    prop_transfer = transfer/total_avg_discharge,
    prop_unknown = unknown/total_avg_discharge
  )

#summarise attendance and destination for all hbs
ae_attendance_summary <- ae_attendance_summary %>% 
  group_by(hb_name, year, month) %>% 
  summarise(num_attendances = sum(number_of_attendances_aggregate),
            admission_to_same = 
              sum(discharge_destination_admission_to_same, na.rm = TRUE),
            other_specialty = 
              sum(discharge_destination_other_specialty, na.rm = TRUE),
            residence = sum(discharge_destination_residence, na.rm = TRUE),
            transfer = sum(discharge_destination_transfer, na.rm = TRUE),
            unknown = sum(discharge_destination_unknown, na.rm = TRUE))

#get proportions for all destinations
ae_attendance_summary <- ae_attendance_summary %>% 
  rowwise() %>% 
  mutate(
    total_avg_discharge = sum(c(admission_to_same, other_specialty,
                                residence, transfer, unknown)),
    prop_admission = admission_to_same/total_avg_discharge,
    prop_other_specialty = other_specialty/total_avg_discharge,
    prop_residence = residence/total_avg_discharge,
    prop_transfer = transfer/total_avg_discharge,
    prop_unknown = unknown/total_avg_discharge
  )

#add hb_name column to all scotland
all_scotland_attendance <- all_scotland_attendance %>% 
  mutate(hb_name = "All Scotland")

#select relevant columns
ae_attendance_summary <- ae_attendance_summary %>% 
  filter(year >=2017) %>% 
  select(year, month, hb_name, num_attendances, prop_admission, 
         prop_other_specialty, prop_residence, prop_transfer, prop_unknown)

all_scotland_attendance <- all_scotland_attendance %>% 
  select(year, month, hb_name, num_attendances, prop_admission,
         prop_other_specialty, prop_residence, prop_transfer, prop_unknown)

#bind rows
ae_attendance_summary <- ae_attendance_summary %>% 
  bind_rows(all_scotland_attendance)

#separate dataset into preCovid and Covid year ranges
covid_ae_attendance <- ae_attendance_summary %>% 
  filter(year >= 2020)

precovid_ae_attendance <- ae_attendance_summary %>% 
  filter(year < 2020)

#get 2017-2019 avgs to add as comparator to covid dataset
precovid_ae_attendance <- precovid_ae_attendance %>% 
  group_by(hb_name, month) %>% 
  summarise(avg_attendances_20171819 = mean(num_attendances),
            avg_prop_admission_20171829 = mean(prop_admission),
            avg_prop_other_specialty_20171819 = mean(prop_other_specialty),
            avg_prop_residence_20171819 = mean(prop_residence),
            avg_prop_transfer_20171819 = mean(prop_transfer),
            avg_prop_unknown_20171819 = mean(prop_unknown)) 

#create key to join with covid_ae_attendance
precovid_ae_attendance <- precovid_ae_attendance %>% 
  mutate(hb_key = paste0(hb_name, "_", month), .before = hb_name)

#select only key and avg variables
precovid_ae_attendance <- precovid_ae_attendance %>% 
  subset(select = -c(hb_name, month))

#add key to covid table
covid_ae_attendance <- covid_ae_attendance %>% 
  mutate(hb_key = paste0(hb_name, "_", month), .before = hb_name)

#join precovid avgs to covid table
covid_ae_attendance <- covid_ae_attendance %>% 
  left_join(precovid_ae_attendance, by = "hb_key")

#add date column
covid_ae_attendance <- covid_ae_attendance %>% 
  mutate(date = (paste0(year, "-", month)), .before = year) %>% 
  mutate(date = ym(date), .before = year)

#put data in long format
covid_ae_attendance <- covid_ae_attendance %>% 
  pivot_longer(
    cols = starts_with("prop"),
    names_to = "destination",
    values_to = "destination_prop"
  )

#get comparator 2017-2019 avg prop for each 2020-2022 prop
covid_ae_attendance <- covid_ae_attendance %>% 
  mutate(
    avg_prop_20171819 = case_when(
      destination == "prop_admission" ~ avg_prop_admission_20171829,
      destination == "prop_other_specialty" ~ avg_prop_other_specialty_20171819,
      destination == "prop_residence" ~ avg_prop_residence_20171819,
      destination == "prop_transfer" ~ avg_prop_transfer_20171819,
      destination == "prop_unknown" ~ avg_prop_unknown_20171819
    )
  )

#select only relevant variables
covid_ae_attendance <- covid_ae_attendance %>% 
  select(date, year, month, hb_key, hb_name, num_attendances, 
         avg_attendances_20171819, destination,
         destination_prop, avg_prop_20171819)

#clean up names of destinations
covid_ae_attendance <- covid_ae_attendance %>% 
  mutate(destination = case_when(
      destination == "prop_admission" ~ "Admission to same facility",
      destination == "prop_other_specialty" ~ 
        "Discharged to private provider/died",
      destination == "prop_residence" ~ "Discharged to private residence",
      destination == "prop_transfer" ~ "Transferred to another NHS provider",
      destination == "prop_unknown" ~ "Unknown or other discharge destination"
    )
  )

write_csv(covid_ae_attendance, "clean_data/covid_ae_attendance.csv")
