#cleaning script

library(tidyverse)
library(janitor)
library(lubridate)
library(leaflet)
library(here)

hb_simd <- read_csv("raw_data/covid_raw_data/hospital_admissions_hb_simd_20220302.csv") %>% 
  clean_names()
board_names <- read_csv("raw_data/covid_raw_data/health_board_names.csv") %>% 
  clean_names()

hb_simd

hb_simd <- left_join(hb_simd, board_names, by = "hb")

hb_simd <- hb_simd %>% 
  select(week_ending, hb, hbqf, simd_quintile, admission_type, admission_type_qf, number_admissions, average20182019, percent_variation, hb_name) 
hb_simd


hb_simd %>% 
  filter(admission_type == "Emergency") %>% 
  mutate(week_ending = ymd(week_ending)) %>% 
  mutate(month = month(week_ending, label = TRUE),
         year = year(week_ending), .after = week_ending) %>% 
  group_by(hb_name, year, simd_quintile) %>% 
  summarise(mean_admissions = mean(number_admissions))



hb_simd <- hb_simd %>% 
  mutate(hb_name = if_else(
    is.na(hb_name),
    "Scotland",
    hb_name
  ))

hb_simd <- hb_simd %>%
  mutate(week_ending = ymd(week_ending))

hb_simd <- hb_simd %>% 
  mutate(month = month(week_ending, label = TRUE),
         year = year(week_ending))



#number of admissions per hb_board 

# hb_simd %>% 
#   filter(admission_type == "Emergency") %>% 
#   
#   leaflet() %>% 
#   addTiles() %>% 
#   addCircleMarkers(lng = -4.975, 
#                    lat = 55.445, 
#                    color = "blue",
#                    popup="Ayrshire and Arran",
#                    radius = sqrt(98202/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -2.83333000, 
#                    lat = 55.58333000, 
#                    color = "blue",
#                    popup="Borders",
#                    radius = sqrt(20656/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -3.857784, 
#                    lat = 54.988285, 
#                    color = "blue",
#                    popup="Dumfries and Galloway",
#                    radius = sqrt(33981/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -3.78535, 
#                    lat = 56.0021, 
#                    color = "blue",
#                    popup="Forth Valley",
#                    radius = sqrt(64750/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -2.988, 
#                    lat = 57.228, 
#                    color = "blue",
#                    popup="Grampian",
#                    radius = sqrt(108032/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -4.71, 
#                    lat = 57.12, 
#                    color = "blue",
#                    popup="Highland",
#                    radius = sqrt(60411/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -3.083999664, 
#                    lat = 55.905496378, 
#                    color = "blue",
#                    popup="Lothian",
#                    radius = sqrt(185589	/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -3.0, 
#                    lat = 59.0, 
#                    color = "blue",
#                    popup="Orkney",
#                    radius = sqrt(3375/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -1.2689, 
#                    lat = 60.3038, 
#                    color = "blue",
#                    popup="Shetland",
#                    radius = sqrt(3248/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -7.02, 
#                    lat =  57.76, 
#                    color = "blue",
#                    popup="Western Isles",
#                    radius = sqrt(5398/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -3.1999992, 
#                    lat =    56.249999, 
#                    color = "blue",
#                    popup="Fife",
#                    radius = sqrt(84101/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -3.7333304, 
#                    lat = 56.6999972, 
#                    color = "blue",
#                    popup="Tayside",
#                    radius = sqrt(104225/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -4.4057, 
#                    lat = 55.90137, 
#                    color = "blue",
#                    popup="Greater Glasgow and Clyde",
#                    radius = sqrt(273189	/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -3.83333, 
#                    lat = 55.583331, 
#                    color = "blue",
#                    popup="Lanarkshire",
#                    radius = sqrt(143644	/ 10), weight = 1) 

hb_simd %>%
  filter(admission_type == "Emergency") %>% 
  group_by(hb, simd_quintile) %>% 
  summarise(sum = sum(number_admissions))

#number of admissions among the most deprived communities 
hb_simd %>% 
  filter(admission_type == "Emergency", 
         #quintile reactive 
         simd_quintile == "1") %>% 
  
  
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(lng = -4.975, 
                   lat = 55.445, 
                   color = "red",
                   popup="Ayrshire and Arran",
                   radius = sqrt(98202/ 10), weight = 1) %>% 
  addCircleMarkers(lng = -2.83333000, 
                   lat = 55.58333000, 
                   color = "red",
                   popup="Borders",
                   radius = sqrt(20656/ 10), weight = 1) %>% 
  addCircleMarkers(lng = -3.857784, 
                   lat = 54.988285, 
                   color = "red",
                   popup="Dumfries and Galloway",
                   radius = sqrt(33981/ 10), weight = 1) %>% 
  addCircleMarkers(lng = -3.78535, 
                   lat = 56.0021, 
                   color = "red",
                   popup="Forth Valley",
                   radius = sqrt(64750/ 10), weight = 1) %>% 
  addCircleMarkers(lng = -2.988, 
                   lat = 57.228, 
                   color = "red",
                   popup="Grampian",
                   radius = sqrt(108032/ 10), weight = 1) %>% 
  addCircleMarkers(lng = -4.71, 
                   lat = 57.12, 
                   color = "red",
                   popup="Highland",
                   radius = sqrt(60411/ 10), weight = 1) %>% 
  addCircleMarkers(lng = -3.083999664, 
                   lat = 55.905496378, 
                   color = "red",
                   popup="Lothian",
                   radius = sqrt(185589	/ 10), weight = 1) %>% 
  addCircleMarkers(lng = -3.0, 
                   lat = 59.0, 
                   color = "red",
                   popup="Orkney",
                   radius = sqrt(3375/ 10), weight = 1) %>% 
  addCircleMarkers(lng = -1.2689, 
                   lat = 60.3038, 
                   color = "red",
                   popup="Shetland",
                   radius = sqrt(3248/ 10), weight = 1) %>% 
  addCircleMarkers(lng = -7.02, 
                   lat =  57.76, 
                   color = "red",
                   popup="Western Isles",
                   radius = sqrt(5398/ 10), weight = 1) %>% 
  addCircleMarkers(lng = -3.1999992, 
                   lat =    56.249999, 
                   color = "red",
                   popup="Fife",
                   radius = sqrt(84101/ 10), weight = 1) %>% 
  addCircleMarkers(lng = -3.7333304, 
                   lat = 56.6999972, 
                   color = "red",
                   popup="Tayside",
                   radius = sqrt(104225/ 10), weight = 1) %>% 
  addCircleMarkers(lng = -4.4057, 
                   lat = 55.90137, 
                   color = "red",
                   popup="Greater Glasgow and Clyde",
                   radius = sqrt(273189	/ 10), weight = 1) %>% 
  addCircleMarkers(lng = -3.83333, 
                   lat = 55.583331, 
                   color = "red",
                   popup="Lanarkshire",
                   radius = sqrt(143644	/ 10), weight = 1)



# ##number of admissions among the second most deprived communities 
# hb_simd %>% 
#   filter(admission_type == "Emergency", 
#          #quintile reactive 
#          simd_quintile == "2") %>% 
#   
#   
#   leaflet() %>% 
#   addTiles() %>% 
#   addCircleMarkers(lng = -4.975, 
#                    lat = 55.445, 
#                    color = "orange",
#                    popup="Ayrshire and Arran",
#                    radius = sqrt(98202/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -2.83333000, 
#                    lat = 55.58333000, 
#                    color = "orange",
#                    popup="Borders",
#                    radius = sqrt(20656/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -3.857784, 
#                    lat = 54.988285, 
#                    color = "orange",
#                    popup="Dumfries and Galloway",
#                    radius = sqrt(33981/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -3.78535, 
#                    lat = 56.0021, 
#                    color = "orange",
#                    popup="Forth Valley",
#                    radius = sqrt(64750/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -2.988, 
#                    lat = 57.228, 
#                    color = "orange",
#                    popup="Grampian",
#                    radius = sqrt(108032/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -4.71, 
#                    lat = 57.12, 
#                    color = "orange",
#                    popup="Highland",
#                    radius = sqrt(60411/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -3.083999664, 
#                    lat = 55.905496378, 
#                    color = "orange",
#                    popup="Lothian",
#                    radius = sqrt(185589	/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -3.0, 
#                    lat = 59.0, 
#                    color = "orange",
#                    popup="Orkney",
#                    radius = sqrt(3375/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -1.2689, 
#                    lat = 60.3038, 
#                    color = "orange",
#                    popup="Shetland",
#                    radius = sqrt(3248/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -7.02, 
#                    lat =  57.76, 
#                    color = "orange",
#                    popup="Western Isles",
#                    radius = sqrt(5398/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -3.1999992, 
#                    lat =    56.249999, 
#                    color = "orange",
#                    popup="Fife",
#                    radius = sqrt(84101/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -3.7333304, 
#                    lat = 56.6999972, 
#                    color = "orange",
#                    popup="Tayside",
#                    radius = sqrt(104225/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -4.4057, 
#                    lat = 55.90137, 
#                    color = "orange",
#                    popup="Greater Glasgow and Clyde",
#                    radius = sqrt(273189	/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -3.83333, 
#                    lat = 55.583331, 
#                    color = "orange",
#                    popup="Lanarkshire",
#                    radius = sqrt(143644	/ 10), weight = 1) 
# 
# 
# ##number of admissions among the 3rd most deprived communities 
# hb_simd %>% 
#   filter(admission_type == "Emergency", 
#          #quintile reactive 
#          simd_quintile == "3") %>% 
#   
#   
#   leaflet() %>% 
#   addTiles() %>% 
#   addCircleMarkers(lng = -4.975, 
#                    lat = 55.445, 
#                    color = "#fc8d59",
#                    popup="Ayrshire and Arran",
#                    radius = sqrt(98202/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -2.83333000, 
#                    lat = 55.58333000, 
#                    color = "##fc8d59",
#                    popup="Borders",
#                    radius = sqrt(20656/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -3.857784, 
#                    lat = 54.988285, 
#                    color = "##fc8d59",
#                    popup="Dumfries and Galloway",
#                    radius = sqrt(33981/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -3.78535, 
#                    lat = 56.0021, 
#                    color = "#fc8d59",
#                    popup="Forth Valley",
#                    radius = sqrt(64750/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -2.988, 
#                    lat = 57.228, 
#                    color = "#fc8d59",
#                    popup="Grampian",
#                    radius = sqrt(108032/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -4.71, 
#                    lat = 57.12, 
#                    color = "#fc8d59",
#                    popup="Highland",
#                    radius = sqrt(60411/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -3.083999664, 
#                    lat = 55.905496378, 
#                    color = "#fc8d59",
#                    popup="Lothian",
#                    radius = sqrt(185589	/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -3.0, 
#                    lat = 59.0, 
#                    color = "#fc8d59",
#                    popup="Orkney",
#                    radius = sqrt(3375/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -1.2689, 
#                    lat = 60.3038, 
#                    color = "#fc8d59",
#                    popup="Shetland",
#                    radius = sqrt(3248/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -7.02, 
#                    lat =  57.76, 
#                    color = "#fc8d59",
#                    popup="Western Isles",
#                    radius = sqrt(5398/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -3.1999992, 
#                    lat =    56.249999, 
#                    color = "#fc8d59",
#                    popup="Fife",
#                    radius = sqrt(84101/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -3.7333304, 
#                    lat = 56.6999972, 
#                    color = "#fc8d59",
#                    popup="Tayside",
#                    radius = sqrt(104225/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -4.4057, 
#                    lat = 55.90137, 
#                    color = "#fc8d59",
#                    popup="Greater Glasgow and Clyde",
#                    radius = sqrt(273189	/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -3.83333, 
#                    lat = 55.583331, 
#                    color = "#fc8d59",
#                    popup="Lanarkshire",
#                    radius = sqrt(143644	/ 10), weight = 1) 
# 
# ###number of admissions among the the 2nd least deprived communities 
# hb_simd %>% 
#   filter(admission_type == "Emergency", 
#          simd_quintile == "4") %>% 
#   
#   
#   leaflet() %>% 
#   addTiles() %>% 
#   addCircleMarkers(lng = -4.975, 
#                    lat = 55.445, 
#                    color = "blue",
#                    popup="Ayrshire and Arran",
#                    radius = sqrt(98202/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -2.83333000, 
#                    lat = 55.58333000, 
#                    color = "blue",
#                    popup="Borders",
#                    radius = sqrt(20656/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -3.857784, 
#                    lat = 54.988285, 
#                    color = "blue",
#                    popup="Dumfries and Galloway",
#                    radius = sqrt(33981/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -3.78535, 
#                    lat = 56.0021, 
#                    color = "blue",
#                    popup="Forth Valley",
#                    radius = sqrt(64750/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -2.988, 
#                    lat = 57.228, 
#                    color = "blue",
#                    popup="Grampian",
#                    radius = sqrt(108032/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -4.71, 
#                    lat = 57.12, 
#                    color = "blue",
#                    popup="Highland",
#                    radius = sqrt(60411/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -3.083999664, 
#                    lat = 55.905496378, 
#                    color = "blue",
#                    popup="Lothian",
#                    radius = sqrt(185589	/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -3.0, 
#                    lat = 59.0, 
#                    color = "blue",
#                    popup="Orkney",
#                    radius = sqrt(3375/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -1.2689, 
#                    lat = 60.3038, 
#                    color = "blue",
#                    popup="Shetland",
#                    radius = sqrt(3248/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -7.02, 
#                    lat =  57.76, 
#                    color = "blue",
#                    popup="Western Isles",
#                    radius = sqrt(5398/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -3.1999992, 
#                    lat =    56.249999, 
#                    color = "blue",
#                    popup="Fife",
#                    radius = sqrt(84101/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -3.7333304, 
#                    lat = 56.6999972, 
#                    color = "blue",
#                    popup="Tayside",
#                    radius = sqrt(104225/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -4.4057, 
#                    lat = 55.90137, 
#                    color = "blue",
#                    popup="Greater Glasgow and Clyde",
#                    radius = sqrt(273189	/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -3.83333, 
#                    lat = 55.583331, 
#                    color = "blue",
#                    popup="Lanarkshire",
#                    radius = sqrt(143644	/ 10), weight = 1) 
# 
# 
# ####number of admissions among the the least deprived communities 
# hb_simd %>% 
#   filter(admission_type == "Emergency", 
#          simd_quintile == "5") %>% 
#   
#   
#   leaflet() %>% 
#   addTiles() %>% 
#   addCircleMarkers(lng = -4.975, 
#                    lat = 55.445, 
#                    color = "purple",
#                    popup="Ayrshire and Arran",
#                    radius = sqrt(98202/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -2.83333000, 
#                    lat = 55.58333000, 
#                    color = "purple",
#                    popup="Borders",
#                    radius = sqrt(20656/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -3.857784, 
#                    lat = 54.988285, 
#                    color = "purple",
#                    popup="Dumfries and Galloway",
#                    radius = sqrt(33981/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -3.78535, 
#                    lat = 56.0021, 
#                    color = "purple",
#                    popup="Forth Valley",
#                    radius = sqrt(64750/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -2.988, 
#                    lat = 57.228, 
#                    color = "purple",
#                    popup="Grampian",
#                    radius = sqrt(108032/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -4.71, 
#                    lat = 57.12, 
#                    color = "purple",
#                    popup="Highland",
#                    radius = sqrt(60411/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -3.083999664, 
#                    lat = 55.905496378, 
#                    color = "purple",
#                    popup="Lothian",
#                    radius = sqrt(185589	/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -3.0, 
#                    lat = 59.0, 
#                    color = "purple",
#                    popup="Orkney",
#                    radius = sqrt(3375/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -1.2689, 
#                    lat = 60.3038, 
#                    color = "purple",
#                    popup="Shetland",
#                    radius = sqrt(3248/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -7.02, 
#                    lat =  57.76, 
#                    color = "purple",
#                    popup="Western Isles",
#                    radius = sqrt(5398/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -3.1999992, 
#                    lat =    56.249999, 
#                    color = "purple",
#                    popup="Fife",
#                    radius = sqrt(84101/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -3.7333304, 
#                    lat = 56.6999972, 
#                    color = "purple",
#                    popup="Tayside",
#                    radius = sqrt(104225/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -4.4057, 
#                    lat = 55.90137, 
#                    color = "purple",
#                    popup="Greater Glasgow and Clyde",
#                    radius = sqrt(273189	/ 10), weight = 1) %>% 
#   addCircleMarkers(lng = -3.83333, 
#                    lat = 55.583331, 
#                    color = "purple",
#                    popup="Lanarkshire",
#                    radius = sqrt(143644	/ 10), weight = 1)


library(plotly)

hb_simd$simd_quintile <- factor(hb_simd$simd_quintile, levels = c("1", "2", "3", "4", "5"))
hb_simd
#mean admissions per SIMD from 2020 to 2022
p2 <- hb_simd %>% 
  filter(admission_type == "Emergency",
         # hb_name reactive
         hb_name == "Scotland")%>% 
  group_by(hb_name, week_ending, simd_quintile, year) %>% 
  summarise(mean_admissions = mean(number_admissions),
            mean_20182019_admissions = mean(average20182019)) %>% 
  
  ggplot()+
  geom_point(aes(x = week_ending, 
                 y = mean_admissions, colour = simd_quintile, 
                 text = paste0("Date: ", week_ending, 
                               "<br>",
                               "Mean admissions: ", 
                               round(mean_admissions, digits = 2), 
                               "<br>",
                               "2018-2019 mean admissions: ", 
                               round(mean_20182019_admissions, digits = 2)))) +
  
  geom_line(aes(x = week_ending,
                y = mean_admissions,
                colour = simd_quintile,
                group = simd_quintile)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  scale_y_sqrt() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size =7)) +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype=4, colour = "grey50")+
  geom_vline(xintercept = as.numeric(as.Date("2021-01-01")), linetype=4, colour = "grey50")+
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype=4, colour = "grey50")+
  labs(title = "Mean admissions per SIMD \n",
       x = "Date",
       y = "Mean admissions",
       colour = "SIMD")


p2 %>% 
  ggplotly(tooltip = "text") %>% 
  config(displayModeBar = FALSE)

