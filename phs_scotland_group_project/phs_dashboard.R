
# read in packages --------------------------------------------------------

library(tidyverse)
library(shiny)
library(bslib)
library(shinydashboard)
library(shinyWidgets)
library(lubridate)
library(plotly)
library(leaflet)


# data wrangling ----------------------------------------------------------

waiting_times <- read_csv("clean_data/wait_times.csv")

waiting_times <- waiting_times %>% 
  mutate(year = year(date))

min_year_wait <- min(waiting_times$year)
max_year_wait <- max(waiting_times$year)

all_years <- waiting_times %>% 
  distinct(year) %>% 
  arrange(year) %>% 
  pull()

all_discharges <- waiting_times %>% 
  distinct(discharge_destination) %>% 
  arrange(discharge_destination) %>% 
  pull()

ae_wait_times <- read_csv("clean_data/non_covid_data/ae_wait_times.csv")

age <- read_csv("clean_data/non_covid_data/age_sex.csv")

all_ages <- age %>% 
  distinct(age) %>% 
  arrange(age) %>% 
  pull()

min_year_age <- min(age$year)
max_year_age <- max(age$year)

sex <- read_csv("clean_data/non_covid_data/age_sex.csv")

all_sex <- sex %>% 
  distinct(sex) %>% 
  arrange(sex) %>% 
  pull()

min_year_sex <- min(sex$year)
max_year_sex <- max(sex$year)

simd <- read_csv("clean_data/non_covid_data/simd.csv")

all_simd <- simd %>%
  drop_na(simd) %>% 
  distinct(simd) %>% 
  arrange(simd) %>% 
  pull()

min_year_simd <- min(simd$year)
max_year_simd <- max(simd$year)

covid_ae_attendances <- read_csv("clean_data/covid_ae_attendance.csv")

all_healthboards <- covid_ae_attendances %>% 
  distinct(hb_name) %>% 
  arrange(hb_name) %>% 
  pull()

hb_simd <- read_csv("clean_data/hb_simd_clean.csv")

all_healthboards_simd <- hb_simd %>% 
  distinct(hb_name) %>% 
  arrange(hb_name) %>% 
  pull()

all_simd_map <- hb_simd %>% 
  distinct(simd_quintile) %>% 
  arrange(simd_quintile) %>% 
  pull()

all_simd_year <- hb_simd %>%
  distinct(year) %>% 
  arrange(year) %>% 
  pull()

# ui ----------------------------------------------------------------------

ui <- navbarPage(
  
  theme = bs_theme(bootswatch = "flatly"),
  
  title = tags$h4("Public Health Scotland Dashboard Project"),
  
  header = tagList(
    useShinydashboard()
  ),
  
  position = "static-top",
  
  fluid = TRUE,
  
  tabPanel(tags$h5("A&E Overview"),
           
           fluidRow(
             
             box(
               title = tags$h3("Percentage of A&E Departments Meeting the 4hr Target Turnaround for Patients"),
               status = "primary",
               solidHeader = TRUE,
               height = 600,
               
               plotlyOutput("ae_wait_times_plot")
             ),
             
             box(
               title = tags$h3("Map Displaying Percentage of A&E Departments Meeting 4hr Target per Healthboard by Year"),
               status = "warning",
               solidHeader = TRUE,
               height = 600,
               
               plotOutput("ae_map")
             )
           )
           
  ),
  
  
  tabPanel(tags$h5("Winter Crisis"),
           
           sidebarLayout(
             
             sidebarPanel = sidebarPanel(
               
               width = 4,
               
               titlePanel(tags$h1("Winter Crisis Plot Controls")),
               
               sliderInput(inputId = "winter_wait",
                           label = tags$h2("Year range"),
                           min = min_year_wait,
                           max = max_year_wait,
                           value = c(min_year_wait, max_year_wait),
                           step = 1,
                           sep = ""
               ),
               
               radioButtons(inputId = "discharge_destination",
                            label = tags$h2("Patient Discharge Destination"),
                            choices = all_discharges
               )
             ),
             
             mainPanel = mainPanel(
               box(
                 title = tags$h3("Proportion of Patients Being Dispatched to Different Destinations"),
                 status = "primary",
                 solidHeader = TRUE,
                 width = 12,
                 height = 600,
                 
                 plotlyOutput("winter_plot")
               )
             )
           )
  ),
  
  
  tabPanel(tags$h5("Impact of COVID-19"),
           
           selectInput(
             inputId = "health_boards",
             label = tags$h3("Select Health Board"),
             choices = all_healthboards
           ),
           
           fluidRow(
             
             box(
               title = tags$h3("Number of attendances at A&E 2020 - 2022"),
               status = "primary",
               solidHeader = TRUE,
               width = 12,
               height = 600,
               
               plotlyOutput("covid_plot_1")
             ),
             
             box(
               title = tags$h3("Destination of attendances at A&E 2020 - 2022"),
               status = "primary",
               solidHeader = TRUE,
               width = 12,
               height = 600,
               
               plotlyOutput("covid_plot_2")
             )
           )
  ),
  
  
  tabPanel(tags$h5("Age Group"),
           
           sidebarLayout(
             
             sidebarPanel = sidebarPanel(
               
               width = 4,
               
               titlePanel(tags$h1("Age Demographic Plot Controls")),
               
               checkboxGroupInput(
                 inputId = "age_groups",
                 label = tags$h3("Select Patient Age Group(s)"),
                 choices = all_ages,
                 selected = all_ages
               ),
               
               sliderInput(inputId = "age_year",
                           label = tags$h2("Year range"),
                           min = min_year_age,
                           max = max_year_age,
                           value = c(min_year_age, max_year_age),
                           step = 1,
                           sep = ""
               )
             ),
             
             mainPanel = mainPanel(
               
               box(
                 width = 12,
                 title = tags$h3("Average Hospital Episodes by Age Groups"),
                 status = "success",
                 solidHeader = TRUE,
                 height = 600,
                 
                 plotlyOutput("age_plot")
               )
             )
           )
  ),
  
  
  tabPanel(tags$h5("Sex"),
           
           sidebarLayout(
             
             sidebarPanel = sidebarPanel(
               
               width = 4,
               
               titlePanel(tags$h1("Sex Demographic Plot Controls")),
               
               checkboxGroupInput(
                 inputId = "sex_groups",
                 label = tags$h3("Select Patient Sex"),
                 choices = all_sex,
                 selected = all_sex
               ),
               
               sliderInput(inputId = "sex_year",
                           label = tags$h2("Year range"),
                           min = min_year_sex,
                           max = max_year_sex,
                           value = c(min_year_sex, max_year_sex),
                           step = 1,
                           sep = ""
               )
             ),
             
             mainPanel = mainPanel(
               
               box(
                 width = 12,
                 title = tags$h3("Average Hospital Episodes by Sex"),
                 status = "success",
                 solidHeader = TRUE,
                 height = 600,
                 
                 plotlyOutput("sex_plot")
               )
             )
           )
  ),
  
  
  tabPanel(tags$h5("SIMD"),
           
           sidebarLayout(
             
             sidebarPanel = sidebarPanel(
               
               width = 4,
               
               titlePanel(tags$h1("SIMD Demographic Plot Controls")),
               
               checkboxGroupInput(
                 inputId = "simd_groups",
                 label = tags$h3("Select Patient SIMD"),
                 choices = all_simd,
                 selected = all_simd
               ),
               
               sliderInput(inputId = "simd_year",
                           label = tags$h2("Year range"),
                           min = min_year_simd,
                           max = max_year_simd,
                           value = c(min_year_simd, max_year_simd),
                           step = 1,
                           sep = ""
               ),
               
               br(),
               
               br(),
               
               br(),
               
               titlePanel(tags$h1("SIMD A&E Attendance Plot Controls")),
               
               selectInput(inputId = "simd_attendance",
                           label = tags$h2("Select Healthboard"),
                           choices = all_healthboards_simd)
               
             ),
             
             mainPanel = mainPanel(
               
               box(
                 width = 12,
                 title = tags$h3("Average Hospital Episodes by SIMD Deprivation score"),
                 status = "success",
                 solidHeader = TRUE,
                 height = 600,
                 
                 plotlyOutput("simd_plot")
               ),
               
               box(
                 width = 12,
                 title = tags$h3("Mean Emergency Admissions per SIMD (2020-2022)"),
                 status = "success",
                 solidHeader = TRUE,
                 height = 600,
                 
                 plotlyOutput("simd_attendance_plot")
               )
             )
           )
  ),
  
  
  tabPanel(tags$h5("Geospatial Maps"),
           
           fluidRow(
             box(
               title = tags$h1("Admissions per Healthboard Area for SIMD Levels"),
               status = "warning",
               solidHeader = TRUE
             ),
             
             box(
               title = tags$h1("Select SIMD Level and Year"),
               status = "warning",
               solidHeader = TRUE,
               
               selectInput(inputId = "simd_map",
                           label = "",
                           choices = all_simd_map),
               
               selectInput(inputId = "simd_map_year",
                           label = "",
                           choices = all_simd_year)
             )

           ),

           leafletOutput("simd_leaflet", height = "100vh", width = "100vw")
  ),
  
  
  tabPanel(tags$h5("Data"),
           DT::dataTableOutput("table_output")
  )
)



# server ------------------------------------------------------------------

server <- function(input, output) {
  
  filtered_winter_plot <- reactive({
    waiting_times %>%
      filter(discharge_destination == input$discharge_destination,
             !is.na(discharge_proportion),
             year >= input$winter_wait[1] & year <= input$winter_wait[2]) %>% 
      group_by(date) %>% 
      summarise(mean_discharge = mean(discharge_proportion))
  })
  
  output$winter_plot <- renderPlotly({
    winter_plotly <- filtered_winter_plot() %>%
      ggplot() +
      geom_point(aes(x = date,
                     y = mean_discharge,
                     text =  paste0("Date: ", date,
                                    "<br>",
                                    "Percentage: ", 
                                    round(mean_discharge*100, digits = 2), 
                                    "%")), size = 0.7) +
      geom_line(aes(x = date,
                    y = mean_discharge)) +
      scale_x_date(date_breaks = "6 months", date_labels =  "%b %Y") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size =7)) +
      geom_vline(xintercept = as.numeric(as.Date("2008-01-01")), linetype=4, colour = "grey50", alpha = 0.7)+
      geom_vline(xintercept = as.numeric(as.Date("2009-01-01")), linetype=4, colour = "grey50", alpha = 0.7)+
      geom_vline(xintercept = as.numeric(as.Date("2010-01-01")), linetype=4, colour = "grey50", alpha = 0.7)+
      geom_vline(xintercept = as.numeric(as.Date("2011-01-01")), linetype=4, colour = "grey50", alpha = 0.7)+
      geom_vline(xintercept = as.numeric(as.Date("2012-01-01")), linetype=4, colour = "grey50", alpha = 0.7)+
      geom_vline(xintercept = as.numeric(as.Date("2013-01-01")), linetype=4, colour = "grey50", alpha = 0.7)+
      geom_vline(xintercept = as.numeric(as.Date("2014-01-01")), linetype=4, colour = "grey50", alpha = 0.7)+
      geom_vline(xintercept = as.numeric(as.Date("2015-01-01")), linetype=4, colour = "grey50", alpha = 0.7)+
      geom_vline(xintercept = as.numeric(as.Date("2016-01-01")), linetype=4, colour = "grey50", alpha = 0.7)+
      geom_vline(xintercept = as.numeric(as.Date("2017-01-01")), linetype=4, colour = "grey50", alpha = 0.7)+
      geom_vline(xintercept = as.numeric(as.Date("2018-01-01")), linetype=4, colour = "grey50", alpha = 0.7)+
      geom_vline(xintercept = as.numeric(as.Date("2019-01-01")), linetype=4, colour = "grey50", alpha = 0.7)+
      geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype=4, colour = "grey50", alpha = 0.7)+
      geom_vline(xintercept = as.numeric(as.Date("2021-01-01")), linetype=4, colour = "grey50", alpha = 0.7)+
      geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype=4, colour = "grey50", alpha = 0.7) +
      labs(title = "Proportion of attendances to selected destination \n",
           x = "\n Date",
           y = "Proportion of attendances")
    
    winter_plotly %>% 
      ggplotly(tooltip = "text") %>% 
      config(displayModeBar = FALSE) %>% 
      layout(hoverlabel = list(bgcolor = "white"))
  })
  
  output$ae_wait_times_plot <- renderPlotly({
    ae_wait_plotly <- ae_wait_times %>% 
      filter(department_type == "Emergency Department") %>% 
      group_by(date, department_type) %>% 
      summarise(avg_4hr_target_made = mean(percent_4hr_target_achieved)) %>% 
      ggplot(aes(x = date, y = avg_4hr_target_made))+
      geom_line(aes(colour = department_type))+
      scale_x_date(date_breaks = "6 months", date_labels =  "%b %Y")+
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size =9))+
      geom_smooth()+
      geom_vline(xintercept = as.numeric(as.Date("2008-01-01")), linetype=4, colour = "grey50", alpha = 0.7)+
      geom_vline(xintercept = as.numeric(as.Date("2009-01-01")), linetype=4, colour = "grey50", alpha = 0.7)+
      geom_vline(xintercept = as.numeric(as.Date("2010-01-01")), linetype=4, colour = "grey50", alpha = 0.7)+
      geom_vline(xintercept = as.numeric(as.Date("2011-01-01")), linetype=4, colour = "grey50", alpha = 0.7)+
      geom_vline(xintercept = as.numeric(as.Date("2012-01-01")), linetype=4, colour = "grey50", alpha = 0.7)+
      geom_vline(xintercept = as.numeric(as.Date("2013-01-01")), linetype=4, colour = "grey50", alpha = 0.7)+
      geom_vline(xintercept = as.numeric(as.Date("2014-01-01")), linetype=4, colour = "grey50", alpha = 0.7)+
      geom_vline(xintercept = as.numeric(as.Date("2015-01-01")), linetype=4, colour = "grey50", alpha = 0.7)+
      geom_vline(xintercept = as.numeric(as.Date("2016-01-01")), linetype=4, colour = "grey50", alpha = 0.7)+
      geom_vline(xintercept = as.numeric(as.Date("2017-01-01")), linetype=4, colour = "grey50", alpha = 0.7)+
      geom_vline(xintercept = as.numeric(as.Date("2018-01-01")), linetype=4, colour = "grey50", alpha = 0.7)+
      geom_vline(xintercept = as.numeric(as.Date("2019-01-01")), linetype=4, colour = "grey50", alpha = 0.7)+
      geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype=4, colour = "grey50", alpha = 0.7)+
      geom_vline(xintercept = as.numeric(as.Date("2021-01-01")), linetype=4, colour = "grey50", alpha = 0.7)+
      geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype=4, colour = "grey50", alpha = 0.7)+
      labs(x = "\nDate",
           y = "Percentage",
           colour = "Department Type")
    
    ggplotly(ae_wait_plotly)
  })
  
  filtered_covid_plot <- reactive({
    covid_ae_attendances %>% 
      filter(hb_name == input$health_boards)
  })
  
  output$covid_plot_1 <- renderPlotly({
    covid_ae_attendance_plotly <- filtered_covid_plot() %>% 
      ggplot() +
      geom_point(aes(x = date,
                     y = num_attendances,
                     text = paste0("Date: ", year, "-", month,
                                   "<br>",
                                   "Number of admissions: ", num_attendances,
                                   "<br>",
                                   "2017-2019 avg admissions: ", 
                                   round(avg_attendances_20171819))
      )) +
      geom_line(aes(x = date,
                    y = num_attendances)) +
      geom_line(aes(x = date,
                    y = avg_attendances_20171819), 
                colour = "grey60", alpha = 0.5) +
      scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size =9))+
      geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype=4, colour = "grey50")+
      geom_vline(xintercept = as.numeric(as.Date("2021-01-01")), linetype=4, colour = "grey50")+
      geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype=4, colour = "grey50")+
      labs(title = "Comparison with 2018-2019 averages\n",
           x = "\nDate",
           y = "Number of Attendances")
    
    covid_ae_attendance_plotly %>% 
      ggplotly(tooltip = "text") %>% 
      config(displayModeBar = FALSE) %>% 
      layout(hoverlabel = list(bgcolor = "white"))
  })
  
  output$covid_plot_2 <- renderPlotly({
    covid_ae_destinations_plotly <- filtered_covid_plot() %>% 
      ggplot() +
      geom_point(aes(x = date,
                     y = destination_prop,
                     colour = destination,
                     text = paste0("Date: ", year, "-", month,
                                   "<br>",
                                   "Percentage: ", 
                                   round(destination_prop*100, digits = 2), 
                                   "%",
                                   "<br>",
                                   "2017-2019 percentage: ", 
                                   round(avg_prop_20171819*100, digits = 2), 
                                   "%"))) +
      geom_line(aes(x = date,
                    y = destination_prop,
                    group = destination)) +
      geom_line(aes(x = date,
                    y = avg_prop_20171819,
                    colour = destination,
                    group = destination), alpha = 0.5) +
      scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
      scale_y_sqrt() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size =9))+
      geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype=4, colour = "grey50")+
      geom_vline(xintercept = as.numeric(as.Date("2021-01-01")), linetype=4, colour = "grey50")+
      geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype=4, colour = "grey50")+
      labs(title = "Comparison with proportion of attendances at A&E 2017 - 2019\n",
           x = "\nDate",
           y = "Proportion of attendances",
           colour = "Destination")
    covid_ae_destinations_plotly %>% 
      ggplotly(tooltip = "text") %>% 
      config(displayModeBar = FALSE)
  })
  
  filtered_age_plot <- reactive({
    age %>% 
      filter(year >= input$age_year[1] & year <= input$age_year[2],
             age %in% input$age_groups) %>% 
      group_by(quarter, age) %>% 
      summarise(avg_episodes = mean(episodes, na.rm = TRUE))
  })
  
  output$age_plot <- renderPlotly({
    age_plotly <- filtered_age_plot() %>% 
      #filter(min_date < year & year < max_date) %>% 
      # group_by(quarter, age) %>% 
      # summarise(avg_episodes = mean(episodes, na.rm = TRUE)) %>% 
      ggplot(aes(x = quarter, y = avg_episodes))+
      geom_line(aes(colour = age, group = age))+ 
      geom_point(aes(colour = age,
                     text = paste0("Date: ", quarter, "<br>",
                                   "Average Episodes: ", round(avg_episodes, digits = 2), "<br>",
                                   "Age Group: ", age)),size = 0.5)+
      labs(x = "\nYear and Quarter",
           y = "Average Episodes\n",
           colour = "Age")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size =9))
    
    ggplotly(age_plotly, tooltip = "text") %>% 
      config(displayModeBar = FALSE)
  })
  
  filtered_sex_plot <- reactive({
    sex %>% 
      filter(year >= input$sex_year[1] & year <= input$sex_year[2],
             sex == input$sex_groups) %>% 
      group_by(quarter, sex) %>% 
      summarise(avg_length_of_episode = mean(average_length_of_episode, na.rm = TRUE))
  })
  
  output$sex_plot <- renderPlotly({
    sex_plotly <- filtered_sex_plot() %>% 
      # group_by(quarter, sex) %>% 
      # summarise(avg_length_of_episode = mean(average_length_of_episode, na.rm = TRUE)) %>% 
      ggplot(aes(x = quarter, y = avg_length_of_episode))+
      geom_line(aes(colour = sex, group = sex))+
      geom_point(aes(colour = sex, 
                     text = paste0("Date: ", quarter, "<br>", 
                                   "Gender: ", sex)),
                 size = 0.5)+
      labs(x = "\nYear and Quarter",
           y = "Average Episodes\n",
           colour = "Sex")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size =9))
    
    
    ggplotly(sex_plotly, tooltip = "text") %>% 
      config(displayModeBar = FALSE)
  })
  
  filtered_simd_plot <- reactive({
    simd %>%
      drop_na(simd) %>% 
      mutate(simd = as.factor(simd)) %>% 
      filter(year >= input$simd_year[1] & year <= input$simd_year[2],
             simd %in% input$simd_groups)
  })
  
  output$simd_plot <- renderPlotly({
    simd_plotly <- filtered_simd_plot() %>% 
      # drop_na(simd) %>%
      # mutate(simd = as.factor(simd)) %>% # gives each simd a separate colour
      group_by(quarter, simd) %>%
      summarise(avg_episodes = mean(episodes, na.rm = TRUE)) %>%
      ggplot(aes(x = quarter, y = avg_episodes, group = simd))+
      geom_line(aes(colour = simd))+
      geom_point(aes(text = paste0("Date: ", quarter, "<br>",
                                   "Average Episodes: ", round(avg_episodes, digits = 2), "<br>",
                                   "SIMD: ", simd),
                     colour = simd),size = 0.5)+
      scale_y_continuous(labels = scales::comma)+
      labs(x = "\nYear and Quarter",
           y = "Average Episodes\n",
           colour = "SIMD")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size =9))
    
    ggplotly(simd_plotly, tooltip = "text") %>% 
      config(displayModeBar = FALSE) 
  })
  
  filtered_simd_attendance_plot <- reactive({
    hb_simd %>% 
      filter(hb_name == input$simd_attendance,
             admission_type == "Emergency")
  })
  
  output$simd_attendance_plot <- renderPlotly({
    simd_attendance_plotly <- filtered_simd_attendance_plot() %>%
      mutate(simd_quintile = factor(simd_quintile, levels = c("1", "2", "3", "4", "5"))) %>% 
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
                                   round(mean_20182019_admissions, digits = 2))), size = 0.7) +
      
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

    simd_attendance_plotly %>% 
      ggplotly(tooltip = "text") %>% 
      config(displayModeBar = FALSE)
  })
  
  filtered_simd_map <- reactive({
    hb_simd %>% 
      filter(simd_quintile == input$simd_map,
             year == input$simd_map_year) %>% 
      filter(admission_type == "Emergency") %>% 
        mutate(week_ending = ymd(week_ending)) %>% 
        mutate(month = month(week_ending, label = TRUE),
               year = year(week_ending), .after = week_ending) %>% 
      group_by(hb_name, year, simd_quintile) %>% 
      summarise(mean_admissions = mean(number_admissions)*100)
  })
  
  
  output$simd_leaflet <- renderLeaflet({
    filtered_simd_map() %>%
      
      leaflet() %>% 
      addTiles() %>% 
      setView(-4, 55.5, zoom = 7) %>% 
      addCircleMarkers(lng = -4.975, 
                       lat = 55.445, 
                       color = "red",
                       popup="Ayrshire and Arran",
                       radius = hb_simd$mean_admissions, weight = 1) %>% 
      addCircleMarkers(lng = -2.83333000, 
                       lat = 55.58333000, 
                       color = "red",
                       popup="Borders",
                       radius = hb_simd$mean_admissions, weight = 1) %>% 
      addCircleMarkers(lng = -3.857784, 
                       lat = 54.988285, 
                       color = "red",
                       popup="Dumfries and Galloway",
                       radius = hb_simd$mean_admissions, weight = 1) %>% 
      addCircleMarkers(lng = -3.78535, 
                       lat = 56.0021, 
                       color = "red",
                       popup="Forth Valley",
                       radius = hb_simd$mean_admissions, weight = 1) %>% 
      addCircleMarkers(lng = -2.988, 
                       lat = 57.228, 
                       color = "red",
                       popup="Grampian",
                       radius = hb_simd$mean_admissions, weight = 1) %>% 
      addCircleMarkers(lng = -4.71, 
                       lat = 57.12, 
                       color = "red",
                       popup="Highland",
                       radius = hb_simd$mean_admissions, weight = 1) %>% 
      addCircleMarkers(lng = -3.083999664, 
                       lat = 55.905496378, 
                       color = "red",
                       popup="Lothian",
                       radius = hb_simd$mean_admissions, weight = 1) %>% 
      addCircleMarkers(lng = -3.0, 
                       lat = 59.0, 
                       color = "red",
                       popup="Orkney",
                       radius = hb_simd$mean_admissions, weight = 1) %>% 
      addCircleMarkers(lng = -1.2689, 
                       lat = 60.3038, 
                       color = "red",
                       popup="Shetland",
                       radius = hb_simd$mean_admissions, weight = 1) %>% 
      addCircleMarkers(lng = -7.02, 
                       lat =  57.76, 
                       color = "red",
                       popup="Western Isles",
                       radius = hb_simd$mean_admissions, weight = 1) %>% 
      addCircleMarkers(lng = -3.1999992, 
                       lat =    56.249999, 
                       color = "red",
                       popup="Fife",
                       radius = hb_simd$mean_admissions, weight = 1) %>% 
      addCircleMarkers(lng = -3.7333304, 
                       lat = 56.6999972, 
                       color = "red",
                       popup="Tayside",
                       radius = hb_simd$mean_admissions, weight = 1) %>% 
      addCircleMarkers(lng = -4.4057, 
                       lat = 55.90137, 
                       color = "red",
                       popup="Greater Glasgow and Clyde",
                       radius = hb_simd$mean_admissions, weight = 1) %>% 
      addCircleMarkers(lng = -3.83333, 
                       lat = 55.583331, 
                       color = "red",
                       popup="Lanarkshire",
                       radius = hb_simd$mean_admissions, weight = 1)
  })
  
}



# run app -----------------------------------------------------------------

shinyApp(ui = ui, server = server)

