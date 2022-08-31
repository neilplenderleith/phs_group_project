
# read in packages --------------------------------------------------------

library(tidyverse)
library(shiny)
library(bslib)
library(shinydashboard)
library(shinyWidgets)
library(lubridate)
library(plotly)


# data wrangling ----------------------------------------------------------

waiting_times <- read_csv("raw_data/non_covid_raw_data/monthly_ae_waitingtimes_202206.csv") %>% janitor::clean_names()


waiting_times <- waiting_times %>% 
  mutate(date = ym(month),
         quarter = quarter(date),
         year = year(date))

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

all_healthboards = c("All Scotland", "Glasgow")


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
                 
                 plotOutput("winter_plot")
               )
             )
           )
  ),
  
  
  tabPanel(tags$h5("Impact of COVID-19"),
           
           sidebarLayout(
             
             sidebarPanel = sidebarPanel(
               
               width = 4,
               
               titlePanel(tags$h1("A&E Admissions Plot Controls")),
               
               radioButtons(
                 inputId = "health_boards",
                 label = tags$h3("Select Health Board"),
                 choices = all_healthboards
               )
             ),
             
             mainPanel = mainPanel(
               
               box(
                 title = "",
                 status = "primary",
                 solidHeader = TRUE,
                 width = 12,
                 height = 600,
                 
                 plotOutput("covid_plot")
               )
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
               )
               
             ),
             
             mainPanel = mainPanel(
               
               box(
                 width = 12,
                 title = tags$h3("Average Hospital Episodes by SIMD Deprivation score"),
                 status = "success",
                 solidHeader = TRUE,
                 height = 600,
                 
                 plotlyOutput("simd_plot")
               )
             )
           )
  ),
  
  
  tabPanel(tags$h5("Geospatial Maps"),
           
           
  ),
  
  
  tabPanel(tags$h5("Data"),
           DT::dataTableOutput("table_output")
  )
)



# server ------------------------------------------------------------------

server <- function(input, output) {
  
  output$percent_ae <- renderValueBox({
    valueBox(
      paste0(96, "%"), "Waiting Time % Less Than 4 Hours", color = "light-blue", icon = icon("hospital"), width = 12
    )
  })
  
  
  filtered_winter_plot <- reactive({
    waiting_times %>%
      filter(discharge_destination == input$discharge_destination,
             !is.na(discharge_proportion),
             year >= input$winter_wait[1] & year <= input$winter_wait[2]) %>% 
      group_by(date) %>% 
      summarise(mean_discharge = mean(discharge_proportion))
  })
  
  output$winter_plot <- renderPlot({
    filtered_winter_plot() %>% 
      ggplot(aes(x = date,
                 y = mean_discharge)) +
      geom_line() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size =9))+
      scale_x_date(date_breaks = "6 months", date_labels =  "%b %Y") +
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
           y = "Proportion")
  }, height = 450)
  
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
  
  filtered_age_plot <- reactive({
    age %>% 
      filter(year >= input$age_year[1] & year <= input$age_year[2],
             age == input$age_groups) %>% 
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
  
}



# run app -----------------------------------------------------------------

shinyApp(ui = ui, server = server)

