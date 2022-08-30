
# read in packages --------------------------------------------------------

library(tidyverse)
library(shiny)
library(bslib)
library(shinydashboard)
library(shinyWidgets)
library(lubridate)

# data wrangling ----------------------------------------------------------

waiting_times <- read_csv("raw_data/non_covid_raw_data/monthly_ae_waitingtimes_202206.csv") %>% janitor::clean_names()

waiting_times <- waiting_times %>% 
  mutate(date = ym(month),
         year = year(date))

waiting_times <- waiting_times %>% 
  mutate(
    "Admission to Same Facility" = (discharge_destination_admission_to_same/number_of_attendances_aggregate)
  ) %>% 
  mutate("Other Speciality" = (discharge_destination_other_specialty/number_of_attendances_aggregate)) %>% 
  mutate("Transfer to Residence" = (discharge_destination_residence/number_of_attendances_aggregate)) %>% 
  mutate("Transfer to other NHS Facility" = (discharge_destination_transfer/number_of_attendances_aggregate)) %>% 
  mutate("Unknown" = (discharge_destination_unknown/number_of_attendances_aggregate))

waiting_times %>% names()

all_years <- waiting_times %>% 
  distinct(year) %>% 
  arrange(year) %>% 
  pull()

all_discharges = c("Admission to Same Facility", "Other Speciality", "Transfer to Residence", "Transfer to other NHS Facility", "Unknown")

all_healthboards = c("All Scotland", "Glasgow")

all_ages = c("0-9 years", "10-19 years", "20-29 years", "30-39 years", "40-49 years", "50-59 years", "60-69 years", "70-79 years", "80-89 years", "90 years and older")

all_sex = c("Female", "Male")

all_simd = c("1", "2", "3", "4", "5")
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
               title = NULL,
               status = "primary",
               solidHeader = FALSE,
               valueBoxOutput(outputId = "percent_ae",
                              width = 12),
               selectInput(inputId = "date_choice",
                           label = tags$h4("Date"),
                           choices = c("2020", "2021")
                           )
             ),
             
             box(
               title = tags$h3("Year of A&E Wait Times"),
               status = "warning",
               solidHeader = TRUE,
               
               selectInput(inputId = "year_choice",
                           label = NULL,
                           choices = all_years)
             )
           ),
           
           fluidRow(
             
             box(
               title = "A&E LINE GRAPH",
               status = "primary",
               solidHeader = TRUE,
               
               plotOutput("ae_wait_times")
             ),
             
             box(
               title = "SCOTLAND MAP",
               status = "warning",
               solidHeader = TRUE,
               
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
                           min = 2016,
                           max = 2022,
                           value = c(2016, 2022),
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
                 title = "WINTER PLOT",
                 status = "primary",
                 solidHeader = TRUE,
                 width = 12,
                 
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
                 title = "COVID PLOT",
                 status = "primary",
                 solidHeader = TRUE,
                 width = 12,
                 
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
                 choices = all_ages
               ),
               
               sliderInput(inputId = "age_year",
                           label = tags$h2("Year range"),
                           min = 2016,
                           max = 2022,
                           value = c(2016, 2022),
                           step = 1,
                           sep = ""
               )
             ),
             
             mainPanel = mainPanel(
               
               box(
                 width = 12,
                 title = "AGE PLOT",
                 status = "success",
                 solidHeader = TRUE,
                 
                 plotOutput("age_plot")
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
                 choices = all_sex
               ),
               
               sliderInput(inputId = "sex_year",
                           label = tags$h2("Year range"),
                           min = 2016,
                           max = 2022,
                           value = c(2016, 2022),
                           step = 1,
                           sep = ""
               )
             ),
             
             mainPanel = mainPanel(
               
               box(
                 width = 12,
                 title = "SEX PLOT",
                 status = "success",
                 solidHeader = TRUE,
                 
                 plotOutput("sex_plot")
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
                 choices = all_simd
               ),
               
               sliderInput(inputId = "simd_year",
                           label = tags$h2("Year range"),
                           min = 2016,
                           max = 2022,
                           value = c(2016, 2022),
                           step = 1,
                           sep = ""
               )
               
             ),
             
             mainPanel = mainPanel(
               
               box(
                 width = 12,
                 title = "SIMD PLOT",
                 status = "success",
                 solidHeader = TRUE,
                 
                 plotOutput("simd_plot")
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
    winter1 <- sym(input$discharge_destination)
    waiting_times %>%
      filter(!is.na(!!winter1)) %>% 
      group_by(date) %>% 
      summarise(mean_discharge = mean(!!winter1))
  })
  
  output$winter_plot <- renderPlot({
    filtered_winter_plot() %>% 
      ggplot(aes(x = date,
                 y = mean_discharge)) +
      geom_line() +
      scale_x_date(date_breaks = "6 months", date_labels =  "%b %Y") +
      geom_vline(xintercept = as.numeric(as.Date("2008-01-01")), linetype=4)+
      geom_vline(xintercept = as.numeric(as.Date("2009-01-01")), linetype=4)+
      geom_vline(xintercept = as.numeric(as.Date("2010-01-01")), linetype=4)+
      geom_vline(xintercept = as.numeric(as.Date("2011-01-01")), linetype=4)+
      geom_vline(xintercept = as.numeric(as.Date("2012-01-01")), linetype=4)+
      geom_vline(xintercept = as.numeric(as.Date("2013-01-01")), linetype=4)+
      geom_vline(xintercept = as.numeric(as.Date("2014-01-01")), linetype=4)+
      geom_vline(xintercept = as.numeric(as.Date("2015-01-01")), linetype=4)+
      geom_vline(xintercept = as.numeric(as.Date("2016-01-01")), linetype=4)+
      geom_vline(xintercept = as.numeric(as.Date("2017-01-01")), linetype=4)+
      geom_vline(xintercept = as.numeric(as.Date("2018-01-01")), linetype=4)+
      geom_vline(xintercept = as.numeric(as.Date("2019-01-01")), linetype=4)+
      geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype=4)+
      geom_vline(xintercept = as.numeric(as.Date("2021-01-01")), linetype=4)+
      geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype=4)
  })
  
}



# run app -----------------------------------------------------------------

shinyApp(ui = ui, server = server)

