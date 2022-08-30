
# read in packages --------------------------------------------------------

library(tidyverse)
library(shiny)
library(bslib)
library(shinydashboard)
library(shinyWidgets)
library(lubridate)

# data wrangling ----------------------------------------------------------

ae_wait_times <- read_csv("raw_data/non_covid_raw_data/monthly_ae_waitingtimes_202206.csv") %>% janitor::clean_names()

ae_wait_times <- ae_wait_times %>% 
  mutate(date = ym(month),
         year = year(date))

all_years <- ae_wait_times %>% 
  distinct(year) %>% 
  arrange(year) %>% 
  pull()

all_discharges = c("Admission to Same Facility", "Other Speciality", "Transfer to Residence", "Transfer to other NHS Facility", "Unknown")

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
               title = NULL,
               status = "primary",
               solidHeader = FALSE,
               valueBoxOutput(outputId = "percent_ae",
                              width = 6),
               selectInput(inputId = "date_choice",
                           label = "Date",
                           choices = c("2020", "2021"))
             ),
             
             box(
               title = NULL,
               status = "warning",
               solidHeader = FALSE,
               selectInput(inputId = "year_choice",
                           label = tags$h3("Year"),
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
                 width = 8,
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
                 width = 8,
                 plotOutput("covid_plot")
               )
             )
           )
  ),
  
  
  tabPanel(tags$h5("Age Group"),
           
           
           
  ),
  
  
  tabPanel(tags$h5("Sex"),
           
           
           
  ),
  
  
  tabPanel(tags$h5("SIMD"),
           
           
           
  ),
  
  
  tabPanel(tags$h5("Geospatial Maps"),
           
           
           
  ),
  
  
  tabPanel(tags$h5("Data"),
           
           
           
  )
  
  
)



# server ------------------------------------------------------------------

server <- function(input, output) {
  
  output$percent_ae <- renderValueBox({
    valueBox(
      paste0(96, "%"), "Waiting Time % Less Than 4 Hours", color = "aqua", icon = icon("hospital"), width = 6
    )
  })
  
}



# run app -----------------------------------------------------------------

shinyApp(ui = ui, server = server)

