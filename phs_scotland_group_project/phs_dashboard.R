
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

all_years <- waiting_times %>% 
  distinct(year) %>% 
  arrange(year) %>% 
  pull()



# ui ----------------------------------------------------------------------

ui <- navbarPage(
  
  theme = bs_theme(bootswatch = "united"),
  
  title = "Public Health Scotland Dashboard Project",
  
  position = "static-top",
  
  fluid = TRUE,
  
  tabPanel("A&E Overview",
           
           fluidRow(
             
             column(6,
                    infoBox(value = "96%", title =tags$h3("Waiting Time"), subtitle = NULL, icon = icon(name = "heart", lib = "font-awesome"))     
             ),
             
             column(6,
                    selectInput(inputId = "year_choice",
                                label = tags$h3("Year"),
                                choices = all_years)
             )
           ),
           
           fluidRow(
             
             box(
               title = "A&E LINE GRAPH",
               status = "primary",
               plotOutput("ae_wait_times")
             ),
             
             box(
               title = "SCOTLAND MAP",
               status = "warning",
               plotOutput("ae_map")
             )
           )
           
           
  ),
  
  
  tabPanel("Winter Crisis",
           
           
           
           ),
  
  
  tabPanel("Impact of COVID-19",
           
           
           
           ),
  
  
  tabPanel("Age Group",
           
           
           
           ),
  
  
  tabPanel("Sex",
           
           
           
           ),
  
  
  tabPanel("SIMD",
           
           
           
           ),
  
  
  tabPanel("Geospatial Maps",
           
           
           
           ),
  
  
  tabPanel("Data",
           
           
           
           )
  
  
)



# server ------------------------------------------------------------------

server <- function(input, output) {
  
  
  
}



# run app -----------------------------------------------------------------

shinyApp(ui = ui, server = server)

