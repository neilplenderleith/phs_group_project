
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


# ui ----------------------------------------------------------------------

ui <- navbarPage(
  
  theme = bs_theme(bootswatch = "flatly"),
  
  title = "Public Health Scotland Dashboard Project",
  
  header = tagList(
    useShinydashboard()
  ),
  
  position = "static-top",
  
  fluid = TRUE,
  
  tabPanel("A&E Overview",
           
           
           
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
  
  
  tabPanel("Winter Crisis",
           
           sidebarLayout(
             
             sidebarPanel = sidebarPanel(
               box(
                 title = "Plot Comparison Controls",
                 status = "primary",
                 solidHeader = TRUE,
                 dateRangeInput(inputId = "winter_wait",
                                label = "Date range",
                                start = as_date("2007-04-01"),
                                end = as_date("2022-10-01"),
                                min = as_date("2007-04-01"),
                                max = as_date("2022-10-01"))
               )
             ),
             
             mainPanel = mainPanel(
               
               plotOutput("winter_plot")
             )
           )
           
           
           
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
  
  output$percent_ae <- renderValueBox({
    valueBox(
      paste0(96, "%"), "Waiting Time % Less Than 4 Hours", color = "aqua", icon = icon("hospital"), width = 6
    )
  })
  
}



# run app -----------------------------------------------------------------

shinyApp(ui = ui, server = server)

