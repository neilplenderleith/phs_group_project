library(leaflet)
library(sf)
library(shiny)
library(tidyverse)
library(scales)
library(plotly)
library(lubridate)
library(here)
here()

#scotland$HBCode[7]

scotland <- st_read(here("clean_data/shapefile/scotland_smaller.gpkg"))

#transform so leaflet is happy with it
scotland <- st_transform(scotland, '+proj=longlat +datum=WGS84')

#rename columns to allow easier shiny running
scotland <- scotland %>%
  dplyr::rename(`2007` = target_2007,
                `2008` = target_2008,
                `2009` = target_2009,
                `2010` = target_2010,
                `2011` = target_2011,
                `2012` = target_2012,
                `2013` = target_2013,
                `2014` = target_2014,
                `2015` = target_2015,
                `2016` = target_2016,
                `2017` = target_2017,
                `2018` = target_2018,
                `2019` = target_2019,
                `2020` = target_2020,
                `2021` = target_2021
  )
#pivot longer to allow filtering for user input
scotland <- scotland %>% 
  pivot_longer(cols = "2007":"2021", names_to = "target_years", values_to = "percentage")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("A&E Departments Meeting 4hr Target"),
  sliderInput(inputId = "leaflet_year_slider",
              label = "Please Select Year",
              min = 2007, max = 2021, sep = "",value = 2007),
  
  
  # Show a plot of the generated distribution
  mainPanel(
    leafletOutput("scotlandmap", height = "100vh", width = "100vw")
    
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #reactive to filter for user input 
  scotland_filtered <- reactive({
    scotland %>%
      filter(target_years == input$leaflet_year_slider)
    
  })
  # palette
  pal <- colorNumeric(
    palette = "viridis",
    domain = scotland$percentage
  )
  
  
  output$scotlandmap <- renderLeaflet({
    m <- leaflet() 
    m %>% addTiles() %>% 
      addPolygons(data=scotland_filtered(),
                  smoothFactor = 0.3, 
                  fillOpacity = 1,
                  fillColor = ~pal(percentage),#fill by percentage of 4hr target
                  label = ~paste0(HBName,"<br>", 
                                  percentage, "%", "<br>", 
                                  target_years) %>% lapply(htmltools::HTML),
                  weight = 1, 
                  highlightOptions = highlightOptions(color = "white", 
                                                      weight = 2, 
                                                      bringToFront = TRUE)) %>%
      setView(-4, 57, zoom = 5.5) %>% 
      addLegend(pal = pal, 
                values = scotland$percentage, 
                opacity = 0.5, 
                title = "% A&E meeting<br> 4hr target") 
    
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)
