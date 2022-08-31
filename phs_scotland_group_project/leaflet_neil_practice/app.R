library(leaflet)
library(sf)
library(shiny)
library(tidyverse)
library(scales)
library(plotly)
library(lubridate)
library(zoo)
library(here)
here()

scotland <- st_read(here("clean_data/shapefile/scotland_smaller.gpkg"))

#transform so leaflet is happy with it
scotland <- st_transform(scotland, '+proj=longlat +datum=WGS84')

pal <- colorNumeric("viridis", NULL) # set colour palette

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),
    sliderInput()

        # Show a plot of the generated distribution
        mainPanel(
          leafletOutput("scotlandmap")
        )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {

  output$scotlandmap <- renderLeaflet({
    m <- leaflet() 
    m %>% addTiles() %>% 
      addPolygons(data=scotland,
                  smoothFactor = 0.3, 
                  fillOpacity = 1,
                  fillColor = ~pal(target_2007),
                  label = ~paste0( HBName,": ", target_2007),
                  weight = 1, 
                  highlightOptions = highlightOptions(color = "white", 
                                                      weight = 2, 
                                                      bringToFront = TRUE)) %>% 
      addLegend(pal = pal, values = scotland$target_2007, opacity = 1)
    
    })

}
# Run the application 
shinyApp(ui = ui, server = server)
