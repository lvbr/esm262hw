#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(tidyverse)
library(RColorBrewer)
# Define UI for application that draws a histogram
ui <- fluidPage(
  sidebarLayout( sidebarPanel(p("This is a shiny app for our 5th homework. Authors: Lina Barbosa & Ignacia Rivera."),
                              p("This app visualizes the gazetter data"), 
                              selectizeInput(inputId = "Feature",
                                                 label ="Select Feature(s)",
                                                 choices = c("Airport","Arch","Area","Arroyo","Bar","Basin","Bay","Beach","Bench","Bend","Bridge","Building","Canal","Cape","Cemetery","Census","Channel","Church","Civil","Cliff","Crater","Crossing","Dam","Falls","Flat","Forest","Gap","Glacier","Gut","Harbor","Hospital","Island","Isthmus","Lake","Lava","Levee","Locale","Military","Mine","Oilfield","Park","Pillar","Plain","PopulatedPlace","PostOffice","Range","Rapids","Reserve","Reservoir","Ridge","School","Sea","Slope","Spring","Stream","Summit","Swamp","Tower","Trail","Tunnel","Valley","Well","Woods"),
                                             selected = "Airport",
                                          multiple = T)),
                 mainPanel(
                   leafletOutput(outputId = "Map")
                 ) )
)

# Define server logic 
server <- function(input, output) {
  load("./www/gaz.rda")
  
  gaz_subset <- reactive({
   gaz %>%
      filter(feature_class %in% input$Feature) %>% 
      select(lng = prim_long_dec, lat = prim_lat_dec, county = county_name, name = feature_name)
  })
  
  colorpal <- reactive({
    colorFactor(palette = "RdYlBu", gaz$county_name)
  })
  
  output$Map <- renderLeaflet({
    
    pal <- colorpal()
    
    leaflet(data = gaz_subset()) %>% 
      addProviderTiles(providers$OpenStreetMap.France,
                       options = providerTileOptions(noWrap = TRUE)) %>% 
      addCircleMarkers(radius = 2, fillColor = ~pal(county), color = NULL, fillOpacity = 1, label = ~paste(name)) %>% 
      addMiniMap()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

