library(shiny)
library(leaflet)
#Load in the data that we gathered
load("TransitTicketsCleanData.RData")

ui <- fluidPage(
  h1("A map of Halifax Transit ticket seller locations"),
  p("Source code can be found at https://github.com/Csislander/HalifaxTransitTicketsMap"),
  numericInput(inputId = "radius", label = "Radius from locations (km)", value = 1, min = 0, max = 5, step = 0.1),
  br(),
  tags$style(type = "text/css", "#TicketMap {height: calc(100vh - 80px) !important;}"),
  leafletOutput("TicketMap")
)

server <- function(input, output) {
   output$TicketMap <- renderLeaflet({
     leaflet(location_tables) %>% 
       addProviderTiles(providers$Stamen.Terrain, 
                        options = providerTileOptions(noWrap = TRUE)) %>%
       addMarkers(~long, 
                  ~lat, 
                  popup = ~location_gmap, 
                  icon = ticketIcon) %>%
       addCircles(~long, 
                  ~lat, 
                  radius = 1000, 
                  group = "Proximity", 
                  fillColor = "grey", 
                  fillOpacity = 0.1, 
                  opacity = 0.6,
                  layerId = location_tables$Location) %>%
       addPolygons(data = halifax_map_shapes, 
                   weight = 1, 
                   smoothFactor = 0.5,
                   opacity = 1.0, 
                   fillOpacity = 0.7,
                   fillColor = ~colorQuantile("Reds", halifax_map_shapes[[23]], 7)(halifax_map_shapes[[23]]),
                   highlightOptions = highlightOptions(color = "white", 
                                                       weight = 2,
                                                       bringToFront = FALSE),
                   group = "Population Density") %>%
       addPolygons(data = halifax_map_shapes, 
                   weight = 1, 
                   smoothFactor = 0.5,
                   opacity = 1.0, 
                   fillOpacity = 0.7,
                   fillColor = ~colorQuantile("Reds", halifax_map_shapes[[22]], 7)(halifax_map_shapes[[22]]),
                   highlightOptions = highlightOptions(color = "white", 
                                                       weight = 2,
                                                       bringToFront = FALSE),
                   group = "Total Population") %>%
       addLayersControl(baseGroups = c("None", "Population Density", "Total Population"),
                        overlayGroups = "Proximity",
                        options = layersControlOptions(collapsed = FALSE))
   })
   
   observeEvent(input$radius, {
      leafletProxy("TicketMap") %>%
       removeShape(layerId = location_tables$Location) %>%
       addCircles(data = location_tables, 
                  ~long, 
                  ~lat, 
                  radius = input$radius*1000, 
                  group = "Proximity", 
                  fillColor = "grey", 
                  fillOpacity = 0.1, 
                  opacity = 0.6, 
                  layerId = location_tables$Location)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

