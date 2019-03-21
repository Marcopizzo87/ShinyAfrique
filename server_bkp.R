server <- function(input, output, session) {
 
  
  # Data color palette that maps factor levels to colors
  pal.liv <- colorFactor(c("Camping","Farming","Fishing","Gathering","Hunting","Other"), domain = c("#267300","#c8c52e","#15648e","#40c40c","#e3e07a","#1f92df"))  
   
  # reactive object that allows to filter istantaneously the data. (not used right now)
  data <- reactive({
    x <- df.vil
  })
  
  
  output$mymap <- renderLeaflet({
    df.vil <- data()
    
    m <- leaflet(data = df.vil) %>%
      # addTiles() %>% # Add default OpenStreetMap map tiles
      addProviderTiles(providers$CartoDB.Positron) %>% # Add specific map style
      #addMarkers(lng = ~Longitude,
      #           lat = ~Latitude,
      #           popup = paste("Village: ", df$PK, "<br>",
      #                         "Year: ", df$YEAR, "<br>",
      #                         "Description: ", df$DESCRIPT))
      addCircleMarkers(lng = ~Longitude,
                      lat = ~Latitude,
                      radius = 5,
                      color = "#ca422c", # Hex color
                      stroke = FALSE,
                      fillOpacity = 0.7,
                      popup = paste("Village: ", df.vil$PK, "<br>",
                                    "Year: ", df.vil$YEAR, "<br>",
                                    "Description: ", df.vil$DESCRIPT),
                      label = df$PK,
                      labelOptions = labelOptions(noHide = F,
                                                  textsize = "10px",
                                                  textOnly = TRUE,
                                                  style = list("font-style" = "italic")))
    
#      addCircleMarkers(data = df.liv,
#                       lng = ~Longitude,
#                       lat = ~Latitude,
#                       radius = 5,
#                       color = ~pal.liv(TYPE), # Color pallette according to the type
#                       stroke = FALSE,
#                       fillOpacity = 0.7,
#                       popup = NULL,
#                       label = NULL)

    m # Print the map
  })
  
}