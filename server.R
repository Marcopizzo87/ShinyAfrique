server <- function(input, output, session) {
 
  
  # Data color palette that maps factor levels to colors
  pal.liv <- colorFactor(c("#15648e","#40c40c","#E3E07A"), domain = c("fishing","gathering","hunting"))  
                            
  # reactive object that allows to filter istantaneously the data. (not used right now)
  data1 <- reactive({
    x <- df.vil
  })
  data2 <- reactive({
    x <- df.liv
  })
  
  
  output$mymap <- renderLeaflet({
    df.1 <- data1()
    df.2 <- data2()
    
    m <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>% # Add specific map style
      #addMarkers(lng = ~Longitude,
      #           lat = ~Latitude,
      #           popup = paste("Village: ", df$PK, "<br>",
      #                         "Year: ", df$YEAR, "<br>",
      #                         "Description: ", df$DESCRIPT))
      addCircleMarkers(data = df.1,
                      lng = ~Longitude,
                      lat = ~Latitude,
                      radius = 6,
                      color = "#ca422c", # Hex color
                      stroke = FALSE,
                      fillOpacity = 0.9,
                      popup = paste("Village: ", df.vil$PK, "<br>",
                                    "Year: ", df.vil$YEAR, "<br>",
                                    "Description: ", df.vil$DESCRIPT),
                      label = df.vil$PK,
                      labelOptions = labelOptions(noHide = F,
                                                  textsize = "10px",
                                                  textOnly = TRUE,
                                                  style = list("font-style" = "italic"))) %>% 
      addCircleMarkers(data = df.2,
                       lng = ~Longitude,
                       lat = ~Latitude,
                       radius = 4,
                       color = ~pal.liv(TYPE), # Color pallette according to the type
                       stroke = FALSE,
                       fillOpacity = 0.6,
                       popup = NULL,
                       label = df.2$TYPE)
    
    
        m # Print the map
  })
  
}