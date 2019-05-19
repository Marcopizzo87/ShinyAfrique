
### Define SERVER functions ----

server <- function(input, output, session) {
  

    # reactive object that allows to filter istantaneously the data. (not used right now)
  # data1 <- reactive({
  #   x <- df.vil[df.vil$minDist <= input$distance, ]
  # })

  ## Reactive objects recalculated every time inputs change
  data1 <-  reactiveValues(x = df.vil)
            observeEvent(input$click.search, {
            temp <- df.vil[df.vil$minDist >= input$distance[1] & df.vil$minDist <= input$distance[2], ]
            data1$x <- temp[temp$FULL_NAME_ == input$village.name, ]
            })
            observeEvent(input$click.all, {
              data1$x <- df.vil[df.vil$minDist <= input$distance, ]
            })
            observeEvent(input$distance, {
              data1$x <- df.vil[df.vil$minDist >= input$distance[1] & df.vil$minDist <= input$distance[2], ]
            })
  
  ## Reactive expression that returns the points in the view
  data2 <- reactive({
    if (is.null(input$mymap_bounds))
      return(df.vil[FALSE,])
    bounds <- input$mymap_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    subset(data1$x,
          LAT >= latRng[1] & LAT <= latRng[2] &
          LONG >= lngRng[1] & LONG <= lngRng[2])
  })
    

  ## Histogram 1
  output$hist <- renderPlot({
    # If there are no villages in the view don't plot
    if (nrow(data2()) == 0)
      return(NULL)
    

    # Scatterplot population VS distance
    ggplot(data2(), aes(x=data2()$minDist)) + 
      #geom_histogram(aes(y = ..count..), binwidth = 4.5,
      geom_histogram(breaks=seq(-5, 50, by=5), colour = "#f5f4f4", fill = "blue", alpha=0.2) +
      scale_x_continuous(breaks = seq(0, 50, 10), limits=c(0, 50)) +
      scale_y_continuous(labels = scales::comma, limits=c(0, 700)) +
       theme_bw() +
      # axis labels
      # labs(title="Number of villages VS distance from rivers \n") +
      labs(x ="Distance in kilometers", y = "Village count") +
      # fixed theme between graphs
      graph.theme
  }) # Closure of the histogram
  
  ## Histogram 2 - Scatterplot population VS distance not displayed
    # output$scatter <- renderPlot({
    #   # df.0 <- data1$x
    #   # If there are no villages in the view don't plot
    #   if (nrow(data2()) == 0)
    #     return(NULL)
    #   
    #   # Scatterplot population VS distance ghsl
    #   ggplot(data2(), aes(x=data2()$minDist, y=data2()$ghsl_pop_avg)) + 
    #     geom_point(shape = 16, color="blue", alpha=0.2, size=2) + 
    #     scale_y_continuous(labels = scales::comma) +
    #     theme_bw() +
    #     # axis labels
    #     labs(title="Population VS distance from rivers \n", x ="Distance in kilometers", y = "Village pop") +
    #     graph.theme
    # }) # Closure scatterplot  
    # 
  
  ## Statistics  
  output$stats <- renderPrint({
    stat.1 <- data2() %>% aggregate(cbind(ghsl_pop_avg,fb_pop_avg) ~ BUFFER, ., sum) %>% rename(., Buffer = BUFFER, GHSL.pop = ghsl_pop_avg, FB.pop = fb_pop_avg)
    stat.2 <- data2() %>% aggregate(cbind(ghsl_pop_avg) ~ BUFFER, ., length) %>% rename(., count.vil = ghsl_pop_avg) %>% select(count.vil)
    cbind(stat.1,stat.2) %>% mutate(GHSL.pop.vil = as.integer(GHSL.pop/count.vil), FB.pop.vil = as.integer(FB.pop/count.vil))
  })
  
  ## Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- df.vil
    if (input$vill.table != "All") {
      data <- df.vil[df.vil$FULL_NAME_ == input$vill.table, ]
    }
    if (input$vill.buf != "All") {
      data <- df.vil[df.vil$BUFFER == input$vill.buf, ]
    }
    if (input$vill.area != "All") {
      data <- df.vil[df.vil$AREA == input$vill.area, ]
    }
    data
  },
  options = list(lengthMenu = list(c(100, 500, 1000, 2000, 3000, -1), c('100', '500', '1000', '2000', '3000', 'All')), pageLength = 500))
  )
  
  
  ## Leaflet map - set view
  output$mmyap <- renderLeaflet({
    leaflet() %>% addTiles() %>% setView(42, 16, 4)
  })
  
  ## Leaflet map set view 
  output$mymap <- renderLeaflet({
    df.1 <- data1$x #isolate(data1$x) # data1()
    df.2 <- buf.20
    df.3 <- buf.50

    
    m <- leaflet() %>%
      addProviderTiles(providers$OpenStreetMap.HOT) %>% # Add specific map style: https://leaflet-extras.github.io/leaflet-providers/preview/
      ## Add circle Markers
      addCircleMarkers(data = df.1,
                      radius = 4,
                      color = ~pal.vil(BUFFER), # Color pallette according to the type
                      stroke = FALSE,
                      fillOpacity = 0.4,
                      popup = paste("<b>Village: </b>", df.1$FULL_NAME_, "<br>",
                                    "<b>Territoire: </b>", df.1$nom_terri, "<br>",
                                    "<b>Secteur: </b>", df.1$nom_sec, "<br>","<br>",
                                    "<b>GHSL pop: </b>", df.1$ghsl_pop_avg, "<br>",
                                    "<b>FBOOK pop: </b>", df.1$fb_pop_avg, "<br>"),
                      popupOptions = popupOptions(closeButton = F),
                      label = df.1$FULL_NAME_,
                      labelOptions = labelOptions(noHide = F,
                                                  textsize = "10px",
                                                  offset=c(10,-12),
                                                  textOnly = TRUE,
                                                  style = list(
                                                    "color" = "#4A4A4A",
                                                    "font-family" = "Segoe UI",
                                                    "font-style" = "bold",
                                                    "font-size" = "14px"
                                                  ))) %>%
      
      ## Add buffer 1
      addPolylines(data = df.2,
                     stroke = TRUE,
                     color = "#257AA6",
                     weight = 1.2,
                     opacity = 0.5,
                     popup = NULL) %>%

      ## Add buffer 2  
      addPolylines(data = df.3,
                   stroke = TRUE,
                   color = "#621E81",
                   weight = 1.2,
                   opacity = 0.5,
                   popup = NULL)
    
      ## Print the map
      m 
  })
}

##-- END OF SERVER SCRIPT --##