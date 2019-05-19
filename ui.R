
### Define UI for dataset viewer app ----

ui <-

    navbarPage("Shiny Afrique",
               tabPanel("Map",
                        
                        sidebarLayout(
                          ## Sidebar with inputs  and graphs
                          sidebarPanel(
                            ## Names list + search button
                            tags$h4("Select village name:", class = "nopad"),
                            selectInput("village.name",
                                        NULL,
                                        c("All",sort(unique(as.character(df.vil$FULL_NAME_)))),
                                        selected = NULL,
                                        multiple = FALSE,
                                        selectize = TRUE,
                                        width = NULL,
                                        size = NULL),
                            actionButton(inputId = "click.search", label = "Search"),
                            
                            ## Second button to clear the selection
                            actionButton(inputId = "click.all", label = "View all villages"),
                            
                            ## Slider
                            tags$h4("Select distance:", class = "pad"),
                            sliderInput(input = "distance", label = NULL, value = c(0,50), min = 0, max = 50),
                            # tags$br(),
                            
                            tags$h4("Num. of villages VS dist. from rivers:", class = "padplus"),
                            ## Histogram
                            plotOutput("hist"),
                            tags$br(),
                            # plotOutput("scatter"), ## decided not to show this
                            
                            ## Stats table
                            verbatimTextOutput("stats")
                          ), # end of sidebarPanel
                          
                          ### Main panel with the map
                          mainPanel(
                            tags$style(type = "text/css", '#mymap {height: calc(100vh - 80px) !important;}'),
                            leafletOutput("mymap", height = "90vh")
                          ) # end of mainPanel
                        ) # end of sidebarLayout
               ), # end of tabPanel
               
               tabPanel("Data",

                  tags$h3("DataTable"),
                        
                   # Create a new Row in the UI for selectInputs
                   fluidRow(
                     column(1,
                            tags$h4("Villages:", class = "nobold"),
                            selectInput("vill.table",
                                        NULL,
                                        c("All", unique(as.character(df.vil$FULL_NAME_))))
                     ),
                     column(1,
                            tags$h4("Buffer:", class = "nobold"),
                            selectInput("vill.buf",
                                        NULL,
                                        c("All", unique(as.character(df.vil$BUFFER))))
                     ),
                     column(1,
                            tags$h4("Area:", class = "nobold"),
                            selectInput("vill.area",
                                        NULL,
                                        c("All", unique(as.character(df.vil$AREA))))
                     )
                   ),
                   # Create a new row for the table.
                   DT::dataTableOutput("table")
                   
                   
                   
               ), # end of tabPanel
               
               tabPanel("About",
                        fluidPage(
                          fluidRow(includeHTML("about.html")) # end of fluidRow
                        ) # end of fluidPage
                        ), # end of tabPanel
               
               tags$head(
                 tags$link(rel="stylesheet", type="text/css", href="shinystyles.css")) # end of tags$head
    ) # end of navbarpage
    

##-- END OF UI SCRIPT --##