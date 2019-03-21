ui <- fluidPage(
  selectInput("village.name",
              "Village name: ",
              list("DRC" = unique(subset(df.vil, LOCALITY_6=="DRC")$PK),
                   "CAM" = sort(unique(subset(df.vil, LOCALITY_6=="CAM")$PK))),
              #unique(df.vil$PK),
              selected = " ",
              multiple = FALSE,
              selectize = TRUE,
              width = NULL,
              size = NULL),
  
  
  leafletOutput("mymap",height = 1000)
)