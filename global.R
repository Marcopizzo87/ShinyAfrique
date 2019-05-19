
#### LOAD LIBRARIES ----

library(shiny)
library(leaflet) 
library(tidyverse)
library(dplyr)
library(ggplot2)
library(DT)
library(markdown)
library(sf)

## Set WD automatically to where the R scrip is
# setwd(dirname(rstudioapi::getSourceEditorContext()$path))


#### LOAD DATA ----
df.vil <- readRDS("./data/gaz.drc.50km_wgs84.8.rds")
buf.50 <- readRDS("./data/buffer.50.dis.rds")
buf.20 <- readRDS("./data/buffer.20.dis.rds")


#### LOAD COLORS ----
pal.vil <- colorFactor(c("#FF5733","#FFC30F"), domain = c("20km","50km"))

#### LOAD FIXED THEME FOR THE GRAPH ----

graph.theme <-   theme(
  # main title
  plot.title = element_text(face = "bold", colour = "#363636" , size = 12),
  # x axis title 
  axis.title.x = element_text(face = "plain", colour = "#6E6E6E", size = 12),
  # y axis title
  axis.title.y = element_text(face = "plain", colour = "#6E6E6E", size = 12),
  # no border
  panel.border = element_blank(),
  # just the axes
  axis.line = element_line(size=0.4, colour = "#6E6E6E"),
  # internal grid
  panel.grid.minor.x = element_line(colour='grey94'),
  panel.grid.minor.y = element_line(colour='grey94'),
  panel.grid.major.x = element_line(colour='lightgrey'),
  panel.grid.major.y = element_line(colour='lightgrey'),
  panel.background = element_rect(fill = "#f5f4f4"),
  plot.background = element_rect(colour = "#f5f4f4", fill = "#f5f4f4"))


##-- END OF GLOBAL SCRIPT --##