#### LIBRARIES ----

#install.packages("shiny")
#install.packages("leaflet")

library(shiny) # load shiny library
library(leaflet) # load leaflet library
library(tidyverse)


df.vil <- readRDS("./data/village_data.rds")

df.liv <- readRDS("./data/livelihood_data.rds")
