load('dfWaterFlow.RData')
# load('df_population.RData')
# country_code <- read.csv("country_code.csv")

library(dplyr)
library(tidyr)
library(plotly)
library(shiny)
library(DT)
library(shinyBS)
# install.packages("leaflet")
# install.packages("rgdal")
# to install the development version from Github, run
# devtools::install_github("rstudio/leaflet")

# # Create color palette
# cus.pal <- RColorBrewer::brewer.pal(5, "Set1")
#
# # Color palette for leaflet map based on cus.pal object
# pal.major <- leaflet::colorFactor(cus.pal, domain = major.cats)
#
# # Read geojson with world country data
# shp <- rgdal::readOGR("layers/countries.geojson", "OGRGeoJSON")
# shp@data$name <- as.character(shp@data$name)
#
# # Values for selectize input
# countries <- shp@data$name
# ISO3 <- shp@data$sov_a3
