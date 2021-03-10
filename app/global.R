# Packages -----
library(tidyverse)
library(leaflet)
library(shiny)
library(shinythemes)
library(rsconnect)
library(shinyWidgets)
library(leaflet.extras)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyEffects)
library(shinyjqui)
library(highcharter)
library(shinyAce)
library(shinyjs)
library(shinycssloaders)
library(tsibble)
library(fable)
library(feasts)
library(lubridate)
library(DT)

# Data -----
NOAA_data <- read_csv("data/jfk_weather_cleaned.csv", 
                 col_types = cols(DATE = col_datetime(format = "%Y-%m-%d %H:%M:%S"))) %>% 
  transmute("station" = "JFK",
            "time_hour" = DATE,
            "visibility" = HOURLYVISIBILITY * 1.60934,
            "temperature" = (HOURLYDRYBULBTEMPF - 32) * (5/9),
            "wind_speed" = HOURLYWindSpeed * 1.60934,
            "relative_humidity" = HOURLYRelativeHumidity) %>% 
  as_tsibble(key = station)