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
library(urca)

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

# Validate function ----
# not_date <- function(input_date, input_agg_model) {
#   dt_1 <- str_split(input_date[1], "-")
#   dt_2 <- str_split(input_date[2], "-")
# 
#   if (input_agg_model == "day" && dt_1[[1]][3] == dt_2[[1]][3]) {
#     "Choose a correct time window and aggregation type."
#   } else if (input_agg_model == "month" && dt_1[[1]][2] == dt_2[[1]][2]) {
#     "Choose a correct time window and aggregation type."
#   } else if (input_agg_model == "quarter" && month(as.period(interval(dt_1[[1]][2], dt_2[[1]][2]))) < 3) {
#     "Choose a correct time window and aggregation type."
#   } else if (input_agg_model == "year" && dt_1[[1]][1] == dt_2[[1]][1]) {
#     "Choose a correct time window and aggregation type."
#   } else {
#     NULL
#   }
# }