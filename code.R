# Title: Getting started to NOAA's database
# Author: Gabriel Teotonio
# Date: 2021/02/10

# Packages -----
library(tidyverse)
library(tsibble)
library(fable)
library(highcharter)
library(lubridate)

# Data ----
data <- read_csv("data/noaa-weather-data-jfk-airport/jfk_weather_cleaned.csv", 
                 col_types = cols(DATE = col_datetime(format = "%Y-%m-%d %H:%M:%S"))) %>% 
  transmute("station" = "JFK",
            "time_hour" = DATE,
            "visibility" = HOURLYVISIBILITY * 1.60934,
            "temperature" = (HOURLYDRYBULBTEMPF - 32) * (5/9),
            "wind_speed" = HOURLYWindSpeed * 1.60934,
            "relative_humidity" = HOURLYRelativeHumidity) %>% 
  as_tsibble(key = station)

# Data pipelines -----
data_min_max <- data %>%
  group_by_key() %>%
  index_by(date = ~ as_date(.)) %>% 
  summarise(
    temp_high = max(temperature, na.rm = TRUE),
    temp_low = min(temperature, na.rm = TRUE)
  ) %>% 
  select(-station)

data_sum <- data %>%
  group_by_key() %>%
  index_by(date = ~ year(.)) %>% 
  summarise(
    temp = mean(temperature, na.rm = TRUE),
    visb = mean(visibility, na.rm = TRUE),
    wind = mean(wind_speed, na.rm = TRUE),
    humi = mean(relative_humidity, na.rm = TRUE)
  ) %>%  
  gather("variable", "series", -c(date, station))

highchart(type = "stock") %>% 
  hc_add_series(data_min_max$temp_high, type = "line") %>% 
  # hc_xAxis(labels = list(format = '{value:%b %d}')) %>% 
  hc_xAxis(type="category", categories=unique(data_min_max$date))

highchart(type = "stock") %>% 
  hc_add_series(as.ts(data_min_max, frequency = 365), type = "line")
#######

hchart(data_sum, "line", hcaes(x = date, y = series, group = variable))

highchart(type = "chart") %>% 
  hc_add_series(data$visibility %>% filter(as.Date(time_hour) == "2010-01-01"), 
                type = "line")

NOAA_data %>%
  filter(as.Date(time_hour) >= "2010-01-01", as.Date(time_hour) <= "2010-01-30") %>% 
  mutate(dt = as.Date(time_hour)) %>% 
  as_tibble() %>% 
  select(visibility, dt) 

data %>%
  group_by_key() %>%
  index_by(date = ~ year(.)) %>% 
  summarise(across(starts_with("visibi"), mean)
  ) %>%  
  gather("variable", "series", -c(date, station))

#######
a <- pacf(NOAA_data$visibility)
df <- tibble(lag = a$lag, value = a$acf)


hchart(
  df, 
  "column",
  hcaes(x = lag, y = value)#,
  #color = c("#7CB5EC", "#F7A35C"),
  #name = c("Year 1999", "Year 2008"),
  #showInLegend = c(TRUE, FALSE) # only show the first one in the legend
) %>% 
  hc_tooltip(pointFormat = '{point.y:.2f} ') %>% 
  hc_yAxis(max = 1,
           plotLines = list(
             list(
               color = "#FF0000",
               width = 2,
               value = 2/sqrt(a$n.used),
               # the zIndex is used to put the label text over the grid lines 
               zIndex = 1
             )
           ),
           plotLines = list(
             list(
               color = "#FF0000",
               width = 2,
               value = -2/sqrt(a$n.used),
               # the zIndex is used to put the label text over the grid lines 
               zIndex = 1
             )
           )
  ) %>% 
  hc_title(text = "Auto-correlogram",
           margin = 24, align = "left",
           style = list(color = "grey", useHTML = TRUE))


NOAA_data %>%
  group_by_key() %>%
  index_by(date = ~ year(.)) %>% 
  summarise(across(starts_with("wind"), mean)) %>% 
  gather("variable", "series", -c(date, station))
