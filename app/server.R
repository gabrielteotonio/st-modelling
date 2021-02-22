server <- function(input, output) {
  addClass(selector = "body", class = "sidebar-collapse")
  ######### Panel 1 ######### 
  
  # Filtering by ts input -----
  selectTimeSeriesData <- reactive({
    if (input$aggregation == "day") {
      return(NOAA_data %>%
               filter(as.Date(time_hour) >= input$dt[1], as.Date(time_hour) <= input$dt[2]) %>% 
               group_by_key() %>%
               index_by(date = ~ as_date(.)) %>% 
               summarise(across(starts_with(input$series), mean)) %>% 
               gather("variable", "series", -c(date, station))
      )  
    } else if (input$aggregation == "month") {
        return(NOAA_data %>%
                 filter(as.Date(time_hour) >= input$dt[1], as.Date(time_hour) <= input$dt[2]) %>% 
                 group_by_key() %>%
                 index_by(date = ~ yearmonth(.)) %>% 
                 summarise(across(starts_with(input$series), mean)) %>% 
                 gather("variable", "series", -c(date, station))
        )  
    } else if (input$aggregation == "quarter") {
        return(NOAA_data %>%
                 filter(as.Date(time_hour) >= input$dt[1], as.Date(time_hour) <= input$dt[2]) %>% 
                 group_by_key() %>%
                 index_by(date = ~ yearquarter(.)) %>% 
                 summarise(across(starts_with(input$series), mean)) %>% 
                 gather("variable", "series", -c(date, station))
        )
    } else {
        return(NOAA_data %>%
                 filter(as.Date(time_hour) >= input$dt[1], as.Date(time_hour) <= input$dt[2]) %>% 
                 group_by_key() %>%
                 index_by(date = ~ year(.)) %>% 
                 summarise(across(starts_with(input$series), mean)) %>% 
                 gather("variable", "series", -c(date, station))
        )
    }
    
  })
  
  output$tsPlot <- renderHighchart({
    data <- selectTimeSeriesData()
    hchart(data, "line", hcaes(x = date, y = series)) %>% 
      hc_yAxis(title = list(text = "Value"),
               opposite = FALSE,
               labels = list(format = "{value}")) %>% 
      hc_title(text = "Time Series",
               margin = 24, align = "left",
               style = list(color = "grey", useHTML = TRUE)) %>% 
      hc_xAxis(title = list(text = "")) %>% 
      hc_tooltip(pointFormat = '{point.y:.2f} ') %>% 
      hc_colors(c("#61729C", "#ABB9DB")) %>% 
      hc_legend(align = "right", verticalAlign = "middle", layout = "vertical")  
  })
  
  output$acfPlot <- renderHighchart({
    data <- selectTimeSeriesData()
    a <- acf(data[, 4])
    df <- tibble(lag = a$lag, value = a$acf)
    hchart(df,
           "column",
           hcaes(x = lag, y = value)) %>%
      hc_tooltip(pointFormat = '{point.y:.2f} ') %>%
      hc_yAxis(max = 1,
               plotLines = list(
                 list(
                   color = "#FF0000",
                   width = 2,
                   value = 2/sqrt(a$n.used),
                   zIndex = 1
                 ),
                 list(
                   color = "#FF0000",
                   width = 2,
                   value = -2/sqrt(a$n.used),
                   zIndex = 1
                 )
               )) %>%
      hc_title(text = "Auto-correlogram",
               margin = 24, align = "left",
               style = list(color = "grey", useHTML = TRUE))
  })
  
  output$pacfPlot <- renderHighchart({
    data <- selectTimeSeriesData()
    a <- pacf(data[, 4])
    df <- tibble(lag = a$lag, value = a$acf)
    hchart(df,
           "column",
           hcaes(x = lag, y = value)) %>%
      hc_tooltip(pointFormat = '{point.y:.2f} ') %>%
      hc_yAxis(max = 1,
               plotLines = list(
                 list(
                   color = "#FF0000",
                   width = 2,
                   value = 2/sqrt(a$n.used),
                   zIndex = 1
                 ),
                 list(
                   color = "#FF0000",
                   width = 2,
                   value = -2/sqrt(a$n.used),
                   zIndex = 1
                 )
               )) %>%
      hc_title(text = "Partial Auto-correlogram",
               margin = 24, align = "left",
               style = list(color = "grey", useHTML = TRUE))
  })
  
  output$locationMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -73.88401, 
              lat = 40.63915,
              zoom = 10) %>% 
      addMarkers(lng = -73.76401, 
                 lat = 40.63915, 
                 label = "NOAA Climatological Data Station",
                 labelOptions = labelOptions(interactive = T))
  })
}