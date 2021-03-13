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
  
  ######### Panel 2 ######### 
  
  # Filtering by ts input model -----
  selectTimeSeriesModelData <- eventReactive(input$action_model, {
    if (input$aggregation_model == "day") {
      if (input$seasonal_step == TRUE) {
        fit <- NOAA_data %>%
          filter(as.Date(time_hour) >= input$dt_model[1], as.Date(time_hour) <= input$dt_model[2]) %>%
          group_by_key() %>%
          index_by(date = ~ as_date(.)) %>% 
          summarise(
            temperature = mean(temperature, na.rm = TRUE),
            visibility = mean(visibility, na.rm = TRUE),
            wind_speed = mean(wind_speed, na.rm = TRUE),
            relative_humidity = mean(relative_humidity, na.rm = TRUE)
          ) %>%  
          gather("variable", "series", -c(date, station)) %>%
          filter(variable == input$series_model) %>% 
          model(
            rw = ARIMA(series ~ pdq(p = 1, d = 0, q = 0) + PDQ(0, 0, 0)),
            ar = ARIMA(series ~ pdq(p = 0:12, d = 0:12, q = 0) + PDQ(0, 0, 0)),
            ma = ARIMA(series ~ pdq(p = 0, d = 0:12, q = 0:12) + PDQ(0, 0, 0)),
            arma = ARIMA(series ~ pdq(p = 0:12, d = 0, q = 0:12) + PDQ(0, 0, 0)),
            arima = ARIMA(series ~ pdq(p = 0:12, d = 0:12, q = 0:12) + PDQ(0, 0, 0)),
            sarima = ARIMA(series ~ pdq(p = 0:12, d = 0:12, q = 0:12) + PDQ(0:24, 0:24, 0:24)))
      } else {
        fit <- NOAA_data %>%
          filter(as.Date(time_hour) >= input$dt_model[1], as.Date(time_hour) <= input$dt_model[2]) %>%
          group_by_key() %>%
          index_by(date = ~ as_date(.)) %>% 
          summarise(
            temperature = mean(temperature, na.rm = TRUE),
            visibility = mean(visibility, na.rm = TRUE),
            wind_speed = mean(wind_speed, na.rm = TRUE),
            relative_humidity = mean(relative_humidity, na.rm = TRUE)
          ) %>%  
          gather("variable", "series", -c(date, station)) %>%
          filter(variable == input$series_model) %>% 
          model(
            rw = ARIMA(series ~ pdq(p = 1, d = 0, q = 0) + PDQ(0, 0, 0)),
            ar = ARIMA(series ~ pdq(p = 0:12, d = 0:12, q = 0) + PDQ(0, 0, 0)),
            ma = ARIMA(series ~ pdq(p = 0, d = 0:12, q = 0:12) + PDQ(0, 0, 0)),
            arma = ARIMA(series ~ pdq(p = 0:12, d = 0, q = 0:12) + PDQ(0, 0, 0)),
            arima = ARIMA(series ~ pdq(p = 0:12, d = 0:12, q = 0:12) + PDQ(0, 0, 0)))
      }
        
      data <- NOAA_data %>%
        filter(as.Date(time_hour) >= input$dt_model[1], as.Date(time_hour) <= input$dt_model[2]) %>%
        group_by_key() %>%
        index_by(date = ~ as_date(.)) %>%
        summarise(
          temperature = mean(temperature, na.rm = TRUE),
          visibility = mean(visibility, na.rm = TRUE),
          wind_speed = mean(wind_speed, na.rm = TRUE),
          relative_humidity = mean(relative_humidity, na.rm = TRUE)
        ) %>%
        gather("variable", "series", -c(date, station)) %>%
        filter(variable == input$series_model)
      
      return(
        list(
          Fit = fit,
          Data = data
        )
      )
     } else if (input$aggregation_model == "month") {
       if (input$seasonal_step == TRUE) {
         fit <- NOAA_data %>%
           filter(as.Date(time_hour) >= input$dt_model[1], as.Date(time_hour) <= input$dt_model[2]) %>%
           group_by_key() %>%
           index_by(date = ~ yearmonth(.)) %>% 
           summarise(
             temperature = mean(temperature, na.rm = TRUE),
             visibility = mean(visibility, na.rm = TRUE),
             wind_speed = mean(wind_speed, na.rm = TRUE),
             relative_humidity = mean(relative_humidity, na.rm = TRUE)
           ) %>%  
           gather("variable", "series", -c(date, station)) %>%
           filter(variable == input$series_model) %>% 
           model(
             rw = ARIMA(series ~ pdq(p = 1, d = 0, q = 0) + PDQ(0, 0, 0)),
             ar = ARIMA(series ~ pdq(p = 0:12, d = 0:12, q = 0) + PDQ(0, 0, 0)),
             ma = ARIMA(series ~ pdq(p = 0, d = 0:12, q = 0:12) + PDQ(0, 0, 0)),
             arma = ARIMA(series ~ pdq(p = 0:12, d = 0, q = 0:12) + PDQ(0, 0, 0)),
             arima = ARIMA(series ~ pdq(p = 0:12, d = 0:12, q = 0:12) + PDQ(0, 0, 0)),
             sarima = ARIMA(series ~ pdq(p = 0:12, d = 0:12, q = 0:12) + PDQ(0:24, 0:24, 0:24)))
       } else {
         fit <- NOAA_data %>%
           filter(as.Date(time_hour) >= input$dt_model[1], as.Date(time_hour) <= input$dt_model[2]) %>%
           group_by_key() %>%
           index_by(date = ~ yearmonth(.)) %>% 
           summarise(
             temperature = mean(temperature, na.rm = TRUE),
             visibility = mean(visibility, na.rm = TRUE),
             wind_speed = mean(wind_speed, na.rm = TRUE),
             relative_humidity = mean(relative_humidity, na.rm = TRUE)
           ) %>%  
           gather("variable", "series", -c(date, station)) %>%
           filter(variable == input$series_model) %>% 
           model(
             rw = ARIMA(series ~ pdq(p = 1, d = 0, q = 0) + PDQ(0, 0, 0)),
             ar = ARIMA(series ~ pdq(p = 0:12, d = 0:12, q = 0) + PDQ(0, 0, 0)),
             ma = ARIMA(series ~ pdq(p = 0, d = 0:12, q = 0:12) + PDQ(0, 0, 0)),
             arma = ARIMA(series ~ pdq(p = 0:12, d = 0, q = 0:12) + PDQ(0, 0, 0)),
             arima = ARIMA(series ~ pdq(p = 0:12, d = 0:12, q = 0:12) + PDQ(0, 0, 0)))
       }

      data <- NOAA_data %>%
        filter(as.Date(time_hour) >= input$dt_model[1], as.Date(time_hour) <= input$dt_model[2]) %>%
        group_by_key() %>%
        index_by(date = ~ yearmonth(.)) %>%
        summarise(
          temperature = mean(temperature, na.rm = TRUE),
          visibility = mean(visibility, na.rm = TRUE),
          wind_speed = mean(wind_speed, na.rm = TRUE),
          relative_humidity = mean(relative_humidity, na.rm = TRUE)
        ) %>%
        gather("variable", "series", -c(date, station)) %>%
        filter(variable == input$series_model)

      return(
        list(
          Fit = fit,
          Data = data
        )
      )
    } else if (input$aggregation_model == "quarter") {
      if (input$seasonal_step == TRUE) {
        fit <- NOAA_data %>%
          filter(as.Date(time_hour) >= input$dt_model[1], as.Date(time_hour) <= input$dt_model[2]) %>%
          group_by_key() %>%
          index_by(date = ~ yearquarter(.)) %>% 
          summarise(
            temperature = mean(temperature, na.rm = TRUE),
            visibility = mean(visibility, na.rm = TRUE),
            wind_speed = mean(wind_speed, na.rm = TRUE),
            relative_humidity = mean(relative_humidity, na.rm = TRUE)
          ) %>%  
          gather("variable", "series", -c(date, station)) %>%
          filter(variable == input$series_model) %>% 
          model(
            rw = ARIMA(series ~ pdq(p = 1, d = 0, q = 0) + PDQ(0, 0, 0)),
            ar = ARIMA(series ~ pdq(p = 0:12, d = 0:12, q = 0) + PDQ(0, 0, 0)),
            ma = ARIMA(series ~ pdq(p = 0, d = 0:12, q = 0:12) + PDQ(0, 0, 0)),
            arma = ARIMA(series ~ pdq(p = 0:12, d = 0, q = 0:12) + PDQ(0, 0, 0)),
            arima = ARIMA(series ~ pdq(p = 0:12, d = 0:12, q = 0:12) + PDQ(0, 0, 0)),
            sarima = ARIMA(series ~ pdq(p = 0:12, d = 0:12, q = 0:12) + PDQ(0:24, 0:24, 0:24)))
      } else {
        fit <- NOAA_data %>%
          filter(as.Date(time_hour) >= input$dt_model[1], as.Date(time_hour) <= input$dt_model[2]) %>%
          group_by_key() %>%
          index_by(date = ~ yearquarter(.)) %>% 
          summarise(
            temperature = mean(temperature, na.rm = TRUE),
            visibility = mean(visibility, na.rm = TRUE),
            wind_speed = mean(wind_speed, na.rm = TRUE),
            relative_humidity = mean(relative_humidity, na.rm = TRUE)
          ) %>%  
          gather("variable", "series", -c(date, station)) %>%
          filter(variable == input$series_model) %>% 
          model(
            rw = ARIMA(series ~ pdq(p = 1, d = 0, q = 0) + PDQ(0, 0, 0)),
            ar = ARIMA(series ~ pdq(p = 0:12, d = 0:12, q = 0) + PDQ(0, 0, 0)),
            ma = ARIMA(series ~ pdq(p = 0, d = 0:12, q = 0:12) + PDQ(0, 0, 0)),
            arma = ARIMA(series ~ pdq(p = 0:12, d = 0, q = 0:12) + PDQ(0, 0, 0)),
            arima = ARIMA(series ~ pdq(p = 0:12, d = 0:12, q = 0:12) + PDQ(0, 0, 0)))
      }

      data <- NOAA_data %>%
        filter(as.Date(time_hour) >= input$dt_model[1], as.Date(time_hour) <= input$dt_model[2]) %>%
        group_by_key() %>%
        index_by(date = ~ yearquarter(.)) %>%
        summarise(
          temperature = mean(temperature, na.rm = TRUE),
          visibility = mean(visibility, na.rm = TRUE),
          wind_speed = mean(wind_speed, na.rm = TRUE),
          relative_humidity = mean(relative_humidity, na.rm = TRUE)
        ) %>%
        gather("variable", "series", -c(date, station)) %>%
        filter(variable == input$series_model)

      return(
        list(
          Fit = fit,
          Data = data
        )
      )
    } else {
      if (input$seasonal_step == TRUE) {
        fit <- NOAA_data %>%
          filter(as.Date(time_hour) >= input$dt_model[1], as.Date(time_hour) <= input$dt_model[2]) %>%
          group_by_key() %>%
          index_by(date = ~ year(.)) %>% 
          summarise(
            temperature = mean(temperature, na.rm = TRUE),
            visibility = mean(visibility, na.rm = TRUE),
            wind_speed = mean(wind_speed, na.rm = TRUE),
            relative_humidity = mean(relative_humidity, na.rm = TRUE)
          ) %>%  
          gather("variable", "series", -c(date, station)) %>%
          filter(variable == input$series_model) %>% 
          model(
            rw = ARIMA(series ~ pdq(p = 1, d = 0, q = 0) + PDQ(0, 0, 0)),
            ar = ARIMA(series ~ pdq(p = 0:12, d = 0:12, q = 0) + PDQ(0, 0, 0)),
            ma = ARIMA(series ~ pdq(p = 0, d = 0:12, q = 0:12) + PDQ(0, 0, 0)),
            arma = ARIMA(series ~ pdq(p = 0:12, d = 0, q = 0:12) + PDQ(0, 0, 0)),
            arima = ARIMA(series ~ pdq(p = 0:12, d = 0:12, q = 0:12) + PDQ(0, 0, 0)),
            sarima = ARIMA(series ~ pdq(p = 0:12, d = 0:12, q = 0:12) + PDQ(0:24, 0:24, 0:24)))
      } else {
        fit <- NOAA_data %>%
          filter(as.Date(time_hour) >= input$dt_model[1], as.Date(time_hour) <= input$dt_model[2]) %>%
          group_by_key() %>%
          index_by(date = ~ year(.)) %>% 
          summarise(
            temperature = mean(temperature, na.rm = TRUE),
            visibility = mean(visibility, na.rm = TRUE),
            wind_speed = mean(wind_speed, na.rm = TRUE),
            relative_humidity = mean(relative_humidity, na.rm = TRUE)
          ) %>%  
          gather("variable", "series", -c(date, station)) %>%
          filter(variable == input$series_model) %>% 
          model(
            rw = ARIMA(series ~ pdq(p = 1, d = 0, q = 0) + PDQ(0, 0, 0)),
            ar = ARIMA(series ~ pdq(p = 0:12, d = 0:12, q = 0) + PDQ(0, 0, 0)),
            ma = ARIMA(series ~ pdq(p = 0, d = 0:12, q = 0:12) + PDQ(0, 0, 0)),
            arma = ARIMA(series ~ pdq(p = 0:12, d = 0, q = 0:12) + PDQ(0, 0, 0)),
            arima = ARIMA(series ~ pdq(p = 0:12, d = 0:12, q = 0:12) + PDQ(0, 0, 0)))
      }

      data <- NOAA_data %>%
        filter(as.Date(time_hour) >= input$dt_model[1], as.Date(time_hour) <= input$dt_model[2]) %>%
        group_by_key() %>%
        index_by(date = ~ year(.)) %>%
        summarise(
          temperature = mean(temperature, na.rm = TRUE),
          visibility = mean(visibility, na.rm = TRUE),
          wind_speed = mean(wind_speed, na.rm = TRUE),
          relative_humidity = mean(relative_humidity, na.rm = TRUE)
        ) %>%
        gather("variable", "series", -c(date, station)) %>%
        filter(variable == input$series_model)

      return(
        list(
          Fit = fit,
          Data = data
        )
      )
    }
    
  })
  
  output$forecastPlot <- renderHighchart({
    SelectData <- selectTimeSeriesModelData()
    fit <- SelectData$Fit
    data <- SelectData$Data

    best_model <- fit %>%
      glance() %>%
      filter(AIC == min(AIC)) %>%
      head(1)
    
    forecast <- fit %>%
      forecast(h = ifelse(input$one_step == "TRUE", 1, input$window_forecast), level = 95) %>%
      filter(.model == best_model$.model) %>%
      hilo(level = 95) %>%
      transmute(station,
                date,
                "Point Forecast" = .mean,
                "Lo 95" = `95%`$lower,
                "Hi 95" = `95%`$upper) %>%
      gather("variable", "series", -c(date, station))
    
    hchart(data, "line", hcaes(x = date, y = series)) %>%
      hc_yAxis(title = list(text = "Value"),
               opposite = FALSE,
               labels = list(format = "{value}")) %>%
      hc_title(text = "Time Series Forecast",
               margin = 24, align = "left",
               style = list(color = "grey", useHTML = TRUE)) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_tooltip(pointFormat = '{point.y:.2f} ') %>%
      hc_colors(c("#61729C", "#ABB9DB")) %>%
      hc_legend(align = "right", verticalAlign = "middle", layout = "vertical") %>%
      hc_add_series(forecast %>% filter(variable == "Point Forecast"),
                    "line",
                    hcaes(x = date, y = series),
                    color = "red",
                    name = "Point Forecast") %>%
      hc_add_series(forecast %>% filter(variable == "Lo 95"),
                    "line",
                    hcaes(x = date, y = series),
                    color = "green",
                    name = "Lower 95") %>%
      hc_add_series(forecast %>% filter(variable == "Hi 95"),
                    "line",
                    hcaes(x = date, y = series),
                    color = "green",
                    name = "Upper 95")
  })
  
  output$residualsPlot <- renderHighchart({
    SelectData <- selectTimeSeriesModelData()
    fit <- SelectData$Fit
    
    best_model <- fit %>%
      glance() %>%
      filter(AIC == min(AIC)) %>%
      head(1)
    
    data <- fit %>% 
      residuals() %>% 
      filter(.model == best_model$.model)
    
    hchart(data, "line", hcaes(x = date, y = .resid)) %>% 
      hc_yAxis(title = list(text = "Value"),
               opposite = FALSE,
               labels = list(format = "{value}")) %>% 
      hc_title(text = "Residuals",
               margin = 24, align = "left",
               style = list(color = "grey", useHTML = TRUE)) %>% 
      hc_xAxis(title = list(text = "")) %>% 
      hc_tooltip(pointFormat = '{point.y:.2f} ') %>% 
      hc_colors(c("#61729C", "#ABB9DB")) %>% 
      hc_legend(align = "right", verticalAlign = "middle", layout = "vertical")  
  })
  
  output$residualsACF <- renderHighchart({
    SelectData <- selectTimeSeriesModelData()
    fit <- SelectData$Fit
    
    best_model <- fit %>%
      glance() %>%
      filter(AIC == min(AIC)) %>%
      head(1)
    
    data <- fit %>% 
      residuals() %>% 
      filter(.model == best_model$.model)
    
    a <- acf(data[, 5])
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
  
  output$qqPlot <- renderHighchart({
    SelectData <- selectTimeSeriesModelData()
    fit <- SelectData$Fit
    
    best_model <- fit %>%
      glance() %>%
      filter(AIC == min(AIC)) %>%
      head(1)
    
    residual <- fit %>% 
      residuals() %>% 
      filter(.model == best_model$.model)
    
    qq <- qqnorm(residual$.resid)
    data <- tibble(x = qq$x, y =  qq$y)
    
    hchart(data,
           "point",
           hcaes(x = x, y = y)) %>% 
      hc_yAxis(title = list(text = "Sample quantiles"),
               opposite = FALSE,
               labels = list(format = "{value}")) %>% 
      hc_title(text = "Normal Q-Q Plot of residuals",
               margin = 24, align = "left",
               style = list(color = "grey", useHTML = TRUE)) %>% 
      hc_xAxis(title = list(text = "Theoretical quantiles")) %>% 
      hc_tooltip(pointFormat = '{point.y:.2f} ') %>% 
      hc_colors(c("#61729C", "#ABB9DB")) %>% 
      hc_legend(align = "right", verticalAlign = "middle", layout = "vertical")  
  })
  
  output$measuresTab <- renderDT({
    SelectData <- selectTimeSeriesModelData()
    fit <- SelectData$Fit
    
    measures_ic <- fit %>% glance()
    measures_fit <- fit %>% accuracy()
    data <- measures_ic %>% 
      inner_join(measures_fit) %>% 
      select(.model, AIC, AICc, BIC, ME, RMSE, MAE, MAPE) %>% 
      mutate(across(2:8, round, 4)) %>% 
      rename(Model = .model)
    
    datatable(data,
              rownames = FALSE,
              extensions = 'Responsive',
              style = 'bootstrap',
              class = 'table-bordered table-condensed',
              options = list(autoWidth = TRUE,
                             searching = FALSE,
                             lengthChange = FALSE,
                             pagingType = 'numbers',
                             dom = 't'))
  })
  
}