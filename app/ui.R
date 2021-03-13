header <- dashboardHeaderPlus(fixed = T,
                              title = div(style="margin-right: 180px",img(src="noaa-logo.png", height = 38, weight = 29), ""),
                              titleWidth = 420,
                              enable_rightsidebar = F,
                              rightSidebarIcon = "bullhorn"
                              
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    tags$style(
      ".main-sidebar {float:top; margin-top:20px; padding-left:0px; padding-right:0px}"
    ),
    menuItem("Exploratory Analysis", tabName = "eda", icon = icon("bar-chart-o")),
    menuItem("Modelling", tabName = "modelling", icon = icon("brain"))
  )
)

body <- dashboardBody(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  tabItems(
    
    # First item ---
    tabItem(tabName = "eda",
            fluidRow(
              column(width = 3,
                     div(h3("Exploratory Analysis"), h5("Data from January 2010 to July 2018"))
              )
            ),
            fluidRow(
              column(width = 3,
                     wellPanel(
                       h4("Filter"),
                       selectInput("series",
                                   "Time series",
                                   choices = colnames(NOAA_data)[-c(1, 2)]),
                       hr(),
                       dateRangeInput("dt",
                                      "Time window",
                                      start = "2010-01-01",
                                      end = "2010-01-30",
                                      min = "2010-01-01",
                                      max = "2018-07-27"),
                       hr(),
                       radioButtons("aggregation",
                                    "Aggregation",
                                    choices = c("day", "month", "quarter", "year"),
                                    selected = "day",
                                    inline = TRUE),
                       hr(),
                       leafletOutput("locationMap", height = "400px") %>% withSpinner(color="#ABB9DB")
                     )
              ),
              column(width = 9,
                     highchartOutput("tsPlot", height = "400px") %>% withSpinner(color="#ABB9DB"),
                     fluidRow(
                       column(width = 6,
                              highchartOutput("acfPlot", height = "400px") %>% withSpinner(color="#ABB9DB")
                              ),
                       column(width = 6,
                              highchartOutput("pacfPlot", height = "400px") %>% withSpinner(color="#ABB9DB")
                              )
                     )
              )
            )
    ),
    
    # Second item ---
    tabItem(tabName = "modelling",
            fluidRow(
              column(width = 3,
                     div(h3("Modelling"), h5("Data from January 2010 to July 2018"))
              )
            ),
            fluidRow(
              column(width = 3,
                     wellPanel(
                       h4("Model setup"),
                       selectInput("series_model",
                                   "Time series",
                                   choices = colnames(NOAA_data)[-c(1, 2)]),
                       hr(),
                       dateRangeInput("dt_model",
                                      "Time window",
                                      start = "2010-01-01",
                                      end = "2010-01-30",
                                      min = "2010-01-01",
                                      max = "2018-07-27"),
                       hr(),
                       radioButtons("aggregation_model",
                                    "Aggregation",
                                    choices = c("day", "month", "quarter", "year"),
                                    selected = "day",
                                    inline = TRUE),
                       hr(),
                       checkboxInput("one_step",
                                     "One-step-ahead prediction"),
                       hr(),
                       sliderInput("window_forecast", "Prediction window",
                                   min = 2, max = 100, value = 30
                       ),
                       hr(),
                       checkboxInput("seasonal_step",
                                     "Include seasonal parameters"),
                       hr(),
                       actionButton("action_model",
                                    "Tap to model!",
                                    icon = icon("magic"))
                     )
              ),
              column(width = 9,
                     fluidRow(
                       column(width = 6,
                              highchartOutput("forecastPlot", height = "400px") %>% withSpinner(color="#ABB9DB")
                              ),
                       column(width = 6,
                              highchartOutput("residualsPlot", height = "400px") %>% withSpinner(color="#ABB9DB")
                              )
                     ),
                     fluidRow(
                       column(width = 6,
                              highchartOutput("residualsACF", height = "400px") %>% withSpinner(color="#ABB9DB")
                              ),
                       column(width = 6,
                              highchartOutput("qqPlot", height = "400px") %>% withSpinner(color="#ABB9DB")
                              )
                     ),
                     fluidRow(
                       column(width = 12,
                              dataTableOutput("measuresTab") %>% withSpinner(color="#ABB9DB")
                              )
                     )
              )
            )
    )
  )
)

ui <- tagList(
  useShinyjs(),
  tags$head(
    tags$link(href = "style.css", rel = "stylesheet")
  ),
  tags$head(tags$link(rel="shortcut icon", href="noaa-favicon.png")),
  
  dashboardPagePlus(title = "Time Series Modelling | NOAA", header, sidebar, body))