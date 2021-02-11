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
            )
    ),
    
    # Second item ---
    tabItem(tabName = "modelling",
            fluidRow(
              column(width = 3,
                     div(h3("Modelling"), h5("Data from January 2010 to July 2018"))
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