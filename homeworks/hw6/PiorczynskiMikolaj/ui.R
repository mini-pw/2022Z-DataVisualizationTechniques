source('global.R')

ui1 <- fluidPage(
  titlePanel("Number of Nobel Prize Laureates by Birth Country"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "category",
        label = "Select categories: \n",
        choices = unique(nobel_df$category), 
        selected = unique(nobel_df$category)
      ),
      sliderInput(
        inputId = "top_n", 
        label = "Select number of displayed countries: ", 
        min = 5, 
        max = 50, 
        value = 10
      )
    ),
    mainPanel(
      wrap_with_spinner(
        plotOutput("countries_plot")
      )
    )
  )
)

ui2 <- fluidPage(
  titlePanel("Number of Nobel Prize Laureates by Continent"), 
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "continent",
        label = "Select continents: \n",
        choices = unique(nobel_df$birth_continent[nobel_df$birth_continent != ""]), 
        selected = "Europe"
      )
    ),
    mainPanel(
      wrap_with_spinner(
        plotly::plotlyOutput("continents_plot")
      )
    )
  )
)

ui3 <- fluidPage(
  titlePanel("Birth Cities of Nobel Prize Laureates"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "map_categories",
        label = "Select categories: \n",
        choices = unique(nobel_df$category), 
        selected = unique(nobel_df$category)
      )
    ),
    mainPanel(
      wrap_with_spinner(
        leaflet::leafletOutput("cities_plot")
      )
    )
  )
)

app_ui <- navbarPage(
  title = "Nobel Prize Data Analysis", 
  tabPanel("Nobel Prize Laureates by Country", ui1, icon = icon("signal", lib="glyphicon")), 
  tabPanel("Nobel Prize Laureates by Continent", ui2, icon = icon("globe-europe")),
  tabPanel("Nobel Prize Laureates on Map", ui3, icon = icon("map")),
  theme = bslib::bs_theme(bootswatch = "flatly"), 
  footer = shiny::HTML(
    "<footer class='text-center text-sm-start' style='width:100%;'>
    <hr>
    <p class='text-center' style='font-size:12px;'>
      © 2021 Copyright: Mikołaj Piórczyński
    </p>
    </footer>"
  )
)

