source('main.R')
#UI1

ui1 <- fluidPage(
  titlePanel("Visualization 1."),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "kategoria",
        label = "Category",
        choices = c("Physics","Economic Sciences","Chemistry", "Peace", "Physiology or Medicine","Literature")
      ),
      
      sliderInput(
        inputId = "ilosc",
        label = "Number of countries",
        min = 5,
        max = 20,
        value = 5
      ),
      width = 3
    ),
    mainPanel(
      plotly::plotlyOutput("plot1")
    )
  )
)

#UI2

ui2 <- fluidPage(
  titlePanel("Visualization 2."),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "kontynent",
        label = "Continent",
        choices = c("North America", "Europe", "Asia", "Africa", "South America", "Oceania")
      ),
      width = 3
    ),
    mainPanel(
      plotOutput("plot2") %>% 
        withSpinner(),
      width = 9
    )
  )
)

#UI3

ui3 <- fluidPage(
  titlePanel("Visualization 3."),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "kategoriaMapa",
        label = "Category",
        choices = unique(df$category),
        selected = unique(df$category)
      ),
      width = 3,
      checkboxGroupInput(
        inputId = "plec",
        label = "Gender",
        choices = c("male", "female"),
        selected = c("male","female")
      )
    ),
    mainPanel(
      plotOutput("plot3") %>% 
        withSpinner(),
      width = 9
    )
  )
)


app_ui <- navbarPage(
  title = "HW6",
  tabPanel("Nobel prizes count by countries",ui1, icon = icon("bar-chart-o")),
  tabPanel("Nobel prizes count by continent", ui2, icon = icon("bar-chart-o")),
  tabPanel("Most awarded countries on map", ui3, icon = icon("map")),
  theme = bslib::bs_theme(bootswatch = 'cosmo'),
  footer = shiny::HTML("
                <footer class='text-center text-sm-start' style='width:100%;'>
                <hr>
                <p class='text-center' style='font-size:12px;'>
                  © 2021 Copyright:
                  <a class='text-dark' href='https://mi2.ai'>Daniel Tytkowski</a>
                </p>
                </footer>
                "),
  header = tags$head(tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css")),
)

