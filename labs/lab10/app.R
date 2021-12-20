#:# TWD LAB 10

# 1. plotly::plotlyOutput, plotly::renderPlotly
# 2. shiny::navbarPage
# 3. bslib::bs_theme, bslib::bootswatch_themes, bslib::page_navbar(theme)
# 4. footer, css, shiny::HTML, shiny::markdown, icon
# 5. Zadanie 1. dodanie wykresu, tabelki i kontrolki do drugiego panelu
# 6. omówić przykłady z https://github.com/Yang-Tang/shinyjqui
# 7. shinycssloaders::withSpinner -> https://daattali.com/shiny/shinycssloaders-demo
# 8. Zadanie 2. wybrać i dodać spinnery do wykresów w aplikacji 
# 9. shiny::bindCache  / https://shiny.rstudio.com/articles/caching.html
# 10. Zadanie 3. dodać caching do statycznych wykresóW/tabel,
#                zwiększyć rozmiar danych i zobaczyć różnicę


library(shiny)
library(dplyr)
library(ggplot2)
library(dplyr)
library(ggrepel)


library(PogromcyDanych)
set.seed(123)
df <- na.omit(auta2012) %>% dplyr::sample_frac(0.01)


server <- function(input, output, session) {
  
  output$plot1 <- plotly::renderPlotly({

    p <- ggplot(df %>% rename(color = input$color),
                aes(x = Cena.w.PLN, y = KM, color = color)) +
      geom_point() +
      scale_y_log10() +
      scale_x_log10() +
      theme_bw() +
      labs(x = "Price [PLN]", y = "Power [KM]", color = input$color,
           title = "Cars: power vs price")

    plotly::ggplotly(p)
  })
  
  output$plot2 <- plotly::renderPlotly({
     #:# Zad 1
  })
  
  # output$plot1 <- renderPlot({
  #   ggplot(df %>% rename(color = input$color), 
  #          aes(x = Cena.w.PLN, y = KM, color = color)) + 
  #     geom_point() + 
  #     scale_y_log10() +
  #     scale_x_log10() +
  #     theme_bw() +
  #     labs(x = "Price [PLN]", y = "Power [KM]", color = input$color,
  #          title = "Cars: power vs price")
  # }) %>% bindCache(input$color)
}


ui <- fluidPage(
  
    titlePanel("Introduction"),

    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "color",
          label = "Color aesthetic:",
          choices = c("Rodzaj.paliwa", "Liczba.drzwi", "Skrzynia.biegow")
        ),
        width = 3
      ),
      mainPanel(
        plotly::plotlyOutput("plot1"),
        # shinycssloaders::withSpinner(plotly::plotlyOutput("plot1")),
        # plotOutput("plot1"),
        width = 9
      )
    )
)

runApp(shinyApp(ui, server))


ui2 <- fluidPage(
  titlePanel("Advanced"),
  sidebarLayout(
    sidebarPanel(
      #:# Zad 1
    ),
    mainPanel(
      # możemy wykorzystać markdown w tworzeniu stron HTML
      shiny::markdown("We need: \n 1. two plot/table outputs - you can use shiny::renderTable() and shiny::tableOuptut() \n 2. two inputs"),
      
      plotly::plotlyOutput("plot2")
    )
  )
)

app_ui <- navbarPage(
  title = "Data analysis: auta2012",
  tabPanel("Introduction", ui),
  tabPanel("Advanced", ui2, icon = icon("database")),  # ?icon
  theme = bslib::bs_theme(bootswatch = "cosmo"),
  # przykład stworzenia footer-a https://mdbootstrap.com/docs/standard/navigation/footer/
  footer = shiny::HTML("
                <footer class='text-center text-sm-start' style='width:100%;'>
                <hr>
                <p class='text-center' style='font-size:12px;'>
                  © 2021 Copyright:
                  <a class='text-dark' href='https://mi2.ai'>MI2</a>
                </p>
                </footer>
                "),
  # przykład dodania CSS do HEAD
  header = tags$head(tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css")),
  # fluid = FALSE
)

shinyApp(app_ui, server)
