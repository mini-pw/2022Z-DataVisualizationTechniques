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
}

## Zakładka 1
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
      width = 9
    )
  )
)

runApp(shinyApp(ui, server))

## Zakładka 2
ui2 <- fluidPage(
  titlePanel("Advanced"),
  sidebarLayout(
    sidebarPanel(
      #:# Zad 1
    ),
    mainPanel(
      plotly::plotlyOutput("plot2")
    )
  )
)


app_ui <- navbarPage(
  title = "Data analysis: auta2012",
  tabPanel("Introduction", ui),
  tabPanel("Advanced", ui2, icon = icon("database")))

runApp(shinyApp(app_ui, server))
