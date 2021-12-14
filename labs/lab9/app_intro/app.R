library(shiny)
library(ggplot2)
library(PogromcyDanych)

## 0. Tworzymy Shiny App - default w R Studio
## 1. Zamiana basic plot na ggplot
## 2. Zmiana danych na serialeIMDB, boxplot dla zmiennej ocena
## 3. Dodajemy podział na sezony
## 4. Dodajemy wybór serialu jako selectInput. 
##    Zmiana na wykres punktowy, kolor = sezon, x = id, y = ocena
## 5. Dodajemy linię trendu jako checkboxInput


server <- shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    
    p1 <- ggplot(
      serialeIMDB[serialeIMDB$serial == input$serial,], 
      aes(x = id, y = ocena)
    ) +
      geom_point(aes(color = sezon))
    
    if (input$liniaTrendu) {
      p1 <- p1 + geom_smooth(se = FALSE)
    }
    
    p1
  })
  
})

ui <- shinyUI(fluidPage(
  ## 2.
  titlePanel("Seriale IMDB"),
  sidebarLayout(
    sidebarPanel(

      selectInput("serial", "Wybierz serial", unique(serialeIMDB$serial)),

      checkboxInput("liniaTrendu", "Linia trendu", FALSE)
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
))

app <- shinyApp(ui, server)

app
