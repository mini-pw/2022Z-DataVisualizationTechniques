library(shiny)
library(SmarterPoland)

## based on https://shiny.rstudio.com/articles/dynamic-ui.html


ui <- fluidPage(
  # 1st
  selectInput("person", "Person", c("jan", "adam", "zeus")),
  conditionalPanel(condition = "output.person_name_is_long",
                   checkboxInput("whatever", "Person name is longer than 3")),
  
  # 2nd
  selectInput("continent", "Continent",
              choices = c("Asia", "Europe", "Africa")),
  uiOutput("countries")
)


server <- function(input, output, session) {
  # 1st
  person_name <- reactive({
    switch(input$person,
           "jan" = nchar("jan"),
           "adam" = nchar("adam"),
           "zeus" = nchar("zeus"))
  })
  output$person_name_is_long <- reactive({
    person_name() > 3
  })
  outputOptions(output, "person_name_is_long", suspendWhenHidden = FALSE)  
  
  # 2nd
  output$countries <- renderUI({
    temp <- countries[countries$continent == input$continent,]$country
    checkboxGroupInput("cities", "Choose countries", temp)
  })
}


shinyApp(ui, server)
