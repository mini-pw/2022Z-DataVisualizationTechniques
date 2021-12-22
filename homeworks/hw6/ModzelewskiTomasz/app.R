library(shiny)

source("nobel_plots.R")
source("app_utils.R")


df <- read.csv("complete.csv")

ui <- fluidPage(
  
  tags$head(
    tags$style("body { margin: 15px; } div#boxplot { margin-top: 50px; width: 70%; }")
  ),
  
  titlePanel("The Nobel Prize - visualizations"),
  
  tags$div(style = "width: 70%;", 
           
           hr(),
           
           tags$h4("Age of Nobel Laureates", style = "margin: 1.5em 0 1.5em 0"),
           
           shinycssloaders::withSpinner(
             plotlyOutput(outputId = "boxplot")
           ),
           
           nobel_slider_time_interval("boxplot_range", df),
           
           hr(),
           
           tags$h4("Countries of birth of the Nobel laureates", style = "margin: 1.5em 0 1.5em 0"),
           
           shinycssloaders::withSpinner(
             plotlyOutput(outputId = "barplot")
           ),
           
                    sliderInput(
                      "barplot_range", 
                      "Time interval:", 
                      min = min(df$awardYear), 
                      max = max(df$awardYear),
                      value = c(1969, max(df$awardYear)),
                      sep = ""
                    ),
                    
                    selectInput(
                      "barplot_category",
                      "Category:",
                      c("All", nobel_categories(df))
                    ),
                    
                    numericInput(
                      "barplot_top_n",
                      "Number of top countries:",
                      10,
                      min = 4,
                      step = "any"
                    ),
                    
              

    ),
  
)

server <- function(input, output, session) {
  
  output$boxplot <- renderPlotly(nobel_boxplot(df, input$boxplot_range[1], input$boxplot_range[2]))
  

  output$barplot <- renderPlotly(nobel_barplot(df, category = input$barplot_category, top_n = input$barplot_top_n, year_start = input$barplot_range[1], year_end = input$barplot_range[2]))

}

shinyApp(ui, server)