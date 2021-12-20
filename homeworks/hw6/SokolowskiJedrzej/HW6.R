# TWD HW6 Jędrzej Sokołowski
# Nr. albumu 313507

library(shiny)
library(dplyr)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(hash)

noble_df <- read.csv("complete.csv")
noble_df <- select(noble_df, c(awardYear,category,fullName,prizeAmount))
noble_1 <- noble_df %>% group_by(awardYear) %>% summarise(Count = length(category))

server <- function(input, output, session) {
  
  output$plot1 <- plotly::renderPlotly({
    
    plot_awardYear <- ggplot(noble_df,
                aes(y = awardYear)) +
      geom_boxplot(fill = input$color) +
      theme_bw() +
      labs(x = "Rok", y = "Wartość",
           title = "Rozkład lat, w których zdobywane były nagrody Nobla") +
      theme(
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    plot_prizeAmount <- ggplot(noble_df,
                aes(y = prizeAmount)) +
      geom_boxplot(fill = input$color) +
      theme_bw() +
      labs(x = "Nagroda", y = "Wartość",
           title = "Rozkład wysokości nagród nobla") +
      theme(
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
    
    h <- hash()
    h[["plot_awardYear"]] <- plot_awardYear
    h[["plot_prizeAmount"]] <- plot_prizeAmount
    
    Sys.sleep(1)
    
    plotly::ggplotly(h[[input$type]])

  }) %>% bindCache(c(input$color, input$type))
  
  output$plot2 <- plotly::renderPlotly({
    
    plot_bar <- ggplot(noble_1,
                             aes(x = awardYear, y = Count)) +
      geom_col(fill = input$color2) +
      theme_bw() +
      labs(x = "Rok", y = "Ilość",
           title = "Ilość nagród Nobla zdobytych na przestrzeni lat")
    plot_line <- ggplot(noble_1,
                               aes(x = awardYear, y = Count)) +
      geom_line(color = input$color2) +
      theme_bw() +
      labs(x = "Rok", y = "Ilość",
           title = "Ilość nagród Nobla zdobytych na przestrzeni lat")
    
    h <- hash()
    h[["plot_line"]] <- plot_line
    h[["plot_bar"]] <- plot_bar
    
    Sys.sleep(0.5)
    
    plotly::ggplotly(h[[input$type2]])
    
  }) %>% bindCache(c(input$color2, input$type2))
  
  output$my_table <- shiny::renderTable(
    if (input$sort == "Nie sortuj"){
    noble_df %>% filter(category == input$categ)}
    else {
      noble_df %>% filter(category == input$categ) %>% arrange(desc(pull(., input$sort)))
    }) %>% bindCache(c(input$categ, input$sort))
}

# Zakładka 1

ui <- fluidPage(
  
  titlePanel("Rozkłady wybranych zmiennych"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "type",
        label = "Zmienna:",
        choices = c("Rok przyznania" = "plot_awardYear", "Nagroda" = "plot_prizeAmount"),
        selected = "plot_awardYear"),
      selectInput(
        inputId = "color",
        label = "Kolor:",
        choices = c("Czerwony" = "red","Niebieski" = "blue","Zielony" = "green")
      ),
      width = 3
      ),
      
    mainPanel(
      shinycssloaders::withSpinner(plotly::plotlyOutput("plot1"), type = 2, color.background = "lightblue", color = "lightblue"),
      width = 9
    )
  )
)

# Zakładka 2

ui2 <- fluidPage(
  titlePanel("Wykresy ilości nagród Nobla w danych latach"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "type2",
        label = "Typ wykresu:",
        choices = c("Słupkowy" = "plot_bar", "Liniowy" = "plot_line"),
        selected = "plot_bar"),
      selectInput(
        inputId = "color2",
        label = "Kolor:",
        choices = c("Czerwony" = "red","Niebieski" = "blue","Zielony" = "green")
      ),
      
      width = 3
    ),
    mainPanel(
      shinycssloaders::withSpinner(plotly::plotlyOutput("plot2"), type = 2, color.background = "lightblue", color = "lightblue"),
    )
  )
)

# Zakładka 3

ui3 <- fluidPage(
  titlePanel("Nagrody Nobla z danej dziedziny"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "categ",
        label = "Dziedzina:",
        choices = c(unique(noble_df$category)),
        selected = "Physics"),
      selectInput(
        inputId = "sort",
        label = "Sortuj malejąco po:",
        choices = c("Nie sortuj", "awardYear", "fullName","prizeAmount"),
        selected = "Nie sortuj"),
      
      width = 3
    ),
    mainPanel(
      shinycssloaders::withSpinner(shiny::tableOutput("my_table"), type = 2, color.background = "lightblue", color = "lightblue"),
    )
  )
)

app_ui <- navbarPage(
  title = "The Noble Prize Data Analysis: TWD HW6",
  tabPanel("Distrubutions", ui),
  tabPanel("Other plots", ui2),
  tabPanel("Tables", ui3),
  theme = bslib::bs_theme(bootswatch = "cerulean"),
  footer = shiny::HTML("
                <footer class='text-center text-sm-start' style='width:100%;'>
                <hr>
                <p class='text-center' style='font-size:12px;'>
                 Autor: Jędrzej Sokołowski
                </p>
                </footer>
                "),

)

shinyApp(app_ui, server)

