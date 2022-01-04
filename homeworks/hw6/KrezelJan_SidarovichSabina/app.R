library(shiny)
library(dplyr)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(readr)
library(DescTools)
library(stringr)
library(shinycssloaders)
library(tidyr)
require(maps)

df <- read_csv("complete.csv")
df["birth_date"] <- as.Date(df$birth_date) 
df["first_letter"] <- strtrim(str_replace(df$givenName, "[$Sir\ ]", ""), 1)

### Prizes data
data <- df %>% select("category", "birth_countryNow")
data <- data %>% 
  group_by(birth_countryNow, category) %>% 
  summarise(total = n()) %>% 
  filter(birth_countryNow != "") %>% 
  pivot_wider(names_from=category, values_from = total)

map.data = map_data("world") %>% select(-subregion)

full.data <- map.data %>% full_join(data, by=c("region"="birth_countryNow"))%>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  filter(region != "Antarctica")
### End of prizes data

server <- function(input, output, session) {
  
  output$plot1 <- plotly::renderPlotly({
    if (input$ignoreUSA) {
      full.data <- full.data %>% filter(region != "USA")
    }
    
    Total <- full.data %>% 
      select(input$categorySummed) %>% 
      rowSums()
    
    p <- full.data %>% 
      mutate(Total = Total) %>% 
      ggplot(aes(x = long, y = lat, group=group, fill = Total)) +
      geom_polygon() +
      scale_fill_gradient(low="#202020", high="#00ffaa") +
      labs(
        x = "", y = "",
        title = "Total Nobel Prizes Won",
        subtitle = "by the type of prize",
        fill = "Number\nof prizes"
      )
      # coord_map()
    
    plotly::ggplotly(p)
  }
  )

  output$plot2 <- plotly::renderPlotly({
    df %>% filter(birth_date >= input$dates[1] & birth_date <= input$dates[2]) %>%
      filter(category %in% input$sub) %>%
      group_by(first_letter) %>% 
      summarise(n = n()) %>%  
      top_n(10) %>% 
      ggplot(aes(x=reorder(first_letter, n), y = n)) +
      geom_col() +
      xlab("The first letter of the name") +
      ylab("The number of laureates") +
      labs(title = "Most common first letters of the laureates' names") +
      theme(plot.title = element_text(size=16)) +
      coord_flip()-> p2
    
     p2 <- plotly::ggplotly(p2) 
     
     p2 %>%  plotly::style(text = paste0("Number:", p2$x$data[[1]]$y))
  })
 }

ui <- fluidPage(
  titlePanel("Prizes Won Per Country"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("categorySummed", "Choose categories to sum up:",
                         c("Economic Sciences"="Economic Sciences",
                           "Physics"="Physics",
                           "Chemistry"="Chemistry",
                           "Peace"="Peace",
                           "Physiology or Medicine"="Physiology or Medicine",
                           "Literature"="Literature"),
                         selected = c("Physics")),
      hr("Ignore USA?"),
      checkboxInput("ignoreUSA", "YES", FALSE),
      width = 3
    ),
    mainPanel(
      shinycssloaders::withSpinner(plotly::plotlyOutput("plot1", width="100%", height="100%")),
      width = 9
    )
  )
)

ui2 <- fluidPage(
  titlePanel("Names"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("sub", "Choose categories to display:",
                         c("Economic Sciences"="Economic Sciences",
                           "Physics"="Physics",
                           "Chemistry"="Chemistry",
                           "Peace"="Peace",
                           "Physiology or Medicine"="Physiology or Medicine",
                           "Literature"="Literature"),
                         selected = c("Economic Sciences","Physiology or Medicine")),
      dateRangeInput("dates", label = h3("Date range"),
                     start = as.Date("1900-01-01"),
                     end = as.Date("2000-01-01"),
                     min = as.Date("1817-11-30")),
      hr(),
      fluidRow(column(4, verbatimTextOutput("value")))
    ),
    
    mainPanel(
      shinycssloaders::withSpinner(plotly::plotlyOutput("plot2"))
    )
  )
)

app_ui <- navbarPage(
  title = "NOBLE STATS YOU DIDN'T KNOW YOU NEED",
  tabPanel("NAMES", ui2, icon = icon("id-card")),
  tabPanel("PRIZES PER COUNTRY", ui, icon = icon("star")), 
  theme = bslib::bs_theme(bootswatch = "lux"),
  footer = shiny::HTML("
                <footer class='text-center text-sm-start' style='width:100%;'>
                <hr>
                <p class='text-center' style='font-size:12px;'>
                  © 2021 Copyright:
                  <a class='text-dark' href='https://github.com/krezelj'>Jasiek</a>
                  and
                  <a class='text-dark' href='https://github.com/notsabina'>Sabina</a>
                </p>
                </footer>
                "),
  header = tags$head(tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css")))#todo


shinyApp(app_ui, server)

