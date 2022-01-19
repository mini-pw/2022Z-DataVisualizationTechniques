library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(stringr)


data <- read.csv("complete.csv")

uiMB <- fluidPage(
  
  titlePanel("Most Nobel Prize Laureates by Country in Chosen Categories"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "categories",
        "Choose categories: \n",
        unique(data$category),
        unique(data$category)
      ),
      sliderInput(
        "top_n",
        "Choose number of displayed countries:",
        5,
        20,
        10
      )
    ),
    
    mainPanel(
      plotlyOutput("mostLaureatesPlot")
    )
  )
)

uiTB <- fluidPage(
  
  titlePanel("Age of Nober Prize Laureates at the Time of Awarding"),
  
  fluidRow(
    column(width = 12,
           checkboxGroupInput(
             inputId = "category_input1",
             label = "Choose category: \n",
             choices = unique(data$category),
             c("Physics"),
             inline = TRUE
           ),
           radioButtons("buttons1",
                        "Display polynomial regression?",
                        choices = c("No","Yes"),
                        "Yes",
                        inline = TRUE
           ),
           fluidRow(
             splitLayout(cellWidths = c("70%", "30%"),
               column(width = 12,
                      plotlyOutput("plot1"),
                      sliderInput("range1",
                                  "Range of years:",
                                  min = 1900,
                                  max = 2019,
                                  value = c(1900, 2019), sep = "", 
                                  width = "100%")
                      ),
               plotlyOutput("plot2")
             )
           ),
    ),
  )
)

server <- function(input, output) {
  
  
  text = paste("Please choose any category \nin order to display a plot")
  emptyPlot <- ggplot() + 
    annotate(geom = "text",
             x = 4,
             y = 25,
             size=8,
             label = text,
    ) + 
    theme_void()
  
  output$mostLaureatesPlot <- plotly::renderPlotly({
    if (length(input$categories) == 0) {
      p <- emptyPlot
    } else {
      p <- data %>% 
        filter(birth_countryNow != "") %>% 
        filter(category %in% input$categories) %>%
        group_by(birth_countryNow) %>%
        summarise(count = n()) %>% 
        mutate(birth_countryNow = forcats::fct_reorder(birth_countryNow, count)) %>%
        arrange(-count) %>%
        head(input$top_n) %>% 
        ggplot(aes(x = count, y = birth_countryNow)) +
        geom_col() +
        labs(
          x = "Number of Nobel Prize Laureates",
          y = ""
        ) +
        scale_x_continuous(expand = expansion(mult = c(0, .1)))
        
    }
    ggplotly(p)
  })
  
  df <- data %>% 
    filter(!(birth_date == "")) %>% 
    mutate(birth_year = as.numeric(str_split(birth_date, pattern = "-", simplify = TRUE)[, 1])) %>% 
    mutate(how_old = awardYear - birth_year) %>% 
    select(awardYear, category, knownName, how_old, affiliation_1)
  
  output$plot1 <- plotly::renderPlotly({
    
    if (length(input$category_input1) == 0) {
      p1 <- emptyPlot
    } else {
      p1 <- df %>% 
        filter(category %in% input$category_input1) %>% 
        ggplot(aes(x = awardYear, y = how_old, color = category)) +
        scale_color_manual(values = c("Economic Sciences" = "#66c2a5", 
                                      "Physics" = "#fc8d62",
                                      "Chemistry" = "#8da0cb",
                                      "Peace" = "#e78ac3",
                                      "Physiology or Medicine" = "#a6d854",
                                      "Literature" = "#ffd92f"
        )) +
        geom_point(aes(text = knownName)) +
        scale_y_continuous(limits = c(0, 100)) +
        scale_x_continuous(limits = input$range1)

      if(input$buttons1 == "Yes"){
        p1 <- p1 + 
          stat_smooth(method = lm, formula = y ~ poly(x, 3, raw = TRUE))
      }
    }
    
    ggplotly(p1, tooltip = c("x", "y", "text")) %>% layout(xaxis = list(title = 'Award Year'), 
                            yaxis = list(title = 'Age'))
  })
  
  output$plot2 <- plotly::renderPlotly({
    
    df$category <- factor((df$category),
                          levels=c("Economic Sciences", "Physics", "Chemistry",
                                   "Peace", "Physiology or Medicine", "Literature"))
    
    p2 <- df %>% 
      filter(awardYear >= input$range1[1] & awardYear <= input$range1[2]) %>% 
      ggplot() +
      geom_boxplot(aes(x = category, y = how_old, fill = category)) +
      scale_y_continuous(limits = c(0, 100)) +
      scale_fill_manual(values = c("Economic Sciences" = "#66c2a5", 
                                   "Physics" = "#fc8d62",
                                   "Chemistry" = "#8da0cb",
                                   "Peace" = "#e78ac3",
                                   "Physiology or Medicine" = "#a6d854",
                                   "Literature" = "#ffd92f"
      )) + 
      theme(legend.position = "none",
      )
    
    ggplotly(p2) %>% layout(yaxis = list(title = ''), 
                            xaxis = list(title = '', tickangle=30))
  })
}

app_ui <- navbarPage(
  title = "Nobel Prize Data Analysis",
  tabPanel("Number of Laureates by Country", uiMB),
  tabPanel("Laureates by Age", uiTB),
  theme = bslib::bs_theme(bootswatch = "flatly")
)


# rsconnect::setAccountInfo(name='name', token='token', secret='secret')
# rsconnect::deployApp('.', appName = "BarcinskiBorowski")

shinyApp(ui = app_ui, server = server)
