
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)

source("Data.R")

ui0 <- fluidPage(
  h1("Welcome!", align = "center"),
  h5("You are now on", span("Nobel Prize Data Analysis", style = "color:orange"), "page.", align = "center"), 
  h5("My name is Jakub Piwko and I created this page as a part of 
      Data Visualization Techniques course for Data Science studies.", align = "center"),
  h5("I encourage you to visit all of 3 tabs, where you can find different plots
         that you can adjust with checkboxes and sliders placed on left panels.", align = "center"),
  h5("Hope you enjoy!", align = "center"),
  column(12, align="center", imageOutput("myImage"))
)

ui <- fluidPage(
  titlePanel("Age vs. Prize Money"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("dec",
                  "Choose decade range:",
                  min = 1910,
                  max = 2010,
                  value = c(1910, 1920),
                  step = 10),
      checkboxGroupInput(inputId = "cate",
                         "Choose categories you want to include:",
                         choices = unique(data$category),
                         selected = "Physics")
    ),
    
    mainPanel(
      plotOutput("distPlot"))
  )
)

ui2 <- fluidPage(
  titlePanel("Individual or Group Win?"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("dec2",
                  "Choose decade range:",
                  min = 1910,
                  max = 2010,
                  value = c(1910, 1920),
                  step = 10),
      checkboxGroupInput(inputId = "group",
                         "Choose groups you want to include:",
                         choices = unique(df2$Won_by),
                         selected = "Group")
    ),
    
    mainPanel(
      plotOutput("distPlot2"))
  )
)

ui3 <- fluidPage(
  titlePanel("Money Gain by Continent"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "cont",
                         "Choose continent:",
                         choices = unique(df3$continent),
                         selected = "Europe"),
      checkboxGroupInput(inputId = "cate2",
                         "Choose categories you want to include:",
                         choices = unique(df3$category),
                         selected = "Physics")
    ),
    mainPanel(
      plotOutput("distPlot3")
    )
  )
)



server <- function(input, output) {
    

    output$distPlot <- renderPlot({
      
        plot1 <- ggplot(df1 %>% 
                          filter(decade >= input$dec[1], decade <= input$dec[2], category %in% input$cate) %>% 
                          mutate(age = awardYear - as.numeric(format(as.Date(birth_date), '%Y'))) ) +
            geom_point(aes(x = age, y = prizeAmount, color = category), size = 3) +
            labs(title = "Corelation between age of winners and prize money they received over the decades",
                 x = "Age",
                 y = "Prize Money",
                 color = "Category:") +
            scale_x_continuous(limits = c(0,100), breaks = seq(0, 100, by = 5)) +
            scale_y_continuous(limits = c(0, 10e6), trans = 'sqrt',
                               breaks = seq(0, 10e6, length.out = 11)) +
            scale_color_manual(values = c("#ffb100", "#92886d", "#ff1200", "#eaff00", "#c2623d", "#520201")) +
            theme(
              plot.background = element_blank(),
              panel.background = element_blank(),
              panel.grid = element_line(color = "grey"),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(size = 14),
              legend.title = element_text(size = 14),
              title = element_text(size = 15)
            )
                           
        plot1
      
        
    })
    
    output$distPlot2 <- renderPlot({
          
        
        plot2 <- ggplot(df2 %>% 
                          filter(decade >= input$dec2[1], decade <= input$dec2[2], Won_by %in% input$group) %>% 
                          group_by(category, Won_by) %>% 
                          summarise(count = sum(count), .groups = "drop") %>% 
                          complete(category, Won_by, fill = list(count = 0)),
                        aes(x = category, y = count , fill = Won_by)) +
          geom_col(position = "dodge", width = 0.8) +
          scale_fill_manual(values = c("#ffb100" ,"#92886d", "#ff1200", "#eaff00")) +
          scale_x_discrete(guide = guide_axis(n = 2)) +
          scale_y_continuous(limits = c(0,100), breaks = seq(0,100,5)) +
          labs(title = "Number of prizes won individually or by groups/organizations in each category over the decades",
               x = "Category",
               y = "Number of prizes won",
               fill = "Prize won by:") +
          theme(
            plot.background = element_blank(),
            panel.background = element_blank(),
            panel.grid = element_line(color = "grey"),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 14),
            legend.title = element_text(size = 14),
            title = element_text(size = 15)
          )
          
        plot2 
    })
    output$distPlot3 <- renderPlot({
        
      plot3 <- ggplot(df3 %>%
                        filter(continent %in% input$cont, category %in% input$cate2) %>%
                        group_by(continent, decade) %>% 
                        summarise(prize = sum(prize), .groups = "drop") %>% 
                        complete(continent, nesting(decade), fill = list(prize = 0)), 
                      aes(x = decade, y = prize, color = continent)) +
        geom_point(size = 3) +
        geom_line(size = 1.2) +
        scale_x_continuous(limits = c(1900, 2010), breaks = seq(1900, 2010, 10)) +
        scale_y_continuous(limits = c(0, 3.5e8), trans = 'sqrt', 
                           breaks = seq(from = 0, to = 3.5e8, by = 0.5e8)) +
        labs(title = "Prize money gained by winners of each continent over the decades including chosen categories",
             x = "Decade",
             y = "Prize Money",
             color = "Continent") +
        scale_color_manual(values = c("#ffb100", "#92886d", "#ff1200", "#eaff00", "#c2623d", "#520201")) +
        theme(
          plot.background = element_blank(),
          panel.background = element_blank(),
          panel.grid = element_line(color = "grey"),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14),
          legend.title = element_text(size = 14),
          title = element_text(size = 15)
        )
      plot3
      })
    
    output$myImage <- renderImage({
      list(src = "nobel_prize.png",
           contentType = 'image/png',
           width = 300,
           height = 300,
           alt = "This is alternate text")
    }, deleteFile = FALSE)
    
}




app_final <- navbarPage(
  title = "Nobel Prize Data Analysis",
  tabPanel("Welcome!", ui0),
  tabPanel("Age vs. Prize Money", ui),
  tabPanel("Individual or Group Win?", ui2), 
  tabPanel("Money Gain by Continent", ui3),
  theme = bslib::bs_theme(bootswatch = "united"))

shinyApp(ui = app_final, server = server)

