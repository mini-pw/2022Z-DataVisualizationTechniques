library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)
library(shinyWidgets)

data <- read.csv('complete.csv', encoding = 'UTF-8') %>% 
  select(awardYear, category, portion)

ui1 <- fluidPage(
  
  setBackgroundColor("ghostwhite"),
  
  title = "Nobel Prize",
  
  titlePanel("Has the Prize become more fragmented?"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("selectInput1",
                  "Select Category",
                  choices = c(unique(data$category)),
                  selected = "Physics"
      )
    ),
    
    mainPanel(
      plotOutput("plot1"),
      br(),
      p("Individual awards are becoming increasingly rare. Literature and Peace seem to be the only exceptions to this rule.")
    )
  )
)

ui2 <- fluidPage(
  
  title = "Nobel Prize",
  
  titlePanel("Which continents dominate the Nobel Prize?"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("selectInput2",
                  "Select Category",
                  choices = c(unique(data$category)),
                  selected = "Physics"
      ),
      sliderInput("years",
                  "Year: ",
                  min = 1901,
                  max = 2019,
                  value = 1901,
                  step = 1,
                  ticks = FALSE,
                  width = '100%',
                  animate = animationOptions(playButton = "play",
                                             pauseButton = "stop",
                                             interval = 1000,
                                             loop = FALSE))
    ),

    
    mainPanel(
      plotOutput("plot2"),
      br(),
      p("Europe and North America dominate the Nobel Prize. Asia is slowly catching on in certain fields, but is still far behind in others.")
    )
  )
)

server <- function(input, output) {
  
  data1 <- read.csv('complete.csv', encoding = 'UTF-8') %>% 
    select(awardYear, category, portion)
  
  data2 <- read.csv('complete.csv', encoding = 'UTF-8') %>% 
    select(awardYear, category, birth_continent) %>% 
    filter(birth_continent != "")

  output$plot1 <- renderPlot({
    
    df1_wide <- data1 %>% 
      filter(category == input$selectInput1) %>% 
      group_by(awardYear, portion, category) %>% 
      summarise(count=n()) %>% 
      pivot_wider(
        id_cols = c(portion,awardYear),
        names_from = category,
        values_from = count
      )
    aux_df1 <- expand.grid(portion = unique(df1_wide$portion),
                          awardYear = unique(df1_wide$awardYear))
    df1_wide <- merge(aux_df1, df1_wide, all=T) 
    df1_wide[is.na(df1_wide)] <- 0
    
    colnames(df1_wide)[3] <- "number"
    
    df1_final_wide <- df1_wide %>% 
      group_by(portion) %>% 
      mutate(across(number, cumsum))
    
    ggplot(df1_final_wide, aes(x = awardYear, y = number, color = portion))+
      geom_line(size=2) +
      labs(x = "Year",
           y = "Number of awards",
           title = paste("Cumulative number of awards given in", input$selectInput1, "up to a given year by portion of award"),
           color = "Portion")+
      scale_y_continuous(expand=c(0,0), limits = c(-1,110))+
      theme_light()+
      theme(axis.text = element_text(size = 14),
            plot.title = element_text(size = 17),
            axis.title = element_text(size = 15),
            legend.key.size = unit(1, 'cm'), 
            legend.key.height = unit(1, 'cm'), 
            legend.key.width = unit(1, 'cm'), 
            legend.title = element_text(size=14), 
            legend.text = element_text(size=10))
    

  })
  
  
  output$plot2 <- renderPlot({
    

    data2_used <- data2 %>% 
      filter(birth_continent != "",
             category == input$selectInput2)
    
    
    df2_wide <- data2_used %>% 
      group_by(awardYear, birth_continent, category) %>% 
      summarise(count=n()) %>% 
      pivot_wider(
        id_cols = c(birth_continent,awardYear),
        names_from = category,
        values_from = count
      ) 
    
    aux_df2 <- expand.grid(birth_continent = unique(df2_wide$birth_continent),
                           awardYear = unique(df2_wide$awardYear))
    df2_wide <- merge(aux_df2, df2_wide, all=T) 
    df2_wide[is.na(df2_wide)] <- 0
    
    colnames(df2_wide)[3] <- "number"
    df2_final_wide <- df2_wide %>% 
      group_by(birth_continent) %>% 
      mutate(across(number, cumsum))
    
    
    df2_final <- df2_final_wide %>% filter(awardYear == input$years)
    
    ggplot(df2_final, aes(x = number, y = birth_continent))+
      geom_col(fill="#07106b")+
      scale_x_continuous(expand = c(0,0), limits = c(0,120))+
      labs(x = "Number of awards received",
           y = "Continent",
           title = paste("Cumulative number of awards given in", input$selectInput2, "up to", input$years, "by birth continent"))+
      theme_light()+
      theme(axis.text = element_text(size = 14),
            plot.title = element_text(size = 17),
            axis.title = element_text(size = 15))
    
  })
  
}

app_ui <- navbarPage(
  title = "Exploring Nobel",
  theme = bslib::bs_theme(bootswatch = "cosmo"),
  tabPanel("Prize fragmentation", ui1),
  tabPanel("Continent dominance ", ui2)
)

shinyApp(app_ui,server)

