library(shiny)
library(ggplot2)
library(dplyr)


nobel <- read.csv("complete.csv")
nobel$birth_date <- substr(nobel$birth_date, 0, 4)
df <- transform(nobel, birth_date = as.numeric(birth_date))%>% 
  rowwise() %>% 
  mutate(Age = sum(awardYear, -birth_date), 
         Decade = awardYear - (awardYear %% 10))

ui <- fluidPage(
  
  title = "Nobel Prize",
  
  titlePanel("Statistics on the gender and nationality of the winners"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("selectInput",
                  "Select Category",
                  choices = c("All",
                              unique(nobel$category)),
                  selected = "All"
      ),
      sliderInput("bins",
                  "Decade: ",
                  min = 1960,
                  max = 2010,
                  value = 1960,
                  step = 10, 
                  ticks = FALSE, 
                  width = '100%',
                  animate = animationOptions(playButton = "play", 
                                             pauseButton = "stop", 
                                             interval = 1500)),
      checkboxInput(
        inputId = "box",
        label = "Show data from all decades"
      ),
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Gender",
                 plotOutput("plot1")), 
        tabPanel("Continent", 
                 plotOutput("plot3"))
      )
    )
  )
)

ui2 <- fluidPage(
  
  title = "Nobel Prize",
  
  titlePanel("Age statistics of the winners"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("selectInput2",
                  "Select Category",
                  choices = c("All",
                              unique(nobel$category)),
                  selected = "All"
      )
    ),
    
    mainPanel(
      plotOutput("plot2")
      )
  )
)

server <- function(input, output) {
  
  nobel <- read.csv("complete.csv")
  nobel$birth_date <- substr(nobel$birth_date, 0, 4)
  df <- transform(nobel, birth_date = as.numeric(birth_date))%>% 
    rowwise() %>% 
    mutate(Age = sum(awardYear, -birth_date), 
           Decade = awardYear - (awardYear %% 10))
  
  output$plot1 <- renderPlot({
    
    if (input$selectInput != "All"){
      if (!input$box){
        
        data2 <- df %>% 
          filter(category == input$selectInput, Decade == input$bins) %>% 
          select(Gender = "gender")
        data2 <- data2[!data2$Gender=="",]
        
        ggplot(data2, aes(x = Gender)) + stat_count() + theme_bw() + scale_y_continuous(expand = c(0,0), limits = c(0,30), breaks = seq(0, 30, by=2))
        
      }else{
        
        data2 <- df %>% 
          filter(category == input$selectInput) %>% 
          select(Gender = "gender")
        data2 <- data2[!data2$Gender=="",]
        
        ggplot(data2, aes(x = Gender)) + stat_count() + theme_bw() + 
          scale_y_continuous(expand = c(0,0), limits = c(0,225), breaks = seq(0, 225, by=25))
        
      }
    }else{
      if (!input$box){
        
        data2 <- df %>% 
          filter(Decade == input$bins) %>% 
          select(Gender = "gender")
        data2 <- data2[!data2$Gender=="",]
        
        ggplot(data2, aes(x = Gender)) + stat_count() + theme_bw() + scale_y_continuous(expand = c(0,0), limits = c(0,120), breaks = seq(0, 120, by=10))
        
      }else{
        
        data2 <- df %>%  
          select(Gender = "gender")
        data2 <- data2[!data2$Gender=="",]
        
        ggplot(data2, aes(x = Gender)) + stat_count() + theme_bw() + 
          scale_y_continuous(expand = c(0,0), limits = c(0,900), breaks = seq(0, 900, by=50))
        
      }
    }
  })
  
  output$plot2 <- renderPlot({
    
    if (input$selectInput2 != "All"){
      
      data2 <- df%>% 
        filter(category == input$selectInput2)
      
      ggplot(data2, aes(x = Age)) + geom_histogram(binwidth = 1) + theme_bw() + 
        scale_y_continuous(breaks = seq(0, 12, by=2), expand = c(0,0), limits = c(0,12)) + 
        scale_x_continuous(breaks = seq(0,100,5))
      
    }else{
      data2 <- df %>%  select(Age)
      
      ggplot(data2, aes(x = Age)) + geom_histogram(binwidth = 1) + theme_bw() + 
        scale_y_continuous(breaks = seq(0,40,5), expand = c(0,0), limits = c(0,40))  + 
        scale_x_continuous(breaks = seq(0,100,5))
    }
  })
  
  output$plot3 <- renderPlot({
    
    if (input$selectInput != "All"){
      if (!input$box){
        
        data2 <- df%>% 
          filter(category == input$selectInput, Decade == input$bins) %>% 
          select(c("Decade", Continent = "birth_continent"))
        data2 <- data2[!data2$Continent=="",]
        
        ggplot(data2, aes(x = Continent)) + stat_count() + theme_bw()+ 
          scale_y_continuous(expand = c(0,0), limits = c(0,16), breaks = seq(0, 16, by=2))
        
      }else{
        
        data2 <- df%>% 
          filter(category == input$selectInput) %>% 
          select(c("Decade", Continent = "birth_continent"))
        data2 <- data2[!data2$Continent=="",]
        
        ggplot(data2, aes(x = Continent)) + stat_count() + theme_bw()+ 
          scale_y_continuous(expand = c(0,0), limits = c(0,120), breaks = seq(0, 120, by=10))
        
      }
    }else{
      if (!input$box){
        
        data2 <- df%>% 
          filter(Decade == input$bins) %>% 
          select(c("Decade", Continent = "birth_continent"))
        data2 <- data2[!data2$Continent=="",]
        
        ggplot(data2, aes(x = Continent)) + stat_count() + theme_bw()+ 
          scale_y_continuous(expand = c(0,0), limits = c(0,65), breaks = seq(0, 65, by=5))
        
      }else{
        
        data2 <- df%>% 
          select(c("Decade", Continent = "birth_continent"))
        data2 <- data2[!data2$Continent=="",]
        
        ggplot(data2, aes(x = Continent)) + stat_count() + theme_bw()+ 
          scale_y_continuous(expand = c(0,0), limits = c(0,525), breaks = seq(0, 525, by=25))
        
      }
    }
  })
  
}

app_ui <- navbarPage(
  title = "Nobel Prize",
  theme = bslib::bs_theme(bootswatch = "cosmo"),
  tabPanel("Gender and Origin", ui),
  tabPanel("Age", ui2)
)

shinyApp(app_ui,server)
