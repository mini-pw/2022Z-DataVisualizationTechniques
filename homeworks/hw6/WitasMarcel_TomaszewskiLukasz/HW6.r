library(dplyr)
library(ggplot2)
library(stringr)
library(shiny)
library(forcats)

df <- read.csv("Dane/complete.csv")

df1 <- df %>% 
  mutate(firstOfGivenName = str_sub(givenName,end =  1)) %>% 
  mutate(firstOfFamilyName = str_sub(familyName,end =  1))

df2 <- df %>% select(awardYear,category,birth_continent,prizeAmountAdjusted,portion) %>% 
  mutate(portion = case_when(portion == 1 ~ 1,
                             portion == "1/2" ~ 0.5,
                             portion == "1/3" ~ 1/3,
                             portion == "1/4" ~ 0.25)) %>% 
  mutate(Prize = prizeAmountAdjusted*portion) %>% 
  select(awardYear,category,birth_continent,Prize)

df2$birth_continent[df1$birth_continent == ""] <- "Not given"

ui1 <- fluidPage(
  titlePanel("Does your first or last name increase your chances to get Nobel prize?"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "whichName",
        label = "Most popular first letters of:",
        choices = list("First name" = "firstOfGivenName", "Last name" = "firstOfFamilyName"),
        selected = "firstOfFamilyName"
      ),
      selectInput(
        inputId = "category",
        label = "Choose category:",
        choices = c("All", unique(df1$category) )
      ),
      sliderInput("num", 
                  label = "Show:", 
                  value = 10,
                  min = 5,
                  max = 20)   
      #width = 5
      
      
    ),
    mainPanel(
      shinycssloaders::withSpinner(
        plotly::plotlyOutput("plot1")
      #width = 7
      )
    )
  )
)

ui2 <- fluidPage(
  titlePanel("Amount of money from Nobel Prizes on plots"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("kategoria", 
                         "Category:",
                         choiceNames = unique(df$category),
                         choiceValues = unique(df$category),
                         selected = unique(df$category)
      ),
      selectInput("wykres",
                  "Choose plot:",
                  c("Boxplot","Bar graph","Line graph")
                  )
    ),
    mainPanel(
      shinycssloaders::withSpinner(
        plotly::plotlyOutput("plot2")
        )
    )
  )
  
)

server <- function(input, output) {
  
  output$plot1 <- plotly::renderPlotly({
    
    if(input$category == "All"){
      chosenCat <- unique(df1$category)
    }else{
      chosenCat <- input$category
    }
    p <- df1 %>% 
      rename(firstLetter = input$whichName) %>% 
      filter(category %in% chosenCat) %>% 
      group_by(firstLetter) %>% 
      summarise(n = n()) %>% 
      filter(firstLetter != "") %>% 
      top_n(input$num, n) %>% 
      mutate(firstLetter = fct_reorder(firstLetter, n)) %>% 
      ggplot( aes(x = firstLetter, y = n)) +
      geom_col(fill = "navyblue") +
      coord_flip()+
      theme_bw()+
      labs(title = "Which letters are the most popular for laureates' names to start with?",
           y="Number of laureates",
           x="Letter")
    
    
    p
  })%>% bindCache(input$whichName, input$category, input$num)
  
  
  output$plot2 <- plotly::renderPlotly({
  data <-  df2 %>% filter(category %in% input$kategoria)
  colnames(data) <- c("Year","Category","Continent","Prize")
  data$Continent <- factor(data$Continent, levels = 
                                c("Africa","Asia","Europe",
                                  "North America","Oceania",
                                  "South America", "Not given"))
      
  if(length(input$kategoria) == 0){
    plot <- ggplot()
  }else if(input$wykres == "Boxplot"){
    plot <- data %>% ggplot(aes(x = Continent, y = Prize/1000000)) +
      geom_boxplot(fill = "grey") +
      labs(
        x="Continent",
        y="Prize amount [milion $]",
        title = "Prize amount for every continent")
  }else if(input$wykres == "Bar graph"){
     plot <- data %>% group_by(Continent) %>% 
      summarise(Total = sum(Prize)/1000000000) %>% 
      ggplot(aes(x=Continent, y = Total)) + 
       geom_col(fill = "navyblue") +
       labs(x = "Continent", 
            y = "Total amount of prizes [billion $]",
            title = "Total amount of prizes for every continent")
  }else if(input$wykres == "Line graph"){
    plot <- data %>% group_by(Year) %>% 
      summarise(Mean = mean(Prize)/1000000) %>% 
      ggplot(aes(x=Year, y=Mean)) +
      geom_line() +
      labs(x = "Year",
           y = "Average prize [milion $]",
           title = "Average prize through years")
  }
  plot + theme_bw()
  }) %>% bindCache(input$kategoria, input$wykres)
  
  
}

app_ui <- navbarPage(
  title="Data analysis: Laureates of Nobel Prizes",
  tabPanel("Most popular first letters", ui1, icon = icon("arrow-alt-circle-right")),
  tabPanel("Prize amount", ui2, icon = icon("arrow-alt-circle-right")),
  theme = bslib::bs_theme(bootswatch = "cosmo"),
  footer = shiny::HTML("
                <footer class='text-center text-sm-start' style='width:100%;'>
                <hr>
                <p class='text-center' style='font-size:12px;'>
                  Authors: Marcel Witas, Łukasz Tomaszewski
                </p>
                </footer>
                ")
)

shinyApp(ui = app_ui, server = server)