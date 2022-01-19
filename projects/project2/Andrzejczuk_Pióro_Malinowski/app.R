################          PROJEKT 2 - Maja Andrzejczuk, Andrzej Pióro, Aleksander Malinowski - kod zamieszczamy jedynie w celach pogladowych ze wzgledu na wrazliwe dane (bez csvek i rdsów) ################
################          LINK DO DZIALAJACEJ APLIKACJI W PLIKU link2shinyapp.txt ################
library(shiny)
library(ggplot2)
library(dplyr)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library(plotly)
library(tidyverse)
library(ggwordcloud)
library(shinydashboard)
library(tm)
library(tidytext)
library(qdap)
library(zoo)
library(tidyr)
library(stringi)
library(reshape2)
defaultW <- getOption("warn")
options(warn = -1)
server <- function(input, output, session) {
  # complete_df <- readRDS("complete_df.rds")
  animation_df <- readRDS("animation_df.rds")
  sentOnly_df <- readRDS("sentOnly_df.rds")
  df_temp <- reactive(sentOnly_df %>% {if (input$osoba != 'all') filter(.,owner == input$osoba) else filter(.,owner == 'a' | owner == 'b' | owner == 'c')})
  
  
  text <- sentOnly_df$content
  docs <- Corpus(VectorSource(text))
  docs <- docs %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace)
  docs <- tm_map(docs, content_transformer(tolower))
  
  x <- apply_as_df(docs,freq_terms,top = 200)
  df2 <- data.frame(word = x$WORD,freq=x$FREQ)
  
  top20 <- head(df2, 20)
  top20$word <- reorder(top20$word, top20$freq)
  top20 <- as.data.frame(top20)
  
  df_steps <- read.csv("stepsCombined.csv")
  df2_steps <- df_steps %>%
    mutate(date2 = as.Date(Data, format = "%d/%m/%Y")) %>%
    filter(date2 > "2021-12-24")
  
  x1 <- reactive({
    text <- df_temp()$content
    docs <- Corpus(VectorSource(text))
    docs <- docs %>%
      tm_map(removeNumbers) %>%
      tm_map(removePunctuation) %>%
      tm_map(stripWhitespace)
    docs <- tm_map(docs, content_transformer(tolower))
    x <- apply_as_df(docs,freq_terms,top = 3000)
  })
  output$plot2 <- renderPlot({
    x <- x1()
    df2 <- data.frame(word = x$WORD,freq=x$FREQ) %>%
      filter(str_length(word) >= input$dlg)  %>%
      filter(freq > input$frequ)
    rownames(df2) <- df2$word
    df2$word <- reorder(df2$word, df2$freq)
    df2 <- as.data.frame(df2)
    set.seed(42)
    p2 <- ggplot(head(df2, 50), aes(label = word, size = freq)) +
      geom_text_wordcloud_area(rm_outside = TRUE,shape = "square") +
      scale_size_area(max_size = 30) +
      theme_minimal() +labs(y= "", x = "")
    
    p2
  })
  output$plot1 <- renderPlot({
    x <- x1()
    df2 <- data.frame(word = x$WORD,freq=x$FREQ) %>%
      filter(str_length(word) >= input$dlg)  %>%
      filter(freq > input$frequ)
    rownames(df2) <- df2$word
    df2$word <- reorder(df2$word, df2$freq)
    df2 <- as.data.frame(df2)
    top20 <- head(df2, 15)
    top20$word <- reorder(top20$word, top20$freq)
    top20<- as.data.frame(top20)
    
    
    
    p <- ggplot(top20, aes(x = word, y = freq)) +
      geom_bar(stat="identity", show.legend = FALSE, fill = "#222d32") +
      coord_flip() +theme(legend.position = "none",
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.background = element_blank(),
                          axis.line=element_blank(),
                          text  = element_text(size = 20)) +
      labs(title = "", x = "", y = "") +
      scale_y_continuous(expand = c(0,0)) 
    
    
    
    p
    
  })
  output$plot1Andrzej <- renderPlotly({
    
    if (input$timePeriod == "Hours"){
      time_period <- c("owner", "hour")
    }
    else if (input$timePeriod == "Weekdays"){
      time_period <- c("owner", "weekday")
    }
    else{
      time_period <- c("owner", "year_mon")
    }
    
    time_group_send_df <- sentOnly_df %>%
      group_by_at(time_period) %>%
      count() %>%
      ungroup() %>%
      group_by(owner) %>%
      mutate(relative_n = n/sum(n))
    
    if (input$timePeriod == "Hours"){
      p_abs <-ggplot(time_group_send_df, aes(x=hour, y=n, fill=owner))+
        geom_col(position = "dodge")+
        scale_y_continuous( expand = expansion(mult = c(0, .05)))
      
      p_rel <-ggplot(time_group_send_df, aes(x=hour, y=relative_n, fill=owner))+
        geom_col(position = "dodge")  +
        scale_y_continuous( expand = expansion(mult = c(0, .05)))
      
    }
    else if (input$timePeriod == "Weekdays"){
      p_abs <-ggplot(time_group_send_df, aes(x=weekday, y=n, fill=owner))+
        geom_col(position = "dodge")+
        scale_y_continuous( expand = expansion(mult = c(0, .05)))
      
      p_rel <-ggplot(time_group_send_df, aes(x=weekday, y=relative_n, fill=owner))+
        geom_col(position = "dodge") +
        scale_y_continuous( expand = expansion(mult = c(0, .05)))
      
    }
    else{
      p_abs <-ggplot(time_group_send_df, aes(x=year_mon, y=n, fill=owner))+
        geom_col(position = "dodge")+
        scale_y_continuous( expand = expansion(mult = c(0, .05)))
      
      p_rel <-ggplot(time_group_send_df, aes(x=year_mon, y=relative_n, fill=owner))+
        geom_col(position = "dodge")+
        scale_y_continuous( expand = expansion(mult = c(0, .05)))
    }
    
    
    # draw the histogram with the specified number of bins
    if (input$absoluteOrRelative == "Absolute"){
      toWebGL(ggplotly(p_abs + scale_fill_manual(values = c("#1F77B4","#FF7F0E","#2CA02C")) + labs(title = "", x = "", y = "messages") + theme_minimal()))
    }
    else{
      toWebGL(ggplotly(p_rel + scale_fill_manual(values = c("#1F77B4","#FF7F0E","#2CA02C")) + labs(title = "", x = "", y = "messages' share") + theme_minimal()))
    }
  })
  output$plot2Andrzej <- renderPlotly({
    
    
    toWebGL(animation_df %>%
              # plot_ly(
              #     x = ~hour,
              #     y = ~relative_n,
              #     frame = ~start_date,
              #     type = 'bar',
              #     showlegend = F
              # )
              do(p=plot_ly(., x = ~hour, y = ~relative_n, type = "bar", frame=~start_date)) %>%
              
              subplot(nrows = 1, shareX = FALSE, shareY = TRUE) %>%
              animation_opts(
                transition=128, easing="linear", frame=128, redraw = FALSE) %>%
              animation_button(x = 0.05, y = 0.05) %>%
              animation_slider(currentvalue = list(prefix = "Start of window: ", font = list(color="purple"))) %>%
              layout(
                yaxis = list(
                  title = "Portion of messages in time period"
                ),
                
                showlegend=FALSE))
    
  })
  output$plot1Alek <- renderPlotly({
    pSteps <- ggplot(df2_steps,aes(x = date2,y = Kroki,fill=owner)) +
      geom_bar(stat="identity", position=position_dodge()) +
      scale_y_continuous( expand = expansion(mult = c(0, .05))) +
      theme_minimal() +
      scale_x_date(breaks = unique(df2_steps$date2)) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
      labs(x = "Date", y = "Steps") +
      scale_fill_manual(values = c("#1F77B4","#FF7F0E","#2CA02C"))
    toWebGL(ggplotly(pSteps))
  })
  
  output$plot2Alek <- renderPlotly({
    df2_messages <- sentOnly_df %>% 
      mutate(date2 = as.Date(sentOnly_df$date)) %>% 
      filter(date2 > "2021-12-25") %>% 
      group_by(date2,owner) %>% 
      count()
    if(input$chooseOwner == 'all'){
      df2_combined <- df2_steps %>% 
        right_join(df2_messages,by="date2") %>% 
        rename(Messages = n) %>% 
        rename(Steps = Kroki) %>% 
        filter(owner.x == owner.y) %>% 
        select(-c("owner.y")) %>% 
        rename(owner = owner.x)
      pSteps2 <- ggplot(df2_combined,aes(y = Steps, x = Messages, color = owner)) +
        geom_point() +
        theme_minimal() +
        scale_color_manual(values = c("#1F77B4","#FF7F0E","#2CA02C"))
      toWebGL(ggplotly(pSteps2))
    }else{
      df2_messages <- df2_messages %>% 
        filter(owner == input$chooseOwner)
      df2_combined <- df2_steps %>% 
        inner_join(df2_messages,by="date2") %>% 
        rename(Messages = n) %>% 
        rename(Steps = Kroki)
      df2_combined <- melt(df2_combined[,c('date2','Steps','Messages')],id.vars=1)
      
      pSteps2 <- ggplot(df2_combined,aes(x = date2, y = value,fill=variable)) +
        geom_bar(stat = "identity",position = "dodge") +
        scale_y_log10() +
        scale_x_date(breaks = unique(df2_combined$date2)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
        labs(x = "Date") +
        scale_fill_manual(values = c("#1F77B4","#FF7F0E","#2CA02C"))
      
      toWebGL(ggplotly(pSteps2))
    }
  })
}


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "wordsused",
            fluidRow(
              box(
                width = 12,
                sliderInput("frequ", "Minimum word frequency:",min = 1,  max = 5000, value = 2),
                selectInput(
                  inputId = "osoba",
                  label = "Choose person:",
                  choices = c("a", "b", "c","all")
                ),
                selectInput("dlg",
                            label = "Minimal word length:",
                            choices = c("3", "4", "5", "6", "7","8","9"), 
                            selected = "6"
                )
              )
            ),
            fluidRow(
              column(width = 12,
              box(
              title = "Wordcloud",
              width = NULL,
              plotOutput("plot2")
            ),
            box(
              title = "The Most Popular Words",
              width = NULL,
              plotOutput("plot1")
            )
            )),
    ),
    tabItem(tabName = "quantity",
            fluidRow(
              column(width = 12,
              box(
                title = "Texting intensity, by time period",width=NULL,
                selectInput(inputId = "timePeriod",
                            label="Time period",
                            choices = list(
                              "Hours",
                              "Weekdays",
                              "Months"
                            ),
                            selected = "Months",
                            multiple = FALSE
                ),
                selectInput(inputId = "absoluteOrRelative",
                            label="Absolute or relative data",
                            choices = list(
                              "Absolute",
                              "Relative"
                            ),
                            selected = "Absolute",
                            multiple = FALSE
                ),
                plotlyOutput("plot1Andrzej")
              ),
              box(
                title = "Messages sent in 30 days window, by hour",
                width=NULL,
                plotlyOutput("plot2Andrzej")
              ))
            )
    ),
    tabItem(tabName = "activity",
            fluidRow(
              column(width = 12,
              box(
                title = "Steps comparision",
                width = NULL,
                plotlyOutput("plot1Alek")
              ),
              box(
                width = NULL,
                title = "Steps vs sent messages",
                selectInput(
                  inputId = "chooseOwner",
                  label = "Choose person:",
                  choices = c("a", "b", "c","all")
                ),
                plotlyOutput("plot2Alek")
              ))
            ))
  ),
  tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
      /* body */
      .content-wrapper, .right-side {

      }
      .box{
          color: #ffffff;
          background:#222d32;
      }
      .box>.box-header {
          color: #ffffff;
      }
      '))))


# We'll save it in a variable `ui` so that we can preview it in the console
ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "Messenger"),
                    dashboardSidebar(
                      sidebarMenu(menuItem("Words used", tabName = "wordsused"),
                                  menuItem("Quantity", tabName = "quantity"),
                                  menuItem("Activity", tabName = "activity"))),
                    body)


# Preview the UI in the console
shinyApp(ui = ui, server = server)
options(warn = defaultW)