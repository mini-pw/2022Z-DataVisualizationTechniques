library(wordcloud2)
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(stringr)
library(tidyverse)
library(stringi)
library(lubridate)
library(shinyWidgets)
library(heatmaply)
library(padr)
library(tidyquant)
library(calendR)
library(data.table)
library(tokenizers)
library(rvest)
library(purrr)
library(ggtext)
# library(emojifont)
# load.emojifont("EmojiOne.ttf")

options(scipen = 50)

# Define UI for application that draws a histogram
uiWordCloud <- fluidPage(
  
  fluidRow(
    column(5, offset = 1, align = "center",
           tags$style(HTML('.js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {
                                background: #0080ff;
                                border-top: 1px solid #0080ff ;
                                border-bottom: 1px solid #0080ff ;}
                                .irs-from, .irs-to, .irs-single { background: #0080ff }')),
           sliderInput("wordsNumber",
                       "Number of words:",
                       min = 3,
                       max = 100,
                       value = 50),
           style = "background-color: #3e4042;
                        vertical-align: middle;
                        padding-top: 10px;"
    ),
    column(5, align = "center",
           radioButtons(inputId = "wordsLen",
                        label = "Minimal words length:",
                        choices = c(2, 3, 4, 5),
                        selected = 5,
                        inline = T),
           style = "background-color: #3e4042;
                    vertical-align: middle;
                    padding-top: 25px;"
    ),
    style = "margin-bottom: 10px;"
  ),
  fluidRow(
    column(10, offset = 1, align = "center",
           h2("Word cloud:"),
           wordcloud2Output("cloud"),
           tags$script(HTML(
             "$(document).on('click', '#canvas', function() {",
             'word = document.getElementById("wcSpan").innerHTML;',
             "Shiny.onInputChange('selected_word', word);",
             "});")),
           style = "background-color: #3e4042;"
    ),
    style = "margin-bottom: 10px;"
  ),
  fluidRow(
    column(10, offset = 1, align = "center",
           h2("The occurrence of the word:"),
           plotlyOutput("plot"),
           style = "background-color: #3e4042;
                    padding: 30px;
                    padding-left: 70px;
                    padding-right: 70px")
  ),
  fluidRow(
    column(10, offset = 1, align = "center",
           h2("Sent by a selected person:"),
           plotOutput("bar1", height = "100%"),
           style = "background-color: #3e4042;
                    padding: 10px;"),
  ),
  fluidRow(
    column(10, offset = 1, align = "center",
           h2("Appears in conversation:"),
           plotOutput("bar2", height = "100%"),
           style = "background-color: #3e4042;
                    padding-bottom: 50px;"
    )
  )
)


################# HEATMAP! #################

heatmappJK <- read.csv("./HeatMap/heatmappJK.csv")
heatmappPW <- read.csv("./HeatMap/heatmappPW.csv")
heatmappKT <- read.csv("./HeatMap/heatmappKT.csv")
heatmapp <- heatmappJK

convosJK <- read.csv("./HeatMap/convosJK.csv")
convosPW <- read.csv("./HeatMap/convosPW.csv")
convosKT <- read.csv("./HeatMap/convosKT.csv")

uiHeatMap <- fluidPage(
  
  fluidRow(
    column(5, offset = 1, align = "center",
           radioButtons(inputId = "type",
                        label = "Type: ",
                        choices = c("Month", "Year"),
                        selected = "Year",
                        inline = T),
           style = "background-color: #3e4042;
                    vertical-align: middle;
                    padding-top: 25px;"
    ),
    column(5, align = "center",
           tags$style(HTML('.js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
                                                  background: #0080ff;
                                                  border-top: 1px solid #0080ff ;
                                                  border-bottom: 1px solid #0080ff ;}
                           .irs-from, .irs-to, .irs-single { background: #0080ff }')),
           sliderTextInput(inputId = "dates", label = "Date:",
                           choices = unique(heatmapp$year), 
                           selected = 2017),
           style = "background-color: #3e4042;
                    vertical-align: middle;
                    padding-top: 10px;"
    ),
    style = "margin-bottom: 10px;"
  ),
  
  fluidRow(
    column(10, offset = 1, align = "center",
           h2("Calendar Heatmap:"),
           shinycssloaders::withSpinner(plotlyOutput("heatmap"), type = 1, color = "#0080ff", size = 2),
           style = "background-color: #3e4042;"
    ),
    style = "margin-bottom: 10px;"
  ),
  
  fluidRow(
    column(10, offset = 1, align = "center",
           h2("Number of messages in conversations:"),
           plotlyOutput("barplot"),
           style = "background-color: #3e4042;
                    padding: 30px;
                    padding-left: 70px;
                    padding-right: 70px")
  )
)

################# Emoji! #################

emojisKacper = data.table(read.csv("Emotions/Kacper_emojis2.csv"))
emojisKacper$time = format(as.POSIXct(emojisKacper$Timestamp / 1000, origin = "1970-01-01"), "%Y-%m-%d %H:%M:%OS3")
emojisKacper$date =as.Date(emojisKacper$time,format="%Y-%m-%d")

emojisJulia = data.table(read.csv("Emotions/Julia_emojis2.csv"))
emojisJulia$time = format(as.POSIXct(emojisJulia$Timestamp / 1000, origin = "1970-01-01"), "%Y-%m-%d %H:%M:%OS3")
emojisJulia$date =as.Date(emojisJulia$time,format="%Y-%m-%d")

emojisPiotr = data.table(read.csv("Emotions/Piotr_emojis2.csv"))
emojisPiotr$time = format(as.POSIXct(emojisPiotr$Timestamp / 1000, origin = "1970-01-01"), "%Y-%m-%d %H:%M:%OS3")
emojisPiotr$date =as.Date(emojisPiotr$time,format="%Y-%m-%d")

wordOrderKacper = data.table(read.csv("Emotions/Kacper_WordOrder.csv"))
wordOrderJulia = data.table(read.csv("Emotions/Julia_WordOrder.csv"))
wordOrderPiotr = data.table(read.csv("Emotions/Piotr_WordOrder.csv"))

uiEmotionsText <- fluidPage(
  fluidRow(
    column(10, offset=1,
           titlePanel(textOutput("whatemotions")),
           fluidRow(
             column(8, align="center", offset = 2,
                    uiOutput("uidateRangeEmojis"),
             )
           ),
           shinycssloaders::withSpinner(plotOutput("emoplot")),
           style = "background-color: #3e4042;"
    ),
    style="margin-bottom: 10px;"
  ),
  fluidRow(
    column(10,offset=1,
           titlePanel(textOutput("whatsay")),
           textInput("textInput", "", "hej", width = "100%"),
           shinycssloaders::withSpinner(plotOutput("wordorderplot")),
           style = "background-color: #3e4042;"
    ),
    style="margin-bottom: 10px;"
  )
  
)



server <- function(input, output, session) {
  reactives <- reactiveValues() 
  observeEvent(input$buttonJ, {
    reactives$person <- "Julia"
  })
  observeEvent(input$buttonP, {
    reactives$person <- "Piotr"
  })
  observeEvent(input$buttonK, {
    reactives$person <- "Kacper"
  })
  
  output$personSelected <- renderText({
    if(is.null(reactives$person)){
      reactives$person <- "Julia"
    }
    paste0("Selected person: ", reactives$person)
  })
  
  output$description <-renderText({ 
    if(input$navbarPage == "Word Cloud"){
      return("On this tab you can analyze the most common words in our Facebook conversations.
        There are three buttons below. By clicking a given button, you can choose
        the person whose conversations you want to analyze. Then you can set
        the word cloud options. You choose how many words are to appear in it
        and the minimum length of the considered words. If you click on a word
        in the cloud, you will receive detailed information about it.
        How many times certain word has been used in a given year,
        how many times a selected person has used a given word
        and in what part of the conversation a given word appears. ")
    }else if(input$navbarPage == "Calendar"){
      return("On this tab you can analyze the frequency of sending Facebook messages.
        You can choose the person whose conversations you want to analyze, 
        by clicking a given button. By selecting Type, you can 
        change calendar to either monthly or yearly display. Slider lets you choose the month
        or the year that should be displayed. By clicking at a day, another plot will appear below. 
        It informs about the number of messages sent and recieved in all the conversations. ")
    }else{
      return("On this tab you can get to know us better. On the first chart you can see which emoticons appear most often in our conversations in a selected period of time. On the second plot you can type a word or phrase and our plot will tell you what are the most common words occuring after the last one from the input in messages of a selected person.")
    }
  })
  
  output$title <- renderText({
    input$navbarPage
  })
  
  output$cloud <- renderWordcloud2({
    
    sizes = c(1, 1.4, 1.2, 0.9, 0.6)
    
    if(reactives$person == "Piotr"){
      folder = "data_person1"
    } else if(reactives$person == "Kacper"){
      folder = "data_person2"
    } else if(reactives$person == "Julia"){
      folder = "data_person3"
    }
    
    dfFreq <- read.csv(paste0("./WordCloud/", folder,"/outputFreqDF.csv"),
                       encoding = "UTF-8")
    
    colnames(dfFreq)[1] <- "word"
    
    dfFreq <- dfFreq %>% filter(str_length(word) >= input$wordsLen)
    wordcloud2(head(dfFreq, input$wordsNumber),
               size = sizes[as.numeric(input$wordsLen)],
               color=rep("#0080ff", input$wordsNumber),
               backgroundColor="#3e4042")
    
  })
  
  output$plot <- renderPlotly({
    
    if(length(input$selected_word) > 0){
      
      if(reactives$person == "Piotr"){
        folder = "data_person1"
      } else if(reactives$person == "Kacper"){
        folder = "data_person2"
      } else if(reactives$person == "Julia"){
        folder = "data_person3"
      }
      
      dfYears <- read.csv(paste0("./WordCloud/", folder,"/outputYearsDF.csv"),
                          encoding = "UTF-8")
      colnames(dfYears)[1] <- "word"
      
      dfYears$year <- as.character(dfYears$year)
      
      selectedWord <- unlist(strsplit(input$selected_word, ":"))[1]
      df <- dfYears %>% filter(word == selectedWord)
      
      p <- ggplot(df, aes(x = year, y = count, fill = count,
                          text = paste(paste("Year: ", year),
                                       paste("Count:", count),
                                       sep = "\n"))) +
        geom_col() +
        labs(
          title = paste0("Analysis of word: ", selectedWord),
          x = "Year",
          y = "Count"
        ) +
        scale_fill_gradient(
          low = "#0080ff",
          high = "#aa00ff",
        ) +
        theme(
          plot.background = element_rect("#3e4042", colour = "#3e4042",
                                         color = "#3e4042"),
          text = element_text(colour = "white"),
          axis.text = element_text(colour = "white"),
          axis.line = element_line(colour = "white"),
          axis.ticks = element_line(colour = "white"),
          panel.background = element_rect("#3e4042", colour = "#3e4042",
                                          color = "#3e4042"),
          legend.position = "none"
        )
      p <- ggplotly(p, tooltip = "text")
      
      p <- p %>% config(displayModeBar = FALSE)
      
    }
    
  })
  
  output$bar1 <- renderPlot({
    
    if(length(input$selected_word) > 0){
      
      if(reactives$person == "Piotr"){
        folder = "data_person1"
      } else if(reactives$person == "Kacper"){
        folder = "data_person2"
      } else if(reactives$person == "Julia"){
        folder = "data_person3"
      }
      
      dfMe <- read.csv(paste0("./WordCloud/", folder,"/outputMeDF.csv"),
                       encoding = "UTF-8")
      colnames(dfMe)[1] <- "word"
      
      selectedWord <- unlist(strsplit(input$selected_word, ":"))[1]
      df <- dfMe %>% filter(word == selectedWord)
      df <- pivot_longer(df, c("mine", "not.mine"))
      all <- df[[1,3]] + df[[2,3]]
      
      ggplot(df, aes(x = word, y = value, fill = name)) + 
        geom_bar(position="fill", stat="identity") + 
        coord_flip() +
        geom_text(aes(label = paste0(round(100*value/all, 2),"%")), 
                  position = position_fill(vjust = 0.5), size = 6,
                  colour = "white") +
        theme_void() +
        theme(
          legend.position = "bottom",
          legend.text = element_text(color = "white", size = 15),
          plot.background = element_rect("#3e4042", colour = "#3e4042",
                                         color = "#3e4042"),
          legend.title = element_blank()
        ) +
        scale_fill_manual(labels = c("Sent", "Received"), values = c("#0080ff", "#aa00ff"))
    }
    
  }, height = 100, alt = NULL)
  
  output$bar2 <- renderPlot({
    
    if(length(input$selected_word) > 0){
      
      if(reactives$person == "Piotr"){
        folder = "data_person1"
      } else if(reactives$person == "Kacper"){
        folder = "data_person2"
      } else if(reactives$person == "Julia"){
        folder = "data_person3"
      }
      
      dfConversations <- read.csv(paste0("./WordCloud/", folder,"/outputConversationstDF.csv"),
                                  encoding = "UTF-8")
      colnames(dfConversations)[1] <- "word"
      
      selectedWord <- unlist(strsplit(input$selected_word, ":"))[1]
      df <- dfConversations %>% filter(word == selectedWord)
      df <- pivot_longer(df, c("convsWITH", "convsWITHOUT"))
      all <- df[[1,3]] + df[[2,3]]
      ggplot(df, aes(x = word, y = value, fill = name)) + 
        geom_bar(position="fill", stat="identity") + 
        coord_flip() +
        geom_text(aes(label = paste0(round(100*value/all, 2),"%")), 
                  position = position_fill(vjust = 0.5), size = 6,
                  color = "white") +
        theme_void() +
        theme(
          legend.position = "bottom",
          legend.text = element_text(color = "white", size = 15),
          plot.background = element_rect("#3e4042", colour = "#3e4042",
                                         color = "#3e4042"),
          legend.title = element_blank()
        ) +
        scale_fill_manual(labels = c("Appears", "Doesn't appear"), values = c("#0080ff", "#aa00ff"))
    }
    
  }, height = 100, alt = NULL)
  
  
  ############# HEATMAP! ############# 
  
  observeEvent({
    input$type
    reactives$person},
    {
      
      if(reactives$person == "Piotr"){
        heatmapp <- heatmappPW
      } else if(reactives$person == "Julia"){
        heatmapp <- heatmappJK
      } else if(reactives$person == "Kacper"){
        heatmapp <- heatmappKT
      }
      
      
      if(input$type == "Year"){
        updateSliderTextInput(session = session, inputId="dates", 
                              selected = 2017,
                              choices = unique(heatmapp$year))
      }else if(input$type == "Month"){
        ind <- which.max(heatmapp$count > 0)
        mindate <- heatmapp[ind, "date"]
        minmonth <- floor_date(ymd(mindate), 'month')
        heatmapp %>% 
          filter(date >= minmonth) -> trial
        
        updateSliderTextInput(session = session, inputId="dates",
                              choices = unique(trial$yearmonth), 
                              selected = trial$yearmonth[1]
        )
      }
    })
  
  
  output$heatmap <- renderPlotly({
    
    if(reactives$person == "Piotr"){
      heatmapp <- heatmappPW
    } else if(reactives$person == "Julia"){
      heatmapp <- heatmappJK
    } else if(reactives$person == "Kacper"){
      heatmapp <- heatmappKT
    }
    
    if(input$type == "Year"){
      heatmapp %>% 
        filter(year(date) == input$dates) -> trial
      
      req(nrow(trial) > 0)
      
      p <- trial %>%
        ggplot(aes(reorder(weekdayf, weekday), -week, fill = count, 
                   text = paste(paste("Date:", date),
                                paste("Count:", count),
                                sep = "\n"))) + 
        geom_tile(colour = "white") +
        scale_fill_gradient(low = "#bebebe", high = "#004eff") +
        #geom_text(aes(label=day(date))) +
        facet_wrap(reorder(trial$yearmonth, trial$yearmonthnum), 
                   nrow = 3, ncol = 4, scales = "free", shrink = FALSE) +
        labs(x = element_blank(), y = element_blank(),
             fill = "Sent \nmessages") +
        theme(
          legend.text = element_text(color = "white", size = 10),
          legend.title = element_text(color = "white", size = 13),
          legend.background = element_rect(fill="#3e4042"),
          legend.key.size = unit(0.5, 'cm'),
          plot.background = element_rect("#3e4042", colour = "#3e4042",
                                         color = "#3e4042"),
          panel.background = element_rect(fill = '#3e4042'),
          panel.spacing.y = unit(0.1 , "lines"),
          strip.background = element_rect(fill="#3e4042"),
          strip.text = element_text(colour = 'white'),
          axis.text.x = element_text(colour="white"),
          axis.text.y = element_blank()
        )
      
      
      p <- ggplotly(p=p, tooltip = "text") %>% config(displayModeBar = FALSE)
    }
    else{
      ind <- which.max(heatmapp$count > 0)
      mindate <- heatmapp[ind, "date"]
      minmonth <- floor_date(ymd(mindate), 'month')
      heatmapp %>% 
        filter(date >= minmonth) %>% 
        filter(yearmonth == input$dates) -> trial
      
      req(nrow(trial) > 0)
      
      tit <- paste("Data for ", reactives$person, ", ", input$dates, sep = "")
      
      p <- trial %>% 
        ggplot(aes(reorder(weekdayf, weekday), -week, fill = count,
                   text = paste(paste("Date:", date),
                                paste("Count:", count),
                                sep = "\n"))) + 
        geom_tile(aes(width = 0.5, height = 0.5), colour = "white") +
        scale_fill_gradient(low = "#bebebe", high = "#004eff") +
        geom_text(aes(label=day(date))) +
        scale_x_discrete(position = "top") +
        labs(x = element_blank(), y = element_blank(), title = tit,
             fill = "Sent \nmessages\n") +
        theme(
          plot.background = element_rect("#3e4042", colour = "#3e4042",
                                         color = "#3e4042"),
          panel.background = element_rect(fill = '#3e4042'),
          axis.text.x = element_text(colour="white"),
          axis.text.y = element_blank(),
          legend.text = element_text(color = "white", size = 10),
          legend.background = element_rect(fill="#3e4042"),
          legend.title = element_text(color = "white", size = 13),
          plot.title = element_text(color = "white")
        )
      
      p <- ggplotly(p=p, tooltip = "text") %>% config(displayModeBar = FALSE)
    }
    
    p %>% event_register("plotly_click")
    
  })
  
  observeEvent(event_data("plotly_click"),
               {
                 clickData <- event_data("plotly_click")
                 
                 output$barplot <- renderPlotly({
                   day1 <- clickData[[3]]
                   week1 <- -as.numeric(clickData[4])
                   
                   if(input$type == "Year"){
                     year1 <- isolate(as.numeric(input$dates))
                   } else {
                     year1 <- isolate(year(as.yearmon(input$dates)))
                     #floor(as.numeric(input$dates)/100))
                   }
                   
                   convoperson <- isolate(reactives$person)
                   if(convoperson == "Piotr"){convos <- convosPW}
                   else if(convoperson == "Julia"){convos <- convosJK}
                   else{convos <- convosKT}
                   
                   bardata <- convos %>% 
                     filter(year == year1 & weekday == day1 & week == week1) %>% 
                     mutate(me = ifelse(me == "yes", "sent", "received"))
                   
                   group.colors <- c("sent" = "#0080ff", "received" = "#aa00ff")
                   
                   
                   if(nrow(bardata) == 0){
                     plotly_empty(type = "scatter", mode = "markers") %>% 
                       layout(plot_bgcolor='#3e4042') %>% 
                       layout(paper_bgcolor='transparent') %>% config(displayModeBar = FALSE)
                     
                   } else {
                     date1 <- bardata$date[[1]]
                     tit <- paste("Data for ", convoperson, ", ", date1, sep = "")
                     
                     p <- bardata %>% 
                       ggplot(aes(x = convoname, y = count, fill = me,
                                  text = paste(paste("Conversation:", convoname),
                                               paste("Count:", count),
                                               sep = "\n"))) +
                       geom_bar(stat='identity', position = "dodge") +
                       scale_fill_manual(values = group.colors) +
                       labs(x = "Conversation name", y = "Count", title = tit,
                            fill = "Messages") +
                       theme(
                         plot.background = element_rect("#3e4042", colour = "#3e4042",
                                                        color = "#3e4042"),
                         panel.background = element_rect("#3e4042", colour = "#3e4042",
                                                         color = "#3e4042"),
                         text = element_text(colour = "white"),
                         axis.text.y = element_text(colour = "white"),
                         axis.text.x = element_text(colour = "white", angle = 45),
                         axis.line = element_line(colour = "white"),
                         axis.ticks = element_line(colour = "white"),
                         legend.text = element_text(color = "white", size = 10),
                         legend.background = element_rect(fill="#3e4042"),
                         legend.title = element_text(color = "white", size = 13),
                       )
                     p <- ggplotly(p=p, tooltip = "text") %>% config(displayModeBar = FALSE)
                   }
                   
                 })
                 
               }
  )
  
  ################## EMOJI! ################## 
  
  
  chooseEmojiDF <- function(person){
    if(person == "Julia"){
      return(emojisJulia)
    }else if(person == "Piotr"){
      return(emojisPiotr)
    }else{
      return(emojisKacper)
    }
  }
  
  output$whatemotions <- renderText({
    paste0("What were the emotions in ",reactives$person,"'s conversations?")
  })
  
  output$uidateRangeEmojis <- renderUI({
    emojis = chooseEmojiDF(reactives$person)
    div(
      tags$style('.input-group-sm>.form-control {font-size: 50px; height:100px}'),
      dateRangeInput(
        "dateRangeEmojis",
        "",
        start = min(emojis$date),
        end = max(emojis$date),
        min = min(emojis$date),
        max = max(emojis$date),
        width = "100%"
        
      ))
  })
  
  
  output$emoplot <- renderPlot({
    dates = input$dateRangeEmojis
    emojis = chooseEmojiDF(reactives$person)
    filtered = emojis[ dates[1]< date][date < dates[2]]
    filtered = filtered[,.(count = .N, label=first(label)), by=.(Emoji)]
    filtered = filtered[order(count, decreasing = TRUE)[1:9]]
    filtered = filtered[!is.na(count)]
    
    offset <- max(filtered$count) / 20
    
    p <- ggplot(data=filtered, aes(x=Emoji, y=count)) +
      geom_bar(stat="identity", fill="#0080ff", color = "#0080ff") +
      geom_text(aes(label=count), position=position_dodge(width=0.9), vjust= -0.5, color="#0080ff", size=10) +
      geom_richtext(aes(y = count + 3.3*offset, label=label), fill = NA, label.color = NA,
                    label.padding = grid::unit(rep(0, 4), "pt")
      ) +
      # geom_text(aes(label=Emoji),family = "EmojiOne", fontface = "bold",  vjust= -0.5, color="yellow", size=20) +
      theme_minimal()+
      ylim(0, max(filtered$count)*1.3)+
      theme(axis.title.y =element_blank(),
            axis.title.x =element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            # axis.text.x = element_text(size=20),
            axis.text.x = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_rect("#3e4042", colour = "#3e4042",
                                           color = "#3e4042"),
            text = element_text(colour = "white"),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            panel.background = element_rect("#3e4042", colour = "#3e4042",
                                            color = "#3e4042"),
            legend.position = "none")
    p
  })
  
  chooseWordOrderDF <- function(person){
    if(person == "Julia"){
      return(wordOrderJulia)
    }else if(person == "Piotr"){
      return(wordOrderPiotr)
    }else{
      return(wordOrderKacper)
    }
  }
  
  output$whatsay <- renderText({
    paste0("What would ",reactives$person," say next?")
  })
  
  output$wordorderplot <- renderPlot({
    text = tolower(input$textInput)
    text = stri_trans_general(text, "Latin-ASCII")
    words = tokenize_words(text)[[1]]
    if(length(words) == 0){
      return()
    }
    last_word = words[length(words)]
    wordOrder = chooseWordOrderDF(reactives$person)
    filtered = wordOrder[Before == last_word]
    filtered = filtered[order(Probability, decreasing = TRUE)[1:10]]
    filtered = filtered[!is.na(After)]
    p <- ggplot(data=filtered, aes(x=reorder(After,Probability), y=Probability)) +
      geom_bar(stat="identity", fill="#0080ff", color = "#0080ff") +
      theme_minimal()+
      coord_flip() +
      theme(axis.title.y =element_blank(),
            axis.title.x =element_text(size=20),
            axis.text.x = element_text(size=20),
            axis.text.y = element_text(size=20),
            plot.background = element_rect("#3e4042", colour = "#3e4042",
                                           color = "#3e4042"),
            text = element_text(colour = "white"),
            axis.text = element_text(colour = "white"),
            panel.background = element_rect("#3e4042", colour = "#3e4042",
                                            color = "#3e4042"),
            legend.position = "none")
    p
  })
  
  
}

header <- fluidPage(
  tags$head(
    tags$style(HTML("
          hr {border-top: 1px solid #FFFFFF;}
          body {
          background-color: #18191a;
          color: white;
        }"))
  ),
  
  # Application title
  titlePanel(
    h1(textOutput("title"), align = "center", style = "margin-bottom: 30px;")
  ),
  hr(),
  
  fluidRow(
    column(10, offset = 1,
           p(textOutput("description")),
           style = "background-color: #3e4042;
                        padding-top: 10px;
                        padding-left: 20px;"
    ),
    style = "margin-bottom: 10px;
                 padding-left: 13.5px;
                 padding-right: 13.5px;"
  ),
  fluidRow(
    column(10, offset = 1, align = "center",
           actionButton("buttonP", "Piotr", width = "20%", style = "margin-right: 10px"),
           actionButton("buttonK", "Kacper", width = "20%"),
           actionButton("buttonJ", "Julia", width = "20%", style = "margin-left: 10px")),
    style = "margin-bottom: 10px;"
  ),
  fluidRow(
    column(10, offset = 1, align = "center",
           h4(textOutput("personSelected")),
           style = "background-color: #3e4042;
                    margin-bottom: 10px;
                    padding-bottom: 15px;"),
    style = "padding-left: 13.5px;
                 padding-right: 13.5px;"),
)

app_ui <- navbarPage(
  id = "navbarPage",
  title = "Messenger data analisys",
  header = header,
  tabPanel("Word Cloud", uiWordCloud),
  tabPanel("Calendar", uiHeatMap),
  tabPanel("Know Better", uiEmotionsText),
  theme = bslib::bs_theme(bootswatch = "cosmo")
)

# Run the application 
shinyApp(app_ui, server)

