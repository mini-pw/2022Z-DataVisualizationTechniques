library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(stringi)
library(plotly)
library("DT")

get_plot_style <- function(){
  text_color <- '#e4e6eb'
  plot_color <- '#358AF8'
  theme(panel.background = element_rect(fill = "transparent", colour = NA),  
        plot.background = element_rect(fill = "transparent", colour = NA),
        axis.text.x = element_text(color = text_color),
        axis.text.y = element_text(color = text_color),
        axis.title.x = element_text(color = text_color),
        axis.title.y = element_text(color = text_color),
        legend.background = element_rect(fill = 'transparent'),
        legend.text = element_text(color = text_color),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(color = text_color))
}

shinyServer(function(input, output, session) {
    
    output$znajomi <- plotly::renderPlotly({
        
        directory <- paste('data/', input$user, '/friends.json', sep = '')
        
        df <- jsonlite::fromJSON(directory)
        df <- data.frame(name = unlist(df[[1]])[1:(length(unlist(df[[1]]))/2)], 
                         timestamp = unlist(df[[1]])[(length(unlist(df[[1]]))/2 + 1):length(unlist(df[[1]]))], stringsAsFactors = FALSE) %>% 
            mutate(plec = ifelse(endsWith(word(name, 1), "a"), "kobieta", "mężczyzna")) %>%
            mutate(rokDodania = format(as.POSIXct(strtoi(timestamp), origin = "1970-01-01"), "%Y"))
        
        p <- df %>% ggplot(aes(x = factor(rokDodania), fill = factor(plec))) + 
            geom_bar(position = position_dodge()) +
            xlab("Rok") +
            ylab("Przyrost znajomych") +
            get_plot_style() +
            scale_fill_manual(values=c("#358AF8", "#3e4042"))
            guides(fill=guide_legend(title="Płeć"))
        plotly::ggplotly(p) %>% 
          plotly::style(hoverinfo = 'none') %>% 
          config(displayModeBar = F)
    })
    
    output$wiadLen <- renderPlotly({
        
        directory <- paste('data/', input$user, sep = '');
        
        if(input$lenDaysOrHours == 'h'){
            directory <- paste(directory, 'dlugoscgodzina.csv', sep = '/');
            data <- read.csv2(directory, sep=';');
            p <- data %>% 
                    ggplot(aes(x = factor(godz), y = count)) +
                    geom_col(position = position_dodge(), fill = "#358AF8") +
                    xlab('Godzina') +
                    ylab('') +
                    ggtitle('Średnia długość wysłanych wiadomości w znakach') +
                    get_plot_style()
            
            plotly::ggplotly(p) %>% 
              plotly::style(hoverinfo = 'none') %>% 
              config(displayModeBar = F)
        }
        else{
            directory <- paste(directory, 'dlugoscdzien.csv', sep = '/');
            data <- read.csv2(directory, sep=';');
            data$dzien <- factor(data$dzien, levels = c("poniedziałek","wtorek","środa","czwartek","piątek","sobota","niedziela"));
            
            p <- data %>% 
              ggplot(aes(x = dzien, y = total)) +
              geom_col(position = position_dodge(), fill = "#358AF8") +
              xlab('Dzień tygodnia') +
              ylab('') +
              ggtitle('Średnia długość wysłanych wiadomości w znakach') +
              get_plot_style()
            
            plotly::ggplotly(p) %>% 
              plotly::style(hoverinfo = 'none') %>% 
              config(displayModeBar = F)
        }
    })
    
    output$wiadCount <- renderPlotly({
        
        directory <- paste('data/', input$user, sep = '');
        
        if(input$countDaysOrHours == 'h'){
            directory <- paste(directory, 'liczbagodzina.csv', sep = '/');
            data <- read.csv2(directory, sep=';');
            
            p <- data %>% 
              ggplot(aes(x = factor(godz), y = count)) +
              geom_col(position = position_dodge(), fill = "#358AF8") +
              xlab('Godzina') +
              ylab('') +
              ggtitle('Liczba wysłanych wiadomości') +
              get_plot_style()
            
            plotly::ggplotly(p) %>% 
              plotly::style(hoverinfo = 'none') %>% 
              config(displayModeBar = F)
        }
        else{
            directory <- paste(directory, 'liczbadzien.csv', sep = '/');
            data <- read.csv2(directory, sep=';');
            data$dzien <- factor(data$dzien, levels = c("poniedziałek","wtorek","środa","czwartek","piątek","sobota","niedziela"));
            
            p <- data %>% 
              ggplot(aes(x = dzien, y = total)) +
              geom_col(position = position_dodge(), fill = "#358AF8") +
              xlab('Dzień tygodnia') +
              ylab('') +
              ggtitle('Liczba wysłanych wiadomości') +
              get_plot_style()
            
            plotly::ggplotly(p) %>% 
              plotly::style(hoverinfo = 'none') %>% 
              config(displayModeBar = F)
        }
    })
    
    output$cloud <- renderWordcloud2({
        
        directory <- paste('data/', input$user, '/', sep = '')
        
        paste(directory, "tokens.csv", sep='')
        
        
        counts <- read.csv(paste(directory, "tokens.csv", sep=''), encoding = 'UTF-8')
        
        przeklenstwa <- c('kurwa', 'chuj', 'chuja', 'pierdole', 'jebac', 'https',
                          'zajebiscie', 'chujowo');
        
        counts <- counts %>% filter(size > input$min_freq, !(token %in% przeklenstwa))
        counts <- counts %>% filter(year == input$year)
        counts <- counts %>% top_n(input$word_count) %>% select(c(token, size))
        
        wordcloud2(data=counts, size=1,
                   shape = 'diamond', widgetsize = 0,
                   minRotation = pi/2, maxRotation = pi/2,
                   backgroundColor = '#1c1e21',
                   color = '#358AF8')
    }) %>% bindCache(input$user,
                     input$year,
                     input$word_count,
                     input$min_freq)
    
    output$topicsTable <- renderDataTable({
        directory <- paste('data', input$user, 'topics.json', sep = '/');
        topics <- jsonlite::fromJSON(directory)$inferred_topics_v2
        topicsCount <- ifelse(input$topicsCount < length(topics), 
                              input$topicsCount,
                              length(topics))
        topics <- sample(topics, topicsCount)
        df = data.frame(topics)
        names(df) <- c('Zainteresowania')
        datatable(df, options = list(dom = 't',
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#3e4042'});",
                                       "}")), 
                  rownames = FALSE) %>% 
          formatStyle(columns = col(df), 
                      backgroundColor = styleRow(1:nrow(df), 
                                                 values = rep_len(c('#358AF8', '#358AF8'),
                                                                  nrow(df))))
    })
    
    output$plot2 <- plotly::renderPlotly({
        df <- read.csv(paste("data", input$user, "reactions.csv", sep = '/'), encoding = "UTF-8")
        colnames(df) <- c("emotki", "iloscWystapien")
        p <- df %>%
            top_n(10) %>% 
            ggplot(aes(x = reorder(emotki, -iloscWystapien), y = iloscWystapien)) +
            geom_bar(stat = "identity", fill = "#358AF8") +
            xlab("Emotki") + 
            ylab("Ilość wystąpień") +
            get_plot_style()
        plotly::ggplotly(p) %>% plotly::style(hoverinfo = 'none')
        
    })
    
})
