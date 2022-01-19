library(shiny)

shinyServer(function(input, output) {
  
    library(dplyr)
    library(varhandle) 
    library(ggplot2)
    library(ggrepel)
    library(hash)
    library(stringi)
    library(tidyr)
    library(lubridate)
    library(shinylogs)
    library(reticulate)
    library(shinycssloaders)
    library(plotly)
    library(png)
    library(data.table)
    library(foreach)

    
    source("scripts/prepare_data_filip.R")
    
  df_jedrek <- read.csv("data/apps_j.csv", encoding="UTF-8")
  df_filip <- read.csv("data/apps_f.csv", encoding="UTF-8")
  df_malwina <- read.csv("data/apps_m.csv", encoding="UTF-8")
  
  df_jedrek <- prepare_jedrek_df(df_jedrek)
  df_filip <- prepare_filip_df(df_filip)
  df_malwina <- prepare_malwina_df(df_malwina)
    
    source("scripts/przygotowanie_ramki.R")
    
    apps_jedrek <- df_jedrek %>% group_by(App.name) %>% summarise(sum = sum(duration_minutes)) %>% arrange(desc(sum)) %>% head(6)
    apps_jedrek <- as.character(apps_jedrek$App.name)
    
    apps_filip <- df_filip %>% group_by(App.name) %>% summarise(sum = sum(duration_minutes)) %>% arrange(desc(sum)) %>% head(6)
    apps_filip <- as.character(apps_filip$App.name)
    
    apps_malwina <- df_malwina %>% group_by(App.name) %>% summarise(sum = sum(duration_minutes)) %>% arrange(desc(sum)) %>% head(6)
    apps_malwina <- as.character(apps_malwina$App.name)
    
    data_j <- read.csv("data/apps_j.csv", encoding="UTF-8", stringsAsFactors=FALSE)
    data_f <- read.csv("data/apps_f.csv", encoding="UTF-8", stringsAsFactors=FALSE)
    data_m <- read.csv("data/apps_m.csv", encoding="UTF-8", stringsAsFactors=FALSE)
    
    apps_all <- unique(c(apps_malwina, apps_filip, apps_jedrek))
    
    min_date <- max(c(min(df_jedrek$Data), min(df_filip$Data), min(df_malwina$Data)))
    max_date <- min(c(max(df_jedrek$Data), max(df_filip$Data), max(df_malwina$Data)))

    Sys.setlocale("LC_TIME", "English")
    
    column1 <- c("Number of starts of the app",
                 "Which day was the the app most often use",
                 "Overall time spent on the app in minutes",
                 "Average time spent on th app in single use in minutes",
                 "What was the longest, single use of the app in minutes")
    
    create.summary <- function(df, apps, app_name) {
      df <- df %>%
      filter(App.name %in% apps) %>% 
      filter(Data >= min_date & Data <= max_date)
      
      df1 <- df %>%
        group_by(App.name, Data) %>% 
        mutate(start_count_per_day = n()) %>% 
        ungroup() %>% 
        group_by(App.name) %>% 
        mutate(start_sum = n(),
               activity_sum = sum(duration_minutes),
               avr_activity_time = activity_sum/start_sum,
               start_count_max = max(start_count_per_day)
               # , activity_max = max(duration_minutes)
               ) %>%
        ungroup()
      
      df2 <- df %>% 
        filter(App.name == app_name) %>% 
        group_by(Data) %>%
        summarise(day_max = n()) %>%
        arrange(desc(day_max)) %>%
        head(1)
      day_max_activity <- df2$Data
      
      df3 <- df %>% 
        filter(App.name == app_name) %>% 
        group_by(Data) %>%
        mutate(activity_max = max(duration_minutes)) %>%
        ungroup() %>% 
        arrange(desc(activity_max)) %>%
        head(1)
      
      df1 <- unique(df1 %>% select(App.name, start_sum,
                                 activity_sum, avr_activity_time)) %>%
        filter(App.name == app_name)
      
      start_sum <- round(df1$start_sum)
      activity_sum <- round(df1$activity_sum)
      avr_activity_time <- round(df1$avr_activity_time, 2)
      day_max_activity <- stri_datetime_format(day_max_activity,
                                               stri_datetime_fstr("%d %B %Y"),
                                               locale="en_US")
      activity_max <- round(df3$activity_max)
      
      # Zero values are for unfinished summary parts
      column2 <- c(start_sum, day_max_activity, activity_sum, avr_activity_time, activity_max)
      
      return(column2)
    }
    
    plot_prepare <- function(df1, all_current_apps, zakres){
      
      col_names <- c("Data", "Godzina", "App.name", "hourly_time")
      add_to_result <- list()
      local_df1 <- copy(df1)
      
      foreach (app=all_current_apps) %do% {
        curr_df <- filter(local_df1, App.name == app)
        godziny <- pull(curr_df, "Godzina")
        godziny <- unfactor(godziny)
        
        missing_hours <- setdiff(zakres, godziny)
        
        
        data <- curr_df[1,1]
        
        foreach (godzina = godziny) %do% {
          
          foreach (hour=missing_hours) %do% {
            
            if (abs(hour - as.integer(godzina)) == 1) {
              row <- list(data, hour, app, 0)
              add_to_result <- rbind(add_to_result, row)
              
            }
          }
        }
      }
      
      
      row.names(add_to_result) <- NULL
      colnames(add_to_result) <- col_names
      result <- rbind(df1, add_to_result)
      result
      
    }
    
    convert_data <- function(data, letter){
      data <- separate(data = data, col = Start.time, into =c("Start.time","right"), sep = "GMT")
      data <- separate(data = data, col = right, into = c("left", "Rok"), sep = " ")
      data <- data %>% select(App.name, Start.time, Duration.ms, Rok)
      data <- data %>% mutate(duration_minutes = Duration.ms/(60*10^3))
      data$Start.time <- as.POSIXlt(strptime(data$Start.time, "%a %b %d %H:%M:%S "))
      data <- data %>% mutate(Godzina_pelna = strftime(data$Start.time, format = "%H:%M:%S"))
      data <- data %>% mutate(Godzina = strftime(data$Start.time, format = "%H"))
      data <- data %>% mutate(Data = strftime(data$Start.time, format = "%m-%d"))
      data$Data <- paste(data$Rok, "-", data$Data, sep='')
      data <- data %>% mutate(across("duration_minutes", round, 2))
      data <- data %>% mutate(Godzina = as.numeric(Godzina))
      
      data <- data %>% mutate(pora_dnia = case_when(
        Godzina < 12 & Godzina >= 5 ~ "Rano",
        Godzina < 20 & Godzina >= 12 ~ "Srodek",
        Godzina >= 20 | Godzina < 5 ~ "Wieczor"
      ))
      data$Osoba <- letter
      data <- data %>% filter(App.name != "Samsung Experience Home") %>% 
        filter(App.name != "Menedżer telefonu") %>% 
        filter(App.name != "Ekran główny Huawei") %>% 
        filter(App.name != "Systemowy menadżer pulpitu")
      data
    }
    
    change.path <- function() {
      switch(input$personInput,
             Malwina = {
               path <- "./www/images/malwina"
             },
             Jędrek = {
               path <- "./www/images/jedrek"
             },
             Filip = {
               path <- "./www/images/filip"
             })
      return(path)
    }
    
    change.df <- function() {
      switch(input$personInput,
             Malwina = {
               df <- df_malwina
             },
             Jędrek = {
               df <- df_jedrek
             },
             Filip = {
               df <- df_filip
             })
      return(df)
    }
    
    change.apps <- function() {
      switch(input$personInput,
             Malwina = {
               apps <- apps_malwina
             },
             Jędrek = {
               apps <- apps_jedrek
             },
             Filip = {
               apps <- apps_filip
             })
      return(apps)
    }
    
    
    data_j <- convert_data(data_j, "j")
    data_f <- convert_data(data_f, "f")
    data_m <- convert_data(data_m, "m")
    
    data <- bind_rows(data_j, data_f, data_m)
    data_static <- data %>% mutate(
      App.name = case_when(
        App.name == "Chrome" | App.name == "Opera Touch" ~ "Browser",
        App.name == "Telefon" | App.name == "Phone" ~ "Phone",
        App.name == "Spotify" | App.name == "TIDAL" ~ "Spotify/Tidal",
        App.name == "Wiadomości" | App.name == "Messeges" ~ "Messages",
        App.name == "Aparat"  ~ "Camera",
        App.name == "Galeria" ~ "Gallery",
        App.name == "Mapy" ~ "Maps",
        App.name == "zalando lounge" ~ "Zalando Lounge",
        TRUE ~ App.name
      )
    )
    
    track_usage(storage_mode = store_json(path = "logs/"))
    
    # phone activity
    output$plot1 <- plotly::renderPlotly({
      
      data <- data_static %>% filter(Osoba == input$osoba)
      top_apps<- data %>% group_by(App.name) %>%
        summarise(suma = sum(Duration.ms)) %>%
        arrange(desc(suma)) %>%
        head(10) %>% pull(App.name)
      
      if (input$pora_dnia != 'all') {
        df1 <- data %>% filter(pora_dnia == input$pora_dnia & Data == input$Data) %>% group_by(Data, Godzina, App.name) %>% summarise(hourly_time = sum(duration_minutes))
        df1 <- df1 %>% mutate(Godzina = as.numeric(Godzina)) }
      else {
        df1 <- data %>% filter(Data == input$Data) %>% group_by(Data, Godzina, App.name) %>% summarise(hourly_time = sum(duration_minutes))
        df1 <- df1 %>% mutate(Godzina = as.numeric(Godzina)) }
      
      df1 <- df1 %>% filter(App.name %in% top_apps)
      
      all_current_apps <- unique(df1$App.name)
      
      df1 <- as.data.frame(lapply(df1, unlist))
      if (input$pora_dnia == "Wieczor") {
        df1$Godzina <- factor(df1$Godzina,levels = c(seq(20,23,1), seq(0,5,1)))
        zakres <- c(seq(20,23,1), seq(0,5,1))
      } else if (input$pora_dnia == "Srodek") {
        df1$Godzina <- factor(df1$Godzina,levels = c(seq(12,19,1)))
        zakres <- c(seq(12,19,1))
      } else if (input$pora_dnia == "Rano") {
        df1$Godzina <- factor(df1$Godzina,levels = c(seq(5,11,1)))
        zakres <- c(seq(5,11,1))
      } else {
        df1$Godzina <- factor(df1$Godzina,levels = c(seq(0,23,1)))
        zakres <- c(seq(0,23,1))
      }
      df1 <- plot_prepare(df1, all_current_apps, zakres)
      df1 <- as.data.frame(lapply(df1, unlist))
      if (input$pora_dnia == "Wieczor") {
        df1$Godzina <- factor(df1$Godzina,levels = c(seq(20,23,1), seq(0,5,1)))
        zakres <- c(seq(20,23,1), seq(0,5,1))
      } else if (input$pora_dnia == "Srodek") {
        df1$Godzina <- factor(df1$Godzina,levels = c(seq(12,19,1)))
        zakres <- c(seq(12,19,1))
      } else if (input$pora_dnia == "Rano") {
        df1$Godzina <- factor(df1$Godzina,levels = c(seq(5,11,1)))
        zakres <- c(seq(5,11,1))
      } else {
        df1$Godzina <- factor(df1$Godzina,levels = c(seq(0,23,1)))
        zakres <- c(seq(0,23,1))
      }
      
      ####
      p <- ggplot(data = df1, aes(x=Godzina, y = hourly_time, group = App.name, color = App.name, fill = App.name)) +
        geom_line(size=1.5) +
        geom_point(size=3) +
        ylab("number of minutes") +
        xlab("time (hours)")+
        labs(fill = "Application name", color = "") +
        theme(
          panel.background = element_rect(fill = "#ffffff", color = "#ffffff"),
          plot.background = element_rect(fill = NULL, color = NULL),
          panel.grid.major.y = element_line(color = "#e0e0e0", size = 0.75, linetype = 1),
          panel.grid.major.x = element_line(color = "#e0e0e0", size = 0.75, linetype = 3),
          panel.border = element_blank(),
          panel.grid = element_blank()
        )
      
      
      #Sys.sleep(1)
      
      plotly::ggplotly(p)})
    
    # all radars
    output$plotMain <- plotly::renderPlotly({
      
      data <- przygotowanie_ramki(malwina, jedrek, filip, input$Date[1], input$Date[2])
      
      data1 <- data[data$App.name %in% input$aplikacje, , drop = FALSE]
      
      if (input$typWykresu == 'radar'){
        p1 <- plot_ly(
          type = 'scatterpolar',
          mode = 'lines',
          fill = 'toself')
        # height = 600,
        # width = 600) 
        
        if(input$Malwina){
          p1 <- p1 %>% add_trace(
            r = data1$Malwina,
            theta = data1$App.name,
            name = 'Malwina', 
            color = I("#F26AC6"), 
            marker = list(size = 2,
                          color = 'rgba(242, 106, 198, .6)',
                          line = list(color = 'rgba(152, 0, 0, .8)',
                                      width = 2))
          )} 
        if(input$Jedrek){
          p1 <- p1 %>% add_trace(
            r = data1$Jedrek,
            theta = data1$App.name,
            name = 'Jędrek', 
            color = I("#42D851"),
            marker = list(size = 2,
                          color = 'rgba(66, 216, 81, .6)',
                          line = list(color = 'rgba(11, 128, 22, .8)',
                                      width = 2))
          )}
        if(input$Filip){
          p1 <- p1 %>% add_trace(
            r = data1$Filip,
            theta = data1$App.name,
            name = 'Filip',
            color = I("#47B9F0"),
            marker = list(size = 2,
                          color = 'rgba(71, 185, 240, .6)',
                          line = list(color = 'rgba(19, 160, 229, .8)',
                                      width = 2))
          )}
        
        p1 <- p1 %>%
          layout(
            legend = list(itemclick = FALSE),
            polar = list(
              radialaxis = list(
                visible = T, 
                title = "minutes")
            ),
            showlegend = T
          )
        
        plotly::ggplotly(p1)
      }
      #wykres słupkowy
      else{
        p1 <- plot_ly(
          type = "bar")
        
        if(input$Malwina){
          p1 <- p1 %>% add_trace(
            y = data1$Malwina,
            x = data1$App.name,
            name = 'Malwina',
            color = I("#F26AC6"),
            marker = list(size = 2,
                          color = 'rgba(242, 106, 198, .6)',
                          line = list(color = 'rgba(152, 0, 0, .8)',
                                      width = 2))
          )}
        if(input$Jedrek){
          p1 <- p1 %>% add_trace(
            y = data1$Jedrek,
            x = data1$App.name,
            name = 'Jędrek',
            color = I("#42D851"),
            marker = list(size = 2,
                          color = 'rgba(66, 216, 81, .6)',
                          line = list(color = 'rgba(11, 128, 22, .8)',
                                      width = 2))
          )}
        if(input$Filip){
          p1 <- p1 %>% add_trace(
            y = data1$Filip,
            x = data1$App.name,
            name = 'Filip',
            color = I("#47B9F0"),
            marker = list(size = 2,
                          color = 'rgba(71, 185, 240, .6)',
                          line = list(color = 'rgba(19, 160, 229, .8)',
                                      width = 2))
          )}
        
        p1 <- p1 %>%
          layout(
            legend = list(itemclick = FALSE),
            yaxis = list(barmode = 'group', 
                         title = "minutes")
          )
        
        plotly::ggplotly(p1)
      }
      
    })
    
    # single radar
    output$plotMalwina <- plotly::renderPlotly({
      
      data <- przygotowanie_ramki(malwina, jedrek, filip, input$Date[1], input$Date[2])
      data1 <- data[data$App.name %in% input$aplikacje, , drop = FALSE]
      
      if (input$typWykresu == 'radar'){
        pMalwa <- plot_ly(
          type = 'scatterpolar',
          mode = 'lines',
          fill = 'toself', 
          color = I('#F26AC6'), 
          marker = list(size = 2,
                        color = 'rgba(242, 106, 198, .6)',
                        line = list(color = 'rgba(152, 0, 0, .8)',
                                    width = 2))
        ) %>% 
          add_trace(
            r = data1$Malwina,
            theta = data1$App.name,
            name = 'Malwina'
          ) %>%
          layout(
            polar = list(
              radialaxis = list(
                visible = T, 
                range = c(0,max(data1$Malwina, data1$Jedrek, data1$Filip)), 
                title = "minutes")
            ),
            showlegend = F,
            title = "Malwina"
          )
        
        plotly::ggplotly(pMalwa)
      } else {
        pMalwa <- plot_ly(
          type = 'bar',
          color = I('#F26AC6'), 
          marker = list(size = 2,
                        color = 'rgba(242, 106, 198, .6)',
                        line = list(color = 'rgba(152, 0, 0, .8)',
                                    width = 2))
        ) %>% 
          add_trace(
            y = data1$Malwina,
            x = data1$App.name,
            name = 'Malwina'
          ) %>%
          layout(
            yaxis = list(
              barmode = 'group', 
              range = list(0,max(data1$Malwina, data1$Jedrek, data1$Filip)), 
              title = "minutes"
            ),
            showlegend = F
          )
        
        plotly::ggplotly(pMalwa)
      }
    })
    
    # single radar
    output$plotJedrek <- plotly::renderPlotly({
      
      data <- przygotowanie_ramki(malwina, jedrek, filip, input$Date[1], input$Date[2])
      data1 <- data[data$App.name %in% input$aplikacje, , drop = FALSE]
      
      if (input$typWykresu == 'radar'){
        pJedrek <- plot_ly(
          type = 'scatterpolar',
          mode = 'lines',
          fill = 'toself', 
          color = I("#42D851"),
          marker = list(size = 2,
                        color = 'rgba(66, 216, 81, .6)',
                        line = list(color = 'rgba(11, 128, 22, .8)',
                                    width = 2))) %>% 
          add_trace(
            r = data1$Jedrek,
            theta = data1$App.name,
            name = 'Jędrek'
          ) %>%
          layout(
            polar = list(
              radialaxis = list(
                visible = T, 
                range = c(0,max(data1$Malwina, data1$Jedrek, data1$Filip)), 
                title = "minutes")
            ),
            showlegend = F,
            title = "Jędrek"
          )
        
        plotly::ggplotly(pJedrek)
      } else {
        pJedrek <- plot_ly(
          type = 'bar',
          color = I("#42D851"),
          marker = list(size = 2,
                        color = 'rgba(66, 216, 81, .6)',
                        line = list(color = 'rgba(11, 128, 22, .8)',
                                    width = 2))) %>% 
          add_trace(
            y = data1$Jedrek,
            x = data1$App.name,
            name = 'Jędrek'
          ) %>%
          layout(
            yaxis = list(
              barmode = 'group', 
              range = list(0,max(data1$Malwina, data1$Jedrek, data1$Filip)), 
              title = "minutes"
            ),
            showlegend = F
          )
        
        plotly::ggplotly(pJedrek)
      }
    })
    
    # single radar
    output$plotFilip <- plotly::renderPlotly({
      
      data <- przygotowanie_ramki(malwina, jedrek, filip, input$Date[1], input$Date[2])
      data1 <- data[data$App.name %in% input$aplikacje, , drop = FALSE]
      
      if (input$typWykresu == 'radar'){
        
        pFilip <- plot_ly(
          type = 'scatterpolar',
          mode = 'lines',
          fill = 'toself',
          color = I("#47B9F0"),
          marker = list(size = 2,
                        color = 'rgba(71, 185, 240, .6)',
                        line = list(color = 'rgba(19, 160, 229, .8)',
                                    width = 2))) %>% 
          add_trace(
            r = data1$Filip,
            theta = data1$App.name,
            name = 'Filip'
          ) %>%
          layout(
            polar = list(
              radialaxis = list(
                visible = T, 
                range = c(0,max(data1$Malwina, data1$Jedrek, data1$Filip)),
                title = "minutes")
            ),
            showlegend = F,
            title = "Filip"
          )
        
        plotly::ggplotly(pFilip)
      } else {
        pFilip <- plot_ly(
          type = 'bar',
          color = I("#47B9F0"),
          marker = list(size = 2,
                        color = 'rgba(71, 185, 240, .6)',
                        line = list(color = 'rgba(19, 160, 229, .8)',
                                    width = 2))) %>% 
          add_trace(
            y = data1$Filip,
            x = data1$App.name,
            name = 'Filip'
          ) %>%
          layout(
            yaxis = list(
              barmode = 'group', 
              range = list(0,max(data1$Malwina, data1$Jedrek, data1$Filip)), 
              title = "minutes"
            ),
            showlegend = F
          )
        
        plotly::ggplotly(pFilip)
      }
    })
    
    output$box1table <- renderTable({
      df <- change.df()
      apps <- change.apps()
      data.frame(column1,
                 create.summary(df, apps,
                                apps[1]))
    }, colnames = FALSE)
    
    output$box2table <- renderTable({
      df <- change.df()
      apps <- change.apps()
      data.frame(column1,
                 create.summary(df, apps,
                                apps[2]))
    }, colnames = FALSE)
    
    output$box3table <- renderTable({
      df <- change.df()
      apps <- change.apps()
      data.frame(column1,
                 create.summary(df, apps,
                                apps[3]))
    }, colnames = FALSE)
    
    output$box4table <- renderTable({
      df <- change.df()
      apps <- change.apps()
      data.frame(column1,
                 create.summary(df, apps,
                                apps[4]))
    }, colnames = FALSE)
    
    output$box5table <- renderTable({
      df <- change.df()
      apps <- change.apps()
      data.frame(column1,
                 create.summary(df, apps,
                                apps[5]))
    }, colnames = FALSE)
    
    output$box6table <- renderTable({
      df <- change.df()
      apps <- change.apps()
      data.frame(column1,
                 create.summary(df, apps,
                                apps[6]))
    }, colnames = FALSE)
    
    output$box1image <- renderImage({
      path <- change.path()
      file_name <- normalizePath(file.path(path, "1.png"))
      return(list(
        src = file_name,
        contentType = "image/png",
        width = "100px",
        height = "100px"
      ))
    }, deleteFile = FALSE)
    
    output$box2image <- renderImage({
      path <- change.path()
      file_name <- normalizePath(file.path(path, "2.png"))
      return(list(
        src = file_name,
        contentType = "image/png",
        width = "100px",
        height = "100px"
      ))
    }, deleteFile = FALSE)
    
    output$box3image <- renderImage({
      path <- change.path()
      file_name <- normalizePath(file.path(path, "3.png"))
      return(list(
        src = file_name,
        contentType = "image/png",
        width = "100px",
        height = "100px"
      ))
    }, deleteFile = FALSE)
    
    output$box4image <- renderImage({
      path <- change.path()
      file_name <- normalizePath(file.path(path, "4.png"))
      return(list(
        src = file_name,
        contentType = "image/png",
        width = "100px",
        height = "100px"
      ))
    }, deleteFile = FALSE)
    
    output$box5image <- renderImage({
      path <- change.path()
      file_name <- normalizePath(file.path(path, "5.png"))
      return(list(
        src = file_name,
        contentType = "image/png",
        width = "100px",
        height = "100px"
      ))
    }, deleteFile = FALSE)
    
    output$box6image <- renderImage({
      path <- change.path()
      file_name <- normalizePath(file.path(path, "6.png"))
      return(list(
        src = file_name,
        contentType = "image/png",
        width = "100px",
        height = "100px"
      ))
    }, deleteFile = FALSE)
    
    output$box1app <- renderText({
      apps <- change.apps()
      apps[1]
    })
    
    output$box2app <- renderText({
      apps <- change.apps()
      apps[2]
    })
    
    output$box3app <- renderText({
      apps <- change.apps()
      apps[3]
    })
    
    output$box4app <- renderText({
      apps <- change.apps()
      apps[4]
    })
    
    output$box5app <- renderText({
      apps <- change.apps()
      apps[5]
    })
    
    output$box6app <- renderText({
      apps <- change.apps()
      apps[6]
    })
    
    output$average_activity_change <- plotly::renderPlotly({
      
      data_frame_transformation1 <- function(df){
        df <- df %>%
          filter(App.name %in% input$appsInput) %>%
          group_by(Device, App.name, Data) %>%
          summarise(mean_time_activity = sum(duration_minutes)/n()) %>%
          ungroup()
        
        return(df)
      }
      
      switch(input$personInput2,
             Malwina = {
               df <- data_frame_transformation1(df_malwina)
             },
             Jędrek = {
               df <- data_frame_transformation1(df_jedrek)
             },
             Filip = {
               df <- data_frame_transformation1(df_filip)
             })
      
      df <- df %>%
        mutate(App.name = fct_reorder(App.name, desc(Device))) %>%
        mutate(App.name = fct_reorder(App.name, desc(mean_time_activity)))
      
      p <- ggplot(df, aes(x = Data, y = mean_time_activity,
                          group = App.name, color = App.name)) +
        geom_line(size = 1) +
        geom_point(size = 2) +
        theme(axis.text.x = element_text(angle = -30)) +
        labs(
          x = "Date",
          y = "Time in minutes",
          fill = "App name",
          color = "App name"
        ) +
        scale_x_date(date_labels = "%Y %b %d") +
        scale_x_date(breaks = "1 week") +
        theme(
          panel.background = element_rect(fill = "#ffffff", color = "#ffffff"),
          plot.background = element_rect(fill = NULL, color = NULL),
          panel.grid.major.y = element_line(color = "#e0e0e0", size = 0.75, linetype = 1),
          panel.grid.major.x = element_line(color = "#e0e0e0", size = 0.75, linetype = 3),
          panel.border = element_blank(),
          panel.grid = element_blank()
        )
      
      plotly::ggplotly(p)
    })
    
    
    output$average_activity <- plotly::renderPlotly({
    # output$average_activity <- renderPlot({
      
      data_frame_transformation2 <- function(df){
        df <- df %>% 
          filter(Data >= input$date_interval[1] &
                   Data <= input$date_interval[2]) %>%
          group_by(App.name) %>% 
          mutate(count = n()) %>% 
          ungroup() %>% 
          filter(count >= 15) %>% 
          group_by(Device, App.name) %>% 
          summarise(mean_time_activity = sum(duration_minutes)/n()) %>% 
          ungroup() %>%
          arrange(desc(mean_time_activity))
        
        rownames(df) <- NULL
        df <- df[1:input$appsNoInput,]
        return(df)
      }
      
      df_jedrek.2 <- data_frame_transformation2(df_jedrek)
      df_filip.2 <- data_frame_transformation2(df_filip)
      df_malwina.2 <- data_frame_transformation2(df_malwina)
      
      df <- rbind(df_jedrek.2, df_filip.2)
      df <- rbind(df, df_malwina.2)
      df <- df %>%
        mutate(App.name = fct_reorder(App.name, desc(Device))) %>%
        mutate(App.name = fct_reorder(App.name, desc(mean_time_activity)))
      
      # max_time_activity <- max(df$mean_time_activity)
      # max_time_activity <- round(max_time_activity, digits = -1)
      
      p <- ggplot(df, aes(x = App.name, y = mean_time_activity, fill = Device)) + 
        geom_col(width = 0.7, position="dodge2") +
        labs(
          fill = "Person",
          x = "App name",
          y = "Time in minutes"
        ) +
      theme(
        axis.text.x = element_text(angle = -30),
        panel.background = element_rect(fill = "#ffffff", color = "#ffffff"),
        plot.background = element_rect(fill = NULL, color = NULL),
        panel.grid.major.y = element_line(color = "#e0e0e0", size = 0.75, linetype = 1),
        panel.border = element_blank(),
        panel.grid = element_blank()
      ) +
      scale_fill_manual(values=c("#8de796", "#90d5f6", "#f7a5dd"))
      
      plotly::ggplotly(p)
    })
    
})
