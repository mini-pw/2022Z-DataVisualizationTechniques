library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(jsonlite)
library(tidyr)
library(stringr)
library(bslib)
library(wordcloud)
library(shinydashboard)
library(shiny)
library(shinycssloaders)
library(stringr)
library(shinyWidgets)
library(plotly)
library(shinyjs)
library(kit)
library(forcats)
library(fresh)
library(gt)

#---------------------------------------------------------------------

# data
wdays <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
wdays2 <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
mnames <- c("January", "February", "March", "April",
            "May", "June", "July", "August",
            "September", "October", "November", "December")
mnames2 <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
hours <- sprintf("%02d:00-%02d:00", 0:23, 1:24)
p2_ed <- NULL

all_days <- c()
for (i in 1:12) {
  all_days <- c(all_days,
                paste(str_pad(i, 2, pad="0"),
                      str_pad(1:(c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)[i]), 2, pad="0")))
}
df_p2_bar <- data.frame(group = all_days)


df_mikolaj <- fromJSON("data/mikołaj/endsong.json") %>% 
  select(ts,
         platform,
         ms_played,
         master_metadata_track_name,
         master_metadata_album_artist_name,
         master_metadata_album_album_name,
         shuffle) %>% 
  mutate(ts = as.POSIXlt(str_replace_all(ts, regex("[TZ]"), " "), tz="UTC")) %>% 
  mutate(weekday = ts$wday + 1,
         hour = ts$hour,
         min = ts$min,
         s = ts$s) %>% 
  mutate(platform = case_when(
    platform %in% c("iOS 12.1.2 (iPhone8,4)",
                    "iOS 14.7 (iPhone8,4)") ~ "Phone",
    platform %in% c("Windows 10 (10.0.19043; x64)",
                    "Windows 8.1 (6.3.9600; x64)") ~ "PC",
    TRUE ~ "Other"
  ))

df_krzysiek <- fromJSON("data/krzysiek/endsong.json") %>% 
  select(ts,
         platform,
         ms_played,
         master_metadata_track_name,
         master_metadata_album_artist_name,
         master_metadata_album_album_name,
         shuffle) %>% 
  mutate(ts = as.POSIXlt(str_replace_all(ts, regex("[TZ]"), " "), tz="UTC")) %>% 
  mutate(weekday = ts$wday + 1,
         hour = ts$hour,
         min = ts$min,
         s = ts$s) %>% 
  mutate(platform = case_when(
    platform %in% c("Android OS 9 API 28 (samsung, SM-J530F)",
                    "iOS 14.7.1 (iPhone13,1)",
                    "iOS 14.8 (iPhone13,1)",
                    "iOS 15.1 (iPhone13,1)",
                    "iOS 15.1.1 (iPhone13,1)") ~ "Phone",
    platform %in% c("Windows 10 (10.0.19041; x64; AppX)",
                    "Windows 10 (10.0.19043; x64; AppX)") ~ "PC",
    TRUE ~ "Other"
  ))

# tymczasowe rozwiązanie dopuki nie ma rozszerzonych danych
df_daniel <- bind_rows(df_mikolaj, df_krzysiek)


#------------------------------------------------------------------------------
# data
streaming_history_df <- fromJSON("data/krzysiek/endsong.json")
features_df <- read.csv("data/krzysiek/tracks_features.csv")
df_krzysiek_M <- left_join(streaming_history_df, features_df, by = "spotify_track_uri", keep = FALSE)

streaming_history_df <- fromJSON("data/mikołaj/endsong.json")
features_df <- read.csv("data/mikołaj/tracks_features.csv")
df_mikolaj_M <- left_join(streaming_history_df, features_df, by = "spotify_track_uri", keep = FALSE)

streaming_history_df <- read.csv("data/daniel/endsong.csv")
features_df <- read.csv("data/daniel/tracks_features.csv")
df_daniel_M <- left_join(streaming_history_df, features_df, by = c("trackName" = "master_metadata_track_name"), keep = FALSE)




# utils to genre plot 
genre_seeds <- c(
  "acoustic",
  "afrobeat",
  "alt-rock",
  "alternative",
  "ambient",
  "anime",
  "black-metal",
  "bluegrass",
  "blues",
  "bossanova",
  "breakbeat",
  "cantopop",
  "chicago-house",
  "children",
  "chill",
  "classical",
  "club",
  "comedy",
  "country",
  "dance",
  "dancehall",
  "death-metal",
  "deep-house",
  "detroit-techno",
  "disco",
  "disney",
  "drum-and-bass",
  "dub",
  "dubstep",
  "edm",
  "electro",
  "electronic",
  "emo",
  "folk",
  "forro",
  "funk",
  "garage",
  "gospel",
  "goth",
  "grindcore",
  "groove",
  "grunge",
  "guitar",
  "happy",
  "hard-rock",
  "hardcore",
  "hardstyle",
  "heavy-metal",
  "hip-hop",
  "hip hop",
  "holidays",
  "honky-tonk",
  "house",
  "idm",
  "indie",
  "indie-pop",
  "industrial",
  "j-dance",
  "j-idol",
  "j-pop",
  "j-rock",
  "jazz",
  "k-pop",
  "kids",
  "latin",
  "latino",
  "malay",
  "mandopop",
  "metal",
  "metal-misc",
  "metalcore",
  "minimal-techno",
  "movies",
  "mpb",
  "new-age",
  "new-release",
  "opera",
  "pagode",
  "party",
  "piano",
  "pop",
  "pop-film",
  "post-dubstep",
  "power-pop",
  "progressive-house",
  "psych-rock",
  "punk",
  "punk-rock",
  "r-n-b",
  "rainy-day",
  "reggae",
  "reggaeton",
  "road-trip",
  "rock",
  "rock-n-roll",
  "rockabilly",
  "romance",
  "sad",
  "salsa",
  "samba",
  "sertanejo",
  "show-tunes",
  "singer-songwriter",
  "ska",
  "sleep",
  "songwriter",
  "soul",
  "soundtrack",
  "study",
  "summer",
  "synth-pop",
  "tango",
  "techno",
  "trance",
  "trap",
  "trip-hop",
  "work-out"
)


# Spotify palette
palette <- c(green = "#1DB954", white = "#FFFFFF", black = "191414")


unify_genres <- function(x, genre_seeds) {
  res <- character(length(x))
  for (i in 1:length(x)) {
    idx <- which(do.call(str_detect, c(x[i], list(genre_seeds))))
    res[i] <- paste0(genre_seeds[idx], collapse = ", ")
  }
  res
}

my_theme <- bs_theme(
  bg = "#121212",
  fg = "#FFFFFF",
  primary = "#1DB954",
)
# ------------------------------------------------------------------------------


server <- function(input, output, session){
  
  
  output$p2_density <- renderPlotly({
    p2_ed <- event_data("plotly_click", source="p2_comp")
    if(is.null(p2_ed)){
      df <- bind_rows(list(Daniel=df_daniel,
                           Mikołaj=df_mikolaj,
                           Krzysiek=df_krzysiek),
                      .id="person") %>%
        filter(ts > input$p2_time[1], ts < input$p2_time[2])
      
      p <- ggplot(df, aes(x=hour*60*60 + min*60 + s,
                          color=person)) +
        geom_density(aes(weight=ms_played,
                         text=paste("Person:", person, "\nClick to see details."))) +
        scale_y_continuous(name="") +
        scale_x_continuous(name = "Hour",
                           breaks = (0:8)*60*60*3,
                           labels = seq(from=0, to=24, by=3)) +
        scale_color_manual(values = c("#1ED760", "#00F5D2", "#23F500")) +
        theme(panel.background = element_rect(fill = "#121212"),
              plot.background = element_rect(fill = "#121212"),
              text = element_text(color = "#FFFFFF"),
              axis.text = element_text(color = "#FFFFFF"),
              legend.text = element_text(colour = "#FFFFFF"),
              legend.background = element_rect(fill="#121212", colour="#888888"),
              axis.ticks = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_line(size = 0.3, colour = "#888888"),
              axis.title.y = element_blank(),
              axis.title.x = element_text()) +
        labs(colour = "Person")
      
      ggplotly(p, source = "p2_comp", tooltip="text") %>%
        layout(yaxis = list(ticks="", showticklabels=FALSE, fixedrange=TRUE),
               xaxis = list(fixedrange=TRUE)) %>% 
        config(displayModeBar=FALSE)
    }
    else{
      p2_person <- c("Daniel", "Krzysiek", "Mikołaj")[p2_ed$curveNumber + 1]
      if(p2_person == "Daniel"){
        df <- df_daniel
      }
      else if(p2_person == "Mikołaj"){
        df <- df_mikolaj
      }
      else {
        df <- df_krzysiek
      }
      df <- df %>% filter(ts > input$p2_time[1], ts < input$p2_time[2])
      df$platform <- factor(df$platform, levels = c("PC", "Phone", "Other"))
      
      p <- ggplot(df, aes(x=hour*60*60 + min*60 + s, y=..count../2000, color=platform)) +
        geom_density(aes(weight=ms_played,
                         text = paste("Device:", platform))) +
        scale_y_continuous(name="") +
        scale_x_continuous(name = "Hour",
                           breaks = (0:8)*60*60*3,
                           labels = seq(from=0, to=24, by=3)) +
        scale_color_manual(values = c("#1ED760", "#00F5D2", "#23F500")) +
        theme(panel.background = element_rect(fill = "#121212"),
              plot.background = element_rect(fill = "#121212"),
              text = element_text(color = "#FFFFFF"),
              axis.text = element_text(color = "#FFFFFF"),
              legend.text = element_text(colour = "#FFFFFF"),
              legend.background = element_rect(fill="#121212", colour="#888888"),
              axis.ticks = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_line(size = 0.3, colour = "#888888"),
              axis.title.y = element_blank(),
              axis.title.x = element_text()) +
        labs(colour = "Device")
      
      ggplotly(p, source = "p2_density", tooltip = "text") %>%
        layout(yaxis = list(ticks="", showticklabels=FALSE, fixedrange=TRUE),
               xaxis = list(fixedrange=TRUE)) %>% 
        config(displayModeBar=FALSE)
    }
  })
  
  output$p2_UI_time_input <- renderUI({
    p2_ed <- event_data("plotly_click", source="p2_comp")
    if(is.null(p2_ed)){
      df <- bind_rows(list(df_daniel,
                           df_mikolaj,
                           df_krzysiek),
                      .id="person")
    }
    else{
      p2_person <- c("Daniel", "Krzysiek", "Mikolaj")[p2_ed$curveNumber + 1]
      if(p2_person == "Daniel"){
        df <- df_daniel
      }
      else if(p2_person == "Mikołaj"){
        df <- df_mikolaj
      }
      else {
        df <- df_krzysiek
      }
    }
    min_time <- min(df$ts)
    max_time <- max(df$ts)
    
    tagList(
      h5("Select time period:", style = "color: #FFFFFF;"),
      sliderInput(
        inputId = "p2_time",
        label = NULL,
        min = min_time,
        max = max_time,
        value = c(min_time,
                  max_time),
        timeFormat = "%b %Y",
        width="100%"
      ) 
    )
  })
  
  output$p2_heatmap <- renderPlotly({
    p2_ed <- event_data("plotly_click", source="p2_comp")
    p2_person <- c("Daniel", "Krzysiek", "Mikołaj")[p2_ed$curveNumber + 1]
    if(p2_person == "Daniel"){
      df <- df_daniel
    }
    else if(p2_person == "Mikołaj"){
      df <- df_mikolaj
    }
    else {
      df <- df_krzysiek
    }
    df <- df %>% filter(ts > input$p2_time[1], ts < input$p2_time[2])
    
    df_heatmap <- expand.grid(weekday = 1:7, hour = 0:23)
    df_heatmap <- df_heatmap %>% 
      merge(
        df %>%
          group_by(weekday, hour) %>% 
          summarise(z = sum(ms_played)),
        by = c("weekday", "hour"),
        all.x=TRUE
      ) %>% 
      mutate(z = ifelse(is.na(z), 0, z))
    
    p <- ggplot(df_heatmap) + 
      geom_tile(aes(x=weekday,
                    y=hour,
                    fill=z,
                    text=paste("Day:", wdays[weekday],
                               "\nTime:", hours[hour],
                               "\nTotal listening time:",
                               strftime(
                                 as.POSIXlt.numeric(z/1000,
                                                    format="%OS",
                                                    origin="")-3600,
                                 format="%H:%M:%OS"))),
                color = "#121212", #powinny być przerwy między kafelkami
                lwd = 1) +         #ale plotly nie dziła :(((
      scale_x_continuous(breaks=1:7, labels=wdays2) +
      scale_y_reverse(breaks=0:23, labels=hours) +
      scale_fill_gradient(high = "#1ED760",
                          low = "black",
                          breaks = c(max(df_heatmap$z), min(df_heatmap$z)),
                          labels = c("More", "Less")) +
      theme(panel.background = element_rect(fill = "#121212"),
            plot.background = element_rect(fill = "#121212"),
            text = element_text(color = "#FFFFFF"),
            axis.text.x = element_text(color = "#FFFFFF"),
            axis.text.y = element_text(color = "#FFFFFF"),
            legend.text = element_text(colour = "#FFFFFF"),
            legend.background = element_rect(fill="#121212", colour="#888888"),
            axis.ticks = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            axis.title.x = element_text()) +
      #coord_fixed(ratio = 1) +
      labs(x = "Day of Week",
           y = "Hour",
           fill = "")
    
    ggplotly(p, source="p2_tile", tooltip="text") %>% 
      config(displayModeBar=FALSE) %>% 
      layout(yaxis = list(fixedrange=TRUE),
             xaxis = list(fixedrange=TRUE))
  })
  
  output$p2_bar <- renderPlotly({
    p2_ed <- event_data("plotly_click", source="p2_comp")
    p2_person <- c("Daniel", "Krzysiek", "Mikołaj")[p2_ed$curveNumber + 1]
    if(p2_person == "Daniel"){
      df <- df_daniel
    }
    else if(p2_person == "Mikołaj"){
      df <- df_mikolaj
    }
    else {
      df <- df_krzysiek
    }
    df <- df %>% 
      select(ts, ms_played) %>% 
      mutate(date = as.Date(ts)) %>% 
      filter(date >= as.Date("2021-01-01")) %>% 
      group_by(group = format(date, input$p2_buckets)) %>% 
      summarise(ms_played = sum(ms_played, na.rm=TRUE))
    if (input$p2_buckets=="%m %d") {
      df <- df_p2_bar %>% 
        merge(df, by = "group", all.x=TRUE) %>% 
        mutate(ms_played = ifelse(is.na(ms_played), 0, ms_played))
    }
    
    p2_tick_labs <- function(x) paste(floor(x/(1000*60*60)),
                                      "h",
                                      floor(x/(1000*60) - 60*floor(x/(1000*60*60))),
                                      "min")
    
    p <- ggplot(df) +
      geom_col(aes(x=group,
                   y=ms_played,
                   text=paste(
                     "Total listening time:",
                     p2_tick_labs(ms_played)
                   )), fill = "#1ED760") +
      theme(panel.background = element_rect(fill = "#121212"),
            plot.background = element_rect(fill = "#121212"),
            text = element_text(color = "#FFFFFF"),
            axis.text = element_text(color = "#FFFFFF"),
            legend.text = element_text(colour = "#FFFFFF"),
            axis.ticks = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(size = 0.3, colour = "#888888")) +
      scale_y_continuous(labels=p2_tick_labs) +
      labs(y = "Total listening time", x = "Month")
    if(input$p2_buckets == "%m") {
      p <- p + 
        scale_x_discrete(breaks = str_pad(1:12, 2, pad="0"), labels=mnames2)
    }
    else {
      p <- p +
        scale_x_discrete(breaks = paste(str_pad(1:12, 2, pad="0"), rep("15", 12)),
                         labels = mnames2)
    }
    ggplotly(p, source = "p2_barplot_time", tooltip = "text") %>% 
      config(displayModeBar=FALSE) %>% 
      layout(yaxis = list(fixedrange=TRUE))
  })
  
  output$p2_UI <- renderUI({
    
    p2_ed <- event_data("plotly_click", source="p2_comp")
    if(is.null(p2_ed)) { tagList(
      h4("What time of the day do we listen to music?", style = "color: #FFFFFF"),
      withSpinner(plotlyOutput("p2_density"), color="#1ED760", type=4),
      uiOutput("p2_UI_time_input") 
    )}
    else {
      p2_person <- c("Daniel", "Krzysiek", "Mikołaj")[p2_ed$curveNumber + 1]
      tagList(
        
        
        fluidRow(
          box(width = 6,
              h4(paste("What time of the day does", p2_person, "listen to music?"), style = "color: #FFFFFF"),
              withSpinner(plotlyOutput("p2_density"), color="#1ED760", type=4),
              background = "red"
          ),
          
          box(width = 6,
              h4(paste("What time of the week does", p2_person, "listen to music?"), style = "color: #FFFFFF"),
              withSpinner(plotlyOutput("p2_heatmap"), color="#1ED760", type=4),
              background = "red"
          )
        ),
        
        br(),
        
        fluidRow(
          box(width = 3,
              h5("Barplot controls:", style = "color: #FFFFFF"),
              br(),
              radioButtons(
                inputId = "p2_buckets",
                label = "",
                choices = c("Month" = "%m",
                            "Day" = "%m %d"),
                selected = "%m",
                width = "100%",
                inline = TRUE
              ),
              background = "red"
          ),
          box(width = 3,
              h5("Return:", style = "color: #FFFFFF"),
              br(),
              actionButton(inputId = "p2_reset",
                           label = "",
                           icon = icon("backward"),
                           width = "100%"),
              background = "red"
          ),
          box(width = 6,
              uiOutput("p2_UI_time_input"),
              background = "red"
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            h4(paste(p2_person, "'s listening time last year", sep=""), style = "color: #FFFFFF"),
            withSpinner(plotlyOutput("p2_bar"), color="#1ED760", type=4),
            background = "red"
          )
        )
      )
    }
  })
  
  observeEvent(input$p2_reset, {
    runjs("Shiny.setInputValue('plotly_click-p2_comp', null);")
  })
  
  
  
  output$tempo_histogram <- renderPlot({
    
    if (input$person == "Daniel") {
      df <- df_daniel_M
    } else if (input$person == "Krzysiek") {
      df <- df_krzysiek_M
    } else {
      df <- df_mikolaj_M
    }
    
    df %>% 
      ggplot(aes(tempo)) + 
      geom_histogram(binwidth = 4, fill = "#1DB954", alpha = 0.87) +
      theme_dark() + 
      xlab("") + 
      ylab("") + 
      theme(
        plot.background = element_rect(fill = "#121212", colour = "#121212"),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(colour = "#FFFFFF")
        
      ) 
  })
  
  output$genres <- renderPlot(bg = "#121212", {
    
    if (input$person == "Daniel") {
      df <- df_daniel_M
    } else if (input$person == "Krzysiek") {
      df <- df_krzysiek_M
    } else {
      df <- df_mikolaj_M
    }
    
    
    df <- df %>% 
      group_by(genres) %>% 
      summarise(count = n()) %>% 
      arrange(-count) %>% 
      mutate(genres = unify_genres(genres, genre_seeds)) %>% 
      group_by(genres) %>% 
      summarise(count = sum(count)) %>% 
      arrange(-count) %>% 
      separate_rows(genres, sep = ", ") %>% 
      group_by(genres) %>% 
      summarise(count = sum(count)) %>% 
      arrange(-count) %>% 
      filter(genres != "") %>% 
      head(30)
    
    
    wordcloud(df$genres, df$count, col = terrain.colors(length(df$genres)), bg = "#121212", scale = c(4, 1.5))
    
    
  })
  
  output$density_plot <- renderPlotly({
    
    
    density_df_krzysiek <- df_krzysiek_M %>% 
      select(danceability, energy, acousticness, instrumentalness, speechiness, valence) %>% 
      lapply(., density, na.rm = TRUE)
    
    density_df_mikolaj <- df_mikolaj_M %>% 
      select(danceability, energy, acousticness, instrumentalness, speechiness, valence) %>% 
      lapply(., density, na.rm = TRUE)
    
    
    density_df_daniel <- df_daniel_M %>% 
      select(danceability, energy, acousticness, instrumentalness, speechiness, valence) %>% 
      lapply(., density, na.rm = TRUE)
    
    feature_names <- names(density_df_daniel)
    
    
    if (input$person == "Krzysiek") {
      alpha_krzysiek <- 1
      alpha_mikolaj <- 0.1
      alpha_daniel <- 0.1
    } else if (input$person == "Mikołaj") {
      alpha_krzysiek <- 0.1
      alpha_mikolaj <- 1
      alpha_daniel <- 0.1
    } else {
      alpha_krzysiek <- 0.1
      alpha_mikolaj <- 0.1
      alpha_daniel <- 1
    }
    
    
    plots <- lapply(feature_names, function(feature_n) {
      plot_ly(
        type = 'scatter',
        mode = 'lines' 
      ) %>% 
        add_trace(
          x = density_df_krzysiek[[feature_n]][['x']],
          y = density_df_krzysiek[[feature_n]][['y']],
          
          color = 'red', 
          opacity = alpha_krzysiek,
          name = 'Krzysiek'
        ) %>% 
        add_trace(
          x = density_df_mikolaj[[feature_n]][['x']],
          y = density_df_mikolaj[[feature_n]][['y']],
          color = 'green', 
          opacity = alpha_mikolaj,
          name = 'Mikołaj'
        ) %>% 
        add_trace(
          x = density_df_daniel[[feature_n]][['x']],
          y = density_df_daniel[[feature_n]][['y']],
          color = 'blue', 
          opacity = alpha_daniel,
          name = 'Daniel'
        ) %>% 
        layout(
          xaxis = list(
            tickvals = list(0, 0.5, 1),
            title = feature_n
          ),
          yaxis = list(showticklabels = FALSE),
          showlegend = FALSE,
          plot_bgcolor  = "#121212",
          paper_bgcolor = "#121212",
          font = list(color="white")
        ) %>% 
        config(displayModeBar = FALSE)
      
      
      
      
    })
    firstRow <- subplot(plots[1:3], nrows = 1, titleX = TRUE) 
    secondRow <- subplot(plots[4:6], shareX = TRUE, nrows = 1, titleX = TRUE) 
    subplot(firstRow, secondRow, nrows = 2, titleX = TRUE, margin = 0.05) %>%
      config(displayModeBar = FALSE)
    
  })
  
  output$radar_plot <- renderPlotly({
    radar_krzysiek <- df_krzysiek_M %>% 
      select(danceability, energy, speechiness, acousticness, instrumentalness, valence) %>% 
      pivot_longer(everything(), names_to = "feature", values_to = "value") %>% 
      group_by(feature) %>% 
      summarise(value = mean(value, na.rm = TRUE)) %>% 
      arrange(factor(feature, c("danceability", "energy",  "acousticness", "instrumentalness", "speechiness", "valence"))) 
    
    radar_mikolaj <- df_mikolaj_M %>% 
      select(danceability, energy, speechiness, acousticness, instrumentalness, valence) %>% 
      pivot_longer(everything(), names_to = "feature", values_to = "value") %>% 
      group_by(feature) %>% 
      summarise(value = mean(value, na.rm = TRUE)) %>% 
      arrange(factor(feature, c("danceability", "energy",  "acousticness", "instrumentalness", "speechiness", "valence"))) 
    
    radar_daniel <- df_daniel_M %>% 
      select(danceability, energy, speechiness, acousticness, instrumentalness, valence) %>% 
      pivot_longer(everything(), names_to = "feature", values_to = "value") %>% 
      group_by(feature) %>% 
      summarise(value = mean(value, na.rm = TRUE)) %>% 
      arrange(factor(feature, c("danceability", "energy",  "acousticness", "instrumentalness", "speechiness", "valence"))) 
    
    
    
    fig <- plot_ly(
      type = 'scatterpolar',
      mode = 'markers',
      fill = 'toself'
    )
    
    if (input$person == "Krzysiek") {
      alpha_krzysiek <- 0.7
      alpha_mikolaj <- 0.5
      alpha_daniel <- 0.5
    } else if (input$person == "Mikołaj") {
      alpha_krzysiek <- 0.5
      alpha_mikolaj <- 0.7
      alpha_daniel <- 0.5
    } else {
      alpha_krzysiek <- 0.5
      alpha_mikolaj <- 0.5
      alpha_daniel <- 0.7
    }
    
    
    fig <- fig %>% add_trace(
      data = radar_krzysiek,
      r = ~value,
      theta = ~feature,
      hovertemplate = paste("Feature: %{theta}<br>Value: %{r:.3f}<extra></extra>"),
      name = "Krzysiek",
      fillcolor = "red",
      opacity = alpha_krzysiek
    )
    
    fig <- fig %>% add_trace(
      data = radar_mikolaj,
      r = ~value,
      theta = ~feature,
      hovertemplate = paste("Feature: %{theta}<br>Value: %{r:.3f}<extra></extra>"),
      name = "Mikołaj",
      fillcolor = "green",
      opacity = alpha_mikolaj
    )
    
    fig <- fig %>% add_trace(
      data = radar_daniel,
      r = ~value,
      theta = ~feature,
      hovertemplate = paste("Feature: %{theta}<br>Value: %{r:.3f}<extra></extra>"),
      name = "Daniel",
      fillcolor = "blue",
      opacity = alpha_daniel
    )
    
    fig  %>%
      layout(
        font = list(color = "#FFFFFF"),
        plot_bgcolor  = "rgba(0, 0, 0, 0)",
        paper_bgcolor = "rgba(0, 0, 0, 0)",
        polar = list(
          bgcolor = "rgba(0, 0, 0, 0)",
          radialaxis = list(
            visible = T,
            range = c(0,1),
            gridcolor = "#FFFFFF",
            color = "#FFFFFF",
            tickfont = list(color = "#FFFFFF")
          ),
          angularaxis = list(
            linecolor = "#FFFFFF",
            tickfont = list(size = 14)
          )
        ),
        showlegend = F
      ) %>% 
      config(displayModeBar = FALSE)
    
  })
  
  
  output$plot3 <- plotly::renderPlotly({
    
    if(input$personPlot3 == "Mikołaj"){
      df <- df_mikolaj
    }
    else if(input$personPlot3 == "Krzysiek"){
      df <- df_krzysiek
    }
    else{
      df <- df_daniel
    }
    
    
    
    df1 <- df %>% 
      group_by(master_metadata_album_artist_name) %>% 
      summarise(Time = sum(ms_played)/60000) %>% 
      arrange(-Time) %>% 
      head(input$n) %>% 
      mutate(master_metadata_album_artist_name = fct_reorder(master_metadata_album_artist_name, Time))
    
    
    p <- ggplot(df1,aes(x = Time, y = master_metadata_album_artist_name, text = paste("Time: ", Time, "\nArtist: ", master_metadata_album_artist_name)))+
      geom_col(fill = "#1ED760")+
      theme(panel.background = element_rect(fill = "#121212"),
            plot.background = element_rect(fill = "#121212"),
            text = element_text(color = "#FFFFFF"),
            axis.text = element_text(color = "#FFFFFF"),
            legend.text = element_text(colour = "#FFFFFF"),
            axis.ticks = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(size = 0.3, colour = "#888888"),
            axis.title.y = element_blank(),
            axis.title.x = element_text())+
      labs(title = "Favourite artists", x = "Minutes listened")
    
    plotly::ggplotly(p, source = "1", tooltip = "text") %>% 
      config(displayModeBar = FALSE) %>% 
      layout(
        yaxis = list(fixedrange = TRUE),
        xaxis = list(fixedrange = TRUE)
      )
  })
  
  output$plot4 <- plotly::renderPlotly({
    
    
    if(input$personPlot3 == "Mikołaj"){
      df <- df_mikolaj
    }
    else if(input$personPlot3 == "Krzysiek"){
      df <- df_krzysiek
    }
    else {
      df <- df_daniel
    }
    
    
    
    df1 <- df %>% 
      group_by(master_metadata_album_album_name) %>% 
      summarise(Time = sum(ms_played)/60000) %>% 
      arrange(-Time) %>% 
      head(input$n) %>% 
      mutate(master_metadata_album_album_name = fct_reorder(master_metadata_album_album_name, Time))
    
    
    p <- ggplot(df1,aes(x = Time, y = master_metadata_album_album_name, text = paste("Time: ", Time, "\nAlbum: ", master_metadata_album_album_name)))+
      geom_col(fill = "#1ED760")+
      theme(panel.background = element_rect(fill = "#121212"),
            plot.background = element_rect(fill = "#121212"),
            text = element_text(color = "#FFFFFF"),
            axis.text = element_text(color = "#FFFFFF"),
            legend.text = element_text(colour = "#FFFFFF"),
            axis.ticks = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(size = 0.3, colour = "#888888"),
            axis.title.y = element_blank(),
            axis.title.x = element_text())+
      labs(title = "Favourite albums", x = "Minutes listened")
    
    plotly::ggplotly(p, source = "2", tooltip = "text") %>% 
      config(displayModeBar = FALSE) %>% 
      layout(
        yaxis = list(fixedrange = TRUE),
        xaxis = list(fixedrange = TRUE)
      )
  })
  
  output$plot5 <- plotly::renderPlotly({
    
    if(input$personPlot3 == "Mikołaj"){
      df <- df_mikolaj
    }
    else if(input$personPlot3 == "Krzysiek"){
      df <- df_krzysiek
    }
    else {
      df <- df_daniel
    }
    
    
    df1 <- df %>% 
      group_by(master_metadata_track_name) %>% 
      summarise(Time = sum(ms_played)/60000) %>% 
      arrange(-Time) %>% 
      head(input$n) %>% 
      mutate(master_metadata_album_album_name = fct_reorder(master_metadata_track_name, Time))
    
    
    p <- ggplot(df1,aes(x = Time, y = master_metadata_track_name, text = paste("Time: ", Time, "\nTrack: ", master_metadata_track_name)))+
      geom_col(fill = "#1ED760")+
      theme(panel.background = element_rect(fill = "#121212"),
            plot.background = element_rect(fill = "#121212"),
            text = element_text(color = "#FFFFFF"),
            axis.text = element_text(color = "#FFFFFF"),
            legend.text = element_text(colour = "#FFFFFF"),
            axis.ticks = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(size = 0.3, colour = "#888888"),
            axis.title.y = element_blank(),
            axis.title.x = element_text())+
      labs(title = "Favourite tracks", x = "Minutes listened")
    
    plotly::ggplotly(p, source = "3", tooltip = "text") %>% 
      config(displayModeBar = FALSE) %>% 
      layout(
        yaxis = list(fixedrange = TRUE),
        xaxis = list(fixedrange = TRUE)
      )
    
  })
  
  
  output$description <- renderText(
    "Welcome to our site! This is the result of our work for Data Visualization Techniques course. We analyzed our data from Spotify, made an interactive dashboard and now we want to share it with you! In the first tab you can see our favorite artists, tracks and albums. In the second tab you can see listening time statistics. The third tab is about our music taste. We tried to make the site as interactive as possible, so don’t forget to click everything. We hope you will enjoy our site.
    \nDaniel, Krzysiek, Mikołaj"
  )
  
  
  observeEvent(event_data("plotly_click", source = "3"), {
    if(input$personPlot3 == "Mikolaj"){
      df <- df_mikolaj
    }
    else if(input$personPlot3 == "Krzysiek"){
      df <- df_krzysiek
    }
    else{
      df <- df_daniel
    }
    
    barData = event_data("plotly_click", source = "3")
    
    df1 <- df %>% 
      group_by(master_metadata_track_name) %>% 
      summarise(Time = sum(ms_played)/60000) %>% 
      arrange(-Time) %>% 
      head(input$n) %>% 
      mutate(master_metadata_track_name = fct_reorder(master_metadata_track_name, Time))
    
    song <- df1[barData$pointNumber+1,1] %>% 
      mutate(master_metadata_track_name = as.character(master_metadata_track_name))
    
    song <- as.character(song)
    
    wykon <- ez <- as.character(df_daniel %>%
                                  filter(master_metadata_track_name == song)%>%
                                  head(1)%>%
                                  select(master_metadata_album_artist_name))
    
    showModal(modalDialog(easyClose = TRUE, title = tags$a(style = "color: white", icon('robot'), as.character(song)),
                          renderText(paste("Wykonawca : ", wykon)),
                          br(),
                          renderPlot({
                            endMonth <- as.character(c("01","02","03","04","05","06","07","08","09","10","11","12"))
                            MonthSum <- c(0,0,0,0,0,0,0,0,0,0,0,0)
                            df1 <- data.frame(endMonth, MonthSum)
                            
                            df2 <- df %>% 
                              mutate(endTime = as.Date(ts)) %>% 
                              mutate(
                                endMonth = strftime(endTime, "%m")
                              ) %>% 
                              filter(master_metadata_track_name == song) %>% 
                              group_by(endMonth) %>% 
                              summarise(MonthSum = sum(ms_played)/60000)
                            
                            df3 <- rbind(df2,df1)
                            
                            ggplot(df3,aes(x = endMonth, y = MonthSum))+
                              geom_col(fill = "#1ED760")+
                              theme(panel.background = element_rect(fill = "#121212"),
                                    plot.background = element_rect(fill = "#121212"),
                                    text = element_text(color = "#FFFFFF"),
                                    axis.text = element_text(color = "#FFFFFF"),
                                    legend.text = element_text(colour = "#FFFFFF"),
                                    axis.ticks = element_blank(),
                                    panel.grid.minor = element_blank(),
                                    panel.grid.major = element_line(size = 0.3, colour = "#888888"),
                                    axis.title.y = element_text(),
                                    axis.title.x = element_text())+
                              labs(x = "Month", y = "Minutes played")+
                              scale_x_discrete(labels = c("Jan","Feb","Mar", "Apr", "May", "June", "Jul","Aug","Sep","Oct","Nov","Dec"))
                          }
                          )
    ))
  })
  
  
  
  observeEvent(event_data("plotly_click", source = "2"), {
    
    if(input$personPlot3 == "Mikolaj"){
      df <- df_mikolaj
    }
    else if(input$personPlot3 == "Krzysiek"){
      df <- df_krzysiek
    }
    else{
      df <- df_daniel
    }
    
    
    barData = event_data("plotly_click", source = "2")
    
    
    df1 <- df %>% 
      group_by(master_metadata_album_album_name) %>% 
      summarise(Time = sum(ms_played)/60000) %>% 
      arrange(-Time) %>% 
      head(input$n) %>% 
      mutate(master_metadata_album_album_name = fct_reorder(master_metadata_album_album_name, Time))
    
    artist <- df1[barData$pointNumber+1,1] %>% 
      mutate(master_metadata_album_album_name = as.character(master_metadata_album_album_name))
    
    artist <- as.character(artist)
    
    favSong <- df %>% 
      filter(master_metadata_album_album_name == artist) %>% 
      group_by(master_metadata_track_name) %>% 
      summarise(Sum = sum(ms_played)/60000) %>%
      arrange(-Sum) %>% 
      head(5)
    colnames(favSong) <- c("Track Name", "Minutes Listened")
    
    wykon <- ez <- as.character(df_daniel %>%
                                  filter(master_metadata_track_name == artist)%>%
                                  head(1)%>%
                                  select(master_metadata_album_artist_name))
    
    
    showModal(modalDialog(title = tags$a(style = "color: white", icon('robot'), as.character(artist)), easyClose = TRUE,
                          renderText(paste("Wykonawca : ", wykon)),
                          br(),
                          renderPlot({
                            endMonth <- as.character(c("01","02","03","04","05","06","07","08","09","10","11","12"))
                            MonthSum <- c(0,0,0,0,0,0,0,0,0,0,0,0)
                            
                            df1 <- data.frame(endMonth, MonthSum)
                            
                            
                            
                            df2 <- df %>% 
                              mutate(endTime = as.Date(ts)) %>% 
                              mutate(
                                endMonth = strftime(endTime, "%m")
                              ) %>% 
                              filter(master_metadata_album_album_name == artist) %>% 
                              group_by(endMonth) %>% 
                              summarise(MonthSum = sum(ms_played)/60000)
                            
                            df3 <- rbind(df2,df1)
                            
                            ggplot(df3,aes(x = endMonth, y = MonthSum))+
                              geom_col(fill = "#1ED760")+
                              theme(panel.background = element_rect(fill = "#121212"),
                                    plot.background = element_rect(fill = "#121212"),
                                    text = element_text(color = "#FFFFFF"),
                                    axis.text = element_text(color = "#FFFFFF"),
                                    legend.text = element_text(colour = "#FFFFFF"),
                                    axis.ticks = element_blank(),
                                    panel.grid.minor = element_blank(),
                                    panel.grid.major = element_line(size = 0.3, colour = "#888888"),
                                    axis.title.y = element_text(),
                                    axis.title.x = element_text())+
                              labs(x = "Month", y = "Minutes played")+
                              scale_x_discrete(labels = c("Jan","Feb","Mar", "Apr", "May", "June", "Jul","Aug","Sep","Oct","Nov","Dec"))
                          }),
                          br(),
                          render_gt({
                            gt(favSong) %>% 
                              tab_header(title = md("Favourite tracks on selected album")) %>% 
                              tab_style(
                                style = list(
                                  cell_text(color = 'white'),
                                  cell_fill(color = "#121212")
                                ),
                                locations = list(
                                  cells_body(),
                                  cells_title(),
                                  cells_column_labels()
                                  
                                )
                              )
                            
                            
                          })
    ))
    
    
  })
  
  
  
  
  observeEvent(event_data("plotly_click", source = "1"), {
    
    if(input$personPlot3 == "Mikolaj"){
      df <- df_mikolaj
    }
    else if(input$personPlot3 == "Krzysiek"){
      df <- df_krzysiek
    }
    else{
      df <- df_daniel
    }
    
    barData = event_data("plotly_click", source = "1")
    
    df1 <- df %>% 
      group_by(master_metadata_album_artist_name) %>% 
      summarise(Time = sum(ms_played)/60000) %>% 
      arrange(-Time) %>% 
      head(input$n) %>% 
      mutate(master_metadata_album_artist_name = fct_reorder(master_metadata_album_artist_name, Time))
    
    artist <- df1[barData$pointNumber+1,1] %>% 
      mutate(master_metadata_album_artist_name = as.character(master_metadata_album_artist_name))
    
    artist <- as.character(artist)
    
    favSong <- df %>% 
      filter(master_metadata_album_artist_name == artist) %>% 
      group_by(master_metadata_track_name) %>% 
      summarise(Sum = sum(ms_played)/60000) %>%
      arrange(-Sum) %>% 
      head(5)
    colnames(favSong) <- c("Track Name", "Minutes Listened")
    
    
    
    showModal(modalDialog(easyClose = TRUE, title = tags$a(style = "color: white", icon('robot'), as.character(artist)), renderPlot({
      
      endMonth <- as.character(c("01","02","03","04","05","06","07","08","09","10","11","12"))
      MonthSum <- c(0,0,0,0,0,0,0,0,0,0,0,0)
      
      df1 <- data.frame(endMonth, MonthSum)
      
      
      
      df2 <- df %>% 
        mutate(endTime = as.Date(ts)) %>% 
        mutate(
          endMonth = strftime(endTime, "%m")
        ) %>% 
        filter(master_metadata_album_artist_name == artist) %>% 
        group_by(endMonth) %>% 
        summarise(MonthSum = sum(ms_played)/60000)
      
      df3 <- rbind(df2,df1)
      
      ggplot(df3,aes(x = endMonth, y = MonthSum))+
        geom_col(fill = "#1ED760")+
        theme(panel.background = element_rect(fill = "#040404"),
              plot.background = element_rect(fill = "#040404"),
              text = element_text(color = "#FFFFFF"),
              axis.text = element_text(color = "#FFFFFF"),
              legend.text = element_text(colour = "#FFFFFF"),
              axis.ticks = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_line(size = 0.3, colour = "#888888"),
              axis.title.y = element_blank(),
              axis.title.x = element_text())+
        labs(x = "Month", y = "Minutes played")+
        scale_x_discrete(labels = c("Jan","Feb","Mar", "Apr", "May", "June", "Jul","Aug","Sep","Oct","Nov","Dec"))
    }),
    
    br(),
    render_gt({
      gt(favSong) %>% 
        tab_header(title = md("Favourite tracks")) %>% 
        tab_style(
          style = list(
            cell_text(color = 'white'),
            cell_fill(color = "#121212")
          ),
          locations = list(
            cells_body(),
            cells_title(),
            cells_column_labels()
            
          )
        )
      
      
    })
    
    ))
  })
  
  output$sidebar <- renderUI({
    if (input$menu == "top") {
      tagList(
        sliderInput(
          inputId = "n", 
          label = "Select number of displayed columns: ", 
          min = 5, 
          max = 15, 
          value = 10
        ),
        selectInput(
          inputId = "personPlot3", 
          label = "Select person: ", 
          choices = c("Krzysiek", "Mikołaj", "Daniel"), 
          selected = "Krzysiek"
        )
      )
    } else if (input$menu == "preferences") {
      selectInput("person", "Select person:", choices = c("Daniel", "Krzysiek", "Mikołaj"))
    }
    
  })
  
}







app_ui <- dashboardPage(
  title = "SpotifyViz",
  dashboardHeader(title = tags$a(img(src='Spotify_Logo_RGB_Green.png', style = "width: 131px; background-color: #040404"))), 
  dashboardSidebar(
    width = "230px",
    sidebarMenu(
      id = "menu",
      menuItem("Top", tabName = "top", icon = icon('heart')),
      menuItem("Activity", tabName = "activity", icon = icon('calendar')),
      menuItem("Preferences", tabName = "preferences", icon = icon('chart-line'))
    ),
    uiOutput("sidebar")
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
      .main-sidebar .sidebar .sidebar-menu {
        width: 230px;
        height: 100%;
        background-color: #040404;
      }
      
      
      html {
        font-size: 16px;
        color: #FFFFFF !important;
      }
      
      .content-wrapper .content {
          background-color: #121212;
      }
      
      * {
          font-family: "Open Sans", sans-serif;
          letter-spacing: -0.35px;
      } 
      
      .skin-blue .main-header .navbar {
        background-color: #040404;
      }       
      
      skin-blue .main-sidebar {
        background-color: #040404;
      }
      
      .box .box-body {
        background-color: #121212;
        -webkit-box-shadow: none; 
        -moz-box-shadow: none;
        box-shadow: none;
      }
      
      .logo {
        background-color:  #040404 !important;
      }
      .navbar {
        background-color:  #040404 !important;
      }
      
      .radio {
        font-weight: normal;
        background-color: #121212;
      }
      
      .control-label {
        font-weight: normal;
      }
      .shiny-bound-input {
        background-color: #040404;

      }
      
      #sidebarCollapsed { 
        background-color: #040404;
      }
      
      
      .skin-blue .main-sidebar .sidebar .sidebar-menu  a{
        border-radius: 5px;
        border-color: transparent;
      }
      
      .skin-blue .main-header .navbar .sidebar-toggle:hover {
          background-color: #1ED760;
      }
      
      
      .radio-inline {
          background-color: #121212 !important;

      }
      
      #p2_buckets {
        background-color: #121212 !important;
      }
      
    
      #p2_reset {
        background-color: #1ED760 !important;
      }
      
      
      .modal-content {
        background-color: #121212;
      }

    '))),
    tabItems(
      tabItem(tabName = "preferences",
              fluidRow(
                box(h4("Prefered tracks tempo", style = "color: #FFFFFF"),
                    withSpinner(plotOutput("tempo_histogram"), color="#1ED760", type=4),
                    background = "red"),
                box(h4("Favourite genres", style = "color: #FFFFFF"),
                    withSpinner(plotOutput("genres"), color="#1ED760", type=4), background = "red")
              ), 
              fluidRow(
                box(
                  h4("Prefered features comparison", style = "color: #FFFFFF"),
                  withSpinner(plotlyOutput("radar_plot", width = "90%"), color="#1ED760", type=4),
                  width = 6, background = "red"
                  
                ),
                box(
                  withSpinner(plotlyOutput("density_plot"), color="#1ED760", type=4),
                  width = 6, background = "red")  
              )
              
      ),
      tabItem(tabName = "top",
              chooseSliderSkin("Flat", "#1ED760"),
              fluidRow(
                box(h2("Welcome!"),p("Welcome to our site! This is the result of our work for Data Visualization Techniques course. We analyzed our data from Spotify, made an interactive dashboard and now we want to share it with you! In the first tab you can see our favorite artists, tracks and albums. In the second tab you can see listening time statistics. The third tab is about our music taste. We tried to make the site as interactive as possible, so don’t forget to click everything. We hope you will enjoy our site."),
                    br(), p("Daniel, Krzysiek, Mikołaj"), background = "red"),
                box(plotlyOutput("plot5"), background = "red")
              ),
              fluidRow(
                box(plotlyOutput("plot3"), background = "red"),
                box(plotlyOutput("plot4"), background = "red")
              )
      ),
      tabItem(tabName = "activity", 
              useShinyjs(),
              chooseSliderSkin("Flat", "#1ED760"),
              h1("Listening time analysis", style = "color: #FFFFFF;"),
              uiOutput("p2_UI")
      )
    )
  )
)

shinyApp(app_ui, server)