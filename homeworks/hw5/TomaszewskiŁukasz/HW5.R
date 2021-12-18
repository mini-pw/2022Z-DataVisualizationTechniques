library(dplyr)
library(plotly)

df <- read.csv("Dane/complete.csv")

df <- df %>% 
  select(Rok = awardYear,Kategoria = category,Kontynent = birth_continent,
         Laureat = knownName,Plec = gender,Kraj = birth_country) %>% 
  arrange(Rok) %>% group_by(Rok) %>% filter(Rok != "",Kontynent !="")


df_medycyna = df %>% filter(Kategoria == "Physiology or Medicine")
df_pokoj = df %>% filter(Kategoria == "Peace")
df_chemia = df %>% filter(Kategoria == "Chemistry")
df_literatura = df %>% filter(Kategoria == "Literature")
df_fizyka = df %>% filter(Kategoria == "Physics")
df_economia = df %>% filter(Kategoria == "Economic Sciences")


plot_ly(type = 'scatter', mode = 'markers') %>% 
  add_trace(data = df, x =~jitter(Rok,factor = 2), y =~Kontynent, color =~Plec,
            text = paste0("Name: ", df$Laureat, "<br>Category: ", df$Kategoria,
                          "<br>Year: ",df$Rok),
            hoverinfo = 'text') %>%
  add_trace(data = df_medycyna, x =~jitter(Rok,factor = 2), y =~Kontynent, color =~Plec,
            text = paste0("Name: ", df_medycyna$Laureat, "<br>Birth country: ", df_medycyna$Kraj,
                          "<br>Year: ",df_medycyna$Rok),
            hoverinfo = 'text',visible=F) %>%
  add_trace(data = df_pokoj, x =~jitter(Rok,factor = 2), y =~Kontynent, color =~Plec,
            text = paste0("Name: ", df_pokoj$Laureat, "<br>Birth country: ", df_pokoj$Kraj,
                          "<br>Year: ",df_pokoj$Rok),
            hoverinfo = 'text',visible=F) %>%
  add_trace(data = df_chemia, x =~jitter(Rok,factor = 2), y =~Kontynent, color =~Plec,
            text = paste0("Name: ", df_chemia$Laureat, "<br>Birth country: ", df_chemia$Kraj,
                          "<br>Year: ",df_chemia$Rok),
            hoverinfo = 'text',visible=F) %>%
  add_trace(data = df_literatura, x =~jitter(Rok,factor = 2), y =~Kontynent, color =~Plec,
            text = paste0("Name: ", df_literatura$Laureat, "<br>Birth country: ", df_literatura$Kraj,
                          "<br>Year: ",df_literatura$Rok),
            hoverinfo = 'text',visible=F) %>%
  add_trace(data = df_fizyka, x =~jitter(Rok,factor = 2), y =~Kontynent, color =~Plec,
            text = paste0("Name: ", df_fizyka$Laureat, "<br>Birth country: ", df_fizyka$Kraj,
                          "<br>Year: ",df_fizyka$Rok),
            hoverinfo = 'text',visible=F) %>%
  add_trace(data = df_economia, x =~jitter(Rok,factor = 2), y =~Kontynent, color =~Plec,
            text = paste0("Name: ", df_economia$Laureat, "<br>Birth country: ", df_economia$Kraj,
                          "<br>Year: ",df_economia$Rok),
            hoverinfo = 'text',visible=F) %>%
  layout(title = "Nobel laureates",
         updatemenus = list(
           list(x=1.15,y=0.8,
             buttons = list(
               list(args = list("visible", c(F,T,T,F,F,F,F,F,F,F,F,F,F,F,F)),
                    label = "All",
                    method = "restyle"),
               list(args = list("visible", c(F,F,F,T,T,F,F,F,F,F,F,F,F,F,F)),
                    label = "Physiology or Medicine",
                    method = "restyle"),
               list(args = list("visible", c(F,F,F,F,F,T,T,F,F,F,F,F,F,F,F)),
                    label = "Peace",
                    method = "restyle"),
               list(args = list("visible", c(F,F,F,F,F,F,F,T,T,F,F,F,F,F,F)),
                    label = "Chemistry",
                    method = "restyle"),
               list(args = list("visible", c(F,F,F,F,F,F,F,F,F,T,T,F,F,F,F)),
                    label = "Literature",
                    method = "restyle"),
               list(args = list("visible", c(F,F,F,F,F,F,F,F,F,F,F,T,T,F,F)),
                    label = "Physics",
                    method = "restyle"),
               list(args = list("visible", c(F,F,F,F,F,F,F,F,F,F,F,F,F,T,T)),
                    label = "Economic Sciences",
                    method = "restyle")
             ))
         ), xaxis = list(
                         rangeslider = list(type = "date"),
                         mirror=TRUE,
                         ticks='outside',
                         showline=TRUE,
                         title =list(text='Year', font = list(size = 15))),
         yaxis = list(autotypenumbers = 'strict',
                      mirror=TRUE,
                      ticks='outside',
                      showline=TRUE,
                      title = list(text='Continent', font = list(size = 15), standoff = 20)))








