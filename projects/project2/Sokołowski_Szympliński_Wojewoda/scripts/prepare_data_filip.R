library(dplyr)
library(stringi)
library(tidyr)

date_conversion <- function(data){
  switch(Sys.info()[['sysname']],
         Windows= {Sys.setlocale("LC_TIME", "English")},
         Linux  = {Sys.setlocale("LC_TIME", "en_US.UTF-8")})
  
  data <- separate(data = data, col = Start.time, into =c("Start.time","right"), sep = "GMT")
  data <- separate(data = data, col = right, into = c("left", "Rok"), sep = " ")
  data <- data %>% select(Device, App.name, Start.time, Duration.ms, Rok)
  data <- data %>% mutate(duration_minutes = Duration.ms/(60*10^3))
  data$Start.time <- as.POSIXlt(strptime(data$Start.time, "%a %b %d %H:%M:%S "))
  data <- data %>% mutate(Godzina_pelna = strftime(data$Start.time, format = "%H:%M:%S"), 
                          Godzina = strftime(data$Start.time, format = "%H"), 
                          Data = strftime(data$Start.time, format = "%m-%d"))
  data$Data <- paste(data$Rok, "-", data$Data, sep='')
  data <- data %>% mutate(Godzina = as.numeric(Godzina))
  data <- data %>% mutate(across("duration_minutes", round, 2))
  data <- data %>% select(Device, App.name, Data, Godzina, duration_minutes)
}

# Jedrek data-------------------------------------------------------------------
prepare_jedrek_df <- function(df_jedrek){
  df_jedrek <- date_conversion(df_jedrek)
levels(df_jedrek$App.name)[levels(df_jedrek$App.name) == "Opera Touch"] <- "Browser"
levels(df_jedrek$App.name)[levels(df_jedrek$App.name) == "Spotify"] <- "Spotify/Tidal"

levels(df_jedrek$Device) <- "Jedrek"
df_jedrek <- df_jedrek %>% 
  filter(App.name != "Samsung Experience Home")
df_jedrek$Data <- as.Date(as.character(df_jedrek$Data))

return(df_jedrek)
}

# Filip data--------------------------------------------------------------------
prepare_filip_df <- function(df_filip){
  df_filip <- date_conversion(df_filip)
levels(df_filip$App.name)[levels(df_filip$App.name) == "Chrome"] <- "Browser"
levels(df_filip$App.name)[levels(df_filip$App.name) == "Spotify"] <- "Spotify/Tidal"
levels(df_filip$App.name)[levels(df_filip$App.name) == "Wiadomości"] <- "Messages"
levels(df_filip$App.name)[levels(df_filip$App.name) == "Telefon"] <- "Phone"
levels(df_filip$App.name)[levels(df_filip$App.name) == "Mapy"] <- "Maps"
levels(df_filip$App.name)[levels(df_filip$App.name) == "Galeria"] <- "Gallery"

levels(df_filip$Device) <- "Filip"
df_filip <- df_filip %>% 
  filter(App.name != "Systemowy menadżer pulpitu")
df_filip$Data <- as.Date(as.character(df_filip$Data))

return(df_filip)
}

# Malwina data------------------------------------------------------------------
prepare_malwina_df <- function(df_malwina){
  df_malwina <- date_conversion(df_malwina)
levels(df_malwina$App.name)[levels(df_malwina$App.name) == "Chrome"] <- "Browser"
levels(df_malwina$App.name)[levels(df_malwina$App.name) == "Spotify"] <- "Spotify/Tidal"
levels(df_malwina$App.name)[levels(df_malwina$App.name) == "Wiadomości"] <- "Messages"
levels(df_malwina$App.name)[levels(df_malwina$App.name) == "Telefon"] <- "Phone"
levels(df_malwina$App.name)[levels(df_malwina$App.name) == "Mapy"] <- "Maps"
levels(df_malwina$App.name)[levels(df_malwina$App.name) == "Galeria"] <- "Gallery"

levels(df_malwina$Device) <- "Malwina"
df_malwina <- df_malwina %>% 
  filter(App.name != "Ekran główny Huawei",
         App.name != "Menedżer telefonu")
df_malwina$Data <- as.Date(as.character(df_malwina$Data))

return(df_malwina)
}