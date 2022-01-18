library(dplyr)
library(stringi)
library(tidyr)

switch(Sys.info()[['sysname']],
       Windows= {Sys.setlocale("LC_TIME", "English")},
       Linux  = {Sys.setlocale("LC_TIME", "en_US.UTF-8")})

# Konwersja daty z formatu Wed Dec 08 22:42:26 GMT+01:00 2021 na dwie kolumny:
# "Date" w formacie 2021-12-08 i "Godzina" w formacie 22:42:26
# zamiana "duration.ms" na duration_minutes w minutach do 2 miejsc po przecinku
convert_data_by_filip <- function(data){
  data <- separate(data = data, col = Start.time, into =c("Start.time","right"), sep = "GMT")
  data <- separate(data = data, col = right, into = c("left", "Rok"), sep = " ")
  data <- data %>% select(App.name, Start.time, Duration.ms, Rok)
  data <- data %>% mutate(duration_minutes = Duration.ms/(60*10^3))
  data$Start.time <- as.POSIXlt(strptime(data$Start.time, "%a %b %d %H:%M:%S "))
  data <- data %>% mutate(Godzina_pelna = strftime(data$Start.time, format = "%H:%M:%S"), 
                          Godzina = strftime(data$Start.time, format = "%H"), 
                          Data = strftime(data$Start.time, format = "%m-%d"))
  data$Data <- paste(data$Rok, "-", data$Data, sep='')
  data <- data %>% mutate(Godzina = as.numeric(Godzina))
  data <- data %>% mutate(across("duration_minutes", round, 2))
  data <- data %>% select(App.name, Data, Godzina, duration_minutes)
}
