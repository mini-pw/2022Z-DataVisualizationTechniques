################################
############  INFO  ############
################################

# Ten plik daje funkcje wczytujace ramki z plikow .json do dalszego korzystania. Sa nimi:
# - load_one_conversation() - ktora wczytuje ramke danych dla pojedynczej konwersacji
# - load_all_conversations() - ktora wczytuje ramke danych dla wszystkich naszych konwersacji
# wiecej info o funkcjach w ich kodzie

# warto zaznaczyc, ze dla ogarniania z czasem nalezy uzywac kolumny Date_Y_M_D - ktora, ma w formacie date dane w Year-month-day


# //www.r-bloggers.com/2018/06/working-with-your-facebook-data-in-r/e  
# ^^^^^^^^^^^^^  ten link mocno pomogl ogarnac co i jak 

#przy okazji wczytuje potrzebne potem biblioteki

library(tidyverse)
library(jsonlite)
library(anytime)
library(stringi)
library(data.table)


#zeby znormalizowac dzialanie zakladam, ze masz ten skrypt w folderze z rozpakowanym zipem od fb
#zeby dziala?o wszystko fajnie bez podawania dlugich sciezek ustawiam working directory
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
current_path <- str_replace_all(getwd(), pattern = "[/]", replacement = "\\\\")


# FUNKCJE WCZYTUJACE RAMKI

load_one_conversation <- function(dir_name,participant_name, new = F){ 
  # funkcja zwraca ramke danych z rozmowy z osoba
  # funkcja przumuje dwa argumenty:
  # dir_name - string, dokladna nazwa folderu konwersacji w ~\messages\inbox
  # participent_name - string, imie i nazwisko osoby, z ktora prowadzilismy konwersacje
  # 
  # wczytywanie uzywajac participent_name jest dluzsze poniewaz, najpierw funkcja musi znalezc odpowiedni folder
  # wczytywanie uzywajac participent_name w przeciwienstwie do dir_name nie zwraca rozmow grupowych, wylacznie indywidualne
  #
  # new jeĹ›li ramki majÄ… juĹĽ przekonwertowane polskie litery
  
  if(!missing(dir_name)){
    path <- paste0(current_path,"\\messages\\inbox\\",dir_name)
    if(new){
      path <-paste0(current_path,"\\new\\messages\\inbox\\",dir_name) 
    }
    files <- dir(path, pattern = "*.json")
  } 
  else if(!is.null(participant_name)){
    path <-paste0(current_path,"\\messages\\inbox\\")
    if(new){
      path <-paste0(current_path,"\\new\\messages\\inbox\\") 
    }
    dirs <-  dir(path)
    for(d in dirs){
      dir_path <-  paste0(path, d)
      files <- dir(dir_path, pattern = "*.json")
      name_check <- fromJSON(paste0(dir_path,"\\",files[1]))
      name_check <- as.character(unlist(name_check$participants))
      if(length(name_check) == 2 && participant_name %in% name_check){
        path <- dir_path
        break
      }
      files <- NULL
    }
    if(is.null(files)){
      warning("no conversation with such person found")
      return()
    }
  }
  
  for (file in files) {
    text <- paste0(path, "\\", file)
    full <-   fromJSON(txt = text)
    if(!exists("one_df")){
      one_df <-  full$messages
    } else {
      one_df <- bind_rows(one_df,full$messages)
    }
  }
  one_df$timestamp_ms <- anytime(one_df$timestamp_ms/1000) #ogarniecie daty
  one_df <- one_df %>% 
    mutate(Date_Y_M_D = as.Date(timestamp_ms))
}




# teraz dla wszystkich konwersacji - wszystkich folderow w inbox


load_all_conversations <- function(show_progress = F, new = F){
  # funkcja zwraca ramke danych ze wszystkich naszych konwersacji w folderze \messages\inbox
  # show_progress - dla TRUE, bedzie wypisywac ile procent konwersacji sie wczytalo, 
  # poniewaz zajmuje to troche czasu i zeby bylo wiadomo, ze cos sie dzieje w tle
  # 
  # new jeĹ›li ramki majÄ… juĹĽ przekonwertowane polskie litery
  path <-paste0(current_path,"\\messages\\inbox\\")
  if(new){
    path <-paste0(current_path,"\\new\\messages\\inbox\\") 
  }
  dirs <-  dir(path)
  n <- length(dirs)
  i = 0
  for(d in dirs){
    i = i + 1
    dir_path <-  paste0(path, d)
    files <- dir(dir_path, pattern = "*.json")
    for (file in files) {
      text <- paste0(dir_path, "\\", file)
      full <-   fromJSON(txt = text)
      if(!exists("full_df")){
        full_df <-  full$messages
      } else {
        full_df <- bind_rows(full_df,full$messages)
      }
    }
    if(show_progress == T){
      print(paste0(round(i/n*100,1),"%"))
    }
  }
  full_df$timestamp_ms <- anytime(full_df$timestamp_ms/1000)
  full_df <- full_df %>% 
    mutate(Date_Y_M_D = as.Date(timestamp_ms))
}

# ramka taka jak do tej pory
# mess_df <- load_all_conversations(show_progress = T)



###################################################
##### TUTAJ JEST DO ZAPISYWANIA RAMKI DO .csv #####
###################################################




make_my_df <- function(file_name){
  reticulate::py_run_file(file = "decode_encode.py")
  
  mess_df <- load_all_conversations(show_progress = T,new = T)
  mess_df <- flatten(mess_df)
  messs_df <- mess_df %>% 
    mutate_if(is.list,as.character) 
  messs_df <- messs_df %>% 
    mutate(photos = ifelse(messs_df$photos == "NULL","NA",messs_df$photos), 
           users = ifelse(messs_df$users == "NULL","NA",messs_df$users), 
           videos = ifelse(messs_df$videos == "NULL","NA",messs_df$videos), 
           gifs = ifelse(messs_df$gifs == "NULL","NA",messs_df$gifs), 
           audio_files = ifelse(messs_df$audio_files == "NULL","NA",messs_df$audio_files), 
           files = ifelse(messs_df$files == "NULL","NA",messs_df$files),
           reactions = ifelse(messs_df$reactions == "NULL","NA",messs_df$reactions))
  
  messs_df$reactions <- sub( ".*reaction = ","",messs_df$reactions)
  messs_df$reactions <- gsub(r"(c\(|\)|\")","",messs_df$reactions)
  messs_df$actor <- sub( ".*actor = ","",messs_df$reactions)
  messs_df$reactions <- sub( ", actor.*","",messs_df$reactions)
  
  # messs_df$reactions <- gsub(r"(<U\+000|<U\+)","",messs_df$reactions)
  
  # messs_df$reactions <- gsub(r"(<U\+000|<U\+)","",messs_df$reactions)
  # messs_df$reactions <- gsub(">","",messs_df$reactions)
  # messs_df$reactions <- gsub(r"(<U\+000)"," ",messs_df$reactions)
  
  # messs_df$actor <- enc2native(messs_df$actor)
  
  messs_df$actor <- stringi::stri_trans_general(messs_df$actor,"Latin-ASCII")
  messs_df$actor <- str_replace_all(messs_df$actor,"3","l")
  
  Encoding(messs_df$sender_name) <- "unknown"
  Encoding(messs_df$content) <- "unknown"
  Encoding(messs_df$actor) <- 'unknown'
  fwrite(messs_df, file_name)              
}





#zrobienie ramki do apki
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# make_my_df("diego.csv")

# diego<- fread("diego.csv",encoding = "UTF-8")



