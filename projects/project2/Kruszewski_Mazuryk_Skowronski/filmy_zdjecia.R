# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# source("wczytanie_json.R")


# df <- load_all_conversations()

photos_videos <- function(big_df){
  photos_nr <- big_df %>% filter(!is.na(photos)) %>%  group_by(sender_name) %>% summarise(photo = n())
  videos_nr <- big_df %>% filter(!is.na(videos)) %>% group_by(sender_name) %>% summarise(video = n())
  DF <- left_join(photos_nr,videos_nr, by="sender_name") 
  DF
}



