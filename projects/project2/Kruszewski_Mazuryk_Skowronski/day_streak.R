



day_streak<- function(show_progress = F){
  #funkcja zwraca tabelke wszystkich day streakow w osobistych konwersacjach, 
  # jest informacja  kiedy sie zaczely i skonczyly, ile trwaly dni i z kim 
  path <-paste0(current_path,"\\messages\\inbox\\")
  dirs <-  dir(path)
  m <- length(dirs)
  j = 0
  for(d in dirs){
    j = j + 1
    dir_path <-  paste0(path, d)
    files <- dir(dir_path, pattern = "*.json")
    participant_check <- fromJSON(paste0(dir_path,"\\",files[1]))
    participant_check <- as.character(unlist(participant_check$participants))
    suppressWarnings(rm(full_df))
    if(length(participant_check)>2){next}
    for (file in files) {
      text <- paste0(dir_path, "\\", file)
      full <-   fromJSON(txt = text)
      if(!exists("date_df")){
        date_df <-  full$messages
      } else {
        date_df <- bind_rows(date_df,full$messages)
      }
    }
  
    date_df$timestamp_ms <- anytime(date_df$timestamp_ms/1000)
    date_df <- date_df %>% 
      mutate(Date_Y_M_D = as.Date(timestamp_ms)) %>% 
      arrange(timestamp_ms) %>% select(Date_Y_M_D) %>% unique()
    
    n <-  length(date_df$Date_Y_M_D)
    if(n<2){next}
    count <-  0
    x <-  0
    start_dates <- Sys.Date()
    for(i in 1:(n)){
      count = count + 1
      if(count == 1){
        start_dates <- c(start_dates,date_df$Date_Y_M_D[i])
      }
      if(!is.na(date_df$Date_Y_M_D[i+1]) && date_df$Date_Y_M_D[i+1] != (date_df$Date_Y_M_D[i] + 1)){
        x <- c(x,count)
        count <- 0
      }
    }
    start_dates <- head(start_dates,-1)
    
    if(!exists("streaks")){
      streaks <-  as.data.frame(list("streak" = x,"start"=start_dates,"end" = (start_dates + x-1),"participant" = participant_check[1]))
    } else {
      streaks <- bind_rows(streaks,as.data.frame(list("streak" = x,"start"=start_dates,"end" = (start_dates + x-1),"participant" = participant_check[1])))
    }
    
    rm(date_df)
    if(show_progress == T){
      print(paste0(round(j/m*100,1),"%"))
    }
  }
  streaks
}

# test1 <- day_streak(show_progress = T) 
# 
# #przykladowe wykresy
# test1 %>% slice_max(streak,n = 6) %>% 
#   ggplot() +
#   geom_segment( aes(x=participant, xend=participant, y=start, yend=end),size = 1) +
#   geom_point(aes(x=participant, y=start), color="#FFC419", size=1) +
#   geom_point( aes(x=participant, y=end), color="#3387FF", size=1 ) +
#   coord_flip()
# 
# ###jakos dziala
# test1 %>% slice_max(streak,n = 6) %>% 
#   ggplot(aes(x = participant,y = streak)) +
#   geom_col(position="dodge2") +
#   coord_flip()




day_streak_1p <- function(dir_name){
  #day_streak, ale dla jednej konwersacji. Raczej nieprzydatny.
  test <- load_one_conversation(dir_name = dir_name) %>% arrange(timestamp_ms)  %>% select(Date_Y_M_D) %>% unique()
  n <-  length(test$Date_Y_M_D)
  count = 0
  x <-  0
  start_dates <- Sys.Date()
  for(i in 1:(n-1)){
    count = count + 1
    if(count == 1){
      start_dates <- c(start_dates,test$Date_Y_M_D[i])
    }
    if(test$Date_Y_M_D[i+1] != test$Date_Y_M_D[i] + 1){
      x <- c(x,count)
      count <- 0
    }
  }
  start_dates <- head(start_dates,-1)
  streaks <- as.data.frame(list("streak" = x,"start"=start_dates,"end" = (start_dates + x-1)))
  streaks <- streaks[-1,] %>% arrange(desc(streak))
}




