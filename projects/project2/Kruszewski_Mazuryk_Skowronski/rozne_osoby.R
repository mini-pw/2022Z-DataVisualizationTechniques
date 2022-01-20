# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# source("wczytanie_json.R")


rozne_osoby <- function(big_df, receiver, start_date =  as.Date("1970/01/01"),end_date = as.Date("2022/01/01")){
  #receiver - string bedacy sender_name jednego z nas
  df1 <- big_df %>% 
    filter(sender_name!=receiver) %>% 
    group_by(month = lubridate::floor_date(Date_Y_M_D,unit = "month")) %>% 
    summarise(il_osob = n_distinct(sender_name)) %>% 
    filter(month > start_date,month <end_date) 
  df1
}

