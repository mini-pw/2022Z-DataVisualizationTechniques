

# make_my_df <- function(big_df,sender){
#   # tylko swoje wiadomosci
#   my <- big_df %>% filter(sender_name == sender)
#   my
# }



get_nr_of_messages <- function(df){
  #zwraca ilosc wiadomosci w zaleznosci od osoby w formie ramki danych
  all_count <- df %>% group_by(sender_name) %>% 
    count(name = "nr_of_messages") %>%
    as.data.frame() %>% 
    slice_max(nr_of_messages,n = 30)
}

#przyklady uzycia
#get_nr_of_messages(mess_df)
#get_nr_of_messages(load_one_conversation(dir_name = "danonki_pdzoaidjcq"))

 

substring_count <- function(my_df, slowa){
  #zaznaczam, tu dzieje nie szukamy slowa w calosci tylko zaliczy tez czesc slowa
  # na przyklad jesli szukam xd to znajde tez xd w xdd, czaisz
  # ze substring tez policzy
  searching <- str_extract_all(my_df$content, paste(slowa, collapse="|")) 
  count_df <- as.data.frame(table(na.omit(unlist(searching)))) %>% arrange(desc(Freq))
  count_df
}

# bruh_i_xd_count <- substring_count(my,c("bruh","xd"))
# siema_count <- substring_count(my,"siema")


word_count <- function(my_df,slowa){
  # to samo co w poprzednim, ale tym razem szukam calych slow, to znaczy jest nie biore pod uwage substringow
  search_by <-  paste0(rep("\\b",length = length(slowa)), slowa,rep("\\b",length = length(slowa)), collapse="|")
  searching <- str_extract_all(my_df$content,search_by) 
  count_df <- as.data.frame(table(na.omit(unlist(searching)))) %>% arrange(desc(Freq))
  count_df
}
#word_count(my,slowa = c("bruh", "siema","ok"))



df_words_timeline <- function(df, words){
  final_df <- data.frame()
  for(word in words){
    slowo_trends <- df[grepl(word,df$content),] 
    slowo_trends <- slowo_trends %>% 
      group_by(month = lubridate::floor_date(Date_Y_M_D,"month")) %>% 
      count() 
    slowo_trends$word <- word
    final_df <- bind_rows(final_df,slowo_trends)
  }
  final_df
}


words_timeline <- function(df){
  #przekazac wynik z df_words_timeline !!!!!!
  final_df <- df %>%
    pivot_wider(names_from = month, values_from = n,values_fill = 0) %>%
    pivot_longer(cols = -word, names_to = "month" ,values_to = "n") %>% 
    mutate(month = as.Date(month))
  final_df
}



get_xd <- function(df){
  string_xd <- str_extract_all(df$content, "\\b[xX]+[dD]+\\b")
  xd_df <- as.data.frame(table(na.omit(unlist(string_xd)))) %>% arrange(desc(Freq))
  as.character(xd_df$Var1[1:5])
}





get_haha <- function(df){
  string_haha <- str_extract_all(df$content, r"(\b(?:a*(?:ha)+h?|h*ha+h[ha]*)\b)")
  haha_df <- as.data.frame(table(na.omit(unlist(string_haha)))) %>% arrange(desc(Freq))
  haha_df <- haha_df[(haha_df$Var!="aha")&(haha_df$Var!="ha"),]
  as.character(haha_df$Var1[1:5])
}





xd_haha_comparison <- function(df,xd,haha){
 
  final_df <- data.frame()
  for(word in xd){
    slowo_trends <- df[grepl(word,df$content),] 
    slowo_trends <- slowo_trends %>% 
      group_by(month = lubridate::floor_date(Date_Y_M_D,"month")) %>% 
      count() 
    slowo_trends$word <- "xd"
    final_df <- bind_rows(final_df,slowo_trends)
  }
  for(word in haha){
    slowo_trends <- df[grepl(word,df$content),] 
    slowo_trends <- slowo_trends %>% 
      group_by(month = lubridate::floor_date(Date_Y_M_D,"month")) %>% 
      count() 
    slowo_trends$word <- "haha"
    final_df <- bind_rows(final_df,slowo_trends)
  }
  final_df <- final_df %>% group_by(word,month) %>% 
    summarise(n = sum(n))
  final_df <- final_df %>%
    pivot_wider(names_from = month, values_from = n,values_fill = 0) %>%
    pivot_longer(cols = -word, names_to = "month" ,values_to = "n") %>%
    mutate(Month = as.Date(month)) %>% 
    rename("Times used" = "n","Word" = "word")
  plt <- ggplot(final_df,aes(x = Month,y = `Times used`,group = Word,color = Word))+
    geom_line() +
    scale_x_date(date_breaks = "1 year",date_labels = "%Y") +
    labs(x = "year", y = "number of words", color = "roznego rodzaju")
  plt
}




messages_sent <- function(df,start_date =  as.Date("1970/01/01"),end_date = Sys.Date()){ #robie to, ze bedzie mozna w shiny dac slidera na przyklad z datami
  #funckja podlicza laczna ilosc wiadomosci w kazdym miesiacu od start_date do end_date
  df <- df %>% 
    group_by(month = lubridate::floor_date(Date_Y_M_D,unit = "month")) %>% 
    count() %>% 
    filter(month > start_date,month <end_date) 
  df
}






yes_or_no <- function(df_only){
  WHITE_TEXT = "#CDCDCD"
  GRAY_DARK = "#343E48"
  GRAY_LIGHT= "#44505A"
  BLUE = "#038FFF"
  SALMON = "#FF586A"
  df1<-df_words_timeline(df_only,c("tak","nie")) %>% mutate(type="no")
  df1$type[which(df1$word == "tak")]<-"yes"
  df1 %>% 
    ggplot(aes(x = month,y = n,group = type,color = type))+
    geom_line() +
    scale_x_date(expand = c(0,0)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15), add = c(0.5, 0))) +
    labs(x = "Year", y = "Times written",title = "Are you \"yes\" or \"no\" person")+
    dark_theme_gray(base_family = "Arial") + 
    scale_color_manual(labels = c("no", "yes"),values = c(BLUE, SALMON))+
    theme(plot.background = element_rect(fill = GRAY_DARK),
          panel.background = element_rect(fill = GRAY_LIGHT),
          plot.title = element_text(hjust = 0.5, size = 20),
          legend.title = element_blank(),
          legend.position = "top",
          legend.background = element_rect(fill = GRAY_LIGHT, color = GRAY_DARK))
}


ch_word_timeline <- function(df_only,ch_word){
  WHITE_TEXT = "#CDCDCD"
  GRAY_DARK = "#343E48"
  GRAY_LIGHT= "#44505A"
  BLUE = "#038FFF"
  SALMON = "#FF586A"
  df_words_timeline(df_only,c(ch_word)) %>% 
    ggplot(aes(x = month,y = n,group=1))+
    geom_line(size=1.5) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15), add = c(0.5, 0))) +
    labs(x = "Year", y = "Times written",title = paste0("\"",ch_word,"\""," timeline"))+
    dark_theme_gray(base_family = "Arial") + 
    theme(plot.background = element_rect(fill = GRAY_DARK),
          panel.background = element_rect(fill = GRAY_LIGHT),
          plot.title = element_text(hjust = 0.5, size = 20),
          legend.title = element_blank(),
          legend.position = "top",
          legend.background = element_rect(fill = GRAY_LIGHT, color = GRAY_DARK))
  }

yes_no_geom <- function(df){
  WHITE_TEXT = "#CDCDCD"
  GRAY_DARK = "#343E48"
  GRAY_LIGHT= "#44505A"
  BLUE = "#038FFF"
  SALMON = "#FF586A"
  final_df<-rbind(word_count(df,"tak"),word_count(df,"nie"))
  final_df$Var1<-c("yes","no")
  ggplot(data=final_df, aes(x=Var1, y=Freq)) + 
    geom_col(fill="#FF586A") +
    geom_text(aes(label=paste0(Var1," - ",Freq)),color='black',size=8, position=position_stack(0.5))+
    labs(y ="",title = "")+
    dark_theme_gray(base_family = "Arial") + 
    theme(plot.background = element_rect(fill = GRAY_DARK),
          panel.background = element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          legend.position="none",
          panel.grid.major=element_blank(),
          plot.title = element_text(hjust = 0.5, size = 20),
          legend.title = element_blank(),panel.grid.minor=element_blank(),
          legend.background = element_rect(fill = GRAY_LIGHT, color = GRAY_DARK))
}



