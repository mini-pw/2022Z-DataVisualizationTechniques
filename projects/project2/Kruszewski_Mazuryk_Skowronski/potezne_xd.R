




get_potezne_xd_messages <- function(df,sender,before = 5,after = 5){
  my <- make_my_df(df,sender)
  
  string_xd <- str_extract_all(my$content, "\\b[xX]+[dD]+\\b")
  xd_uni <- unique(na.omit(unlist(string_xd)))
  potezne_xd <- xd_uni[which.max(nchar(xd_uni))]
  one <- which(grepl(df$content,pattern = potezne_xd))
  two <- which(df$sender_name == sender)
  index <- one[one %in% two]
  wynik <- df[(index-10):(index+10),] %>% select(timestamp_ms,sender_name,content)
  wynik
}

get_potezne_xd <- function(df){
  string_xd <- str_extract_all(df$content, "\\b[xX]+[dD]+\\b")
  xd_uni <- unique(na.omit(unlist(string_xd)))
  potezne_xd <- xd_uni[which.max(nchar(xd_uni))]
}

get_potezne_haha <- function(df){
  string_haha <- str_extract_all(df$content,  r"(\b(?:a*(?:ha)+h?|h*ha+h[ha]*)\b)")
  haha_uni <- unique(na.omit(unlist(string_haha)))
  potezne_xd <- haha_uni[which.max(nchar(haha_uni))]
}

get_potezne_k <- function(df){
  string_ku <- str_extract_all(df$content,  "\\b[kK]+[uU]+[rR]+[wW]+[aA]+\\b")
  ku_uni <- unique(na.omit(unlist(string_ku)))
  potezne_ku <- ku_uni[which.max(nchar(ku_uni))]
}
get_potezne_f <- function(df){
  string_f <- str_extract_all(df$content,  "\\b[fF]+[uU]+[cC]+[kK]+\\b")
  f_uni <- unique(na.omit(unlist(string_f)))
  potezne_f <- f_uni[which.max(nchar(f_uni))]
}

# moje_xd <- get_potezne_xd(df,"Damian Skowroński")


#jakos to sie zaprezentuje fajnie