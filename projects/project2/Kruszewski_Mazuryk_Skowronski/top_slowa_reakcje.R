




top_slowa <- function(df,min= 0){
  slowa <- as.data.frame(table(tolower(unlist(str_split(df$content, pattern = " "))))) %>% 
    mutate(Var1 = as.character(Var1)) %>% 
    filter(nchar(Var1)>min-1) %>% 
    arrange(desc(Freq))
}

# test <- top_slowa(my,5)
# head(test,20) #kurwa dopiero na 16, wiedzialem ze nie jestem wulgarnym czlowiekiem



# emoji <- jsonlite::fromJSON("emoji-en-US.json")
# emoji_names <- c()
# n <- length(emoji)
# for(i in 1:n){
#   emoji_names <-  c(emoji_names,emoji[[i]][1])
# }
# 
# emoji_df <- data.frame(emoji = names(emoji),emoji_names = emoji_names,raw = enc2native(names(emoji))) 
# emoji_df[127,3] <- "<U+2764>"
# fwrite(emoji_df,"emoji_df.csv")



emojis <- fread("emoji_df.csv",encoding = "UTF-8")

top_reakcje <- function(df,act){
  df$actor <- enc2native(df$actor)
  react_df <- df %>% select(reactions,actor) %>% filter(!is.na(reactions))  %>%
    mutate(actor = strsplit(actor, ", "),reactions = strsplit(reactions, ", ")) %>% 
    unnest(actor,reactions)  %>% 
    filter(actor == act) %>% table() %>% as.data.frame() %>% na.omit() %>% slice_max(Freq,n = 20) 
  test <- merge(react_df,emojis, by.x = "reactions", by.y = "raw")[,-1]
  test[,-1]
}

