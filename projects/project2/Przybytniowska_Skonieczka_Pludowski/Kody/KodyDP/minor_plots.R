library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(dplyr)
library(stringi)
library(ggplot2)

xd.preprocessing <- function(string_chain, type) {
  regex <- paste(ifelse(type == "all", "[xX][dD]", type), "+", sep = "")
  s <- stri_split_regex(string_chain, "[,.:;~`\"\\s]+", simplify = T, omit_empty = T)
  dim(s) <- NULL
  names(s) <- NULL
  s[stri_detect_regex(s, regex)]
}

mkXDPlot <- function(df, type = "all", minyear = 2000, maxyear = 2021, color = "#EE82EE") {
  
  palette1 <- rep(c("#1E90FF", "#8A2BE2", "#EE82EE"), 10)
  
  df %>% 
    mutate(year = as.integer(format(as.POSIXct(time), format="%Y"))) %>% 
    filter(year >= minyear, year <= maxyear) %>% 
    pull(content) %>% 
    xd.preprocessing(type = type) %>% 
    data.frame() %>% 
    rename(xd = ".") %>% 
    group_by(xd) %>% 
    summarise(freq = n()) %>% 
    arrange(-freq) %>% 
    head(10) %>% 
    mutate(len = as.factor(stri_length(xd) - 1)) %>%
    filter(!is.na(len)) %>% 
    ggplot(aes(x = len, y = freq, fill = len)) +
    geom_col(fill = color) + 
    coord_flip() + 
    theme(axis.text = element_text(size = 12)) +
    ylab("occurrences") +
    xlab("'D' in 'XD") +
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "#ffffff")
      )
}

mkLongestXD <- function(df, username) {
  res <- df %>% 
    filter(sender == username) %>% 
    pull(content) %>% 
    xd.preprocessing(type = "all") %>% 
    data.frame() %>% 
    rename(xd = ".") %>% 
    group_by(xd) %>% 
    summarise(freq = n()) %>% 
    mutate(len = stri_length(xd)) %>% 
    arrange(-len) %>% 
    head(1) %>% 
    pull(xd)
  l <- (stri_length(res) - 1)
  paste("Your longest 'XD' is:", res, "(it's", as.character(l), "'D'!)")
}

# percentage of messages

mkTotalMessagesPercentage <- function(df, username) {
  df %>% 
    group_by(sender) %>% 
    summarise(n = n()) -> res
  
  res %>% filter(sender == username) %>% pull(n) /
    res %>% pull(n) %>% sum()
}

# percentage of xD

mkTotalXDPercentage <- function(df) {
  df %>% 
    pull(content) %>% 
    xd.preprocessing(type = "all") -> res1
  res1 <- length(res1[!is.na(res1)])
  
  res1 / length(df[,1])
}
