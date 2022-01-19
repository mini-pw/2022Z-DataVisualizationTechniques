library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(dplyr)
library(stringi)
library(ggplot2)

string.preprocessing <- function(string_chain) {
  s <- tolower(string_chain)
  s <- stri_split_regex(s, "[,.:;~`\"()\\s]+", simplify = T, omit_empty = T)
  dim(s) <- NULL
  names(s) <- NULL
  s
}

# wordcloud
wordcloud2a <- function (data, size = 1, minSize = 0, gridSize = 0, fontFamily = "Segoe UI", 
                         fontWeight = "bold", color = "random-dark", backgroundColor = "white", 
                         minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE, 
                         rotateRatio = 0.4, shape = "circle", ellipticity = 0.65, 
                         widgetsize = NULL, figPath = NULL, hoverFunction = NULL) 
{
  if ("table" %in% class(data)) {
    dataOut = data.frame(name = names(data), freq = as.vector(data))
  }
  else {
    data = as.data.frame(data)
    dataOut = data[, 1:2]
    names(dataOut) = c("name", "freq")
  }
  if (!is.null(figPath)) {
    if (!file.exists(figPath)) {
      stop("cannot find fig in the figPath")
    }
    spPath = strsplit(figPath, "\\.")[[1]]
    len = length(spPath)
    figClass = spPath[len]
    if (!figClass %in% c("jpeg", "jpg", "png", "bmp", "gif")) {
      stop("file should be a jpeg, jpg, png, bmp or gif file!")
    }
    base64 = base64enc::base64encode(figPath)
    base64 = paste0("data:image/", figClass, ";base64,", 
                    base64)
  }
  else {
    base64 = NULL
  }
  weightFactor = size * 180/max(dataOut$freq)
  settings <- list(word = dataOut$name, freq = dataOut$freq, 
                   fontFamily = fontFamily, fontWeight = fontWeight, color = color, 
                   minSize = minSize, weightFactor = weightFactor, backgroundColor = backgroundColor, 
                   gridSize = gridSize, minRotation = minRotation, maxRotation = maxRotation, 
                   shuffle = shuffle, rotateRatio = rotateRatio, shape = shape, 
                   ellipticity = ellipticity, figBase64 = base64, hover = htmlwidgets::JS(hoverFunction))
  chart = htmlwidgets::createWidget("wordcloud2", settings, 
                                    width = widgetsize[1], height = widgetsize[2], sizingPolicy = htmlwidgets::sizingPolicy(viewer.padding = 0, 
                                                                                                                            browser.padding = 0, browser.fill = TRUE))
  chart
}

mkWordcloud <- function(df, minlen = 5, maxlen = 10,
                        minyear = 2010, maxyear = 2021,
                        n = 500, color = "#1e90ff") {
  
  words <- df %>% 
    mutate(year = as.integer(format(as.POSIXct(time), format="%Y"))) %>% 
    filter(year >= minyear, year <= maxyear) %>% 
    pull(content) %>% 
    string.preprocessing() %>% 
    data.frame() %>% 
    rename(word = ".") %>% 
    group_by(word) %>% 
    summarise(freq = n()) %>% 
    filter(stri_count_regex(word, "<.*>") == 0) %>%
    arrange(-freq) %>%
    filter(stri_length(word) >= minlen, stri_length(word) <= maxlen) %>% 
    head(n)
  
  wordcloud2a(data=words, size = 1.6, color = color)
  
}

# custom word count

customWords <- c("herbata", "kawa", "piwo")

mkCustomWordPlot <- function(df, words = customWords,
                             minyear = 2010, maxyear = 2021,
                             color = "#8A2BE2") {
  
  palette1 <- c("#1E90FF", "#8A2BE2", "#EE82EE", "#d696bb")
  
  df %>% 
    mutate(year = as.integer(format(as.POSIXct(time), format="%Y"))) %>% 
    filter(year >= minyear, year <= maxyear) %>% 
    pull(content) %>% 
    string.preprocessing() %>% 
    data.frame() %>% 
    rename(word = ".") %>% 
    filter(word %in% words) %>%
    group_by(word) %>% 
    summarise(freq = n()) %>% 
    arrange(-freq) %>% 
    mutate(word = factor(word, levels = word)) -> res
  
  ggplot(res, aes(x = word, y = freq, fill = word)) +
    geom_col(fill = color) +
    coord_flip() + 
    theme(axis.text = element_text(size = 16)) +
    xlab("") +
    ylab("occurrences") +
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "#ffffff")
    )
}

# percentage of words

mkAvgWordsPercentage <- function(df) {
  df %>% 
    mutate(wordsNum = lengths(gregexpr("\\W+", content)) + 1) %>% 
    summarise(avgLength = mean(wordsNum)) %>% pull(avgLength)
}

mkMostPopularWord <- function(df) {
  df %>% 
    pull(content) %>% 
    string.preprocessing() %>% 
    data.frame() %>% 
    rename(word = ".") %>% 
    group_by(word) %>% 
    summarise(freq = n()) %>% 
    filter(word != "") %>% 
    filter(stri_length(word) >= 5) %>% 
    arrange(-freq) -> res
  
  res %>% pull(word) %>% head(1)
  
}