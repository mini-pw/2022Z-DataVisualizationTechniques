library(tidytext)
library(dplyr)
library(ggplot2)
library(tidyr)
library(wordcloud2)

assertthat::assert_that(file.exists("../data/data.csv"));
data <- read.csv("../data/data.csv", encoding = "UTF-8");
df <- tibble(data)

# Tworzy ramkę danych dla określonej liczby słów (n)
n_grams <- function(df, n) {
  n_grams <- df %>% 
    unnest_tokens(n_gram, text, token="ngrams", n=n)
  n_grams_separated <- n_grams %>%
    separate(n_gram, sapply(1:n, function(x) {paste("word", x, sep="")}), sep=" ")
  result <- n_grams_separated %>% 
    count(n_grams_separated[, (2+1):(2+n)], sort=TRUE) %>%
    filter_all(all_vars(!. %in% stop_words$word))
  tibble(data.frame(word = do.call(paste, c(result[ , 1:n], list(sep = ' '))), n = result$n))
}

# wordcloud (połączony 2-gram i 3-gram)
wordcloud2(
  rbind(
    n_grams(df, 2),
    n_grams(df, 3)
  ) %>%
    arrange(-n) %>% 
    slice_max(n, n= 50),
  size = 0.7,
  color = 'black',
  fontFamily = "High Tower Text",
  # figPath = "speeches/ngrams/t.png"
)

# word frequency
bigrams <- n_grams(df, 2)
bigrams %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x = n, y = word)) + geom_col() + ggtitle("Najczęściej używane kombinacje dwóch słów") + xlab("Liczba wystąpień") + ylab("Słowo")

# Kombinacje słów w poszczególnych wystąpieniach
top_bigrams <- bigrams[1:8, ]$word

bigrams_grouped <- do.call(
  rbind, lapply(c(1:length(df$date)), function(i) { 
    result <- n_grams(df[i, ], 2) %>% mutate(date = df[i, 2]) 
    empty_bigrams <- tibble(data.frame(word = bigrams$word[ !(bigrams$word %in% result$word ) ], n = 0, "date$date" = result$date$date[1], check.names = FALSE))
    colnames(empty_bigrams) <- colnames(result)
    rbind(result, empty_bigrams)
    }
  )
) %>% filter(word %in% top_bigrams) %>% arrange(date$date)

bigrams_grouped <- bigrams_grouped %>% group_by(word, date) %>% summarize(n = sum(n))

fig <- bigrams_grouped %>% ggplot(aes(date$date, word, fill = n)) + 
  geom_bin2d() +
  xlab("speech date") +
  ylab("word combination") +
  ggtitle("Number of word combinations in particular speeches") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust=1.15),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  scale_fill_gradientn(colors =c('#000000', '#3D3104', '#796208','#F1C40F', '#F1C40F')) + geom_tile(colour = "white", size = 1)
fig

