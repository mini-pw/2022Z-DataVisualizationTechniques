library(tidytext)
library(dplyr)
library(ggplot2)
        
assertthat::assert_that(file.exists("data/data.csv"));
data <- read.csv("data/data.csv", encoding = "UTF-8");
df <- tibble(data)

tidy_speeches <- df %>% 
  unnest_tokens(word, text) %>% 
  mutate(wordnumber = row_number()) %>%
  anti_join(stop_words)

# positive / negative words

bing_word_counts <- tidy_speeches %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +
  labs(
    title = "Analiza sentymentu",
    subtitle = "Najbardziej pozytywne i negatywne słowa użyte w przemówieniach",
    x = "Wkład w sentyment",
    y = NULL)

