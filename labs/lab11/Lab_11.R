###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 11           ###
###########################################

## 0) dane 
library(dplyr)
library(ggplot2)
library(SmarterPoland)
data(countries)

## 1) gganimate
# https://gganimate.com/articles/gganimate.html
library(gganimate)
library(Rcpp)
library(gifski)
library(av)

p <- ggplot(countries, aes(x = birth.rate, y = death.rate)) +
  geom_point()

p

anim <- p + 
  transition_states(continent,
                    transition_length = 2,
                    state_length = 1)
anim

anim2 <- p +
  labs(title = "{closest_state}") + 
  transition_states(continent,
                    transition_length = 2,
                    state_length = 1)
anim2

p2 <- ggplot(countries, aes(x = birth.rate, y = death.rate)) +
  geom_point(aes(color = continent))

anim3 <- p2 + 
  labs(title = "{closest_state}") + 
  transition_states(continent,
                    transition_length = 2,
                    state_length = 1)
anim3  

## 2) rbokeh
# https://hafen.github.io/rbokeh/articles/rbokeh.html
library(rbokeh)

figure() %>% 
  ly_points(birth.rate, death.rate, data = countries, 
            color = continent)


## 3) ggiraph
# https://davidgohel.github.io/ggiraph/articles/offcran/using_ggiraph.html
library(ggiraph)

my_gg <- ggplot(countries, aes(x = birth.rate, y = death.rate, color = continent)) +
  geom_point_interactive(aes(tooltip = country))

girafe(code = print(my_gg))

## 4) vegalite
# https://github.com/hrbrmstr/vegalite
devtools::install_github("hrbrmstr/vegalite")
library(vegalite)

dat <- jsonlite::fromJSON('[
    {"a": "A","b": 28}, {"a": "B","b": 55}, {"a": "C","b": 43},
    {"a": "D","b": 91}, {"a": "E","b": 81}, {"a": "F","b": 53},
    {"a": "G","b": 19}, {"a": "H","b": 87}, {"a": "I","b": 52}
  ]')

vegalite() %>%
  add_data(dat) %>%
  encode_x("a", "ordinal") %>%
  encode_y("b", "quantitative") %>%
  mark_bar() -> vl

vl

## 5) googleVis
# https://cran.r-project.org/web/packages/googleVis/vignettes/googleVis_examples.html
library(googleVis)

df = data.frame(
  country = c("US", "GB", "BR"),
  val1 = c(10, 13, 14),
  val2 = c(23, 12, 32)
)

Bar <- gvisBarChart(df)
plot(Bar)
