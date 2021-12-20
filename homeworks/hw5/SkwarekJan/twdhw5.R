library(dplyr)
library(plotly)

df <- read.csv(file = "~/work-temp/3 sem/techniki wizualizacji danych/homework/hw5/complete.csv")

df1 <- df %>%
  select(c("category", "birth_continent")) %>%
  filter(category != "") %>%
  filter(birth_continent != "") %>%
  group_by(birth_continent) %>%
  count()

names(df1)[2] <- "number_of_winners_by_birth_continent"

df2 <- df %>%
  select(c("category", "death_continent")) %>%
  filter(category != "") %>%
  filter(death_continent != "") %>%
  group_by(death_continent) %>%
  count()

names(df2)[2] <- "number_of_winners_by_death_continent"

df1 <- cbind(df1, number_of_winners_by_death_continent = df2$number_of_winners_by_death_continent)

wykresik <- plot_ly(
  data = df1,
  x = ~number_of_winners_by_birth_continent,
  y = ~number_of_winners_by_death_continent,
  color = ~birth_continent,
  type = "scatter",
  mode = "markers",
  frame = c(1, 2, 3, 4, 5, 6),
  showlegend = T,
  size = 20
) %>% layout(
  title = "Stosunek narodzin do śmierci na poszczególnych kontynentach wszystkich noblistów",
  xaxis = list(
    title = "Liczba narodzin noblistów"
  ),
  yaxis = list(
    title = "Liczba zgonów noblistów"
  ),
  updatemenus = list(
    list(
      type = "buttons",
      x = 1.2,
      y = 0.7,
      buttons = list(
        list(method = "restyle",
             args = list("marker.symbol", "circle"),
             label = "Kółka"),
        list(method = "restyle",
             args = list("marker.symbol", "square"),
             label = "Kwadraciki"),
        list(method = "restyle",
             args = list("marker.symbol", "x"),
             label = "Krzyżyki")
      ))
  )
)

## PO PRZEJŚCIU DO KOLEJNEGO ETAPU ANIMACJI NALEŻY WYBRAĆ "ZNAK"
## TYLKO WTEDY LEGENDA ZOSTANIE POPRAWNIE ZAKTUALIZOWANA
wykresik
