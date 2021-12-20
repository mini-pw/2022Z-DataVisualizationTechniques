library(plotly)
library(dplyr)
df <- read.csv("C:/Users/micha/Downloads/complete.csv")

df1 <- df %>% 
  select(gender, prizeAmountAdjusted,category) %>% 
  filter(gender != "")

fig <- plot_ly(
  data = df1,
  x = ~gender,
  y = ~prizeAmountAdjusted,
  type = 'box'
) %>% 
  layout(
    title = "Rozkład wysokości nagród (uwzględniając inflację) z wybranej kategorii dla 
    kobiet i mężczyzn",
    xaxis = list(title = ""),
    yaxis = list(title = "Wysokość nagrody"),
    updatemenus = list(
      list(x = 1, y = 1,
           buttons = list(
        list(method = "restyle",
             args = list("type","box"),
             label = "Boxplot"),
        list(method = "restyle",
             args = list("type", "violin"),
             label = "violin")
      )),
      list(x = 0, y = 1,
           buttons = list(
             list(method = "restyle",
                  args = list("y", list(df1$prizeAmountAdjusted)),
                  label = "All"),
             list(method = "restyle",
                  args = list("y", list(df1 %>% filter(category == "Physics") %>%
                                          pull(prizeAmountAdjusted))),
                  label = "Fizyka"),
             list(method = "restyle",
                  args = list("y", list(df1 %>% filter(category == "Chemistry") %>%
                                          pull(prizeAmountAdjusted))),
                  label = "Chemia"),
             list(method = "restyle",
                  args = list("y", list(df1 %>% filter(category == "Peace") %>%
                                          pull(prizeAmountAdjusted))),
                  label = "Pokojowa"),
             list(method = "restyle",
                  args = list("y", list(df1 %>% filter(category == "Economic Sciences") %>%
                                          pull(prizeAmountAdjusted))),
                  label = "Nauki Ekonomiczne"),
             list(method = "restyle",
                  args = list("y", list(df1 %>% filter(category == "Literature") %>%
                                          pull(prizeAmountAdjusted))),
                  label = "Literatura"),
             list(method = "restyle",
                  args = list("y", list(df1 %>% filter(category == "Physiology or Medicine") %>%
                                          pull(prizeAmountAdjusted))),
                  label = "Fizjologia lub Medycyna")
           ))
    )
  )
fig
