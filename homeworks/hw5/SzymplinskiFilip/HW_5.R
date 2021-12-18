library(plotly)
library(dplyr)

setwd("~/Pulpit/Filip/Studia/Semestr III/Techniki wizualizacji danych/HW/HW_5")
df <- read.csv("complete.csv")

df <- df %>%
  filter(prizeStatus != "recived") %>% 
  select(awardYear, portion, category, prizeAmountAdjusted) %>% 
  mutate(prizeMoney = case_when(
    portion == "1/4" ~ as.integer(prizeAmountAdjusted/4),
    portion == "1/3" ~ as.integer(prizeAmountAdjusted/3),
    portion == "1/2" ~ as.integer(prizeAmountAdjusted/2),
    portion == "1" ~ prizeAmountAdjusted
  )) %>% 
  mutate(ten_year_interval = awardYear - (awardYear %% 10))
  

fig <- plot_ly(data = df,
               x = ~category,
               y = ~prizeMoney,
               frame = ~ten_year_interval,
               type = "violin") %>% 
  layout(title = "Money prize of the Nobel Prize for different categories",
         xaxis = list(title = "Category of the Nobel Prize"),
         yaxis = list(range = c(0, 15000000), title = "Money prize")
         ) %>% 
  animation_opts(frame = 1300,transition = 400, 
                 easing = "elastic") %>%
  animation_button(x = 0.05, y = 0.05) %>%
  animation_slider(currentvalue = list(prefix = "Decade: ", font = list(color="black")))

fig