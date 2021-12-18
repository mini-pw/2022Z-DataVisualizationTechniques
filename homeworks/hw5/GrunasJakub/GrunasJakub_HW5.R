library(plotly)
library(dplyr)

df <- read.csv("complete.csv")

df <- df %>%
  mutate(age = awardYear - as.numeric(substr(birth_date, 1, 4))) %>%
  filter(!is.na(age))

p1 <- plot_ly(
              df,
              x = ~age,
              frame = ~category,
              type = "histogram",
              color = ~gender,
              colors = c("red", "blue")
              ) %>%
  layout(title = "Age of Noble prizes winners in different categories",
         xaxis = list(title = "age",
                      fixedrange = TRUE,
                      range = list(0, 100)
                      ), 
         yaxis = list(title = "count",
                      fixedrange = TRUE,
                      range = list(0, 40)
                      ),
         barmode = "stack",
         bargap = 0.1
         ) %>%
  animation_button(visible = FALSE) %>% 
  animation_slider(
                   currentvalue = list(prefix = "Category:", 
                                       font = list(color="black")
                                       ),
                   pad = list(t = 50),
                   labels = "x"
                   ) %>%
  config(displayModeBar = FALSE)

p1
