library(dplyr)
library(plotly)
library(tidyr)
library(forcats)

raw.data <- read.csv("complete.csv")

data <- raw.data %>% 
  select(awardYear, birth_country)

# Państwa, które sumarycznie uzyskały >= 20 nagród

data <- data %>% 
  mutate(birth_country = case_when(
    birth_country == "USA" ~ "USA",
    birth_country == "United Kingdom" ~ "UK",
    birth_country == "Germany" ~ "Germany",
    birth_country == "France" ~ "France",
    birth_country == "Sweden" ~ "Sweden",
    birth_country == "Japan" ~ "Japan",
    birth_country == "Canada" ~ "Canada",
    TRUE ~ "Other"
  ))

all_comb = data.frame(expand.grid(unique(data$awardYear), unique(data$birth_country)))

data <- data %>% 
  group_by(awardYear, birth_country) %>% summarise(prizeCount = n()) %>% 
  right_join(all_comb, by=c("awardYear"="Var1", "birth_country"="Var2")) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  group_by(birth_country) %>% arrange(awardYear) %>% mutate(total = cumsum(prizeCount))


fig <- plot_ly(data = data, x = ~birth_country, y = ~total, frame = ~awardYear, type = "bar") %>% 
  layout(xaxis = list(categoryorder = "total descending", title = 'Country'),
         title = "Total Prizes Won Per Country", 
         yaxis = list(title = 'Total Prizes Won')) %>% 
  config(displayModeBar = FALSE)

fig <- fig %>% 
  animation_opts(frame = 200, transition = 0) %>% 
  animation_button(x = 0.05, y = 0.08) %>%
  animation_slider(currentvalue = list(prefix = "YEAR: ", font = list(color="black")))

fig

htmlwidgets::saveWidget(fig, "hw5animation.html")
