# author: Mikołaj Piórczyński
# data: https://www.kaggle.com/imdevskp/nobel-prize

# libraries
library(dplyr)
library(plotly)
library(stringr)

# loading data
nobel_df <- read.csv('data/nobel_prize.csv', encoding = 'UTF-8')

# data preprocessing
df <- nobel_df %>% 
  select(awardYear, category, motivation, name, gender, birth_continent, birth_countryNow, birth_date) %>% 
  filter(birth_date != "", gender != "") %>% 
  mutate(birth_year = str_extract(birth_date, "^[0-9]{4}")) %>% 
  mutate(age = as.integer(awardYear) - as.integer(birth_year)) %>% 
  arrange(awardYear)

df_overall <- df %>% group_by(awardYear) %>% summarise(age = mean(age)) %>% ungroup() %>% mutate(mean_age = mean(age))
df_physics <- df %>% filter(category == "Physics") %>% mutate(mean_age = mean(age))
df_chemistry <- df %>% filter(category == "Chemistry") %>% mutate(mean_age = mean(age))
df_medicine <- df %>% filter(category == "Physiology or Medicine") %>% mutate(mean_age = mean(age))
df_literature <- df %>% filter(category == "Literature") %>% mutate(mean_age = mean(age))
df_peace <- df %>% filter(category == "Peace") %>% mutate(mean_age = mean(age))
df_economic <- df %>% filter(category == "Economic Sciences") %>% mutate(mean_age = mean(age))

# visualisation
updatemenus <- list(
  list(x = 1, y = 1.1,
       buttons = list(
         list(args = list("visible",  c(F, T, T, F, F, F, F, F, F, F, F, F, F, F, F)),
              label = "Overall",
              method = "restyle"), 
         list(args = list("visible", c(F, F, F, T, T, F, F, F, F, F, F, F, F, F, F)),
              label = "Physics",
              method = "restyle"),
         list(args = list("visible", c(F, F, F, F, F, T, T, F, F, F, F, F, F, F, F)),
              label = "Chemistry",
              method = "restyle"), 
         list(args = list("visible", c(F, F, F, F, F, F, F, T, T, F, F, F, F, F, F)),
              label = "Physiology or Medicine",
              method = "restyle"), 
         list(args = list("visible", c(F, F, F, F, F, F, F, F, F, T, T, F, F, F, F)),
              label = "Literature",
              method = "restyle"), 
         list(args = list("visible", c(F, F, F, F, F, F, F, F, F, F, F, T, T, F, F)),
              label = "Peace",
              method = "restyle"), 
         list(args = list("visible", c(F, F, F, F, F, F, F, F, F, F, F, F, F, T, T)),
              label = "Economic Sciences",
              method = "restyle")
       ))
)


plot_ly(type = 'scatter', mode = 'markers+lines') %>% 
  add_trace(data = df_overall,
            x = ~awardYear,
            y = ~age, 
            text = paste0("Mean age: ", df_overall$age, "<br>Year: ", df_overall$awardYear),
            hoverinfo = 'text',
            name = "Overall", 
            line = list(color = 'rgb(166,206,227)'), 
            marker = list(color = 'rgb(166,206,227)')
  ) %>%
  add_trace(data = df_overall,
            x = ~awardYear,
            y = ~mean_age, 
            line = list(dash = 'dash', color = 'rgb(166,206,227)'),
            text = paste0("Mean age: ", round(df_overall$mean_age, 3)),
            hoverinfo = 'text',
            name = "Overall", 
            mode = 'lines'
  ) %>%
  add_trace(data = df_physics,
            x = ~awardYear,
            y = ~age,
            text = paste0("Name: ", df_physics$name, "<br>Age: ", df_physics$age, "<br>Birth country: ", df_physics$birth_countryNow, "<br>Year: ",df_physics$awardYear),
            hoverinfo = 'text', 
            visible = F, 
            name = "Physics",
            line = list(color = 'rgb(31,120,180)'), 
            marker = list(color = 'rgb(31,120,180)')
  ) %>%
  add_trace(data = df_physics,
            x = ~awardYear,
            y = ~mean_age, 
            line = list(dash = 'dash', color = 'rgb(31,120,180)'),
            text = paste0("Mean age: ", round(df_physics$mean_age, 3)),
            hoverinfo = 'text',
            visible = F, 
            name = "Physics", 
            mode = 'lines'
  ) %>%
  add_trace(data = df_chemistry,
            x = ~awardYear,
            y = ~age,
            text = paste0("Name: ", df_chemistry$name, "<br>Age: ", df_chemistry$age, "<br>Birth country: ", df_chemistry$birth_countryNow, "<br>Year: ",df_chemistry$awardYear),
            hoverinfo = 'text', 
            visible = F,
            name = "Chemistry",
            line = list(color = 'rgb(178,223,138)'), 
            marker = list(color = 'rgb(178,223,138)')
  ) %>%
  add_trace(data = df_chemistry,
            x = ~awardYear,
            y = ~mean_age, 
            line = list(dash = 'dash', color = 'rgb(178,223,138)'),
            text = paste0("Mean age: ", round(df_chemistry$mean_age, 3)),
            hoverinfo = 'text',
            visible = F, 
            name = "Chemistry", 
            mode = 'lines'
  ) %>%
  add_trace(data = df_medicine,
            x = ~awardYear,
            y = ~age,
            text = paste0("Name: ", df_medicine$name, "<br>Age: ", df_medicine$age, "<br>Birth country: ", df_medicine$birth_countryNow, "<br>Year: ",df_medicine$awardYear),
            hoverinfo = 'text', 
            visible = F, 
            name = "Physiology or Medicine",
            line = list(color = 'rgb(51,160,44)'), 
            marker = list(color = 'rgb(51,160,44)')
  ) %>% 
  add_trace(data = df_medicine,
            x = ~awardYear,
            y = ~mean_age, 
            line = list(dash = 'dash', color = 'rgb(51,160,44)'),
            text = paste0("Mean age: ", round(df_medicine$mean_age, 3)),
            hoverinfo = 'text',
            visible = F, 
            name = "Physiology or Medicine", 
            mode = 'lines'
  ) %>%
  add_trace(data = df_literature,
            x = ~awardYear,
            y = ~age,
            text = paste0("Name: ", df_literature$name, "<br>Age: ", df_literature$age, "<br>Birth country: ", df_literature$birth_countryNow, "<br>Year: ",df_literature$awardYear),
            hoverinfo = 'text', 
            visible = F, 
            name = "Literature",
            line = list(color = 'rgb(251,154,153)'), 
            marker = list(color = 'rgb(251,154,153)')
  ) %>%
  add_trace(data = df_literature,
            x = ~awardYear,
            y = ~mean_age, 
            line = list(dash = 'dash', color = 'rgb(251,154,153)'),
            text = paste0("Mean age: ", round(df_literature$mean_age, 3)),
            hoverinfo = 'text',
            visible = F, 
            name = "Literature", 
            mode = 'lines'
  ) %>%
  add_trace(data = df_peace,
            x = ~awardYear,
            y = ~age,
            text = paste0("Name: ", df_peace$name, "<br>Age: ", df_peace$age, "<br>Birth country: ", df_peace$birth_countryNow, "<br>Year: ",df_peace$awardYear),
            hoverinfo = 'text', 
            visible = F, 
            name = "Peace",
            line = list(color = 'rgb(227,26,28)'), 
            marker = list(color = 'rgb(227,26,28)')
  ) %>%
  add_trace(data = df_peace,
            x = ~awardYear,
            y = ~mean_age, 
            line = list(dash = 'dash', color = 'rgb(227,26,28)'),
            text = paste0("Mean age: ", round(df_peace$mean_age, 3)),
            hoverinfo = 'text',
            visible = F, 
            name = "Peace", 
            mode = 'lines'
  ) %>%
  add_trace(data = df_economic,
            x = ~awardYear,
            y = ~age,
            text = paste0("Name: ", df_economic$name, "<br>Age: ", df_economic$age, "<br>Birth country: ", df_economic$birth_countryNow, "<br>Year: ",df_economic$awardYear),
            hoverinfo = 'text', 
            visible = F, 
            name = "Economic Sciences",
            line = list(color = 'rgb(253,191,111)'), 
            marker = list(color = 'rgb(253,191,111)')
  ) %>%
  add_trace(data = df_economic,
            x = ~awardYear,
            y = ~mean_age, 
            line = list(dash = 'dash', color = 'rgb(253,191,111)'),
            text = paste0("Mean age: ", round(df_economic$mean_age, 3)),
            hoverinfo = 'text',
            visible = F, 
            name = "Economic", 
            mode = 'lines'
  ) %>%
  layout(
    title = "Nobel Prize laureates age",
    xaxis = list(title = list(text = "Year", font = list(size = 15), standoff = 10),
                 range = c(1900, 2020),
                 fixedrange = TRUE,
                 mirror=TRUE,
                 ticks='outside',
                 showline=TRUE
    ),
    yaxis = list(title = list(text = "Age", font = list(size = 15), standoff = 10),
                 range = c(15, 100),
                 fixedrange = TRUE,
                 autotypenumbers = 'strict',
                 mirror=TRUE,
                 ticks='outside',
                 showline=TRUE
    ),
    updatemenus = updatemenus, 
    showlegend = FALSE
  ) %>% 
  config(displayModeBar = FALSE)




