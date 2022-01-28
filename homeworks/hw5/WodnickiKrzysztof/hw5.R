# PAKIETY
library(plotly)
library(dplyr)
library(tidyr)

# DANE
full_data <- read.csv2("~/Projekty/twd/prace_domowe/hw5/complete.csv", sep=",")
df <- full_data %>%
      select(category, categoryFullName, fullName, gender, birth_countryNow) %>% 
      filter(gender!="") %>%
      group_by(birth_countryNow, category) %>%
      summarize(Count = n()) %>%
      pivot_wider(names_from = category, values_from = Count)

names(df) <- stringr::str_replace_all(names(df), c(" " = "."))

df <- df %>% mutate(Sum = sum(Literature,
                              Physics,
                              Peace,
                              Physiology.or.Medicine,
                              Chemistry,
                              Economic.Sciences, na.rm=TRUE)) %>% 
             filter(Sum > 5) # to po to żeby cokolwiek dało się pzreczytać na wykresie

country <- df$birth_countryNow
Sum <- df$Sum
Literature <- df$Literature
Physics <- df$Physics
Peace <- df$Peace
Medicine <- df$Physiology.or.Medicine
Chemistry <- df$Chemistry
Economy <- df$Economic.Sciences

# PODSTAWOWY WYKRES
fig <- plot_ly(
  x=country,
  y=Sum,
  type="bar"
)

# PRZYCISKI
updatemenus <- list(
  list(
    active = -1,
    type = "buttons",
    buttons = list(
      list(
        label = "Sum",
        method = "restyle",
        args = list("y", list(Sum))
      ),
      list(
        label = "Literature",
        method = "restyle",
        args = list("y", list(Literature))
      ),
      list(
        label = "Physics",
        method = "restyle",
        args = list("y", list(Physics))
      ),
      list(
        label = "Peace",
        method = "restyle",
        args = list("y", list(Peace))
      ),
      list(
        label = "Physiology or Medicine",
        method = "restyle",
        args = list("y", list(Medicine))
      ),
      list(
        label = "Chemistry",
        method = "restyle",
        args = list("y", list(Chemistry))
      ),
      list(
        label = "Economic Sciences",
        method = "restyle",
        args = list("y", list(Economy))
      )
    )
  )
)

# DODANIE PZRYCISKOW
fig <- fig %>% layout(
  title = "Number of awards in choosen category by birth country of award winner.",
  yaxis = list(title = "Count", tick=1),
  xaxis = list(title = "Birth country of award winner"),
  showlegend = FALSE,
  updatemenus = updatemenus
) %>% 
config(displayModeBar=FALSE)

fig
