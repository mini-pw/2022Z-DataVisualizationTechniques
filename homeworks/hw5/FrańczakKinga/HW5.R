library("plotly")
library("dplyr")
library("stringi")

nobel <- read.csv("complete.csv")

tmp <- nobel %>% 
  select(category, birth_continent, awardYear, birth_date) 
tmp$birth_date <- stri_extract_first_regex(tmp$birth_date, '[0-9]+')
tmp$birth_date <- as.integer(tmp$birth_date)

tmp <- tmp %>% 
  mutate(age = awardYear - birth_date)

wykres <- plot_ly(x = tmp$category ,y = tmp$age, type = "box", color = tmp$birth_continent)


chart_type <- list(
  type = "buttons",
  direction = "right",
  xanchor = 'center',
  yanchor = "bottom",
  x = 0.5,
  y = 1.27,
  buttons = list(
    list(method = "restyle",
         args = list("type", "box"),
         label = "Boxplot"),
    list(method = "restyle",
         args = list("type", "violin"),
         label = "Violin plot")
  )
)

countries <- list(
  x = 0.75,
  y = 1.25,
  buttons = list(
    list(method = "update",
         args = list(
           list(visible = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)),
           list(title = "Plot of ages of Nobel price winners from all contientents")
         ),
         label = "All"),
    list(method = "update",
         args = list(
           list(visible = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)),
           list(title = "Plot of ages of Nobel price winners from North America")
         ),
         label = "North America"),
    list(method = "update",
         args = list(
           list(visible = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)),
           list(title = "Plot of ages of Nobel price winners from Europe")
         ),
         label = "Europe"),
    list(method = "update",
         args = list(
           list(visible = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)),
           list(title = "Plot of ages of Nobel price winners from Asia")
         ),
         label = "Asia"),
    list(method = "update",
         args = list(
           list(visible = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)),
           list(title = "Plot of ages of Nobel price winners from Africa")
         ),
         label = "Africa"),
    list(method = "update",
         args = list(
           list(visible = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)),
           list(title = "Plot of ages of Nobel price winners from South America")
         ),
         label = "South America"),
    list(method = "update",
         args = list(
           list(visible = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)),
           list(title = "Plot of ages of Nobel price winners from Oceania")
         ),
         label = "Oceania")
  )
)

wykres %>% 
  layout(
    xaxis = list(title = "Category"),
    yaxis = list(title = "Winner's age"),
    boxmode = "group",
    updatemenus = list(chart_type, countries),
    title = "Plot of ages of Nobel price winners from all contientents"
  )
