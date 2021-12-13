library(dplyr)
library(tidyverse)
library(plotly)

if ( options()$stringsAsFactors )
  options(stringsAsFactors=FALSE)

df_nobel <- read.csv("complete.csv")

df <- df_nobel %>% 
  filter(!(birth_date == "")) %>% 
  mutate(birth_year = as.numeric(str_split(birth_date, pattern = "-", simplify = TRUE)[, 1])) %>% 
  mutate(how_old = awardYear - birth_year) %>% 
  select(awardYear, category, knownName, how_old, affiliation_1) %>% 
  pivot_wider(names_from = category, values_from = how_old)

colnames(df)[c(4, 8)] <-
  c("Economic_Science", "Physiology_or_Medicine")

fig <- plot_ly(
  data = df,
  x = ~awardYear,
  text = paste0("", df$knownName, "<br>", df$affiliation_1)
)
fig <- fig %>% add_markers(y = ~Economic_Science, name = "Economic_Science")
fig <- fig %>% add_markers(y = ~Physics, name = "Physics", visible = F)
fig <- fig %>% add_markers(y = ~Chemistry, name = "Chemistry", visible = F)
fig <- fig %>% add_markers(y = ~Peace, name = "Peace", visible = F)
fig <- fig %>% add_markers(y = ~Physiology_or_Medicine, name = "Physiology_or_Medicine", visible = F)
fig <- fig %>% add_markers(y = ~Literature, name = "Literature", visible = F)

fig <- fig %>% layout(
  title = list(text = "Age distribution of Noble Prize laureates at the time of awarding across categories", y = 0.8),
  xaxis = list(domain = c(0.1, 1), title = "Year"),
  yaxis = list(title = "Age"),
  showlegend = FALSE,
  updatemenus = list(
    list(
      y = 0.7,
      buttons = list(
        list(method = "restyle",
             args = list("visible", list(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)),
             label = "Economic_Science"),
        
        list(method = "restyle",
             args = list("visible", list(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)),
             label = "Physics"),
        
        list(method = "restyle",
             args = list("visible", list(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)),
             label = "Chemistry"),
        
        list(method = "restyle",
             args = list("visible", list(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)),
             label = "Peace"),
        
        list(method = "restyle",
             args = list("visible", list(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)),
             label = "Physiology_or_Medicine"),
        
        list(method = "restyle",
             args = list("visible", list(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)),
             label = "Literature")
      ))
  ))

fig
