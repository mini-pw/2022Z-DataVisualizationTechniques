library(dplyr)
library(stringi)
library(plotly)

archive <-  read.csv("HW5/archive/complete.csv") 
archive$birth_date <- substr(archive$birth_date, 0, 4)

df<- transform(archive, birth_date = as.numeric(birth_date))%>% 
  rowwise() %>% 
  mutate(Age = sum(awardYear, -birth_date), 
         Decade = awardYear - (awardYear %% 10)) %>% 
  select(c("Decade", Category = "category", "Age", Gender = "gender")) %>% 
  group_by(Category, Decade, Gender) %>% 
  summarise(Sum = n())

df <- df[!df$Gender=="",]


plot_ly(
  data = df,
  x = ~Category,
  y = ~Sum,
  color = ~Gender,
  colors = "Pastel1",
  frame = ~Decade,
  type = "bar", 
  text = paste0("Gender: ", df$Gender),
  hoverinfo = 'x+y+text')%>% 
  animation_opts(frame = 1500, transition = 300) %>% 
  animation_button(x = 0, y = 0) %>%
  animation_slider(currentvalue = list(prefix = "Decade: "),)%>% 
  layout(
    title = "The number of Nobel Awards from each category in decades.",
    yaxis = list(title = '')
)
