library(dplyr)
library(plotly)
library(tidyr)


df2 <- read.csv("complete.csv")

temp <- 
  df2 %>% 
  select(awardYear, gender) %>% 
  filter(gender != "") %>% 
  group_by(awardYear, gender) %>% 
  count() %>% 
  mutate(awardDecade = awardYear-awardYear%%10) %>% 
  group_by(awardDecade, gender) %>% 
  count()

males <- 
  temp %>% 
  filter(gender=="male") %>% 
  rename(NumberOfMales = n)

females <- 
  temp %>% 
  filter(gender=="female")%>% 
  rename(NumberOfFemales = n)

both <- 
  males %>% 
  left_join(females, by =c("awardDecade")) %>% 
  mutate(
    percentageOfFemales = ifelse(
      is.na(NumberOfFemales) == FALSE, NumberOfFemales/(NumberOfMales+NumberOfFemales)*100, 0
    )
  ) %>% 
  mutate(
    percentageOfMales = ifelse(
      is.na(NumberOfFemales) == FALSE, NumberOfMales/(NumberOfMales+NumberOfFemales)*100, 100
    )
  ) %>% 
  replace_na(list(gender.y = "female", NumberOfFemales = 0)) %>% 
  rename(percentage_female = percentageOfFemales) %>% 
  rename(percentage_male = percentageOfMales) %>% 
  select(-NumberOfMales,-NumberOfFemales, -gender.x, -gender.y)
  

bothLong <- 
  both  %>% 
  pivot_longer(!awardDecade, names_to = c("useless", "gender"), names_sep = "_",  values_to = "percent") %>% 
  select(-useless)


plot_ly(data = bothLong, x = ~awardDecade, y = ~percent, type = "bar", color = ~gender) %>% 
  layout(
    title = "Contriubution of Men and Women in Nobel Prizes each decade",
    xaxis = list(dtick = 10, title = "Start Year of Decade"),
    yaxis = list(dtick = 10, title = "Contribution [%]"),
    
    barmode = "stack",
    updatemenus = list(
      list(
        x = -0.15, y = 1,
        buttons = list(
          list(method = "restyle",
               args = list("type", "bar"),
               label = "BarPlot"),
          list(method = "restyle",
               args = list("type", "line"),
               label = "LinePlot")
        )
      )
    )
  )
