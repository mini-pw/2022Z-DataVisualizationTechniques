library(dplyr)
library(plotly)

complete <- read.csv("complete.csv")


# wykres 1

timesYearCategory <- complete %>% 
  select(awardYear, category) %>% 
  group_by(awardYear, category) %>% 
  summarise(rewardsPerYear = n()) %>% 
  group_by(category, rewardsPerYear) %>% 
  summarise(yearCount = n()) %>% 
  mutate(rewardsPerYear = factor(rewardsPerYear))


fig <- plot_ly(data = timesYearCategory, 
               x = timesYearCategory %>% filter(category == "Chemistry") %>% 
                 pull(rewardsPerYear), 
               y = timesYearCategory %>% filter(category == "Chemistry") %>% 
                 pull(yearCount), 
               type = 'bar')

fig <- fig %>% layout(
  title = list(text = "Number of awards per year in each category", x = 0.75),
  yaxis = list(title ="Number of years"),
  xaxis = list(title ="Number of awards"),
  updatemenus = list(
    list(
      type = "buttons",
      x = -0.15,
      y = 0.75,
      buttons = list(
        list(method = "restyle", 
             args = list("y", list(timesYearCategory %>% 
                                     filter(category == "Chemistry") %>% 
                                     pull(yearCount))),
             label = "Chemistry"),
        list(method = "restyle", 
             args = list("y", list(timesYearCategory %>% 
                                     filter(category == "Economic Sciences") %>% 
                                     pull(yearCount))),
             label = "Economic Sciences"),
        list(method = "restyle", 
             args = list("y", list(timesYearCategory %>% 
                                     filter(category == "Literature") %>% 
                                     pull(yearCount))),
             label = "Literature"),
        list(method = "restyle", 
             args = list("y", list(timesYearCategory %>% 
                                     filter(category == "Peace") %>% 
                                     pull(yearCount))),
             label = "Peace"),
        list(method = "restyle", 
             args = list("y", list(timesYearCategory %>% 
                                     filter(category == "Physics") %>% 
                                     pull(yearCount))),
             label = "Physics"),
        list(method = "restyle",
             args = list("y", list(timesYearCategory %>% 
                                     filter(category == "Physiology or Medicine") %>% 
                                     pull(yearCount))),
             label = "Physiology or Medicine")
      ))
  )
) %>% 
  config(displayModeBar = FALSE)

fig

# animacja

fig2 <- timesYearCategory %>% 
  mutate(rewardsPerYear = factor(rewardsPerYear)) %>% 
  plot_ly(
    x = ~rewardsPerYear,
    y = ~yearCount,
    color = ~category,
    frame = ~category
  ) %>% 
  add_bars() %>% 
  layout(title = "Number of awards per year in each category",
         yaxis = list(title ="Number of years"),
         xaxis = list(title ="Number of awards")) %>% 
  config(displayModeBar = FALSE)

fig2 <- fig2 %>% 
  animation_opts(frame = 1000, transition = 500) %>% 
  animation_button(x = 0, y = 0) %>% 
  animation_slider(y = -0.05, currentvalue = list(
    prefix = "Category: ", font = list(color="black"), xanchor = "center"))

fig2
