library(plotly)
library(dplyr)

data <- read.csv("complete.csv")
continents <- c("Africa", "Asia", "Europe", 
                "North America", "Oceania", "South America")


df <- data %>% 
  filter(prizeStatus == "received" &
           !(birth_continent == "" &
              org_founded_continent == "")) %>% 
  select(knownName, awardYear, portion, category,
         prizeAmountAdjusted, org_founded_continent, birth_continent) %>% 
  mutate(prize = case_when(
    portion == "1" ~ prizeAmountAdjusted,
    portion == "1/2" ~ as.integer(prizeAmountAdjusted/2),
    portion == "1/3" ~ as.integer(prizeAmountAdjusted/3),
    portion == "1/4" ~ as.integer(prizeAmountAdjusted/4)
  )) %>% 
  mutate(decade = awardYear - awardYear %% 10) %>% 
  mutate(continent = case_when(
    birth_continent != "" ~ birth_continent,
    birth_continent == "" ~ org_founded_continent
  )) %>% 
  group_by(continent, decade, category) %>% 
  summarise(sumprize = sum(prize))

plot_ly(
  data = df,
  x = ~continent,
  y = ~sumprize,
  color = ~category,
  colors =c("#7e817e", "#a64fff", "#4342ff", "#ff94fa", "#37c1ea", "#324071"),
  type = "bar",
  frame = ~decade
) %>% 
  layout(
    title = "Nobel laureats of which continent have won the biggest prize money summary
    in each category over the decades?",
    xaxis = list(fixedrange = TRUE, title = "Continent",
                 ticks = continents),
    yaxis = list(fixedrange = TRUE, title = "Prize money", range = c(0, 100e6)),
    legend = list(
      x = 1, y = 1, 
      title = list(text = "Category:"), 
      bgcolor = "#E2E2E2")) %>% 
  animation_opts(2000, transition = 400, 
                 easing = "elastic", redraw = TRUE) %>% 
  animation_slider(currentvalue = list(prefix = "Decade: "),
                   font = list(color="black"))

