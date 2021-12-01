# data: https://ec.europa.eu/eurostat/web/transport/data/database
#       https://www.jato.com/new-car-co2-emissions-hit-the-highest-average-in-europe-since-2014/

library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(ggrepel)
library(ggimage)

dfTotal <- read.csv("../plotsData/number_of_total.csv")
dfEvs <- read.csv("../plotsData/number_of_evs.csv")
dfCO2 <- read.csv("../plotsData/countries_co2.csv")
dfPop <- read.csv("../plotsData/population_of_countries_formated.csv")

dfTotal <- dfTotal %>% 
  filter(TIME == 2019 & Value != ":") %>% 
  select(TIME, GEO, Value)

dfEvs <- dfEvs %>% 
  filter(TIME == 2019 & Value != ":") %>% 
  select(TIME, GEO, Value)

dfPop <- dfPop %>% 
  select(country = Country, population = X2019)

df <- merge(dfTotal, dfEvs, by = "GEO") %>% 
  select(year = TIME.x, country = GEO, total = Value.x, EVs = Value.y)

df[["country"]][4] = "Czech Republic"

df <- merge(df, dfPop, by = "country")
df <- merge(df, dfCO2, by.x = "country", by.y = "region")

df$total <- as.numeric(str_replace_all(df$total, ",", ""))
df$EVs <- as.numeric(str_replace_all(df$EVs, ",", ""))

df <- df %>% mutate(EVsPer1000ppl = 1000*EVs/total)

dfnN <- df %>% 
  filter(country!= "Norway")

dfnN <- dfnN %>% mutate(flag = paste("flags/", dfnN$country, ".png", sep = ""))

ggplot(dfnN, aes(x = EVsPer1000ppl, y = emission)) +
  geom_image(aes(image = flag), size = 0.05) +
  geom_smooth(method="lm", se=TRUE, fullrange=FALSE, level=0.9) +
  labs(
    x = "Number of EVs per 1000 people",
    y = "Average emission of CO2 (km/g)"
  ) +
  theme_bw()


# ggplot(dfnN, aes(x = EVsPer1000ppl, y = emission)) +
#   geom_image(aes(image = flag), size = 0.05) +
#   labs(
#     x = "Number of EVs per 1000 people",
#     y = "Average emission of CO2 (km/g)"
#   ) +
#   theme_bw() +
#   scale_x_continuous(limits = c(0,3)) +
#   scale_y_continuous(limits = c(115,132.5))
