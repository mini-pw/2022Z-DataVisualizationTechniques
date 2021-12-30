library(tidyverse)
library(maps)

emissions_2015 <- read.csv("data/emissions_2015.csv")

df_map <- map_data("world") %>% 
  filter(region != "Antarctica") %>% 
  mutate(Country.Code = iso.alpha(region, n = 3)) %>% 
  select(long, lat, group, Country.Code) %>% 
  left_join(emissions_2015, by = "Country.Code")

df_world_intervals <- df_map %>% 
  mutate(level = case_when(
    MtCO2eq <= 10 ~ 1,
    MtCO2eq > 10 & MtCO2eq <= 50 ~ 2,
    MtCO2eq > 50 & MtCO2eq <= 100 ~ 3,
    MtCO2eq > 100 & MtCO2eq <= 500 ~ 4,
    MtCO2eq > 500 & MtCO2eq <= 1000 ~ 5,
    MtCO2eq > 1000 & MtCO2eq <= 1500 ~ 6,
    MtCO2eq > 1500 & MtCO2eq <= 2000 ~ 7,
    MtCO2eq > 2000 ~ 8
  ))
df_world_intervals$interval <- factor(df_world_intervals$level,
                                      levels = 1:8,
                                      labels = c("0 - 10", 
                                                 "10 - 50", 
                                                 "50 - 100",
                                                 "100 - 500", 
                                                 "500 - 1000", 
                                                 "1000 - 1500",
                                                 "1500 - 2000",
                                                 ">2000"))
map_world_intervals <- df_world_intervals %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = interval)) +
  geom_polygon(colour = "black") +
  scale_fill_manual(name = "million tones of\nCO2 equivalent", 
                    values = c("#f7fcf5", "#e5f5e0", "#c7e9c0", 
                               "#a1d99b", "#74c476", "#41ab5d", 
                               "#238b45", "#005a32"),
                    drop = FALSE,
                    na.value = "grey") +
  labs(title = "Greenhouse gas emissions in food industry",
       subtitle = "Year 2015") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),)

map_world_intervals

ggsave(map_world_intervals, filename = "map.svg", bg = "transparent")

