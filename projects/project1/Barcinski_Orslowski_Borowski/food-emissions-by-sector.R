library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(stringr)
library(patchwork)


options(scipen = 100)

zloto <- read.csv("emissions-by-sector.csv")

zloto <- zloto %>% 
  mutate(FOOD_system_stage = replace(FOOD_system_stage, FOOD_system_stage == "LULUC (Production)", "Land Use")) %>% 
  mutate(FOOD_system_stage = replace(FOOD_system_stage, FOOD_system_stage == "End_of_Life", "End of Life"))
  

usa <- zloto %>%
  filter(Country_code_A3 == "USA") %>% 
  select(-c(Country_code_A3, Name, C_group_IM24_sh, dev_country, Substance)) %>%
  mutate_at(vars(-FOOD_system_stage), as.numeric) %>% 
  group_by(FOOD_system_stage) %>%
  summarise_each(sum) %>% 
  pivot_longer(!FOOD_system_stage, names_to = "Year", values_to = "total.emissions") %>% 
  mutate(Year = as.numeric(str_sub(Year, 3, -1)),
         total.emissions.in.mln.t = total.emissions / 1000) %>% 
  ggplot(aes(x = Year, y = total.emissions.in.mln.t, fill = FOOD_system_stage)) +
  geom_area() +
  scale_y_continuous(limits = c(0, 2000), labels = unit_format(unit = "", accuracy = 1)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_manual(name = "Sector", values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#e6ab02", "#66a61e", "#a6761d", "#666666")) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    plot.margin = margin(1, 1, 1, 1, "lines"),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
  ) +
  labs(title = "Total greenhouse emissions caused by food production by sector",
       subtitle = "USA",
       x = "Year",
       y = "Emissions  [mln t]")


india <- zloto %>%
  filter(Country_code_A3 == "IND") %>% 
  select(-c(Country_code_A3, Name, C_group_IM24_sh, dev_country, Substance)) %>%
  mutate_at(vars(-FOOD_system_stage), as.numeric) %>% 
  group_by(FOOD_system_stage) %>%
  summarise_each(sum) %>% 
  pivot_longer(!FOOD_system_stage, names_to = "Year", values_to = "total.emissions") %>% 
  mutate(Year = as.numeric(str_sub(Year, 3, -1)),
         total.emissions.in.mln.t = total.emissions / 1000) %>% 
  ggplot(aes(x = Year, y = total.emissions.in.mln.t, fill = FOOD_system_stage)) +
  geom_area() +
  scale_y_continuous(limits = c(0, 2000), labels = unit_format(unit = "", accuracy = 1)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_manual(name = "Sector", values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#e6ab02", "#66a61e", "#a6761d", "#666666")) +
  theme(
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    plot.margin = margin(1, 1, 1, 1, "lines"),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
  ) +
  labs(
       subtitle = "India",
       x = "Year",
       y = "Emissions [mln t]")

p <- usa / india 

ggsave(p, filename = "usa_india.svg")
ggsave(p, filename = "usa_india.png")
