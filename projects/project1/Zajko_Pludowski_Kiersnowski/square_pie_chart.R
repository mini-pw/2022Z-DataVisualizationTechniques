
vege <- read.csv2("./data/FAO_for_waffle_enchanced/vege.csv", header = T, sep = ",")
meat <- read.csv2("./data/FAO_for_waffle_enchanced/meat.csv", header = T, sep = ",")
diary <- read.csv2("./data/FAO_for_waffle_enchanced/diary.csv", header = T, sep = ",")

library(dplyr)
library(waffle)
library(ggplot2)
library(forcats)
library(tidyr)
library(ggpubr)
library(stringr)



vege <- mutate(vege, type = "vege") %>% 
  filter(Item %in% c("Fruit Primary", "Vegetables Primary"))
meat <- mutate(meat, type = "livestock")
diary <- mutate(diary, type = "livestock")

food <- bind_rows(vege, meat, diary) %>% 
  select(Year, Value, type) %>% 
  group_by(Year, type) %>% 
  summarise(sum = sum(Value)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Year, values_from = sum, names_prefix = 'r') %>% 
  pivot_longer(cols = c("r1993", "r2013"), names_to = "Year", values_to = "Value") %>% 
  arrange(Year)

food1993 <- food %>% filter(Year == 'r1993')
food2013 <- food %>% filter(Year == 'r2013')

AVG_LOSS <- 0.33

food1993 <- food1993 %>%
  mutate(Value = Value / 1E7) %>% 
  mutate(WasteValue = Value * AVG_LOSS) %>% 
  mutate(Value = Value - WasteValue) %>% 
  mutate(Value = as.integer(Value), WasteValue = as.integer(WasteValue)) %>% 
  pivot_longer(names_to = 'isWasted', cols = c(Value, WasteValue), values_to = 'Value') %>% 
  mutate(v = paste(type, isWasted)) %>% 
  arrange(isWasted)

food2013 <- food2013 %>%
  mutate(Value = Value / 1E7) %>% 
  mutate(WasteValue = Value * AVG_LOSS) %>% 
  mutate(Value = Value - WasteValue) %>% 
  mutate(Value = as.integer(Value), WasteValue = as.integer(WasteValue)) %>% 
  pivot_longer(names_to = 'isWasted', cols = c(Value, WasteValue), values_to = 'Value') %>% 
  mutate(v = paste(type, isWasted)) %>% 
  arrange(isWasted)


food1993$v <- fct_inorder(food1993$v)
food2013$v <- fct_inorder(food2013$v)

food_all <- union(food1993, food2013) %>% 
  mutate(Year = str_sub(Year, start = 2))

# WCALE NIE W MILIONACH TYLKO DZIESI¥TKACH MILIONÓW
ggplot(food_all, aes(fill = v, values = Value)) +
  geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
  facet_wrap(~Year, nrow = 1, strip.position = "bottom") +
  scale_x_discrete() + 
  scale_y_continuous(breaks = c(), labels = c()) +
  coord_equal() +
  labs(
    title = "Produkcja ¿ywnoœci w latach 1993 i 2013",
    subtitle = "",
    y = "",
    x = "",
    fill = "Typy ¿ywnoœci"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = c("#258e25", "#cc0000", "#85e085", "#ff9999"),
                    labels = c("Warzywa i owoce", "Miêso i nabia³",
                               "Warzywa i owoce zmarnowane", "Miêso i nabia³ zmarnowane"))


