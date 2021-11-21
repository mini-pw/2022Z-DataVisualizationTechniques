###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###               WYKŁAD 8              ###
###########################################

library(ggplot2)
library(dplyr)


df <- read.csv(file = "https://raw.githubusercontent.com/R-Ladies-Warsaw/PoweR/master/Cz%C4%99%C5%9B%C4%87%202%20-%20Formatowanie%20danych/R/data/vgsales.csv", 
               encoding = "UTF-8")

head(df)


# Pierwszy wykres ---------------------------------------------------------
# Liczba wydanych gier akcji w kolejnych latach.

df %>% 
  filter(Genre == "Action", Year != "N/A") %>% 
  group_by(Genre, Year) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = Year, y = n)) + 
  geom_col(fill = "darkgreen") + 
  scale_y_continuous(expand = c(0,0), limits = c(0, 280)) + 
  labs(title = "Liczba wydanych gier akcji w kolejnych latach.",
       x = "Rok wydania",
       y = "Liczba gier") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90)) 




# Drugi wykres ------------------------------------------------------------
# Liczba wydanych gier akcji w kolejnych latach w podziale na platformę.

df %>% 
  filter(Genre == "Action", Year != "N/A", Platform %in% c("PS", "PS2", "PS3", "PS4", "PSP", "PSV", "XOne", "XB", "X360")) %>% 
  group_by(Genre, Year, Platform) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = Year, y = n, fill = Platform)) + 
  geom_col() + 
  scale_y_continuous(expand = c(0,0), limits = c(0, 280)) + 
  labs(title = "Liczba wydanych gier akcji w kolejnych latach.",
       x = "Rok wydania",
       y = "Liczba gier",
       fill = "Platforoma") +
  theme(axis.text.x = element_text(angle = 90))
