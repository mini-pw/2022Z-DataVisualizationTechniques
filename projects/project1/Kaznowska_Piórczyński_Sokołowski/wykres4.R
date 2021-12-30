#C:/Users/farbo/Desktop/Studenckie ¿ycie/Semestr 3/Techniki Wizualizacji Danych/proj1/happy_index/Happy&Percent
library(dplyr)
library(ggplot2)
library(readxl)
library(RColorBrewer)

# dane ze strony
# https://happyplanetindex.org/countries/

df <- read_excel("edited.xlsx")
dd <- read.csv("owid-energy-data.csv")


df %>% 
  select(Country, Continent, Year, "Ladder of life (Wellbeing) (0-10)") -> df
names(df) <- c("Country", "Continent", "Year", "Wellbeing")

#countries <- c('Austria','Belgium','Bulgaria','Cyprus','Czechia','Germany',
#'Denmark','Estonia','Greece','Spain','Finland','France','Croatia','Hungary',
#'Ireland','Iceland','Italy','Lithuania','Luxembourg','Latvia','North Macedonia',
#'Netherlands','Norway','Poland','Portugal','Romania','Sweden','Slovenia',
#'Slovakia','Turkey','United Kingdom', 'Russia', 'Belarus', 'Ukraine', "Switzerland")

countries <- c("Tajikistan", "Armenia", "Uzbekistan", "Kyrgyzstan", "Azerbaijan",
               "Montenegro", "Kazakhstan", "Turkmenistan")

df <- df %>% 
  filter(Year == 2018 & (Continent == 3 | Continent == 7)) %>% 
  select(Country, Year, Wellbeing) %>% 
  filter(!(Country %in% countries))


dd <- dd %>% 
  select(country, year, renewables_share_energy) %>% 
  filter(year == 2018) %>% 
  select(country, year, renewables_share_energy)

names(dd) <- c("Country", "Year", "Percent")

combined <- left_join(dd, df, by = c("Country", "Year"))

combined <- na.omit(combined)


#############################
quantile(combined$Percent)
combined <- combined %>% 
  mutate(Skala = factor(case_when(
    Percent < 7 ~ 1,
    Percent < 14 ~ 2,
    Percent < 22 ~ 3,
    TRUE ~ 4
  )))

combined %>% 
  filter(Year == 2018) %>% 
  ggplot(aes(x = Wellbeing, y = reorder(Country, Wellbeing), fill = Skala)) +
  geom_col() +
  scale_fill_manual(values=c("#FFF59D", "#DCE775", "#8BC34A", "#388E3C"),
                    labels = c("< 7%", "7% - 14%", "14% - 22%", "> 22%"),
                    name = "Ecological \nenergy \nshare")+
  labs(title = "Happiness and greenness",
       subtitle = "Year 2018",
       x = "Happiness rate (0-10)",
       y = "Country")+
  theme(axis.title.y = element_text(size = 18, face = "bold"),
        axis.title.x = element_text(size = 18, face = "bold"),
        plot.title = element_text(size = 30, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(face = "bold", size = 20, hjust = 0.5),
        plot.margin = unit(c(0.5,1,0.75,0.75), "cm"),
        plot.background = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey"),
        panel.grid.minor = element_line(color = "grey"),
        panel.grid.major.y = element_blank(),
        legend.title = element_text(face = "bold", size = 20),
        legend.text = element_text(size = 15))

############################
combined <- combined %>% 
  mutate(Skala = factor(case_when(
    Percent < 15 ~ 1,
    Percent < 30 ~ 2,
    Percent < 50 ~ 3,
    TRUE ~ 4
  )))

combined %>% 
  filter(Year == 2018) %>% 
  ggplot(aes(x = Wellbeing, y = reorder(Country, Wellbeing), fill = Skala)) +
  geom_col() +
  scale_fill_manual(values=c("#f1eef6", "#bdc9e1", "#74a9cf", "#0570b0"),
                    labels = c("< 15%", "15% - 30%", "30% - 50%", "50+%"),
                    name = "Ekologicznosc") +
  labs(title = "Przedstawienie szczescia i ekologicznosci krajow Europy",
       subtitle = "Rok 2018",
       x = "Wspolczynnik szczescia (0-10)",
       y = "Panstwa Europy") +
  theme(axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        title = element_text(size = 15),
        plot.subtitle = element_text(face = "italic", size = 14),
        plot.margin = unit(c(0.5,1,0.75,0.75), "cm"))

################################

combined %>% 
  filter(Year == 2018) %>% 
  ggplot(aes(x = Wellbeing, y = reorder(Country, Wellbeing), fill = Percent)) +
  geom_col() +
  labs(title = "Przedstawienie szczescia i ekologicznosci krajow Europy",
       subtitle = "Rok 2018",
       x = "Wspolczynnik szczescia (0-10)",
       y = "Panstwa Europy") +
  theme(axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        title = element_text(size = 15),
        plot.subtitle = element_text(face = "italic", size = 14),
        plot.margin = unit(c(0.5,1,0.75,0.75), "cm")) +
  scale_fill_gradient(low = "red", high = "green",
                      name = "Ekologicznosc")
