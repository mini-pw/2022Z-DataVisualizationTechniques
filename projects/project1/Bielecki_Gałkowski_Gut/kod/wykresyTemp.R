library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(forcats)
library(lubridate)
library(patchwork)
Temp <- read.csv("GlobalLandTemperaturesByCountry.csv")

countries <- c("United States", "Australia", "Botswana", "Canada")
Temp2 <- Temp %>% filter(Country %in% countries) %>% filter(!is.na(AverageTemperature))
Temp2 <- Temp2 %>% mutate(Year = year(dt), Month = month(dt), Decade = Year - Year %% 10,
                          Season = ifelse(Month %in% c(1,2,3), "Winter",
                                          ifelse(Month %in% c(4,5,6), "Spring",
                                                 ifelse(Month %in% c(7,8,9), "Summer", "Fall")))) 


Temperatures <- Temp2 %>% mutate(color = ifelse(Season == "Spring", "#2ca25f",
                                                ifelse(Season == "Summer", "#f03b20",
                                                       ifelse(Season == "Fall", "#feb24c", "#9ecae1"))))%>% filter(Year>1850)

Temperatures <- Temperatures %>% mutate(date = dt) %>% select(-dt)

TemperaturesSeasonal <- Temperatures %>% group_by(Country, Year, Season) %>% summarise(mean = mean(AverageTemperature, sdSeasonal = sd(AverageTemperature))) 


colors = c("#2ca25f", "#f03b20", "#feb24c", "#9ecae1")

p1<-TemperaturesSeasonal %>% filter(Country == "Australia") %>% ggplot(aes(x = Year, y = mean))+
  geom_jitter(size=1, alpha=0.85, aes(color = Season)) +
  scale_color_manual(values = c("#f03b20", "#a1d99b", "#feb24c", "#9ecae1"))+
  geom_smooth(aes(group = Season), size = 0.75, color = "black") + theme(panel.background = element_rect(fill = "#5C7EA8"),
                                                                         panel.grid.major = element_blank(),
                                                                         panel.grid.minor = element_blank(),
                                                                         axis.text = element_text(size = 6, colour = "white")) + 
  labs(x = "Year", y = "mean temp. in C", title = "Australia")

p2<-TemperaturesSeasonal %>% filter(Country == "Botswana") %>% ggplot(aes(x = Year, y = mean))+
  geom_jitter(size=1, alpha=0.85, aes(color = Season)) +
  scale_color_manual(values = c("#f03b20", "#a1d99b", "#feb24c", "#9ecae1"))+
  geom_smooth(aes(group = Season), size = 0.75, color = "black") + theme(panel.background = element_rect(fill = "#5C7EA8"),
                                                                         panel.grid.major = element_blank(),
                                                                         panel.grid.minor = element_blank(),
                                                                         axis.text = element_text(size = 6, colour = "white")) + 
  labs(x = "Year", y = "mean temp. in C", title = "Botswana")

p3<-TemperaturesSeasonal %>% filter(Country == "Canada") %>% ggplot(aes(x = Year, y = mean))+
  geom_jitter(size=1, alpha=0.85, aes(color = Season)) +
  scale_color_manual(values = c("#f03b20", "#a1d99b", "#feb24c", "#9ecae1"))+
  geom_smooth(aes(group = Season), size = 0.75, color = "black") + theme(panel.background = element_rect(fill = "#5C7EA8"),
                                                                         panel.grid.major = element_blank(),
                                                                         panel.grid.minor = element_blank(),
                                                                         axis.text = element_text(size = 6, colour = "white")) + 
  labs(x = "Year", y = "mean temp. in C", title = "Canada")

p4<-TemperaturesSeasonal %>% filter(Country == "United States") %>% ggplot(aes(x = Year, y = mean))+
  geom_jitter(size=1, alpha=0.85, aes(color = Season)) +
  scale_color_manual(values = c("#f03b20", "#a1d99b", "#feb24c", "#9ecae1"))+
  geom_smooth(aes(group = Season), size = 0.75, color = "black") + theme(panel.background = element_rect(fill = "#5C7EA8"),
                                                                         panel.grid.major = element_blank(),
                                                                         panel.grid.minor = element_blank(),
                                                                         axis.text = element_text(size = 6, colour = "white")) + 
  labs(x = "Year", y = "mean temp. in C", title = "United States of America")


p1+p2 + plot_layout(guides = "collect") & theme_minimal() & 
  plot_annotation(theme = theme(plot.background = element_rect(fill = "#5C7EA8"),
                                panel.background = element_rect(fill = "#5C7EA8"))) &
  theme(axis.text = element_text(size = 9, colour = "white"), legend.position = "none",
        axis.title.x = element_text(colour = "white", size = 14),
        axis.title.y = element_text(colour = "white", size = 14),
        axis.title = element_text(colour = "white"),
        title = element_text(colour = "white", size = 16))

p3+p4 + plot_layout(guides = "collect") & theme_minimal() & 
  plot_annotation(theme = theme(plot.background = element_rect(fill = "#5C7EA8"),
                                panel.background = element_rect(fill = "#5C7EA8"))) &
  theme(axis.text = element_text(size = 9, colour = "white"),
        axis.title.x = element_text(colour = "white", size = 14),
        axis.title.y = element_text(colour = "white", size = 14),
        axis.title = element_text(colour = "white"),
        title = element_text(colour = "white", size = 16),
        legend.text = element_text(colour = "white", size = 10),
        legend.title = element_text(size = 14))
