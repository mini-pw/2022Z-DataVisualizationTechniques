library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)

# wykresy USA, Australia, Canada, Botswana

gas_sector <- read.csv('ghg-emissions-by-sector.csv', sep=',')
head(gas_sector)

gas_sector <- gas_sector %>%
  filter(Entity %in% c("Australia","United States", "Botswana", "Canada"))

colnames(gas_sector)<- c("Country","Code","Year", "Agriculture",
                         "Land-Use Change and Forestry", "Waste", "Industry",
                         "Manufacturing & Construction", "Transport", 
                         "Electricity & Heat", "Buildings", 
                         "Fugitive emissions", "Other fuel combustion", 
                         "Aviation and shipping")

gas_sector <- gas_sector %>% 
  filter(Year >= 2000 & Year <= 2016) %>% 
  group_by(Country) %>% 
  mutate(Agriculture = mean(Agriculture)) %>% 
  mutate(`Land-Use Change and Forestry` = mean(`Land-Use Change and Forestry`)) %>%
  mutate(Waste = mean(Waste)) %>%
  mutate(Industry = mean(Industry)) %>%
  mutate(`Manufacturing & Construction` = mean(`Manufacturing & Construction`)) %>%
  mutate(Transport = mean(Transport)) %>%
  mutate(`Electricity & Heat` = mean(`Electricity & Heat`)) %>%
  mutate(Buildings = mean(Buildings)) %>%
  mutate(`Fugitive emissions` = mean(`Fugitive emissions`)) %>%
  mutate(`Other fuel combustion` = mean(`Other fuel combustion`)) %>%
  mutate(`Aviation and shipping` = mean(`Aviation and shipping`)) %>% 
  filter(Year == 2016)

gas_sector <- gas_sector %>% pivot_longer(cols = colnames(gas_sector)[4:14], 
                                          names_to = "Categories", 
                                          values_to = "Values")

colors <- c("#53A85C", "#5A697D", "#B46CC5", '#D24749', 
            "#949595", "#A44953", "#77C292", "#D9847D",
            "#4BA4AA", "#F0824D", "#5278B6")

USA <- gas_sector %>% filter(Country == "United States")

ggplot(USA, aes(x = Values, 
                y = fct_reorder(factor(Categories), Values))) + 
  geom_col(width = 0.65, fill = ifelse(USA$Values > 0, "#e34a33", "#2ca25f")) +
  geom_text(aes(label = ifelse(round(Values/1000000, 2) > 1000,
                               format(x = paste0(round(Values/1000000000, 2), " bil t")),
                               format(x = paste0(round(Values/1000000, 2), " mil t"))), 
                hjust = ifelse(Values > 0, -.1, 1.1)), 
            size = 4, position = position_dodge2(width = 0.9),color = "white") +
  labs(title = "Greenhouse gas emissions by sector - United States 2000-2016") +
  scale_y_discrete(expand = expansion(mult = c(0.05, 0.05))) +
  scale_x_continuous(breaks = c(0, 100000000, 300000000, 
                                500000000, 1000000000, 
                                1500000000, 2000000000),
                     labels = c("0", "100 mil", "300 mil", 
                                "500 mil", "1 bilion", 
                                "1.5 bilion", "2 bilion"), 
                     expand = expansion(mult = c(0.15, 0.15))) +
  theme(legend.position = "none",
        axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.y = element_blank(),
        title = element_text(size = 14, family = "sans", face = "bold"),
        plot.background = element_rect(fill = "#5C7EA8", color = "#5C7EA8"),
        panel.background = element_rect(fill = "#5C7EA8", color = "#5C7EA8"),
        axis.text.y = element_text(family = "sans", size = 12, color = "white"),
        axis.text.x = element_text(color = "white", size = 11),
        text = element_text(color = "white"))

Australia <- gas_sector %>% filter(Country == "Australia")


ggplot(Australia, aes(x = Values, 
                y = fct_reorder(factor(Categories), Values))) +
  geom_col(width = 0.65, fill = ifelse(Australia$Values > 0, "#e34a33", "#2ca25f")) +
  geom_text(aes(label = ifelse(round(Values/1000000, 2) > 1000,
                               format(x = paste0(round(Values/1000000000, 2), " bil t")),
                               format(x = paste0(round(Values/1000000, 2), " mil t"))), 
                hjust = ifelse(Values > 0, -.1, 1.1)), 
            size = 4, position = position_dodge2(width = 0.9), color = "white") +
  labs(title = "Greenhouse gas emissions by sector - Australia 2000-2016") +
  scale_y_discrete(expand = expansion(mult = c(0.05, 0.05))) +
  scale_x_continuous(breaks = c(-50000000,0, 20000000, 40000000, 
                                60000000, 80000000, 
                                100000000,150000000, 200000000),
                     labels = c("-50 mil","0", "20 mil", "40 mil", 
                                "60 mil", "80 mil", 
                                "100 mil", "150 mil", "200 mil"), 
                     expand = expansion(mult = c(0.15, 0.15))) +
  theme(legend.position = "none",
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "#5C7EA8", color = "#5C7EA8"),
        panel.background = element_rect(fill = "#5C7EA8", color = "#5C7EA8"),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(family = "sans", size = 12, color = "white"),
        axis.text.x = element_text(color = "white", size = 11),
        title = element_text(size = 14, family = "sans", face = "bold", color = "white"))

Botswana <- gas_sector %>% filter(Country == "Botswana")

ggplot(Botswana, aes(x = Values, 
                      y = fct_reorder(factor(Categories), Values))) + 
  geom_col(width = 0.65, fill = ifelse(Botswana$Values > 0, "#e34a33", "#2ca25f")) +
  geom_text(aes(label = ifelse(round(Values/1000000, 2) > 1000,
                               format(x = paste0(round(Values/1000000000, 2), " bil t")),
                               format(x = paste0(round(Values/1000000, 2), " mil t"))), 
                hjust = ifelse(Values > 0, -.1, 1.1)), 
            size = 4, position = position_dodge2(width = 0.9), color = "white") +
  labs(title = "Greenhouse gas emissions by sector - Botswana 2000-2016") +
  scale_y_discrete(expand = expansion(mult = c(0.05, 0.05))) +
  scale_x_continuous(breaks = c(0, 2000000, 10000000, 40000000),
                     labels = c("0", "2 mil", "10 mil", "40 mil"), 
                     expand = expansion(mult = c(0.02, 0.15))) +
  theme(legend.position = "none",
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "#5C7EA8", color = "#5C7EA8"),
        panel.background = element_rect(fill = "#5C7EA8", color = "#5C7EA8"),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(family = "sans", size = 12, color = "white"),
        axis.text.x = element_text(color = "white", size = 11),
        title = element_text(size = 14, family = "sans", face = "bold", color = "white"))


Canada <- gas_sector %>% filter(Country == "Canada")

ggplot(Canada, aes(x = Values, 
                      y = fct_reorder(factor(Categories), Values))) + 
  geom_col(width = 0.65, fill = ifelse(Canada$Values > 0, "#e34a33", "#2ca25f")) +
  geom_text(aes(label = ifelse(round(Values/1000000, 2) > 1000,
                               format(x = paste0(round(Values/1000000000, 2), " bil t")),
                               format(x = paste0(round(Values/1000000, 2), " mil t"))), 
                hjust = ifelse(Values > 0, -.1, 1.1)), 
            size = 4, position = position_dodge2(width = 0.9), color = "white") +
  labs(title = "Greenhouse gas emissions by sector - Canada 2000-2016") +
  scale_y_discrete(expand = expansion(mult = c(0.05, 0.05))) +
  scale_x_continuous(breaks = c(-50000000,0, 20000000, 40000000, 
                                60000000, 80000000, 
                                100000000,150000000, 200000000),
                     labels = c("-50 mil","0", "20 mil", "40 mil", 
                                "60 mil", "80 mil", 
                                "100 mil", "150 mil", "200 mil"), 
                     expand = expansion(mult = c(0.02, 0.15))) +
  theme(legend.position = "none",
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "#5C7EA8", color = "#5C7EA8"),
        panel.background = element_rect(fill = "#5C7EA8", color = "#5C7EA8"),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(family = "sans", size = 12, color = "white"),
        axis.text.x = element_text(color = "white", size = 11),
        title = element_text(size = 14, family = "sans", face = "bold", color = "white"))


