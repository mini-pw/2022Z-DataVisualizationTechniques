library(ggplot2)
library(forcats) 
library(dplyr)
library(patchwork)

if ( options()$stringsAsFactors )
  options(stringsAsFactors=FALSE)

####################### DATA ##########################

data_supply_chain <- read.csv("food-emissions-supply-chain.csv")

colnames(data_supply_chain)[c(4, 5, 6, 7, 8, 9, 10)] <- c("land", "farm", "feed", "processing", "transport",
                                                          "retail", "packaging")
data_supply_chain %>% 
  mutate(suma = (land + farm + feed + processing + transport + retail + packaging)) %>% 
  arrange(-suma) -> data_supply_chain

result <- data_supply_chain %>%
  filter(!(Entity == "Olive Oil" | Entity == "Cane Sugar" | Entity == "Beet Sugar" |
             Entity == "Other Fruit"|
             Entity == "Other Vegetables"| Entity == "Brassicas" |
             Entity == "Nuts" | Entity == "Rapeseed Oil" | Entity == "Groundnuts" | Entity == "Other Pulses" |
             Entity == "Cassava" | Entity == "Onions & Leeks" | Entity == "Sunflower Oil" |
             Entity == "Citrus Fruits" | Entity == "Palm Oil" |
             Entity == "Soybean Oil" | Entity == "Citrus Fruit")) %>% 
  select("Entity", "land", "farm", "feed", "processing", "transport",
         "retail", "packaging") %>% 
  pivot_longer(cols = c("land", "farm", "feed", "processing", "transport",
                        "retail", "packaging"),
               names_to = "type", values_to = "Emissions")

####################### PLOT ##################################

p6 <- ggplot(result,
             aes(x = (Emissions), y = reorder(Entity, (Emissions)), fill = fct_rev(type))) + 
  geom_col() +
  theme(
    legend.position = "bottom"
  ) + scale_fill_manual(values=c("#cb181d", "#66c2a5", "#fdbf6f", "#ffd92f","#a6d854", "#e78ac3",
                                 "#8da0cb")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.04)), breaks = breaks_width(10)) +
  labs(x = "Greenhouse gas emissions per kilogram of food product (CO2 equivalent)",
       title = "Greenhouse gas emissions across the supply chain",
       fill = "type") + theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position = c(0.9, 0.25),
        legend.key.size = unit(0.8, 'cm'),
        rect = element_rect(fill = "transparent"),
        panel.background = element_rect(colour = 'white'))
p6 