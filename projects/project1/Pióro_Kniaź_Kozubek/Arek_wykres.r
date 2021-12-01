

library(tidyr)
library(ggplot2)
library(maps)
require(maps)
library(dplyr)
library(readr)
library(SmarterPoland)
library(ggrepel)
library(patchwork)


co2_gdp <- data.frame(read_csv("co2_gdp.csv"))
colnames(co2_gdp) <- c("Entity", "Code","Year","co2","gdp","a","population","c")
co2_gdp<-filter(co2_gdp,Year=="2018") %>% 
 drop_na(Code) %>% 
   drop_na(co2) %>% 
   drop_na(gdp) 
co2_gdp<-co2_gdp[-c(163), -c(6,8)]
countries<-countries[, -c(2,3,4)]
co2_gdp<-mutate(co2_gdp,Entity_2 = case_when(Entity == "Czechia" ~ "Czech Republic",
                            Entity == "Vietnam" ~ "Viet Nam",
                            Entity == "Russia" ~ "Russian Federation",
                            TRUE ~ Entity))
co2_gdp<-full_join(co2_gdp,countries, by = c("Entity_2"="country" )) 
co2_gdp<-na.omit(co2_gdp) 
options(scipen=10000)
co2_gdp<-mutate(co2_gdp,Entity_3 = case_when(Entity == "Australia" ~ "Australia",
                                             Entity == "Austria" ~ "Austria",
                                             Entity == "Bangladesh" ~ "Bangladesh",
                                             Entity == "Chile" ~ "Chile",
                                             Entity == "China" ~ "China",
                                             Entity == "Croatia" ~ "Croatia",
                                             Entity == "Egypt" ~ "Egypt",
                                             Entity == "Estonia" ~ Entity,
                                             Entity == "Benin" ~ Entity,
                                             Entity == "Lesotho" ~ Entity,
                                             Entity == "France" ~ Entity,
                                             Entity == "Germany" ~ Entity,
                                             Entity == "Hungary" ~ Entity,
                                             Entity == "Iceland" ~ Entity,
                                             Entity == "India" ~ Entity,
                                             Entity == "Indonesia" ~ Entity,
                                             Entity == "Poland" ~ Entity,
                                             Entity == "United Kingdom" ~ Entity,
                                             Entity == "Russia" ~ Entity,
                                             Entity == "United States" ~ Entity,
                                             Entity == "Zimbabwe" ~ Entity,
                                             Entity == "Rwanda" ~ Entity,
                                             Entity == "Oman" ~ Entity,
                                             Entity == "Norway" ~ Entity,
                                             Entity == "Niger" ~ Entity,
                                             Entity == "Canada" ~ Entity,
                                             Entity == "Kuwait" ~ Entity,
                                             Entity == "Trinidad and Tobago" ~ Entity,
                                             Entity == "Qatar" ~ Entity,
                                             Entity == "Luxembourg" ~ Entity,
                                             Entity == "Ethiopia" ~ Entity,
                                             Entity == "Bahrain" ~ Entity,
                                             Entity == "Kazakhstan" ~ Entity,
                                             Entity == "Mongolia" ~ Entity,
                                             Entity == "Mali" ~ Entity,
                                             Entity == "Nigeria" ~ Entity,
                                             Entity == "United Arab Emirates" ~ Entity,
                                             Entity == "Guatemala" ~ Entity,
                                             Entity == "Burundi" ~ Entity,
                                             Entity == "Brazil" ~ Entity,
                                             Entity == "Switzerland" ~ Entity,
                                             Entity == "Central African Republic" ~ Entity,
                                             TRUE ~ ""))

p3<-ggplot(co2_gdp,aes(x=gdp, y=co2,label =Entity_3))+ 
  geom_point(aes(size = population, color = continent),show.legend = FALSE) + 
  scale_x_log10()+
  scale_size_continuous(range = c(4, 10),)+
  geom_text_repel(aes(color = continent))+
  labs(x = "GDP per capita",
       y = "CO2 emissions per capita",color="Continent"
       )+
  theme(panel.grid.minor = element_blank(),
        panel.grid =element_line(color = "white",size = 0.05,linetype = 1))
  theme_light() 
p3













