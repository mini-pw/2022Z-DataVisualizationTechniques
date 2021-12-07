library(dplyr)
library(ggplot2)
library(data.table)


load_data = function (name){
  print(paste("data/",name,".csv"))
  data = read.csv(paste("data/",name,".csv", sep=""), sep = "|",  col.names = c("DT", "Country", "Partner", "Industry", "Year", "Value"))
  data = data.table(data)
  data[Partner== "WLD" & Industry=="DTOTAL"][,c("DT","Partner", "Industry"):=NULL]
}


## million tonnes
name = "FD_CO2"
everything = load_data("FD_CO2")
# import = load_data("IMGR_TCO2")
# export = load_data("EXGR_TCO2")
# territorial = load_data("PROD_CO2")
gdp = read.csv("data/co2-emissions-vs-gdp.csv")


# import <- rename(import, Imported = Value)
# export <- rename(export, Exported = Value)
# im_ex <- inner_join(import, export, by=c("Country", "Year"))


interestingEntites = c("United States", "China", "Australia", "Argentina", "South Africa", "Germany")

# kg/$

inner_join(everything, gdp, by=c("Country"="Code", "Year")) %>% 
  select(-X145446.annotations, -Continent, -Country) %>%
  filter(Entity %in% interestingEntites) %>% 
  rename(Balance = Value,
         Population = Total.population..Gapminder..HYDE...UN.,
         Country = Entity) %>% 
  mutate(Index = Balance*1e9 / Population / GDP.per.capita) %>%
  mutate(Index_norm = (Index - min(Index)) / (max(Index) - min(Index))) %>% 
  ggplot() +
  geom_line(aes(x = Year, y = Index_norm, colour = Country), size = 1.1, linejoin="mitre") +
  scale_x_continuous(breaks = 2005:2015, expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0.1, 1, by=0.1), limits=c(0, 1), expand = c(0 ,0, 0.05, 0)) +
  labs(x = "Year",
       y = "Normalized index") +
  theme_bw() +
  scale_color_manual(values = c("#FF61C3", "#00B9E3", "#00BA38", "#F2E411", "white", "red")) +
  theme(panel.background = element_rect(fill="black", color = "black"),
        plot.background = element_rect(fill="black"),
        legend.background = element_rect(fill = "black"),
        legend.position = "none",
        text = element_text(color = "white", face = "bold", size=17),
        axis.text = element_text(color = "white", face = "bold", size=14, margin = margin(t=20)),
        axis.line = element_line(color = "white"),
        panel.grid = element_line(color = "#5c5c5c"),
        plot.margin = margin(0.5, 2, 0.5, 1,'cm'))

