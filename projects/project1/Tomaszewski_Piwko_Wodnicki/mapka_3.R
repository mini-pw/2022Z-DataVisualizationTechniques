#Biblioteki:
library(ggplot2)
library(dplyr)
library(sf)
library("rnaturalearth")
library("rnaturalearthdata")
require(maps)
library(svglite)


#Dane ze strony: https://ourworldindata.org/natural-disasters

Deaths <- read.csv("natural-disaster-deaths-ihme.csv")
Displaced <- read.csv("internally-displaced-persons-from-disasters.csv")
Affected <- read.csv("number-of-people-directly-affected-by-natural-disasters.csv")
Population <- read.csv("world-population-1750-2015-and-un-projection-until-2100.csv")
world1 <- ne_countries(scale = "medium", returnclass = "sf")
world <- map_data("world")



Pop <- Population %>% 
  filter(Year < 2021, Year > 2009, Entity == "Our World In Data") %>% 
  rename(World_population = 4) %>% 
  select(3, 4)

Populacje_krajow <- world1 %>% 
  select("pop_est", "brk_name")
class(Populacje_krajow) <- c("data.frame") 
Populacje_krajow <- Populacje_krajow[,c(1,2)]

Deaths_function <- function() {
  tmp1 <- Deaths %>% 
    rename(deaths = 4) %>% 
    filter(Year > 2009, Year < 2019) 
}
d <- Deaths_function()


Affected_function <- function() {
  tmp3 <- Affected %>% 
    rename(affected = 4) %>% 
    filter(Year > 2009, Year < 2019)
  
  pomocnicza <- Deaths %>%  rename(deaths = 4) %>% 
    filter(Year > 2009, Year < 2020) %>% 
    mutate(deaths = round(deaths)) %>% 
    select(1,3,4)
  
  tmp3 <- full_join(full_join(tmp3, Pop, by = "Year"), pomocnicza, by = c("Entity", "Year")) 
  tmp3$deaths[is.na(tmp3$deaths)] <- 0
  tmp3 <- tmp3 %>% 
    mutate(affected = round((affected * World_population)/100000)) %>% 
    select(1:4)
  tmp3
}
a <- Affected_function()

Result_f <- function() {
  Result <- full_join(d, a, by = c("Entity", "Year"))
  Result$deaths[is.na(Result$deaths)] <- 0
  Result$affected[is.na(Result$affected)] <- 0
  Result <- Result %>% 
    mutate(Sum_affected = deaths + affected) %>% 
    select(1,3,7)
  Result <- full_join(Result, Populacje_krajow, by = c("Entity" = "brk_name")) %>% 
    mutate(Wskaznik = Sum_affected/pop_est) %>% 
    group_by(Entity) %>% 
    summarise(Indicator = mean(Wskaznik, na.rm = T))
  Result
}

r <- Result_f()
r[283, 1] <- "UK"
r[284, 1] <- "USA"

Data = full_join(world, r, by=c("region" = "Entity")) %>% 
  rename(I = Indicator) %>% 
  mutate(Indicator = case_when(
    is.na(I) ~ "Brak danych",
    I < 0.001 ~ "1. Niedotkliwe (< 0.001) ",
    I >= 0.001 & I < 0.2 ~ "2. Mało dotkliwe (< 0.2)",
    I >= 0.2 & I < 0.5 ~ "3. Dotkliwe  (< 0.5)",
    I >= 0.5 & I < 1 ~ "4. Bardzo dotkliwe  (< 1)",
    I >= 1 ~ "5. Ekstremalnie dotkliwe(> 1)"
  ))

ggplot(data = Data, aes(long, lat, group=group, fill = Indicator)) +
  geom_polygon(color = "#555555") +
  scale_fill_manual(values = c("#fdffb6", "#dcddd4", "#adc6e9", "#6ea8ff", "#5c7fd1", "#f8f8f8")) +
  labs(#title = "Stopień dotkliwości katastrof naturalnych w poszczególnych krajach",
    #subtitle = "W latach 2010 - 2018",
    fill = "Dotkliwość na bazie \n wartości wkaźnika: \n",
    x = NULL,
    y = NULL)+
  scale_x_continuous(guide = NULL) + 
  scale_y_continuous(guide = NULL, limits = c(-55,90)) +
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom",
    title = element_text(size = 18),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 16),
  )

ggsave("mapka2.svg", width = 35, height = 20, units = "cm", bg = "transparent")