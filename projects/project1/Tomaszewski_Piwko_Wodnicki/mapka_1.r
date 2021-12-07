drought_a <- read.csv("Data/drought_affected_annual_number.csv")
drought_d <- read.csv("Data/drought_deaths_annual_number.csv")
earthquake_a <- read.csv("Data/earthquake_affected_annual_number.csv")
earthquake_d <- read.csv("Data/earthquake_deaths_annual_number.csv")
epidemic_a <- read.csv("Data/epidemic_affected_annual_number.csv")
epidemic_d <- read.csv("Data/epidemic_deaths_annual_number.csv")
flood_a <- read.csv("Data/flood_affected_annual_number.csv")
flood_d <- read.csv("Data/flood_deaths_annual_number.csv")
temperature_a <- read.csv("Data/extreme_temperature_affected_annual_number.csv")
temperature_d <- read.csv("Data/extreme_temperature_deaths_annual_number.csv")
storm_a <- read.csv("Data/storm_affected_annual_number.csv")
storm_d <- read.csv("Data/storm_deaths_annual_number.csv")

library(dplyr)
library(stringr)

drought_a <- drought_a %>% setNames(c("Country",1970:2008))
drought_d <- drought_d %>% setNames(c("Country",1970:2008))
earthquake_a <- earthquake_a %>% setNames(c("Country",1970:2008))
earthquake_d <- earthquake_d %>% setNames(c("Country",1970:2008))
epidemic_a<- epidemic_a %>% setNames(c("Country",1970:2008))
epidemic_a[is.na(epidemic_a)] <- 0
epidemic_d<- epidemic_d %>% setNames(c("Country",1970:2008))
epidemic_d[is.na(epidemic_d)] <- 0
flood_a<- flood_a %>% setNames(c("Country",1970:2008))
flood_d<- flood_d %>% setNames(c("Country",1970:2008))
temperature_a<- temperature_a %>% setNames(c("Country",1971:2008))
temperature_a[is.na(temperature_a)] <- 0
temperature_d<- temperature_d %>% setNames(c("Country",1971:2008))
temperature_d[is.na(temperature_d)] <- 0
storm_a<- storm_a %>% setNames(c("Country",1970:2008))
storm_d<- storm_d %>% setNames(c("Country",1970:2008))

options(scipen=999)


for(i in 1970:2008){
  x <- str_ends(drought_a[,as.character(i)], "k")
  y <- drought_a[x,as.character(i)]
  y <- gsub('.{1}$', '', y)
  y <- as.numeric(y)*1000
  drought_a[x,as.character(i)] <-y
  
  x <- str_ends(drought_a[,as.character(i)], "M")
  y <- drought_a[x,as.character(i)]
  y <- gsub('.{1}$', '', y)
  y <- as.numeric(y)*1000000
  drought_a[x,as.character(i)] <-y
  
  x <- str_ends(drought_d[,as.character(i)], "k")
  y <- drought_d[x,as.character(i)]
  y <- gsub('.{1}$', '', y)
  y <- as.numeric(y)*1000
  drought_d[x,as.character(i)] <-y
  
  x <- str_ends(drought_d[,as.character(i)], "M")
  y <- drought_d[x,as.character(i)]
  y <- gsub('.{1}$', '', y)
  y <- as.numeric(y)*1000000
  drought_d[x,as.character(i)] <-y
  
  drought_a[, as.character(i)] <- as.numeric(drought_a[, as.character(i)])
}
for(i in 1970:2008){
  x <- str_ends(earthquake_a[,as.character(i)], "k")
  y <- earthquake_a[x,as.character(i)]
  y <- gsub('.{1}$', '', y)
  y <- as.numeric(y)*1000
  earthquake_a[x,as.character(i)] <-y
  
  x <- str_ends(earthquake_a[,as.character(i)], "M")
  y <- earthquake_a[x,as.character(i)]
  y <- gsub('.{1}$', '', y)
  y <- as.numeric(y)*1000000
  earthquake_a[x,as.character(i)] <-y
  
  x <- str_ends(earthquake_d[,as.character(i)], "k")
  y <- earthquake_d[x,as.character(i)]
  y <- gsub('.{1}$', '', y)
  y <- as.numeric(y)*1000
  earthquake_d[x,as.character(i)] <-y
  
  x <- str_ends(earthquake_d[,as.character(i)], "M")
  y <- earthquake_d[x,as.character(i)]
  y <- gsub('.{1}$', '', y)
  y <- as.numeric(y)*1000000
  earthquake_d[x,as.character(i)] <-y
  
  earthquake_a[, as.character(i)] <- as.numeric(earthquake_a[, as.character(i)])
}
for(i in 1970:2008){
  x <- str_ends(epidemic_a[,as.character(i)], "k")
  y <- epidemic_a[x,as.character(i)]
  y <- gsub('.{1}$', '', y)
  y <- as.numeric(y)*1000
  epidemic_a[x,as.character(i)] <-y
  
  x <- str_ends(epidemic_a[,as.character(i)], "M")
  y <- epidemic_a[x,as.character(i)]
  y <- gsub('.{1}$', '', y)
  y <- as.numeric(y)*1000000
  epidemic_a[x,as.character(i)] <-y
  
  x <- str_ends(epidemic_d[,as.character(i)], "k")
  y <- epidemic_d[x,as.character(i)]
  y <- gsub('.{1}$', '', y)
  y <- as.numeric(y)*1000
  epidemic_d[x,as.character(i)] <-y
  
  x <- str_ends(epidemic_d[,as.character(i)], "M")
  y <- epidemic_d[x,as.character(i)]
  y <- gsub('.{1}$', '', y)
  y <- as.numeric(y)*1000000
  epidemic_d[x,as.character(i)] <-y
  
  epidemic_a[, as.character(i)] <- as.numeric(epidemic_a[, as.character(i)])
}
for(i in 1970:2008){
  x <- str_ends(flood_a[,as.character(i)], "k")
  y <- flood_a[x,as.character(i)]
  y <- gsub('.{1}$', '', y)
  y <- as.numeric(y)*1000
  flood_a[x,as.character(i)] <-y
  
  x <- str_ends(flood_a[,as.character(i)], "M")
  y <- flood_a[x,as.character(i)]
  y <- gsub('.{1}$', '', y)
  y <- as.numeric(y)*1000000
  flood_a[x,as.character(i)] <-y
  
  x <- str_ends(flood_d[,as.character(i)], "k")
  y <- flood_d[x,as.character(i)]
  y <- gsub('.{1}$', '', y)
  y <- as.numeric(y)*1000
  flood_d[x,as.character(i)] <-y
  
  x <- str_ends(flood_d[,as.character(i)], "M")
  y <- flood_d[x,as.character(i)]
  y <- gsub('.{1}$', '', y)
  y <- as.numeric(y)*1000000
  flood_d[x,as.character(i)] <-y
  
  flood_a[, as.character(i)] <- as.numeric(flood_a[, as.character(i)])
}

for(i in 1971:2008){
  x <- str_ends(temperature_a[,as.character(i)], "k")
  y <- temperature_a[x,as.character(i)]
  y <- gsub('.{1}$', '', y)
  y <- as.numeric(y)*1000
  temperature_a[x,as.character(i)] <-y
  
  x <- str_ends(temperature_a[,as.character(i)], "M")
  y <- temperature_a[x,as.character(i)]
  y <- gsub('.{1}$', '', y)
  y <- as.numeric(y)*1000000
  temperature_a[x,as.character(i)] <-y
  
  x <- str_ends(temperature_d[,as.character(i)], "k")
  y <- temperature_d[x,as.character(i)]
  y <- gsub('.{1}$', '', y)
  y <- as.numeric(y)*1000
  temperature_d[x,as.character(i)] <-y
  
  x <- str_ends(temperature_d[,as.character(i)], "M")
  y <- temperature_d[x,as.character(i)]
  y <- gsub('.{1}$', '', y)
  y <- as.numeric(y)*1000000
  temperature_d[x,as.character(i)] <-y
  
  temperature_a[, as.character(i)] <- as.numeric(temperature_a[, as.character(i)])
}

for(i in 1970:2008){
  x <- str_ends(storm_a[,as.character(i)], "k")
  y <- storm_a[x,as.character(i)]
  y <- gsub('.{1}$', '', y)
  y <- as.numeric(y)*1000
  storm_a[x,as.character(i)] <-y
  
  x <- str_ends(storm_a[,as.character(i)], "M")
  y <- storm_a[x,as.character(i)]
  y <- gsub('.{1}$', '', y)
  y <- as.numeric(y)*1000000
  storm_a[x,as.character(i)] <-y
  
  x <- str_ends(storm_d[,as.character(i)], "k")
  y <- storm_d[x,as.character(i)]
  y <- gsub('.{1}$', '', y)
  y <- as.numeric(y)*1000
  storm_d[x,as.character(i)] <-y
  
  x <- str_ends(storm_d[,as.character(i)], "M")
  y <- storm_d[x,as.character(i)]
  y <- gsub('.{1}$', '', y)
  y <- as.numeric(y)*1000000
  storm_d[x,as.character(i)] <-y
  
  storm_a[, as.character(i)] <- as.numeric(storm_a[, as.character(i)])
}

drought_a <- drought_a %>% mutate(Sum = rowSums(.[,c(-1)])) %>% select(Country,Sum)
earthquake_a <- earthquake_a %>% mutate(Sum = rowSums(.[,c(-1)])) %>% select(Country,Sum)
epidemic_a <- epidemic_a %>% mutate(Sum = rowSums(.[,c(-1)])) %>% select(Country,Sum)
flood_a <- flood_a %>% mutate(Sum = rowSums(.[,c(-1)])) %>% select(Country,Sum)
storm_a <- storm_a %>% mutate(Sum = rowSums(.[,c(-1)])) %>% select(Country,Sum)
temperature_a <- temperature_a %>% mutate(Sum = rowSums(.[,c(-1)])) %>% select(Country,Sum)

df = drought_a %>% 
  full_join(earthquake_a, by="Country") %>% 
  full_join(epidemic_a, by="Country") %>% 
  full_join(flood_a, by="Country") %>% 
  full_join(storm_a, by="Country") %>% 
  full_join(temperature_a, by="Country")

df <- df %>% arrange(Country)
df[is.na(df)] <- 0
colnames(df)<- c("Country","Drought", "Earthquake", "Epidemic", "Flood", "Storm", "Temperature")

df[, "max"] <- apply(df[,c(-1)],1,max)

df <- df %>% mutate(Katastrofa = case_when(df$max == df$Drought ~ "Susza",
                                           df$max == df$Earthquake ~ "Trzêsienie ziemi",
                                           df$max == df$Epidemic ~ "Epidemia",
                                           df$max == df$Flood ~ "PowódŸ",
                                           df$max == df$Storm ~ "Burze i huragany",
                                           df$max == df$Temperature ~ "Ekstremalna temperatura")) %>% 
  select(Country,Katastrofa)


library(sf)
library("rnaturalearth")
library("rnaturalearthdata")
library(rgeos)
library(ggplot2)

#world <- ne_countries(scale = "medium", returnclass = "sf")
#world <- world[c(-12),]
#result <- full_join(world, df, by = c("brk_name" = "Country"))
#result$Katastrofa[is.na(result$Katastrofa)] <- "Brak danych"
#result$Katastrofa <- factor(result$Katastrofa, levels = 
#                              c("Epidemia","Burze i huragany","PowódŸ",
#                                "Susza","Trzêsienie ziemi",
#                                "Wysoka temperatura", "Brak danych"))

#ggplot(data = result) +
#  geom_sf(aes(fill = Katastrofa)) +
#  scale_fill_manual(values = c("#FFBF00","#71FFB8","#71BCFF","#FFFC92","#674A00","#FF4E4E","#A2A2A2")) +
#  labs(title = "NajgroŸniejsza katastrofa naturalna w danym kraju",
#       fill = "Typ katastrofy", subtitle = "Dane z lat 1971-2008") +
#  theme(panel.background = element_blank(),
#        plot.background = element_blank(),
#        legend.position = 'bottom',
#        axis.text.x = element_blank(),
#        axis.ticks.x = element_blank())

require(maps)

df$Country[df$Country == "United States"] <- "USA"
df$Country[df$Country == "United Kingdom"] <- "UK"

mapdata <- map_data("world")
result <- left_join(mapdata,df,by=c("region" = "Country"))
result$Katastrofa[is.na(result$Katastrofa)] <- "Brak danych"
result <- result %>%filter(region != "Antarctica")
result$Katastrofa <- factor(result$Katastrofa, levels = 
                              c("Epidemia","Burze i huragany","PowódŸ",
                                "Susza","Trzêsienie ziemi",
                                "Ekstremalna temperatura", "Brak danych"))


ggplot(result, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Katastrofa), color ="#555555") +
  scale_fill_manual(values = c("#55d6c2","#ffc6ff","#a0c4ff","#fdffb6","#dab894","#f08080","#555555")) +
  labs(title = "NajgroŸniejsza katastrofa naturalna w danym kraju",
       fill = "Typ katastrofy", subtitle = "Dane z lat 1971-2008") +
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
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    )

#ggsave("Najgrozniejsza_katastrofa_mapa.svg", bg = "transparent", 
#       width = 35, height = 20, units = c("cm"))

                    