#Wykres kolumnowy
library(viridis) 
library(ggplot2)
library(stringr)
library(dplyr)
library(tidyr)
library(stringr)
library(plotly)

lpi = read.csv("global-living-planet-index.csv")
lpi %>% 
  select(Entity,Year, Living.Planet.Index) %>% 
  filter(Entity == "World") %>% 
  group_by(Year) %>% 
  summarise(Living.Planet.Index = mean(Living.Planet.Index)) -> lpi




hlpi <- read.csv("populat.csv",
                 encoding = 'UTF-9',
                 check.names = FALSE)
hlpi <- hlpi[,c(1,5:(length(colnames(hlpi))-1))]
colnames(hlpi)[1] <- "country"
hlpi <- pivot_longer(hlpi, 
                     !country,
                     names_to = "Rok", 
                     values_to = "population") 

hlpi$Rok <- as.numeric(str_remove(hlpi$Rok, "Year_"))
hlpi %>% 
  filter(country == "World", Rok >= 1970) -> hlpi

zylonajpierw <- filter(hlpi, Rok == 1970)$population

hlpi -> HLPI

hlpi %>% 
  select(country, Rok, population) %>% 
  group_by(Rok) %>% 
  summarise(Living.Planet.Index = population*100/zylonajpierw) -> hlpi

colnames(hlpi)[1] <- "Year"

names(lpi)[2] <- "LPIanimals"
names(hlpi)[2] <- "LPIhumans"
hlpi <- cbind(lpi[1:(length(lpi$Year)-1),], select(hlpi,!Year))

filter(hlpi, Year %% 10 == 0) -> result

tekst <- c(1:20)
tekst[c(TRUE, FALSE)] <- result$LPIanimals
tekst[c(FALSE, TRUE)] <- result$LPIhumans

fig <- plot_ly(result) 
fig <- fig %>% add_trace(x =~Year,  
                         y = ~LPIanimals,
                         type = 'bar', 
                         name = 'LPI', 
                         marker = list(color = "#006D2C"), 
                         text = round(result$LPIanimals,2), textposition = 'outside')
fig <- fig %>% add_trace(x =~Year,
                         y = result$LPIhumans, 
                         name = 'HPI', 
                         marker = list(color = "#74C476"), 
                         text = round(result$LPIhumans,1), 
                         textposition = 'outside',type = 'bar')
fig <- fig %>% layout(title = 'Living Planet Index i Human Planet Index',
                      xaxis = list(
                        title = "Rok",
                        tickfont = list(
                          size = 14,
                          color = 'rgb(107, 107, 107)')),
                      yaxis = list(
                        title = 'Indeks',
                        titlefont = list(
                          size = 16,
                          color = 'rgb(107, 107, 107)'),
                        tickfont = list(
                          size = 14,
                          color = 'rgb(107, 107, 107)')),
                      barmode = 'group', bargap = 0.15, bargroupgap = 0.1) %>% 
  layout(yaxis = list(ticksuffix = "%"), legend = list(font = list(size = 30)))
fig