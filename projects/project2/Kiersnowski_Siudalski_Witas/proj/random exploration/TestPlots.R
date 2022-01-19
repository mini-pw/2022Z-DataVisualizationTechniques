library(dplyr)
library(ggplot2)
library(lubridate)
library(maps)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(maptools)
library(ggmap)
df <-data.frame(read.csv('../data/DataCSV/Pointstest.csv'))
Polish_map<-map_data("world")#),xlim = c(49.29899,54.79086), ylim = c(14.25,23.9))

df["Date"]=as.POSIXct((df$TimeStampInMS)/1000, origin="1970-01-01", tz="Europe/Warsaw")
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
Warsaw <- get_stamenmap(bbox = c(left = 20.913532, bottom = 52.133049, 
                                  right = 21.220532, top = 52.340049), 
                         zoom = 11)
ggplot()+
  geom_sf(data = world)+
  geom_point(data = df, aes(x=Longitude/10^7, y=Latitude/10^7, color = TimeStampInMS))+
  ylim(c(52.133049,52.340049))+
  xlim(c(20.913532,21.220532))

ggmap(Warsaw)+
  geom_point(data = df, aes(x=Longitude/10^7, y=Latitude/10^7, color = TimeStampInMS))
  

