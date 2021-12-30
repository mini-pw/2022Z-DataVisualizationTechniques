mies_synop_d <- read.csv("ramki/mies_synop_d.csv")

library(tidyverse)
library(rgdal)
library(rgeos)
library(broom) 
library(maptools)
library(scales)
library(patchwork)


wojewodztwa <- readOGR("mapy/A01_Granice_wojewodztw.shp", "A01_Granice_wojewodztw", encoding = "UTF-8")

wojewodztwa <- spTransform(wojewodztwa, CRS("+init=epsg:4326"))

wojewodztwa_nazwy <- wojewodztwa@data %>% select(JPT_KOD_JE, JPT_NAZWA_)
wojewodztwa_df <- tidy(wojewodztwa, region = "JPT_KOD_JE")
wojewodztwa_df <- left_join(wojewodztwa_df, wojewodztwa_nazwy, by=c("id"="JPT_KOD_JE"))

#srednie sumy w dekadach

suma1970s <- mies_synop_d %>% 
  mutate(Miasto = str_split_fixed(mies_synop_d$Nazwa.Stacji, "-", n= 2)) %>% 
  filter(Rok > 1970 & Rok < 1981) %>% 
  group_by(Miasto = Miasto[,1]) %>% 
  summarise(suma1970s = sum(Suma.opadow)/ 10) %>% 
  filter(Miasto == "WARSZAWA" | 
           Miasto ==  "KRAK?W" | 
           Miasto == "SZCZECIN" |
           Miasto == "KATOWICE" | 
           Miasto == "RZESZ?W" | 
           Miasto == "POZNA?" |
           Miasto == "BIA?YSTOK" |
           Miasto == "OLSZTYN" |
           Miasto == "LUBLIN" |
           Miasto == "WROC?AW" |
           Miasto == "KIELCE" |
           Miasto == "??D?" |
           Miasto == "GDA?SK" |
           Miasto == "OPOLE" | 
           Miasto == "ZIELONA G?RA" |
           Miasto == "TORU?") 


suma2010s <- mies_synop_d %>% 
  mutate(Miasto = str_split_fixed(mies_synop_d$Nazwa.Stacji, "-", n= 2)) %>% 
  filter(Rok > 2010 & Rok < 2021) %>% 
  group_by(Miasto = Miasto[,1]) %>% 
  summarise(suma2010s = sum(Suma.opadow)/ 10) %>% 
  filter(Miasto == "WARSZAWA" | 
           Miasto ==  "KRAK?W" | 
           Miasto == "SZCZECIN" |
           Miasto == "KATOWICE" | 
           Miasto == "RZESZ?W" | 
           Miasto == "POZNA?" |
           Miasto == "BIA?YSTOK" |
           Miasto == "OLSZTYN" |
           Miasto == "LUBLIN" |
           Miasto == "WROC?AW" |
           Miasto == "KIELCE" |
           Miasto == "??D?" |
           Miasto == "GDA?SK" |
           Miasto == "OPOLE" | 
           Miasto == "ZIELONA G?RA" |
           Miasto == "TORU?") 

join <- inner_join(suma1970s, suma2010s, by = "Miasto") %>% 
  select(c("Miasto","s1970" = "suma1970s", "s2010" = "suma2010s")) 



#------------------

df <- data.frame(nazwa = wojewodztwa_nazwy$JPT_NAZWA_, id = 1:16)
  


kolejnosc <- c(5, 3, 7,2,11,15,12,13,8,10,16,6,14,1,4,9)
suma1970s <- cbind(suma1970s, id = c(5, 3, 7,2,11,15,12,13,8,10,16,6,14,1,4,9))
suma1970s <- inner_join(df, suma1970s, by="id")
suma2010s <- cbind(suma2010s, id = c(5, 3, 7,2,11,15,12,13,8,10,16,6,14,1,4,9))
suma2010s <- inner_join(df, suma2010s, by="id")

wojewodztwa_df1970 <-  left_join(wojewodztwa_df, suma1970s, by=c("JPT_NAZWA_"="nazwa"))

  


#roznica

joined <- join %>% 
  mutate(roznica = s2010-s1970)

joined <- cbind(joined, id = kolejnosc)
joined <- inner_join(df, joined, by="id")

wojewodztwa_dfjoin <-  left_join(wojewodztwa_df, joined, by=c("JPT_NAZWA_"="nazwa"))

p3 <- ggplot(wojewodztwa_dfjoin) +
  geom_polygon(aes(long, lat, group=group, fill=roznica), color="#636363") +
  coord_map() +
  theme_void() +
  scale_fill_gradient2(low = muted("blue"),
                       mid = "white",
                       high = muted("red"),
                        midpoint = 0) +
  labs( title = "Różnica",
        fill = "Różnica [mm]") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "right")
  


p3
 
 
