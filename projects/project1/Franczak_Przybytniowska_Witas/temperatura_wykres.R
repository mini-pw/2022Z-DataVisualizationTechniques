library(dplyr)
library(ggplot2)
library(data.table)

mies_synop_d <- read.csv("ramki/mies_synop_d.csv") 

mies_dane_temp <- mies_synop_d %>% 
  select(c(Miasto = "Nazwa.Stacji","Rok", "Miesiac", "Srednia.temp")) %>%
  filter(Miasto == "WARSZAWA-OKĘCIE" |
           Miasto ==  "KRAKÓW-BALICE" | 
           Miasto == "SZCZECIN" |
           Miasto == "KATOWICE-MUCHOWIEC" | 
           Miasto == "RZESZÓW-JASIONKA" | 
           Miasto == "POZNAŃ-ŁAWICA" |
           Miasto == "BIAŁYSTOK" |
           Miasto == "OLSZTYN" |
           Miasto == "LUBLIN-RADAWIEC" |
           Miasto == "WROCŁAW-STRACHOWICE" |
           Miasto == "KIELCE-SUKÓW" |
           Miasto == "ŁÓDŹ-LUBLINEK" |
           Miasto == "GDAŃSK-PORT PÓŁNOCNY" |
           Miasto == "GDAŃSK-RĘBIECHOWO" |
           Miasto == "GDAŃSK-ŚWIBNO" |
           Miasto == "OPOLE" | 
           Miasto == "ZIELONA GÓRA" |
           Miasto == "TORUŃ") %>% 
  group_by(Rok, Miasto) %>% 
  summarise(Srednia = mean(Srednia.temp))

rok_2020 <- mies_dane_temp %>% 
  filter(Rok == "2020")

rok_1991 <- mies_dane_temp %>% 
  filter(Rok == "1991")

colnames(rok_2020)[3] <- "Srednia_2020"

colnames(rok_1991)[3] <- "Srednia_1991"

join <- inner_join(rok_1991, rok_2020, by = "Miasto") %>% 
  select(Miasto, Srednia_1991,Srednia_2020)
join[1,1] <- "PODLASKIE"
join[2,1] <- "POMORSKIE"
join[3,1] <- "ŚLĄSKIE"
join[4,1] <- "ŚWIĘTOKRZYSKIE"
join[5,1] <- "MAŁOPOLSKIE"
join[6,1] <- "LUBELSKIE"
join[7,1] <- "ŁÓDZKIE"
join[8,1] <- "WARMIŃSKO-MAZURSKIE"
join[9,1] <- "OPOLSKIE"
join[10,1] <- "WIELKOPOLSKIE"
join[11,1] <- "PODKARPACKIE"
join[12,1] <- "ZACHODNIO-POMORSKIE"
join[13,1] <- "KUJAWSKO-POMORSKIE"
join[14,1] <- "MAZOWIECKIE"
join[15,1] <- 'DOLNOŚLĄSKIE'
join[16,1] <- "LUBUSKIE"
join$legends <- rep("legend", 16)
roznica <- join %>% 
  group_by(Miasto) %>% 
  mutate(Roznica = sum(Srednia_1991, -Srednia_2020))

my_plot <- join %>% 
  ggplot() +
  geom_segment( aes(x=reorder(Miasto, Srednia_2020), xend=Miasto, y=Srednia_1991 , yend=Srednia_2020), color="lightgrey") +
  geom_point( aes(x=Miasto, y=Srednia_1991), color=rgb(0,0.75,1,0.6), size=3) +
  geom_point( aes(x=Miasto, y=Srednia_2020), color=rgb(1,0.647,0,0.6, names = "2020"), size=3.4 ) +
  coord_flip() +
  labs(x = "Województwo",
       y = "Temperatura")+ 
  theme_minimal() + scale_y_continuous(expand = c(0,0), limits = c(0,12))

my_plot