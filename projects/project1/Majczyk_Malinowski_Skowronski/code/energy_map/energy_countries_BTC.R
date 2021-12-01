library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
library(stringr)
library(gghighlight)

powercsv <- read.csv("powerconsumption.csv", sep=',', header = TRUE, stringsAsFactors=FALSE)


#tutaj mam roczne zuzycie energii na BTC
btc2 <- powercsv %>% 
  group_by(Year) %>%
  summarise(BTC = sum(Monthly.consumption..TWh)) %>% 
  filter(Year>2018) %>% 
  select(BTC) 

btc2 <- as.vector(unlist(t(btc2)))
btc2[3] <-  btc2[3]*(12/9)
btc2 <- btc2*0.0036



#tu ogarnianie tego BP
BP <- read.csv("bp-stats-review-2021-all-dataANSI.csv", header = TRUE)


BP <- 
  BP %>% 
  mutate(X1965 = as.numeric(X1965),
         X1966 = as.numeric(X1966),
         X1967 = as.numeric(X1967),
         X1968 = as.numeric(X1968),
         X1969 = as.numeric(X1969),
         X1970 = as.numeric(X1970),
         X1971 = as.numeric(X1971),
         X1972 = as.numeric(X1972),
         X1973 = as.numeric(X1973),
         X1974 = as.numeric(X1974),
         X1975 = as.numeric(X1975),
         X1976 = as.numeric(X1976),
         X1977 = as.numeric(X1977),
         X1978 = as.numeric(X1978),
         X1979 = as.numeric(X1979),
         X1980 = as.numeric(X1980),
         X1981 = as.numeric(X1981),
         X1982 = as.numeric(X1982),
         X1983 = as.numeric(X1983),
         X1984 = as.numeric(X1984),
         X1985 = as.numeric(X1985),
         X1986 = as.numeric(X1986),
         X1987 = as.numeric(X1987),
         X1988 = as.numeric(X1988),
         X1989 = as.numeric(X1989)) %>% 
  pivot_longer(!Year, names_to = "consum", values_to = "energy") %>% 
  mutate(consum = str_sub(consum, 2,5)) %>% 
  mutate(consum = as.numeric(consum)) %>% 
  rename(Country = Year, Year = consum) %>% 
  filter(Year>=2010) %>% 
  pivot_wider(names_from = Year, values_from = energy)


final2 <- BP %>% 
  select(Country,`2020`)

names2 <- final2 %>% select(Country) 
names2[92:94,] <- c("BTC 2019","BTC 2020","BTC 2021\n(przewidywany)")
names2[3,1] <- "USA"
names2[47,1] <-  "UK"
names2[52,1] <-  "Russia"

values2 <- final2 %>% select(Rok_2020 = `2020`)
values2 <- values2[-92,]
values2 <-rbind(values2, data.frame(Rok_2020 = btc2))

final2 <- cbind(names2,values2)



# ogarnianie do ggplota
final2 <- final2 %>%
  mutate(energy = as.numeric(Rok_2020)) %>% 
  mutate(energy = energy/0.0036) %>% 
  arrange(desc(energy)) 
energy_df <- final2


final2$Country <- factor(final2$Country, levels = final2$Country)

final3 <- final2[80:91,]

final3 %>%
  ggplot(aes(x = Country, y = energy)) +
  geom_col()+
  gghighlight(Country == "BTC 2019" | Country == "BTC 2020" | Country == "BTC 2021\n(przewidywany)")+
  labs(title = "Porównanie energii uzytej przez panstwa i do bitcoina", subtitle = "Rok 2020", x = "Panstwa", y = "Energia w TWh")






###############################################################
# Tutaj sie ogarania z mapa, pozdrawiam
###############################################################

country <- map_data("world")

energy_df <- energy_df[-c(81,90),]
energy_df[85,1] <-  "BTC" #pojebany jestem?, czemu w df btc jest na 86 a wybieram 85 myslalem ze jest od 1 indexowane, a nie od 0




my_breaks = round(exp(seq(log(27), log(40405), length=6)), -1)#to do legendy
#ogolnie to zalezy nam na pokazaniu tych malych wielkosci najlepiej bo btc jest jakis 10 od dolu z 90 

countries <-left_join(country,energy_df, by = c("region" = "Country")) 
mapa <- countries%>% 
  ggplot(aes(long,lat)) +
  geom_polygon(aes(group = group,fill = energy),size=0, alpha=0.9) +
  theme_void() +
  #scale_fill_gradient(trans = "sqrt",name = "TWh", low = "#FEF4E8",high = "#F7931A") #tu mozna testowac skale filla
  scale_fill_gradient(name = "TWh", trans = "log",breaks = my_breaks, labels = my_breaks, low = "#FEF4E8",high = "#F7931A")
mapa

