library(PogromcyDanych)
library(dplyr)
library(stringr)
library(tidyr)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

## 1. Z którego rocznika jest najwięcej aut i ile ich jest?

auta2012 %>% group_by(Rok.produkcji) %>% summarise(n = n()) %>% arrange(-n)
  
  
## Odp: najwiecej aut jest z 2011 roku (17418).


## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?
auta2012 %>% filter(Rok.produkcji == 2011) %>% group_by(Marka) %>% summarise(n = n()) %>% arrange(-n)

## Odp: Najwięcej aut z 2011 roku jest marki Skoda.


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?

auta2012 %>% filter(Rok.produkcji > 2004, Rok.produkcji < 2012, Rodzaj.paliwa == "olej napedowy (diesel)") %>% count()

## Odp: Takich aut jest 59534


## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?
auta2012 %>% filter(Rok.produkcji == 2011, Rodzaj.paliwa == "olej napedowy (diesel)") %>% group_by(Marka) %>% summarise(mean_price_PLN = mean(Cena.w.PLN)) %>% arrange(-mean_price_PLN)

## Odp: Średnio najdroższe samochody z 2011 roku z napędem diesla było marki Porsche


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?

auta2012 %>% filter(Rok.produkcji == 2011, Marka == "Skoda") %>% group_by(Model) %>%
  summarise(mean_price_PLN = mean(Cena.w.PLN)) %>% arrange(mean_price_PLN)
## Odp: Skoda Fabia była średnio najtańszym modelem marki Skoda z 2011 roku. 


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?

auta2012 %>% filter(Liczba.drzwi == "2/3") %>% mutate(stosunek = Cena.w.PLN/KM) %>%
  filter(stosunek > 600) %>% group_by(Skrzynia.biegow) %>% summarise(n = n()) %>% arrange(-n)

## Odp: najwiecej tego typu aut ma automatyczną skrzynię biegów.


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?
temp <- auta2012 %>% filter(Marka == "Skoda", Rodzaj.paliwa == c("benzyna", "olej napedowy (diesel)")) %>%
  group_by(Model, Rodzaj.paliwa) %>% summarise(mean_price_PLN = mean(Cena.w.PLN), na.rm = TRUE) %>%
  pivot_wider(names_from = Rodzaj.paliwa, values_from = mean_price_PLN)
temp %>% mutate(difference = abs(.data$`olej napedowy (diesel)` - .data$benzyna), na.rm = TRUE) %>%
  arrange(desc(-difference, na.rm = TRUE))

## Odp: Skoda Felicia ma najmniejszą różnicę w cenie pomiędzy modelami benzynowymi a dieslowymi.


## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini
lamborghini=auta2012 %>% filter(Marka == "Lamborghini")
wyposazenie = sapply(lamborghini$Wyposazenie.dodatkowe, function (x) str_split(x, ","))
wyposazenie = unlist(wyposazenie)  
rarity <- data.frame(wyposazenie) %>% group_by(wyposazenie) %>% summarise(n = n()) %>% arrange(n)

najczesciej <- rarity[rarity$n == max(rarity$n),]
najrzadziej <- rarity[rarity$n == min(rarity$n),]
najczesciej
najrzadziej

## Odp: Najbardziej popularne wyposażenie dodatkowe to alufelgi, wspomaganie kierownicy, ABS, 
##      najmniej popularne to blokada skrzyni biegów, klatka.



## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi

# modele<-unique(Audi$Model)

Audi <- auta2012 %>% filter(Marka == "Audi") %>% 
  mutate(series = ifelse(substr(Model, 2, 2) =='S', substr(Model, 1, 2), substr(Model, 1, 1))) %>% 
  filter(series == "A" | series == "S" | series == "RS") %>% group_by(series) %>% 
  summarise(median = median(KM, na.rm = TRUE), mean = mean(KM, na.rm = TRUE))


##              mediana   średnia
## Odp: A         140     160
##      RS        450     500
##      S         344     344


## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.

Marki<-auta2012 %>% group_by(Marka) %>% summarise(n=n()) %>% arrange(-n) %>% filter(n>10000)
marki <- as.vector(Marki$Marka)

Modele<-auta2012 %>% filter(Marka %in% marki) %>% group_by(Marka, Model) %>%
  summarise(n=n()) %>% ungroup %>% group_by(Marka) %>% filter(n==max(n))
modele <- as.vector(Modele$Model)

auta2012 %>% select(Marka, Model, Kolor) %>% filter(Marka %in% marki, Model %in% modele, Kolor != '') %>% 
  group_by(Marka, Model, Kolor) %>% count() %>% ungroup() %>% group_by(Marka) %>% filter(n==max(n)) 

## Odp: 
# Marka         Model  Kolor
#  Audi          A4     czarny-metallic
#  BMW           320    srebrny-metallic
#  Ford          Focus  srebrny-metallic
#  Mercedes-Benz C 220  srebrny-metallic
#  Opel          Astra  srebrny-metallic
#  Renault       Megane srebrny-metallic
#  Volkswagen    Passat srebrny-metallic
