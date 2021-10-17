library(PogromcyDanych)
library(dplyr)
library(stringr)
library(tidyr)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

## Aby policzyć ceny w zadaniach 4-7, utwórzmy nową kolumnę, która ma cenę brutto w PLN dla każdego rekordu

auta2012 <- auta2012 %>% mutate(Cena.brutto = ifelse(Brutto.netto == "netto", Cena.w.PLN * 1.23, Cena.w.PLN))

## 1. Z którego rocznika jest najwięcej aut i ile ich jest?

auta2012 %>% group_by(Rok.produkcji) %>% summarise(number = n()) %>% arrange(-number) %>% head(1)

## Odp: Z roku 2011, jest ich 17418.


## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?

auta2012 %>% filter(Rok.produkcji == 2011) %>% group_by(Marka) %>% 
  summarise(number = n()) %>% arrange(-number) %>% head(1)

## Odp: Skoda.


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?

auta2012 %>% filter(Rodzaj.paliwa == "olej napedowy (diesel)", Rok.produkcji %in% 2005:2011) %>%
  summarise(number = n())


## Odp: Jest ich 59534.


## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?

auta2012 %>% filter(Rodzaj.paliwa == "olej napedowy (diesel)", Rok.produkcji == 2011) %>%
  group_by(Marka) %>% summarise(srednia.cena = mean(Cena.brutto, na.rm = T)) %>% arrange(-srednia.cena) %>%
  head(1)
  
## Odp: Porsche.


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?

auta2012 %>% filter(Marka == "Skoda", Rok.produkcji == 2011) %>%
  group_by(Model) %>% summarise(srednia.cena = mean(Cena.brutto, na.rm = T)) %>% 
  arrange(srednia.cena) %>% head(1)

## Odp: Fabia.


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?

auta2012 %>% filter(Liczba.drzwi == "2/3" ) %>% 
  na.omit(Cena.w.PLN, KM) %>%
  mutate(stosunek.ceny.do.KM = Cena.brutto / KM ) %>%
  filter(stosunek.ceny.do.KM > 600) %>% group_by(Skrzynia.biegow) %>% summarise(n = n()) %>%
  arrange(-n) %>% head(1)

## Odp: Automatyczna.


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?

auta_new <- auta2012 %>% filter(Marka == "Skoda") %>% 
  group_by(Model, Rodzaj.paliwa) %>%
  summarise(srednia_cena = mean(Cena.brutto)) %>% 
  pivot_wider(names_from = Rodzaj.paliwa, values_from = srednia_cena, values_fill = 0)
  colnames(auta_new) <- c("Model", "benzyna", "diesel", "benzyna+LPG", "etanol")
  auta_new %>% mutate(Roznica = abs(benzyna - diesel)) %>% arrange(Roznica) %>% head(1)

## Odp: Felicia.


## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini

auta2012_new <- filter(auta2012, Marka == "Lamborghini")
wyposazenie <- str_split(auta2012_new$Wyposazenie.dodatkowe, ", ")
wyposazenie %>% unlist() %>% table() %>% sort()
## Odp: Najczesciej - ABS, alufelgi, wspomaganie kierownicy - 18
##      Najrzadziej - blokada skrzyni biegów, klatka - 1


## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi

auta2012 %>% filter(Marka == "Audi") %>% 
  mutate(Seria = ifelse(substr(Model, 2, 2) == "S", substr(Model, 1, 2), substr(Model, 1, 1))) %>%
  group_by(Seria) %>% filter(Seria == "A" | Seria == "S" | Seria == "RS") %>%
  summarise(Srednia.moc = mean(KM, na.rm = T), Mediana.mocy = median(KM, na.rm = T))

## Odp: Modele A mają średnią moc 160 KM i medianę 140 KM, mocniejsze silniki od nich mają modele
## S - średnia moc i mediana mocy 344 KM. Najmocniejsze silniki mają modele RS - średnia moc 500 KM, a mediana 450 KM.


## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.

auta2012 %>% select(Marka, Model, Kolor) %>%group_by(Marka) %>% mutate(liczba.aut = n()) %>% filter(liczba.aut > 10000) %>% group_by(Marka, Model) %>%
  mutate(liczba.modeli = n()) %>% group_by(Marka, Model, Kolor) %>% mutate(liczba.kolor = n()) %>% 
  unique() %>% arrange(-liczba.kolor) %>% group_by(Marka) %>% filter(liczba.modeli == max(liczba.modeli)) %>%
  filter(liczba.kolor == max(liczba.kolor)) %>% arrange(Marka)

## Odp: 
# Audi            A4     czarny-metallic
# 2 BMW           320    srebrny-metallic
# 3 Ford          Focus  srebrny-metallic
# 4 Mercedes-Benz C 220  srebrny-metallic
# 5 Opel          Astra  srebrny-metallic
# 6 Renault       Megane srebrny-metallic
# 7 Volkswagen    Passat srebrny-metallic