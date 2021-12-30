library(PogromcyDanych)
library(dplyr)
library(stringr)

dim(auta2012)
colnames(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

## 1. Z którego rocznika jest najwięcej aut i ile ich jest?
auta2012 %>% 
  group_by(Rok.produkcji) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  head(1)

## Odp: 2011


## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?
auta2012 %>% 
  filter(Rok.produkcji == 2011) %>% 
  group_by(Marka) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  head(1)

## Odp: Skoda


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?
auta2012 %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)", between(Rok.produkcji, 2005, 2011)) %>% 
  summarise(n = n())
  
## Odp: 59534


## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?
auta2012 %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)", Rok.produkcji == 2011) %>% 
  group_by(Marka) %>% 
  summarise(mean_prize = mean(Cena.w.PLN, na.rm=T)) %>% 
  arrange(-mean_prize) %>% 
  head(1)

## Odp: Porsche          


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?
auta2012 %>% 
  filter(Marka == "Skoda", Rok.produkcji == 2011) %>% 
  group_by(Model) %>% 
  summarise(Srednia.cena = mean(Cena.w.PLN, na.rm=T)) %>% 
  arrange(Srednia.cena) %>% 
  head(1)

## Odp: Fabia


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?
auta2012 %>% 
  mutate(PLN.do.KM = Cena.w.PLN / KM) %>% 
  filter(Liczba.drzwi == "2/3", PLN.do.KM > 600) %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  head(1)

## Odp: automatyczna


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?
auta2012 %>% 
  filter(Marka == "Skoda", Rodzaj.paliwa == "benzyna") %>% 
  group_by(Model) %>% 
  summarise(Srednia.cena = mean(Cena.w.PLN, na.rm=TRUE)) -> benzyna

auta2012 %>% 
  filter(Marka == "Skoda", Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Model) %>% 
  summarise(Srednia.cena = mean(Cena.w.PLN, na.rm=TRUE)) -> diesel

inner_join(benzyna, diesel, by = "Model", suffix = c(".benzyna", ".diesel")) %>% 
  mutate(Roznica = abs(Srednia.cena.benzyna - Srednia.cena.diesel)) %>% 
  arrange(Roznica) %>% 
  head(1)

## Odp: Felicia


## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini
auta2012 %>% 
  filter(Marka == "Lamborghini") %>% 
  pull(Wyposazenie.dodatkowe) -> wyposazenie

wyposazenie <- unlist(sapply(wyposazenie, str_split, pattern = ", "))
wyposazenie <- table(wyposazenie)
wyposazenie[which(wyposazenie == max(wyposazenie))]
wyposazenie[which(wyposazenie == min(wyposazenie))]

## Odp: najrzadziej: blokada skrzyni biegAlw, klatka
##      najczęściej: ABS, alufelgi, wspomaganie kierownicy


## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi
auta2012 %>% 
  filter(Marka == "Audi") %>% 
  mutate(Grupa = case_when(
    str_starts(Model, "A") ~ "A",
    str_starts(Model, "S") ~ "S",
    str_starts(Model, "RS") ~ "RS",
    TRUE ~ "Inny"
  )) %>% 
  filter(Grupa != "Inny") %>% 
  group_by(Grupa) %>% 
  summarise(Srednia.KM = mean(KM, na.rm=T), Mediana.KM = median(KM, na.rm=T)) %>% 
  arrange(Mediana.KM)

## Odp: średnia: A < S < RS
##      mediana: A < S < RS


## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.
auta2012 %>% 
  group_by(Marka) %>% 
  tally() %>% 
  filter(n > 10000) %>% 
  pull(Marka) -> marki

auta2012 %>% 
  filter(Marka %in% marki) %>% 
  group_by(Marka, Model) %>% 
  mutate(n = n()) %>% 
  ungroup(Model) %>% 
  filter(n == max(n)) %>% 
  group_by(Kolor, .add = TRUE) %>% 
  mutate(n.kolor = n()) %>% 
  ungroup(Kolor) %>% 
  select(Marka, Model, Kolor, n.kolor)%>% 
  filter(n.kolor == max(n.kolor)) %>% 
  slice(1)

## Odp: Audi          A4     czarny-metallic    
##      BMW           320    srebrny-metallic   
##      Ford          Focus  srebrny-metallic    
##      Mercedes-Benz C 220  srebrny-metallic   
##      Opel          Astra  srebrny-metallic    
##      Renault       Megane srebrny-metallic   
##      Volkswagen    Passat srebrny-metallic    