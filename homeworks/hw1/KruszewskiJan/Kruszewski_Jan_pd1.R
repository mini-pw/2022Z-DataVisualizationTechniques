library(PogromcyDanych)
library(dplyr)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

## 1. Z którego rocznika jest najwięcej aut i ile ich jest?
auta2012 %>% 
  group_by(Rok.produkcji) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

## Odp: 2011, jest ich 17418


## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?
auta2012 %>% 
  filter(Rok.produkcji == 2011) %>% 
  group_by(Marka) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

## Odp: Skoda


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?
auta2012 %>% 
  filter(Rok.produkcji >= 2005 & Rok.produkcji <= 2011 & Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  summarise(n = n())

## Odp: 59534


## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?
auta2012 %>% 
  filter(Rok.produkcji == 2011 & Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Marka) %>% 
  summarise(srednia_cena = mean(Cena.w.PLN)) %>% 
  arrange(-srednia_cena)
 
## Odp: Porsche


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?
auta2012 %>% 
  filter(Marka == "Skoda" & Rok.produkcji == 2011) %>% 
  group_by(Model) %>% 
  summarise(srednia_cena = mean(Cena.w.PLN)) %>% 
  arrange(srednia_cena)

## Odp: Fabia


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?
auta2012 %>%
  mutate(stosunek = Cena.w.PLN / KM) %>% 
  filter(Liczba.drzwi == "2/3" & stosunek > 600) %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

## Odp: automatyczna


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?
auta2012 %>% 
  filter(Marka == "Skoda") %>% 
  group_by(Model, Rodzaj.paliwa) %>% 
  summarise(srednia = mean(Cena.w.PLN)) %>% 
  filter(Rodzaj.paliwa == "benzyna" | Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  mutate(diff = abs(srednia - lag(srednia, default = first(srednia)))) %>% 
  filter(diff != 0) %>% 
  arrange(diff)
  

## Odp: Felicia


## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini
wyp <- auta2012 %>% 
  filter(Marka == "Lamborghini", Wyposazenie.dodatkowe != "") %>% 
  select(Wyposazenie.dodatkowe)
sort(table(unlist(str_split(unlist(wyp), ", "))))

## Odp: Najczęściej ABS, alufelgi i wspomaganie kierownicy.
# Najrzadziej blokada skrzyni biegów i klatka.


## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi
  auta2012 %>%
    filter(Marka == "Audi") %>% 
    mutate(jaki_model = ifelse(str_detect(Model, "RS"), "RS", ifelse(str_detect(Model, "A"), "A", ifelse(str_detect(Model, "S"), "S", "inny")))) %>% 
    group_by(jaki_model) %>% 
    summarise(mediana = median(KM, na.rm = TRUE), średnia = mean(KM, na.rm = TRUE)) %>% 
    filter(jaki_model != "inny")
    
## Odp:
#  jaki_model mediana średnia        
#   A           140    160.
#   RS          450    493.
#   S           340    340.

## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.
auta2012 %>% 
  group_by(Marka) %>% 
  filter(n() > 10000) %>% 
  group_by(Marka, Model) %>% 
  summarise(n = n(), kolor = names(which.max(table(Kolor)))) %>% 
  filter(n == max(n)) %>% 
  select(-n)

## Odp:Audi A4 czarny-metallic 
#      BMW  320 srebrny-metallic
#      Ford Focus srebrny-metallic
#      Mercedes-Benz C 220 srebrny-metallic
#      Opel Astra srebrny-metallic
#      Renault Megane srebrny-metallic
#      Volkswagen Passat srebrny-metallic