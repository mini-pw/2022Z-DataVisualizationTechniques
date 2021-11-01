library(PogromcyDanych)
library(dplyr)
library(stringi)
library(reshape2)
library(stringr)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))


## 1. Z którego rocznika jest najwięcej aut i ile ich jest?
tail(names(sort(table(auta2012$Rok.produkcji))), 1)
auta2012 %>%
  filter(Rok.produkcji == "2011") %>%
  count()

## Odp: 
# Z 2011 roku. Jest ich 17418


## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?
auta2012 %>%
  filter(Rok.produkcji == "2011") %>%
  group_by(Marka) %>%
  summarise(count=n()) %>%
  arrange(desc(count)) %>%
  head(1)

## Odp:
# Najczęściej w 2011 roku występuje SKODA.


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?
auta2012 %>%
  filter(Rodzaj.paliwa == "olej napedowy (diesel)") %>%
  filter(Rok.produkcji >= 2005 & Rok.produkcji <= 2011) %>%
  count()

## Odp:
# Jest 59534 aut z silnikiem diesla wyprodukowanych w latach 2005-2011.


## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?
auta2012 %>%
  filter(Rodzaj.paliwa == "olej napedowy (diesel)") %>%
  filter(Rok.produkcji >= 2005 & Rok.produkcji <= 2011) %>%
  group_by(Marka) %>%
  summarise(srednia = mean(Cena.w.PLN, na.rm=TRUE)) %>%
  arrange(desc(srednia)) %>%
  head(1)

## Odp:
# Spośród aut spełniających warunki zadania średnio najdroższe było Porsche.


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?
auta2012 %>%
  filter(Marka == "Skoda") %>%
  filter(Rok.produkcji == "2011") %>%
  group_by(Model) %>%
  summarise(srednia = mean(Cena.w.PLN, na.rm=TRUE)) %>%
  arrange(srednia) %>%
  head(1)

## Odp:
# Spośród aut spełniających warunki zadania średnio najtańsza jest Fabia.


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?
auta2012 %>%
  filter(Liczba.drzwi == "2/3") %>%
  filter(Cena.w.PLN / KM > 600) %>%
  group_by(Skrzynia.biegow) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(1)
  
## Odp: 
# Automatyczna skrzynia biegów.


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?
auta2012 %>%
  filter(Marka == "Skoda") %>%
  filter(Rodzaj.paliwa == "benzyna" | Rodzaj.paliwa == "olej napedowy (diesel)") %>%
  group_by(Model, Rodzaj.paliwa) %>%
  summarise(srednia = mean(Cena.w.PLN)) %>%
  group_by(Model) %>%
  summarise(roznica = abs(diff(srednia))) %>%
  arrange(roznica) %>%
  head(1)

## Odp: 
# Najmniejsza różnica średnich należy do Felicii.


## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini
lemon_lambo <- auta2012 %>%
  filter(Marka == "Lamborghini")

auta_fixed = data.frame(table(unlist(str_split(lemon_lambo$Wyposazenie.dodatkowe, ", "))))
colnames(auta_fixed)
auta_fixed %>%
  arrange(Freq)

## Odp: 
# Najczęściej występujące wyposażenie: wspomaganie kierownicy, alufelgi, ABS
# Najrzadziej występujące wyposażenie: blokada skrzyni biegów, klatka


## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi
auta2012 %>%
  filter(Marka == "Audi") %>%
  mutate(wruuum_jak_eres7 = case_when(str_detect(Model, "^A") ~ "A",
                            str_detect(Model, "^S") ~ "S",
                            str_detect(Model, "^RS") ~ "RS",
                            TRUE ~ "Szroty")) %>%
  group_by(wruuum_jak_eres7) %>%
  summarise(roznica = mean(KM, na.rm = TRUE) - median(KM, na.rm = TRUE)) %>%
  arrange(roznica)

## Odp:
# Różnica średniej i mediany dla każdego modelu:
# S: -0.263
# A: 19.6
# RS: 50


## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.
basicowe_auteczka <- auta2012 %>%
  group_by(Marka) %>%
  summarise(count = n()) %>%
  filter(count > 10000)

basicowe_modele <- inner_join(basicowe_auteczka, auta2012, by = "Marka") %>%
  group_by(Marka, Model) %>%
  summarise(basicowy_model = n()) %>%
  top_n(1)

inner_join(basicowe_modele, auta2012) %>%
  group_by(Marka, Model, Kolor) %>%
  summarise(kolorek = n()) %>%
  top_n(1)
  
## Odp: 
# Audi - A4 - czarny-metallic
# BMW - 320 - srebrny-metallic
# Ford - Focus - srebrny-metallic
# Mercedes-Benz - C 220 - srebrny-metallic
# Opel - Astra - srebrny-metallic
# Renault - Megane - srebrny-metallic
# Volkswagen - Passat - srebrny-metallic
