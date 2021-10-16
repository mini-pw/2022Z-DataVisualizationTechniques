library(PogromcyDanych)
library(dplyr)
library(stringi)
library(tidyr)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

## 1. Z którego rocznika jest najwięcej aut i ile ich jest?
auta2012 %>%
  group_by(Rok.produkcji) %>% 
  summarise(n()) %>% 
  slice_max(`n()`)

## Odp: 
# 17418 z 2011 roku

## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?
auta2012 %>% 
  filter(Rok.produkcji == 2011) %>% 
  group_by(Marka) %>% 
  summarise(n()) %>% 
  slice_max(`n()`)

## Odp:
# Skoda

## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?
auta2012$Rodzaj.paliwa %>% unique()
auta2012 %>% 
  filter(Rok.produkcji >= 2005 & Rok.produkcji <= 2011 & Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  count()

## Odp:
# 59534

## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?
auta2012 %>% 
  filter(Rok.produkcji == 2011 & Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Marka) %>% 
  summarise(mean(Cena.w.PLN)) %>% 
  slice_max(`mean(Cena.w.PLN)`)

## Odp:
# Porsche

## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?
auta2012 %>% 
  filter(Rok.produkcji == 2011 & Marka == "Skoda") %>%
  group_by(Model) %>% 
  summarise(mean(Cena.w.PLN)) %>% 
  slice_min(`mean(Cena.w.PLN)`)

## Odp:
# Fabia

## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?
auta2012 %>% 
  filter(Liczba.drzwi == "2/3" & Cena.w.PLN/KM >= 600) %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(n())

## Odp: 
# automatyczna

## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?
auta2012 %>% 
  filter(Marka == "Skoda", Rodzaj.paliwa == "benzyna" | Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Model, Rodzaj.paliwa) %>% 
  summarise(mean_cena = mean(Cena.w.PLN), .groups = "drop") %>% 
  pivot_wider(names_from = Rodzaj.paliwa, values_from = mean_cena) %>%
  slice_min(abs(benzyna - `olej napedowy (diesel)`), n = 1)
  

## Odp: 
# Felicia

## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini
auta2012 %>%
  filter(Marka == "Lamborghini") %>% 
  transmute(Wyposazenie.dodatkowe = stri_split_fixed(Wyposazenie.dodatkowe,",")) %>% 
  unlist() %>% 
  table() %>% 
  sort()

## Odp: 
# blokada skrzyni biegAlw, klatka / alufelgi, wspomaganie kierownicy, ABS

## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi
auta2012 %>% 
  filter(Marka == "Audi") %>% 
  mutate(Grupa = stri_extract_first_regex(Model, "A|RS|S")) %>% # przyjąłem, że TT S liczy się jako klasa S (analogicznie TT RS)
  group_by(Grupa) %>% 
  summarise(Mediana.mocy = median(KM,na.rm = T), Srednia.mocy = mean(KM,na.rm = T))

## Odp:
# Grupa Mediana.mocy Srednia.mocy
# 1 A              140         160.
# 2 RS             450         493.
# 3 S              340         340.

## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.
auta2012 %>% 
  group_by(Marka) %>% 
  filter(n() > 1e4) %>% 
  group_by(Marka, Model) %>% 
  mutate(n = n()) %>% 
  group_by(Marka) %>%
  filter(n == max(n), Kolor != "") %>% 
  group_by(Marka,Model,Kolor) %>% 
  mutate(n = n()) %>% 
  group_by(Marka,Model) %>% 
  filter(n == max(n)) %>% 
  group_by(Marka,Model,Kolor) %>% 
  summarise()
  
  
  

## Odp: 
# Marka         Model  Kolor           
# 1 Audi          A4     czarny-metallic 
# 2 BMW           320    srebrny-metallic
# 3 Ford          Focus  srebrny-metallic
# 4 Mercedes-Benz C 220  srebrny-metallic
# 5 Opel          Astra  srebrny-metallic
# 6 Renault       Megane srebrny-metallic
# 7 Volkswagen    Passat srebrny-metallic