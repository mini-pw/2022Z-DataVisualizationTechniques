library(PogromcyDanych)
library(dplyr)
library(stringi)
library(stringr)
library(tidyr)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))
df <- auta2012


## 1. Z którego rocznika jest najwięcej aut i ile ich jest?

auta2012 %>% 
  group_by(Rok.produkcji) %>% 
  summarise(count = n()) %>% 
  filter(count == max(count, na.rm=T))

## Odp: 
### Najwięcej jest aut z rocznika 2011 i jest ich 17418


## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?

auta2012 %>% 
  filter(Rok.produkcji == 2011) %>% 
  group_by(Marka) %>% 
  summarise(count = n()) %>% 
  slice_max(count) %>% 
  select(Marka)

## Odp:
### Skoda


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?

auta2012 %>% 
  filter(Rok.produkcji>=2005 & Rok.produkcji<=2011) %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  summarise(count = n())

## Odp:
### 59534

## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?

auta2012 %>% 
  filter(Rok.produkcji == 2011) %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Marka) %>% 
  summarise(Srednia.cena = mean(Cena.w.PLN)) %>% 
  slice_max(Srednia.cena) %>% 
  select(Marka)

## Odp:
### Porsche


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?

auta2012 %>% 
  filter(Marka == "Skoda" & Rok.produkcji == 2011) %>% 
  group_by(Model) %>% 
  summarise(Srednia.cena = mean(Cena.w.PLN)) %>% 
  slice_min(Srednia.cena) %>% 
  select(Model)

## Odp:
### Fabia


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?

auta2012 %>% 
  mutate(PLNdoKM = Cena.w.PLN/KM) %>% 
  filter(PLNdoKM > 600 & Liczba.drzwi == "2/3") %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(count = n()) %>% 
  slice_max(count) %>% 
  select(Skrzynia.biegow)

## Odp: 
### automatyczna


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?

auta2012 %>% 
  filter(Marka == "Skoda") %>% 
  group_by(Model, Rodzaj.paliwa) %>% 
  summarise(Srednia.cena = mean(Cena.w.PLN)) -> ans7

ans7 %>% 
  filter(Rodzaj.paliwa == "benzyna") -> ans7ben

ans7 %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)") -> ans7di

merged <- inner_join(ans7ben, ans7di, by = "Model")

merged %>% 
  mutate(Roznica = abs(Srednia.cena.x - Srednia.cena.y)) %>% 
  arrange(Roznica) %>% 
  head(1) %>% 
  select(Model)

## Odp: 
### Felicia


## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini

auta2012 %>% 
  filter(Marka == "Lamborghini") %>% 
  select(Wyposazenie.dodatkowe) -> wyposazenie

str_split(wyposazenie[,1], ",") -> wyposazenie

wyposazenie <- lapply(wyposazenie, data.frame, stringsAsFactors = FALSE)
wyposazenie <- bind_rows(skladnik = wyposazenie)
names(wyposazenie) <- "skladnik"

wyposazenie %>% 
  group_by(skladnik) %>% 
  summarise(count = n()) -> wyposazenie

# najwiecej
wyposazenie %>% 
  filter(count == max(count, na.rm=T)) %>% 
  select(skladnik)

# najmniej 
wyposazenie %>% 
  filter(count == min(count, na.rm=T)) %>% 
  select(skladnik)

## Odp: 
### Najczesciej wystepujace wyposazenia dodatkowe to 
### alufelgi, wspomaganie kierownicy i ABS

### Najrzadziej wystepujace wyposazenia dodatkowe to 
### klatka i blokada skrzyni biegow


## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi

auta2012 %>% 
  filter(Marka == "Audi") %>% 
  mutate(Model2 = case_when(
    startsWith(as.character(Model), "A") ~ "A", 
    startsWith(as.character(Model), "S") ~ "S", 
    startsWith(as.character(Model), "RS") ~ "RS",
    TRUE ~ "Other")) %>% 
  filter(Model2 == "A" | Model2 == "S" | Model2 == "RS") %>% 
  group_by(Model2) %>% 
  summarise(Srednia.KM = mean(KM, na.rm=T), Mediana.KM = median(KM, na.rm=T))

## Odp:
### Najwieksza roznica pomiedzy srednia a mediana wystepuje w modelu RS, 
### najmniejsza (zerowa) w modelu S


## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.
  
# marki
auta2012 %>% 
  group_by(Marka) %>% 
  summarise(count = n()) %>% 
  filter(count > 10000) %>% 
  pull(Marka) -> marki

marki <- as.character(marki)

auta2012 %>% 
  filter(Marka %in% marki) %>% 
  group_by(Marka, Model) %>% 
  summarise(count = n()) %>% 
  arrange(Marka, -count)-> popularne

#modele
modele <- rep(NA, length(marki))

for (i in 1:length(marki)){
  modele[i] <-  popularne %>% 
    filter(Marka == marki[i]) %>% 
    slice_head() %>% 
    ungroup() %>% 
    select(Model)
} 

modele <- as.data.frame(modele)
modele <- pivot_longer(modele, cols = everything())
modele <- modele[, 2]
modele <- droplevels(modele)
modele <- unlist(modele)

#kolory
kolory <- auta2012 %>% 
  group_by(Marka, Model, Kolor) %>% 
  summarise(count = n()) %>% 
  arrange(-count)

kolory %>% 
  filter((Marka %in% marki & Model %in% modele) & count==max(count)) -> test

cbind(data.frame(marki), data.frame(modele)) -> wynik
names(wynik) <- c("Marka", "Model")
wynik <- left_join(wynik, test, by = c("Model", "Marka"))

## Odp: 
### Czesto wystepujace marki: Audi, BMW, Ford, Mercedes-Benz,
### Opel, Renault, Volkswagen

### Najczesciej wystepujace modele dla kazdej marki:
### Audi - A4; BMW - 320; Ford - Focus; Mercedes-Benz - C 220; 
### Opel - Astra; Renault - Megane; Volkswagen - Passat

### Najczesciej wystepujace kolory dla poszczegolnych marek:
### czarny-metallic dla Audi A4, srebrny-metallic dla reszty
