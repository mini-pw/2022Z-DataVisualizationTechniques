library(PogromcyDanych)
library(dplyr)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

## 1. Z którego rocznika jest najwiêcej aut i ile ich jest?

auta2012 %>%
  group_by(Rok.produkcji) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

## Odp: 2011: 17418 cars


## 2. Która marka samochodu wystêpuje najczêœciej wœród aut wyprodukowanych w 2011 roku?

auta2012 %>%
  filter(Rok.produkcji == 2011) %>%
  group_by(Marka) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

## Odp: Skoda


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?

auta2012 %>% 
  filter(Rok.produkcji <= 2011 & Rok.produkcji >= 2005 & Rodzaj.paliwa == "olej napedowy (diesel)") %>%
  summarise(n = n())
  

## Odp: 59534


## 4. Spoœród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest œrednio najdro¿sza?

auta2012 %>%
  filter(Rok.produkcji == 2011 & Rodzaj.paliwa == "olej napedowy (diesel)") %>%
  group_by(Marka) %>%
  summarise(avg_cost = mean(Cena.w.PLN, na.rm = T)) %>%
  arrange(desc(avg_cost))

## Odp: Porsche


## 5. Spoœród aut marki Skoda wyprodukowanych w 2011 roku, który model jest œrednio najtañszy?

auta2012 %>% 
  filter(Rok.produkcji == 2011 & Marka == "Skoda") %>%
  group_by(Model) %>%
  summarise(avg_cost = mean(Cena, na.rm = T)) %>%
  arrange(-desc(avg_cost))

## Odp: Fabia      


## 6. Która skrzynia biegów wystêpuje najczêœciej wœród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?

auta2012 %>%
  mutate(ratio = Cena.w.PLN / KM) %>%
  filter(Liczba.drzwi == "2/3") %>%
  filter(ratio > 600) %>%
  group_by(Skrzynia.biegow) %>%
  summarise(n = n())

## Odp: automatyczna


## 7. Spoœród aut marki Skoda, który model ma najmniejsz¹ ró¿nicê œrednich cen 
##    miêdzy samochodami z silnikiem benzynowym, a diesel?

library(tidyr)
library(forcats)

df <- auta2012
levels(df$Rodzaj.paliwa)[levels(df$Rodzaj.paliwa) == "benzyna+LPG"] = "benzyna"

df %>%
  filter(Marka == "Skoda") %>%
  filter(Rodzaj.paliwa == "olej napedowy (diesel)" | Rodzaj.paliwa == "benzyna") %>%
  group_by(Model, Rodzaj.paliwa) %>%
  summarise(mean = mean(Cena.w.PLN, na.rm = T)) %>%
  pivot_wider(names_from = Rodzaj.paliwa, values_from = mean) %>%
  rename(diesel = "olej napedowy (diesel)") %>%
  mutate(diff = abs(benzyna - diesel)) %>%
  arrange(-desc(diff))
 
## Odp: Favorit    


## 8. ZnajdŸ najrzadziej i najczêœciej wystêpuj¹ce wyposa¿enie/a dodatkowe 
##    samochodów marki Lamborghini

library(stringr)

auta2012 %>%
  filter(Marka == "Lamborghini") %>%
  pull(Wyposazenie.dodatkowe) -> wyp

wyp <- as.character(wyp)
wyp <- paste(wyp, collapse = ", ")
wyp <- str_split(wyp, ", ")
wyp <- table(wyp)
wyp[order(wyp)]

res[order(res)]

## Odp: 
# najczêœciej: wspomaganie kierownicy, alufelgi, ABS
# najrzadziej: blokada skrzyni biegów, klatka

## 9. Porównaj œredni¹ i medianê mocy KM miêdzy grupami modeli A, S i RS 
##    samochodów marki Audi

auta2012 %>% 
  filter(Marka == "Audi") %>%
  mutate(Group = str_match(Model, "[A-Z]*")) %>%
  filter(Group == "A" | Group == "S" | Group == "RS") %>%
  group_by(Group) %>%
  summarise(Avg = mean(KM, na.rm = T), Median = median(KM, na.rm = T))

## Odp:   
# Group         Avg Median
# 1 A          160.    140
# 2 RS         500.    450
# 3 S          344.    344


## 10. ZnajdŸ marki, których auta wystêpuj¹ w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla ka¿dej z tych marek.

auta2012 %>% 
  group_by(Marka) %>%
  summarise(n = n()) %>%
  filter(n > 10000) %>%
  pull(Marka) -> marki

marki <- as.character(marki)

sapply(marki, FUN = function(m) {
  auta2012 %>%
    filter(Marka == m) %>%
    group_by(Model) %>%
    summarise(n = n()) %>%
    slice_max(order_by = n) %>%
    pull(Model)
}) -> modele

modele <- as.character(modele)  

sapply(modele, FUN = function(m) {
  auta2012 %>%
    filter(Model == m) %>%
    group_by(Kolor) %>%
    summarise(n = n()) %>%
    slice_max(order_by = n) %>%
    pull(Kolor)
}) -> kolory

kolory <- as.character(kolory)

cat(paste(marki, modele, kolory, sep = " - "))

## Odp:
# Audi - A4 - czarny-metallic
# BMW - 320 - srebrny-metallic
# Ford - Focus - srebrny-metallic 
# Mercedes-Benz - C 220 - srebrny-metallic
# Opel - Astra - srebrny-metallic
# Renault - Megane - srebrny-metallic
# Volkswagen - Passat - srebrny-metallic