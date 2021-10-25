library(PogromcyDanych)
library(dplyr)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))
auta <- auta2012

## 1. Z którego rocznika jest najwięcej aut i ile ich jest?

sum(is.na(auta["Rok.produkcji"])) # 0
auta %>% group_by(Rok.produkcji) %>% summarise("Liczba.aut" = n()) %>% 
  arrange(desc(Liczba.aut)) %>% head(1)

## Odp: Z rocznika 2011, jest ich 17418.


## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?

auta[, c("Rok.produkcji", "Marka")] %>% filter(Rok.produkcji == 2011) %>%
  group_by(Marka) %>%  summarise("Liczba.aut.tej.marki" = n()) %>% 
  arrange(desc(Liczba.aut.tej.marki)) %>% head(1)

## Odp: Skoda


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?

zad3_df <- auta[, c("Rodzaj.paliwa", "Rok.produkcji")] %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)", Rok.produkcji <= 2011 & Rok.produkcji >= 2005)

dim(zad3_df)[1]

## Odp: 59534


## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?

auta[, c("Rodzaj.paliwa", "Rok.produkcji", "Marka", "Cena.w.PLN")] %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)", Rok.produkcji == 2011) %>% 
  select("Marka", "Cena.w.PLN") %>% 
  group_by(Marka) %>% 
  summarise("Srednia.cena.w.PLN" = mean(Cena.w.PLN)) %>% 
  arrange(desc(Srednia.cena.w.PLN)) %>% head(1)
  


## Odp: Porsche


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?

auta[, c("Model", "Rok.produkcji", "Marka", "Cena.w.PLN")] %>% 
  filter(Marka == "Skoda", Rok.produkcji == 2011) %>% 
  select("Model", "Cena.w.PLN") %>% group_by(Model) %>% 
  summarise("Srednia.cena.w.PLN" = mean(Cena.w.PLN)) %>% 
  arrange(Srednia.cena.w.PLN) %>% head(1)

## Odp: Fabia


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?

auta[, c("Liczba.drzwi", "KM", "Cena.w.PLN", "Skrzynia.biegow")] %>% 
  filter(Liczba.drzwi == "2/3", (Cena.w.PLN / KM) > 600) %>% 
  group_by(Skrzynia.biegow) %>% summarise("Ilosc.aut.z.dana.skrzynia" = n()) %>% 
  arrange(desc(Ilosc.aut.z.dana.skrzynia)) %>% head(1)

## Odp: Automatyczna


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?

zad7_df <- auta[, c("Marka", "Model", "Rodzaj.paliwa", "Cena.w.PLN")] %>% 
  filter(Marka == "Skoda", Rodzaj.paliwa == "benzyna" | Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Model, Rodzaj.paliwa) %>% 
  summarise("Srednia.cena" = mean(Cena.w.PLN))

zad7_df %>%  group_by(Model) %>% summarise("Szukana.roznica" = abs(diff(Srednia.cena))) %>% 
  arrange(Szukana.roznica) %>% head(1)

## Odp: Felicia


## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini

wyposazenia <- auta[, c("Marka", "Wyposazenie.dodatkowe")] %>% 
  filter(Marka == "Lamborghini")

all_wyposazenia <- vector()

for(i in 1:nrow(wyposazenia)){
  x <-unlist(strsplit(toString(wyposazenia[i,2]), "[,]"))
  all_wyposazenia <- append(all_wyposazenia, x)
}
table(all_wyposazenia)
wynik <- as.data.frame(all_wyposazenia)
wynik <- data.frame(counter = rep(1,dim(wynik)[1]), wynik)

wynik %>% 
  group_by(all_wyposazenia) %>% 
  summarise("Liczba.wystapien" = sum(counter)) %>% 
  arrange(desc(Liczba.wystapien)) %>% 
  head(5)

wynik %>% 
  group_by(all_wyposazenia) %>% 
  summarise("Liczba.wystapien" = sum(counter)) %>% 
  arrange(Liczba.wystapien) %>% 
  head(5)

## Odp: \/

# 5 najrzadziej wyst�puj�cych to:
# blokada skrzyni biegow,
# klatka
# pod. przednia szyba,
# niezalezne ogrzewanie
# reg. wysokosc podwozia

# 5 najczesciej wystepujacych to:
# alufelgi,
# wspomaganie kierownicy,
# ABS,
# centralny zamek,
# szkorzana tapicerka

## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi
library(stringr)

auta %>% select(Marka, Model, KM) %>% 
  filter(Marka == "Audi") %>%  filter(str_detect(Model, "^A")) %>% 
  pull(KM) %>% 
  mean(na.rm = TRUE)

auta %>% select(Marka, Model, KM) %>% 
  filter(Marka == "Audi") %>%  filter(str_detect(Model, "^A")) %>% 
  pull(KM) %>% 
  median(na.rm = TRUE)
 
auta %>% select(Marka, Model, KM) %>% 
  filter(Marka == "Audi") %>%  filter(str_detect(Model, "^RS")) %>% 
  pull(KM) %>% 
  mean(na.rm = TRUE)

auta %>% select(Marka, Model, KM) %>% 
  filter(Marka == "Audi") %>%  filter(str_detect(Model, "^RS")) %>% 
  pull(KM) %>% 
  median(na.rm = TRUE)

auta %>% select(Marka, Model, KM) %>% 
  filter(Marka == "Audi") %>%  filter(str_detect(Model, "^S")) %>% 
  pull(KM) %>% 
  mean(na.rm = TRUE)

auta %>% select(Marka, Model, KM) %>% 
  filter(Marka == "Audi") %>%  filter(str_detect(Model, "^S")) %>% 
  pull(KM) %>% 
  median(na.rm = TRUE)

## Odp: 
# Dla modeli A, srednia i mediana wynosi�y kolejno 159.5799 oraz 140
# Dla modeli RS, srednia i mediana wynosi�y kolejno 500.0282 oraz 450
# Dla modeli S, srednia i mediana wynosi�y kolejno 343.7371 oraz 344


## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.

te_marki <- auta %>% select(Marka) %>% count(Marka) %>% filter(n>10000) %>% pull(Marka)

zad10df <- auta %>% filter(Marka %in% te_marki)

zad10df2 <- zad10df %>% group_by(Marka, Model) %>% count(Model)

te_modele <- zad10df2 %>% group_by(Marka) %>% filter(n == max(n)) %>% pull(Model)

zad10df %>% filter(Model %in% te_modele) %>% group_by(Marka, Model) %>% count(Kolor) %>% filter(n == max(n))

## Odp: Ponizej kolejno najpopularniejsze modele i ich najpopularniejsze kolory dla odpowiednich marek
# Audi A4 czarny-metallic
# BMW 320 srebrny-metallic 
# Ford Focus srebrny-metallic
# Mercedes-Benz C 220 srebrny-metallic
# Opel Astra srebrny-metallic
# Renault Megane srebrny-metallic
# Volkswagen Passat srebrny-metallic
