library(PogromcyDanych)
library(dplyr)
library(tidyr)
library(stringr)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

## 1. Z którego rocznika jest najwięcej aut i ile ich jest?
auta2012 %>% 
  group_by(Rok.produkcji) %>% 
  summarise(count = n()) %>% 
  arrange(-count)

## Odp: 2011 rok - 17418 aut


## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?
auta2012 %>% 
  select(c("Marka", "Rok.produkcji")) %>% 
  filter(Rok.produkcji == 2011) %>% 
  group_by(Marka) %>% 
  summarise(count = n()) %>% 
  arrange(-count)

## Odp: Skoda


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?
auta2012 %>% 
  filter(Rok.produkcji <= 2011, Rok.produkcji >=2005, Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  count()

## Odp: 59534


## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?
auta2012 %>%
  filter(Rok.produkcji == 2011, Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  select(c("Marka", "Cena.w.PLN")) %>% 
  group_by(Marka) %>% 
  summarise(Srednia.cena = mean(Cena.w.PLN, na.rm=TRUE)) %>% 
  arrange(-Srednia.cena)

## Odp: Porsche - średnio 345669 zl


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?
auta2012 %>% 
  filter(Rok.produkcji == 2011, Marka == "Skoda") %>% 
  select(c("Cena.w.PLN", "Model")) %>% 
  group_by(Model) %>% 
  summarise(Srednia.cena = mean(Cena.w.PLN, na.rm=TRUE)) %>% 
  arrange(Srednia.cena)

## Odp: Fabia - 42015 PLN


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?
auta2012 %>% 
  filter(Liczba.drzwi == "2/3", Cena.w.PLN/KM > 600) %>% 
  select(Skrzynia.biegow) %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(count = n())

## Odp: automatyczna


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?
tmp <- auta2012 %>% 
  filter(Marka == "Skoda", (Rodzaj.paliwa == "benzyna" | Rodzaj.paliwa == "olej napedowy (diesel)")) %>% 
  select(c("Model", "Rodzaj.paliwa", "Cena.w.PLN")) %>% 
  group_by(Model, Rodzaj.paliwa) %>%
  summarise(Srednia.cena = mean(Cena.w.PLN)) %>%
  pivot_wider(names_from = Rodzaj.paliwa, values_from = Srednia.cena)

names(tmp) <- c("Marka", "benzyna", "diesel")
tmp %>% 
  mutate(roznica = abs(benzyna - diesel)) %>% 
  arrange(roznica)

## Odp: Felicia


## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini
wyposazenie <- (auta2012 %>% filter(Marka == "Lamborghini"))$Wyposazenie.dodatkowe
wyposazenie <- wyposazenie %>% 
  str_split(", ", simplify = TRUE) %>% 
  as.vector()

wyposazenie[wyposazenie!=""] %>% 
  table() %>% 
  as.data.frame() %>% 
  arrange(-Freq)
  

## Odp: najczesciej: ABS, alufelgi, wspomaganie kierownicy; najrzadziej: blokada skrzyni biegow, klatka


## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi
tmp <- auta2012 %>% 
  filter(Marka == "Audi") %>% 
  select(Model, KM) %>% 
  filter(!is.na(KM))

A <- tmp[str_detect(tmp$Model, "^A"),]
S <- tmp[str_detect(tmp$Model, "^S[^R]"),]
RS <- tmp[str_detect(tmp$Model, "^RS"),]

Amean <- mean(A$KM)
Amedian <- median(A$KM)

Smean <- mean(S$KM)
Smedian <- median(S$KM)

RSmean <- mean(RS$KM)
RSmedian <- median(RS$KM)

## Odp: A ma najmniejsze średnią i medianę, RS ma je największe, S jest gdzieś pomiędzy


## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.
marki <- auta2012 %>% 
  group_by(Marka) %>% 
  summarise(n = n()) %>% 
  filter(n > 10000)

marki <- as.vector(marki$Marka)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


modele <- auta2012 %>% 
  filter(Marka %in% marki) %>% 
  select(Marka, Model, Kolor) %>% 
  group_by(Marka) %>% 
  summarise(m = Mode(Model))

modele <- as.vector(modele$m)

auta2012 %>%
  filter(Marka %in% marki, Model %in% modele) %>% 
  select(Marka, Model, Kolor) %>% 
  group_by(Marka, Model) %>% 
  summarise(k = Mode(Kolor))

## Odp: Dzieki temu ze jest malo tych marek to mozemy recznie sprawdzic
## ze tylko jedne z popularnych modeli sie powtarza w dwoch markach
## a poniewaz mamy w wektorach zapisane marki i modele w pasujacej kolejnosci
## to po prostu pomijamy niepasujacy wynik
## bo nie ma po co komplikowac i tak juz dlugiego roziwazania

#1 Audi          A4     czarny-metallic 
#2 BMW           320    srebrny-metallic
#3 Ford          Focus  srebrny-metallic
#4 Mercedes-Benz C 220  srebrny-metallic
#5 Opel          Astra  srebrny-metallic
#6 Renault       Megane srebrny-metallic
#7 Volkswagen    Passat srebrny-metallic