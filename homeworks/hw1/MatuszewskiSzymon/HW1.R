library(PogromcyDanych)
library(dplyr)
library(tidyr)
library(stringi)
library(stringr)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

## 1. Z którego rocznika jest najwięcej aut i ile ich jest?

auta2012 %>% 
  group_by(Rok.produkcji) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  head(1)
  

## Odp: Najwięcej aut jest z rocznika 2011 i jest ich 17418.


## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?

auta2012 %>% 
  filter(Rok.produkcji == "2011") %>% 
  group_by(Marka) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  head(1)

## Odp: Najczęściej wśród aut wyprodukowanych w 2011 roku występuje Skoda.


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?

auta2012 %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)" & Rok.produkcji >= 2005 & Rok.produkcji <= 2011) %>%
  nrow()

## Odp: Aut z silnikiem diesla wyprodukowanych w latach 2005-2011 jest 59534.


## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?

auta2012 %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)" & Rok.produkcji == "2011") %>% 
  group_by(Marka) %>% 
  summarise(Cena.srednia = mean(Cena.w.PLN)) %>% 
  arrange(-Cena.srednia) %>% 
  head(1)

## Odp: Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku średnio najdroższa jest marka Porsche (cena śr. wynosi 345669zł).


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?

auta2012 %>% 
  filter(Marka == "Skoda" & Rok.produkcji == "2011") %>% 
  group_by(Model) %>% 
  summarise(Cena.srednia = mean(Cena.w.PLN)) %>% 
  arrange(Cena.srednia) %>% 
  head(1)

## Odp: Spośród aut marki Skoda wyprodukowanych w 2011 roku średnio najtańszy jest model Fabia (cena śr. wynosi 42015zł).


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?

auta2012 %>% 
  filter(Liczba.drzwi == "2/3") %>% 
  drop_na(KM) %>%
  mutate(Cena.w.PLN.do.KM = Cena.w.PLN/KM) %>% 
  filter(Cena.w.PLN.do.KM > 600) %>% 
  drop_na(Skrzynia.biegow) %>%  
  group_by(Skrzynia.biegow) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

## Odp: Najczęściej wśród 2/3-drzwiowych aut, których stosunek ceny w PLN do KM wynosi ponad 600 występuje automatyczna skrzynia biegóW (708 przypadków).


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?

diesel_df <- auta2012 %>% 
  filter(Marka == "Skoda", (Rodzaj.paliwa == "olej napedowy (diesel)")) %>% 
  group_by(Model) %>% 
  summarise(Srednia.cena.diesel = mean(Cena.w.PLN))

benzyna_df <- auta2012 %>% 
  filter(Marka == "Skoda", (Rodzaj.paliwa == "benzyna")) %>% 
  group_by(Model) %>% 
  summarise(Srednia.cena.benzyna = mean(Cena.w.PLN)) 

inner_join(diesel_df, benzyna_df, by = 'Model') %>% 
  group_by(Model) %>% 
  summarise(Roznica = abs(Srednia.cena.diesel - Srednia.cena.benzyna)) %>% 
  arrange(Roznica) %>% 
  head(1)

## Odp: Spośród aut marki Skoda modelem, który ma najmniejszą różnicę średnich cen między samochodami z silnikiem benzynowym a diesel jest Felicia
## (różnica wynosi 70.5).


## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini

lambo <- auta2012 %>% 
  filter(Marka == "Lamborghini") %>% 
  select(Wyposazenie.dodatkowe)
lambo <- as.character(lambo$Wyposazenie.dodatkowe) %>% 
  str_split(", ") %>% 
  unlist()

table(lambo)

max(table(lambo))
min(table(lambo))

## Odp: Najrzadziej występującymi wyposażeniami dodatkowymi samochodów marki Lamborghini są: blokada skrzyni biegóW oraz klatka (1 wystąpienie), a najczęściej występującymi
## system ABS, alufelgi oraz wspomaganie kierowcy (18 wystąpień).


## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi

srednia_A_S <- auta2012 %>% 
  filter(Marka == "Audi") %>% 
  group_by(gr = substr(Model,1,1)) %>% 
  filter(gr == "A" | gr == "S") %>% 
  summarise(Srednia = mean(KM, na.rm = TRUE))
  
mediana_A_S <- auta2012 %>% 
  filter(Marka == "Audi") %>% 
  group_by(gr = substr(Model,1,1)) %>% 
  filter(gr == "A" | gr == "S") %>% 
  summarise(Mediana = median(KM, na.rm = TRUE))

A_S <- inner_join(srednia_A_S,mediana_A_S,by = "gr")

srednia_RS <- auta2012 %>% 
  filter(Marka == "Audi") %>% 
  group_by(gr = substr(Model,1,2)) %>% 
  filter(gr == "RS") %>% 
  summarise(Srednia = mean(KM, na.rm = TRUE))

mediana_RS <- auta2012 %>% 
  filter(Marka == "Audi") %>% 
  group_by(gr = substr(Model,1,2)) %>% 
  filter(gr == "RS") %>% 
  summarise(Mediana = median(KM, na.rm = TRUE))

RS <- inner_join(srednia_RS,mediana_RS,by = "gr")

audi <- A_S %>% 
  add_row(RS) %>% 
  mutate(Roznica = abs(Srednia - Mediana))
audi

## Odp: Najmniejsza różnica między średnią a medianą mocy KM występuje przy modelach S (jest ona bliska 0 - dokładnie wynosi 0.263). Największą taką różnicę
## możemy znaleźć w samochodach z grupy modeli RS (różnica równa 50).
## Ponadto, przeciętnie to modele RS posiadają największą moc KM, która przeważnie wynosi ok. 3 razy więcej niż moc w samochodach klasy A.


## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.

najpopularniejsze_auta <- auta2012 %>% 
  group_by(Marka) %>% 
  summarise(n = n()) %>% 
  filter(n > 10000) %>% 
  arrange(-n)

najpopularniejszy_model <- auta2012 %>% 
  drop_na(Marka,Model,Kolor) %>% 
  group_by(Marka,Model) %>% 
  summarise(n = n(), .groups = "keep") %>% 
  ungroup() %>% 
  group_by(Marka) %>% 
  filter(n == max(n, na.rm = TRUE))

najpopularniejszy_model_kolor <- left_join(najpopularniejszy_model, auta2012, by = c("Marka","Model")) %>% 
  group_by(Marka,Model,Kolor) %>% 
  summarise(n = n(), .groups = "keep") %>% 
  ungroup() %>% 
  group_by(Marka) %>% 
  filter(n == max(n, na.rm = TRUE))

left_join(najpopularniejsze_auta,najpopularniejszy_model_kolor, by = "Marka")

## Odp: Marki, których auta występują w danych ponad 10000 razy (w kolejności malejącej): Volkswagen, Opel, Ford, Renault, Audi, Mercedes-Benz, BMW.
## MARKA - NAJPOPULARNIEJSZY MODEL - NAJPOPULARNIEJSZY KOLOR TEGO MODELU
## Volkswagen - Passat - srebrny (metalik)
## Opel - Astra - srebrny(metalik)
## Ford - Focus - srebrny(metalik)
## Renault - Megane - srebrny(metalik)
## Audi - A4 - czarny(metalik)
## Mercedes-Benz - C220 - srebrny(metalik)
## BMW - 320 - srebrny(metalik)