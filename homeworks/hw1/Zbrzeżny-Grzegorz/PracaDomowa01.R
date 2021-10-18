library(PogromcyDanych)
library(dplyr)
library(stringi)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

## 1. Z którego rocznika jest najwiecej aut i ile ich jest?

auta2012 %>% count(Rok.produkcji) %>% arrange(-n) %>% head(1)

## Odp: Z 2011, jest ich 17418


## 2. Która marka samochodu wystepuje najczesciej wsród aut wyprodukowanych w 2011 roku?

auta2012 %>% select(Marka, Rok.produkcji) %>% filter(Rok.produkcji == 2011) %>% count(Marka) %>% arrange(-n) %>% head(1)

## Odp: Skoda


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?

auta2012 %>% select(Rodzaj.paliwa, Rok.produkcji) %>% filter(Rok.produkcji >= 2005 & Rok.produkcji <= 2011) %>% count(Rodzaj.paliwa) %>% arrange(-n)

## Odp: 59534


## 4. Sposród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest srednio najdrozsza?

auta2012 %>% filter(Rok.produkcji == 2011, Rodzaj.paliwa == "olej napedowy (diesel)") %>% select(Cena.w.PLN, Marka) %>% group_by(Marka) %>% 
  summarise(Srednia = mean(Cena.w.PLN)) %>% arrange(-Srednia) %>% head(1)

## Odp: Porsche


## 5. Sposród aut marki Skoda wyprodukowanych w 2011 roku, który model jest srednio najtanszy?

auta2012 %>% filter(Rok.produkcji == 2011, Marka == "Skoda") %>% select(Model, Cena.w.PLN) %>%
  group_by(Model) %>% summarise(CenaSrednia = mean(Cena.w.PLN)) %>% arrange(CenaSrednia) %>% head(1)

## Odp: Fabia


## 6. Która skrzynia biegów wystepuje najczesciej wsród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?

auta2012 %>% filter(Liczba.drzwi == "2/3", Cena.w.PLN/KM > 600) %>% select(Skrzynia.biegow) %>% count(Skrzynia.biegow)

## Odp: Automatyczna


## 7. Sposród aut marki Skoda, który model ma najmniejsza róznice srednich cen 
##    miedzy samochodami z silnikiem benzynowym, a diesel?

df1 <- auta2012 %>% filter(Marka == "Skoda", Rodzaj.paliwa == "benzyna") %>% select(Model, Cena) %>% group_by(Model) %>% summarise(meanBenz = mean(Cena))
df2 <- auta2012 %>% filter(Marka == "Skoda", Rodzaj.paliwa == "olej napedowy (diesel)") %>% select(Model, Cena) %>% group_by(Model) %>% summarise(meanDies = mean(Cena))

df1 %>% inner_join(df2, "Model") %>% mutate(Róznica = abs(meanBenz - meanDies)) %>% arrange(Róznica) %>% head(1)

## Odp: Praktik


## 8. Znajdz najrzadziej i najczesciej wystepujace wyposazenie/a dodatkowe 
##    samochodów marki Lamborghini

df1 <- auta2012 %>% filter(Marka == "Lamborghini") %>% select(Wyposazenie.dodatkowe)
sort(table(strsplit(toString(df1$Wyposazenie.dodatkowe), ", ")), decreasing = TRUE)

## Odp: Najczesciej: ABS, alufelgi i wspomaganie kierownicy, najrzadziej: blokada skrzyni biegAlw, klatka


## 9. Porównaj srednia i mediane mocy KM miedzy grupami modeli A, S i RS 
##    samochodów marki Audi

dfA <- auta2012 %>% filter(Marka == "Audi", grepl("^A", Model)) %>% select(Model, KM)
dfS <- auta2012 %>% filter(Marka == "Audi", grepl("^S", Model)) %>% select(Model, KM)
dfRS <- auta2012 %>% filter(Marka == "Audi", grepl("^(RS)", Model)) %>% select(Model, KM)

## Odp:

#Srednia A

mean(dfA$KM, na.rm = TRUE)

#Mediana A

median(dfA$KM, na.rm = TRUE)

#Srednia S

mean(dfS$KM, na.rm = TRUE)

#Mediana S

median(dfS$KM, na.rm = TRUE)

#Srednia RS

mean(dfRS$KM, na.rm = TRUE)

#Mediana RS

median(dfRS$KM, na.rm = TRUE)


## 10. Znajdz marki, których auta wystepuja w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla kazdej z tych marek.


## Odp: 
auta2012 %>% group_by(Marka) %>% summarise(Encounters = n()) %>% filter(Encounters > 10000) %>% inner_join(auta2012, by="Marka") %>% 
  group_by(Marka, Model) %>% summarise(Liczba = n()) %>% top_n(1, Liczba) %>%
  inner_join(auta2012, by = c("Marka" = "Marka", "Model" = "Model")) %>% group_by(Marka, Model, Kolor) %>% summarise(LiczbaKolorow = n()) %>% 
  top_n(1, LiczbaKolorow) %>% select(Marka, Model, Kolor)