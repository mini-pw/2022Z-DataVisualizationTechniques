library(PogromcyDanych)
library(dplyr)
library(stringi)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

## 1. Z którego rocznika jest najwiêcej aut i ile ich jest?

auta2012 %>% count(Rok.produkcji) %>% arrange(-n) %>% head(1)

## Odp: Z 2011, jest ich 17418


## 2. Która marka samochodu wystêpuje najczêœciej wœród aut wyprodukowanych w 2011 roku?

auta2012 %>% select(Marka, Rok.produkcji) %>% filter(Rok.produkcji == 2011) %>% count(Marka) %>% arrange(-n) %>% head(1)

## Odp: Skoda


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?

auta2012 %>% select(Rodzaj.paliwa, Rok.produkcji) %>% filter(Rok.produkcji >= 2005 & Rok.produkcji <= 2011) %>% count(Rodzaj.paliwa) %>% arrange(-n)

## Odp: 59534


## 4. Spoœród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest œrednio najdro¿sza?

auta2012 %>% filter(Rok.produkcji == 2011, Rodzaj.paliwa == "olej napedowy (diesel)") %>% select(Cena, Marka) %>% group_by(Marka) %>% 
  summarise(Œrednia = mean(Cena)) %>% arrange(-Œrednia) %>% head(1)

## Odp: Volvo


## 5. Spoœród aut marki Skoda wyprodukowanych w 2011 roku, który model jest œrednio najtañszy?

auta2012 %>% filter(Rok.produkcji == 2011, Marka == "Skoda") %>% select(Model, Cena) %>%
  group_by(Model) %>% summarise(CenaŒrednia = mean(Cena)) %>% arrange(CenaŒrednia) %>% head(1)

## Odp: Fabia


## 6. Która skrzynia biegów wystêpuje najczêœciej wœród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?

auta2012 %>% filter(Liczba.drzwi == "2/3", Cena.w.PLN/KM > 600) %>% select(Skrzynia.biegow) %>% count(Skrzynia.biegow)

## Odp: Automatyczna


## 7. Spoœród aut marki Skoda, który model ma najmniejsz¹ ró¿nicê œrednich cen 
##    miêdzy samochodami z silnikiem benzynowym, a diesel?

df1 <- auta2012 %>% filter(Marka == "Skoda", Rodzaj.paliwa == "benzyna") %>% select(Model, Cena) %>% group_by(Model) %>% summarise(meanBenz = mean(Cena))
df2 <- auta2012 %>% filter(Marka == "Skoda", Rodzaj.paliwa == "olej napedowy (diesel)") %>% select(Model, Cena) %>% group_by(Model) %>% summarise(meanDies = mean(Cena))

df1 %>% inner_join(df2, "Model") %>% mutate(Ró¿nica = abs(meanBenz - meanDies)) %>% arrange(Ró¿nica) %>% head(1)

## Odp: Praktik


## 8. ZnajdŸ najrzadziej i najczêœciej wystêpuj¹ce wyposa¿enie/a dodatkowe 
##    samochodów marki Lamborghini

df1 <- auta2012 %>% filter(Marka == "Lamborghini") %>% select(Wyposazenie.dodatkowe)
sort(table(strsplit(toString(df1$Wyposazenie.dodatkowe), ", ")), decreasing = TRUE)

## Odp: Najczêœciej: ABS, alufelgi i wspomaganie kierownicy, najrzadziej: blokada skrzyni biegAlw, klatka


## 9. Porównaj œredni¹ i medianê mocy KM miêdzy grupami modeli A, S i RS 
##    samochodów marki Audi

dfA <- auta2012 %>% filter(Marka == "Audi", grepl("^A", Model)) %>% select(Model, KM)
dfS <- auta2012 %>% filter(Marka == "Audi", grepl("^S", Model)) %>% select(Model, KM)
dfRS <- auta2012 %>% filter(Marka == "Audi", grepl("^(RS)", Model)) %>% select(Model, KM)

## Odp:

#Œrednia A

mean(dfA$KM, na.rm = TRUE)

#Mediana A

median(dfA$KM, na.rm = TRUE)

#Œrednia S

mean(dfS$KM, na.rm = TRUE)

#Mediana S

median(dfS$KM, na.rm = TRUE)

#Œrednia RS

mean(dfRS$KM, na.rm = TRUE)

#Mediana RS

median(dfRS$KM, na.rm = TRUE)


## 10. ZnajdŸ marki, których auta wystêpuj¹ w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla ka¿dej z tych marek.


## Odp: 
auta2012 %>% group_by(Marka) %>% summarise(Encounters = n()) %>% filter(Encounters > 10000) %>% inner_join(auta2012, by="Marka") %>% 
  group_by(Marka, Model) %>% summarise(Liczba = n()) %>% top_n(1, Liczba) %>%
  inner_join(auta2012, by = c("Marka" = "Marka", "Model" = "Model")) %>% group_by(Marka, Model, Kolor) %>% summarise(LiczbaKolorow = n()) %>% 
  top_n(1, LiczbaKolorow) %>% select(Marka, Model, Kolor)

