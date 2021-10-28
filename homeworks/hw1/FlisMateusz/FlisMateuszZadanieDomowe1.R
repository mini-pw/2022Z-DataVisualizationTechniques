install.packages("PogromcyDanych")
library(PogromcyDanych)
library(dplyr)
library(stringr)
colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))
View(auta2012)

## 1. Z którego rocznika jest najwiêcej aut i ile ich jest?

auta2012 %>% 
  group_by(Rok.produkcji) %>%
  summarise(n = n()) %>% 
  top_n(1,n)

## Odp: Z roku 2011 jest ich najwiêcej. Dok³adnie 17418.


## 2. Która marka samochodu wystêpuje najczêœciej wœród aut wyprodukowanych w 2011 roku?

auta2012 %>% 
  filter(Rok.produkcji==2011) %>% 
  group_by(Marka) %>% 
  summarise(n = n()) %>% 
  top_n(1,n)

## Odp: Skoda wystêpuje najczêœciej wœród aut wyprodukowanych w 2011 roku.


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?

auta2012 %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)" & Rok.produkcji > 2004 & Rok.produkcji < 2012) %>% 
  count()
  

## Odp: Takich samochodów jest 59534.


## 4. Spoœród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest œrednio najdro¿sza?

auta2012 %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)" & Rok.produkcji == 2011) %>%
  group_by(Marka) %>% 
  summarise(SredniKoszt = mean(Cena.w.PLN)) %>% 
  top_n(1,SredniKoszt)

## Odp: Spoœród aut z silnikiem diesla wyprodukowanych w 2011 roku, Porsche jest œrednio najdro¿sze.


## 5. Spoœród aut marki Skoda wyprodukowanych w 2011 roku, który model jest œrednio najtañszy?

auta2012 %>% 
  filter(Marka=="Skoda" & Rok.produkcji == 2011) %>%
  group_by(Model) %>% 
  summarise(SredniKoszt = mean(Cena.w.PLN)) %>% 
  top_n(1,-SredniKoszt)

## Odp: Spoœród aut marki Skoda wyprodukowanych w 2011 roku, model Fabia jest najtañszy.


## 6. Która skrzynia biegów wystêpuje najczêœciej wœród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?

auta2012 %>% 
  mutate(StosunekCenyDoKM = (Cena.w.PLN/KM)) %>% 
  filter(StosunekCenyDoKM > 600 & Liczba.drzwi=="2/3") %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(n = n()) %>% 
  top_n(1,n)
  
## Odp: Automatyczna skrzynia biegów wystêpuje najczêœciej wœród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600


## 7. Spoœród aut marki Skoda, który model ma najmniejsz¹ ró¿nicê œrednich cen 
##    miêdzy samochodami z silnikiem benzynowym, a diesel?

baza1 <- auta2012 %>% 
  filter(Marka=="Skoda" & Rodzaj.paliwa=="olej napedowy (diesel)") %>% 
  group_by(Model) %>% 
  summarise(SredniaCenaOlej = mean(Cena.w.PLN))

baza2 <- auta2012 %>% 
  filter(Marka=="Skoda" & (Rodzaj.paliwa=="benzyna" | Rodzaj.paliwa=="benzyna+LPG")) %>% 
  group_by(Model) %>% 
  summarise(SredniaCenaBenzyna = mean(Cena.w.PLN))

inner_join(baza1,baza2,by=c("Model")) %>% 
  filter(Model != "inny") %>% 
  mutate(Roznica = abs(SredniaCenaBenzyna - SredniaCenaOlej)) %>% 
  top_n(1,-Roznica)

## Odp:  Spoœród aut marki Skoda, model Favorit ma najmniejsz¹ ró¿nicê œrednich cen 
##    miêdzy samochodami z silnikiem benzynowym, a diesel. Ro¿nica jest równa 132z³.


## 8. ZnajdŸ najrzadziej i najczêœciej wystêpuj¹ce wyposa¿enie/a dodatkowe 
##    samochodów marki Lamborghini


lambo <- auta2012 %>% 
  filter(Marka=="Lamborghini") 

wyposazenia <- unique(str_split(lambo$Wyposazenie.dodatkowe,pattern = ", ",simplify = FALSE))



## Odp: 


## 9. Porównaj œredni¹ i medianê mocy KM miêdzy grupami modeli A, S i RS 
##    samochodów marki Audi

auta2012 %>% 
  filter(Marka == "Audi") %>% 
  mutate(ModelNew = case_when(str_detect(Model,"RS") ~ "RS",
                              str_detect(Model,"S") ~ "S",
                              str_detect(Model,"A") ~ "A",
                              TRUE ~ "Other")) %>% 
  select(Marka,ModelNew,Model,KM) %>%
  filter(ModelNew!="Other") %>% 
  group_by(ModelNew) %>% 
  summarise(SredniaKM = mean(KM,na.rm=TRUE), MedianaKM = median(KM,na.rm=TRUE))
  
  


## Odp: œrednia i mediana mocy KM: 
## Dla grupy A: œrednia wiêksza od mediany,
## Dla grupy RS: œrednia wiêksza od mediany,
## Dla grupy S: œrednia równa medianie.


## 10. ZnajdŸ marki, których auta wystêpuj¹ w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla ka¿dej z tych marek.

auta2012 %>% 
  group_by(Marka) %>% 
  summarise(n = n()) %>% 
  filter(n > 10000) %>% 
  select(Marka)

auta2012 %>% 
  filter(Marka == "Audi" | Marka == "BMW" | Marka == "Ford" | Marka == "Mercedes-Benz" | Marka == "Opel" | Marka == "Renault" | Marka == "Volkswagen") %>% 
  group_by(Marka) %>% 
  count(Model,Kolor) %>% 
  slice(which.max(n))

## Odp: Marka - Model - kolor;
## Audi - A4 - czarny metaliczny;
## BMW - 320 - srebrny metaliczny;
## Ford - Focus - srebrny metaliczny;
## Mercedes-Benz - C 220 - srebrny metaliczny;
## Opel - Astra - srebrny metaliczny;
## Renault - Scenic - srebrny metaliczny;
## Volkswagen - Passat - srebrny metaliczny.