library(PogromcyDanych)
library(dplyr)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

## 1. Z którego rocznika jest najwięcej aut i ile ich jest?

auta2012 %>% group_by(Rok.produkcji) %>% summarise(count = n()) %>% arrange(desc(count)) %>% slice(1)

## Odp: Najwięcej aut jest z rocznika 2011, jest ich 17418.


## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?

auta2012 %>% filter(Rok.produkcji == 2011) %>% group_by(Marka) %>%
  summarise(count = n()) %>% arrange(desc(count)) %>% slice(1) %>% select(Marka)

## Odp:Skoda.


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?

auta2012 %>% filter(Rok.produkcji >= 2005 & Rok.produkcji <= 2011 & Rodzaj.paliwa == "olej napedowy (diesel)") %>% count() 


## Odp: 59534.


## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?

auta2012 %>% filter(Rok.produkcji == 2011 & Rodzaj.paliwa == "olej napedowy (diesel)") %>%
  group_by(Marka) %>% summarise(SumPrice = sum(Cena.w.PLN), Count = n()) %>%  
  mutate(MeanPrice = SumPrice/Count) %>% arrange(desc(MeanPrice)) %>% slice(1) %>% select(Marka)

## Odp: Porsche.


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?

auta2012 %>% filter(Rok.produkcji == 2011 & Marka == "Skoda") %>%
  group_by(Model) %>% summarise(SumPrice = sum(Cena.w.PLN), Count = n()) %>%  
  mutate(MeanPrice = SumPrice/Count) %>% arrange(MeanPrice) %>% slice(1) %>% select(Model)

## Odp: Fabia.


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?

auta2012 %>% filter(Liczba.drzwi == "2/3" & Cena.w.PLN/KM > 600) %>%
  group_by(Skrzynia.biegow) %>% summarise(Count = n()) %>% arrange(desc(Count)) %>%
  slice(1) %>% select(Skrzynia.biegow)

## Odp: Automatyczna.


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?

df1 <- auta2012 %>% filter(Marka == "Skoda" & Rodzaj.paliwa == "olej napedowy (diesel)") %>%
    group_by(Model) %>% summarise(SumPrice = sum(Cena.w.PLN), Count = n()) %>%
    mutate(MeanPrice =  SumPrice / Count) %>% select(Model, MeanPrice)

df2 <- auta2012 %>% filter(Marka == "Skoda" & Rodzaj.paliwa == "benzyna") %>%
    group_by(Model) %>% summarise(SumPrice = sum(Cena.w.PLN), Count = n()) %>%
    mutate(MeanPrice =  SumPrice / Count) %>% select(Model, MeanPrice)

    inner_join(df1, df2, by="Model") %>% mutate(Difference = abs(MeanPrice.x - MeanPrice.y)) %>%
    arrange(Difference) %>% slice(1) %>% select(Model)
      
## Odp: Felicia.


## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini
library(stringi)
    
wyp <- auta2012 %>% filter(Marka == "Lamborghini")
wyp <- unlist(lapply(wyp$Wyposazenie.dodatkowe, function(x) stri_split_fixed(x, ',', omit_empty=TRUE, simplify=TRUE)))
wyp <- table(wyp)
max_wyp <- max(wyp)
min_wyp <- min(wyp)
wyp <- as.data.frame(wyp)
wyp %>% filter(Freq == max_wyp) %>% select(wyp)
wyp %>% filter(Freq == min_wyp) %>% select(wyp)

## Odp: Najczęściej występujące wyposażenie : alufelgi, wspomaganie kierownicy, ABS.
## Najrzadziej wystepujące: blokada skrzyni biegów, klatka.


## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi

audi <- auta2012 %>% filter(Marka == "Audi")
audi$Model <- as.character(audi$Model)
audiA <- subset(audi, grepl("^A.", Model))
audiS <- subset(audi, grepl("^S.", Model))
audiRS <- subset(audi, grepl("^RS.", Model))
mean(audiA$KM, na.rm = TRUE)
median(audiA$KM, na.rm = TRUE)
mean(audiS$KM, na.rm = TRUE)
median(audiS$KM, na.rm = TRUE)
mean(audiRS$KM, na.rm = TRUE)
median(audiRS$KM, na.rm = TRUE)
## Odp:Audi A średnia: 159.5799, mediana: 140
    ##Audi S średnia: 343.7371, mediana: 344
    ##Audi RS średnia: 500.0282, mediana: 450

## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.

auta2012 %>% group_by(Marka) %>% summarise(Count = n()) %>% filter(Count > 10000) %>%
  select(Marka) %>% inner_join(auta2012, by = "Marka") %>% group_by(Marka, Model) %>% 
  summarise(Count = n()) %>% top_n(1, Count) %>% select(Model) %>% inner_join(auta2012, by = c("Marka", "Model")) %>%
  group_by(Marka, Model, Kolor) %>% summarise(Count = n()) %>% top_n(1, Count)

## Odp: 
## Marka          Model  Najczęstszy Kolor
##
## BMW            320    srebrny-metallic
## Audi           A4     czarny-metallic
## Opel           Astra  srebrny-metallic
## Mercedes-Benz  C 220  srebrny-metallic 
## Ford           Focus  srebrny-metallic
## Renault        Megane srebrny-metallic
## Volkswagen     Passat srebrny-metallic