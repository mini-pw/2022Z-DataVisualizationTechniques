library(PogromcyDanych)
library(dplyr)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

## 1. Z którego rocznika jest najwięcej aut i ile ich jest?
  

## Odp: 

auta2012 %>% 
  group_by(Rok.produkcji) %>% 
  summarise(ilosc = n()) %>% 
  arrange(-ilosc) %>% 
  head(1)

## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?


## Odp:

auta2012 %>% 
  filter(Rok.produkcji == 2011) %>% 
  group_by(Marka) %>% 
  summarise(ilosc = n()) %>% 
  arrange(-ilosc) %>% 
  head(1)

## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?


## Odp:

auta2012 %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)", between(Rok.produkcji, 2005, 2011)) %>% 
  nrow()

## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?


## Odp:

auta2012 %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)", Rok.produkcji == 2011) %>% 
  group_by(Marka) %>% 
  summarise(Cena.w.PLN.srednia = mean(Cena.w.PLN)) %>% 
  arrange(-Cena.w.PLN.srednia) %>% 
  head(1)

## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?


## Odp:

auta2012 %>% 
  filter(Marka == "Skoda", Rok.produkcji == 2011) %>% 
  group_by(Model) %>% 
  summarise(Cena.w.PLN.srednia = mean(Cena.w.PLN)) %>% 
  arrange(Cena.w.PLN.srednia) %>% 
  head(1)

## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?


## Odp: 

auta2012 %>% 
  filter(Liczba.drzwi == "2/3", Cena.w.PLN / KM > 600) %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(ilosc = n()) %>% 
  arrange(-ilosc) %>% 
  head(1)

## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?
 

## Odp: 

library(tidyr)

auta2012 %>% 
  filter(Marka == "Skoda", Rodzaj.paliwa == "benzyna" | Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Model, Rodzaj.paliwa) %>% 
  summarise(srednia = mean(Cena.w.PLN)) %>% 
  pivot_wider(names_from = "Rodzaj.paliwa", values_from = "srednia") %>% 
  transmute(roznica = abs(benzyna - `olej napedowy (diesel)`)) %>% 
  arrange(roznica) %>% 
  head(1)

## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini


## Odp: 

library(stringr)

lamborghini <- auta2012 %>% 
  filter(Marka == "Lamborghini")
  
list <- str_split(lamborghini$Wyposazenie.dodatkowe, ", ")
wyposazenie <- unlist(list)

wyposazenie.ilosc <- data.frame(wyposazenie) %>% 
  group_by(wyposazenie) %>% 
  summarise(ilosc = n())

# najrzadziej
wyposazenie.ilosc %>% 
  arrange(ilosc) %>% 
  head(1)

# najczesciej
wyposazenie.ilosc %>% 
  arrange(-ilosc) %>% 
  head(1)

## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi


## Odp:

auta2012 %>% 
  filter(Marka == "Audi") %>% 
  mutate(Model.general = case_when(str_starts(Model, "A") ~ "A",
                                   str_starts(Model, "S") ~ "S",
                                   str_starts(Model, "RS") ~ "RS",
                                   TRUE ~ "Other")) %>%
  filter(Model.general != "Other") %>% 
  group_by(Model.general) %>% 
  summarise(srednia.moc = mean(KM, na.rm = T),
            mediana.moc = median(KM, na.rm = T),
            srednia.wieksza.od.mediany = srednia.moc > mediana.moc)
  
## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.


## Odp: 

popularne.marki <- auta2012 %>% 
  group_by(Marka) %>% 
  summarise(ilosc = n()) %>% 
  filter(ilosc > 10000) %>% 
  select(Marka)

popularne.marki
  
najpopularniejsze.modele <- auta2012 %>%
  filter(Marka %in% popularne.marki$Marka) %>%
  group_by(Marka, Model) %>%
  summarize(n = n()) %>% 
  top_n(1)

najpopularniejsze.modele %>% 
  inner_join(auta2012) %>% 
  group_by(Marka, Model, Kolor) %>% 
  summarise(n = n()) %>% 
  top_n(1) %>% 
  select(Marka, Model, Kolor)
  
