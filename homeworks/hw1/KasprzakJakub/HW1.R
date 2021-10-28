library(PogromcyDanych)
library(dplyr)
library(tidyr)
library(stringr)

## 1. Z którego rocznika jest najwięcej aut i ile ich jest?


table(auta2012$Rok.produkcji)[which.max(table(auta2012$Rok.produkcji))]

## Odp: 2011 rok, 17418


## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?

auta2012 %>% 
  filter(Rok.produkcji == 2011) %>% 
    group_by(Marka) %>%
     summarise(Ilosc = n()) %>% 
        top_n(1, Ilosc)

## Odp: Skoda


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?

auta2012 %>% 
  select(Rodzaj.paliwa, Rok.produkcji) %>% 
    filter(auta2012$Rok.produkcji >= 2005, auta2012$Rok.produkcji <= 2011, 
         auta2012$Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
      nrow()

## Odp: 59534


## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?

auta2012[, c("Cena.w.PLN", "Marka")] %>% 
  filter(auta2012$Rok.produkcji == 2011, 
         auta2012$Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
    group_by(Marka) %>% 
      summarise(AvgPrice = mean(Cena.w.PLN)) %>% 
        arrange(desc(AvgPrice)) %>% 
          top_n(1)

## Odp: Porsche


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?
auta2012 %>% 
  filter(Marka == "Skoda",
         Rok.produkcji == 2011) %>% 
    group_by(Model) %>% 
      summarise(Srednia.cena = mean(Cena.w.PLN)) %>% 
        arrange(Srednia.cena) %>% 
          head(1)

## Odp: Fabia


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?

auta2012 %>% 
  filter(Liczba.drzwi == "2/3", Cena.w.PLN / KM  > 600) %>% 
    group_by(Skrzynia.biegow) %>% 
      summarise(Ilosc = n()) %>% 
        top_n(1, Ilosc)

## Odp: Automatyczna


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?

auta2012 %>% 
  filter(Marka == "Skoda",
         Rodzaj.paliwa == "benzyna" 
         | Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
    group_by(Model, Rodzaj.paliwa) %>% 
      summarise(Srednia.cena = mean(Cena.w.PLN)) %>% 
        filter(n() == 2) %>% 
          summarise(Diff = abs(Srednia.cena - lag(Srednia.cena))) %>% 
           na.omit() %>% 
             ungroup() %>% 
               filter(Diff == min(Diff))

## Odp: Felicia


## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini

auta2012 %>% 
  filter(Marka == "Lamborghini") %>%
    select(Wyposazenie.dodatkowe) %>% 
      separate_rows(1, sep = ", ") %>% 
        group_by(Wyposazenie.dodatkowe) %>% 
          summarise(Ilosc = n()) %>% 
            filter(Ilosc == max(Ilosc) | Ilosc == min(Ilosc)) %>% 
              arrange(Ilosc)

## Odp: Najrzadziej: blokada skrzyni biegów, klatka
##      Najczęściej: ABS, alufelgi, wspomaganie kierownicy


## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi

x <- auta2012 %>% 
  filter(Marka == "Audi") %>% 
    select(Model, KM) %>% 
      na.omit()

x$Model <- sub("A.+", "A", x$Model)
x$Model <- sub("S.+", "S", x$Model)
x$Model <- sub("RS.+", "RS", x$Model)

filter(x, Model == "A" | Model == "S" | Model == "RS") %>% 
  group_by(Model) %>% 
    mutate(Srednia = mean(KM), Mediana = median(KM)) %>% 
      select(Model, Srednia, Mediana) %>% 
        unique()
  
  

## Odp:
# Grupa    Srednia  Mediana
# A        160.     140
# S        344.     344
# RS       500.     450


## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.

auta2012 %>% 
  select(Marka, Model, Kolor) %>% 
    group_by(Marka) %>% 
      filter(n() > 10000) %>% 
        group_by(Marka, Model) %>% 
          mutate(Ilosc = n()) %>%
            group_by(Marka, Model,Kolor) %>% 
              mutate(Ilosc.kolor = n()) %>% 
                group_by(Marka) %>% 
                  unique() %>% 
                    filter(Ilosc == max(Ilosc)) %>% 
                      filter(Ilosc.kolor == max(Ilosc.kolor)) 


## Odp:
# Marka         Model  Kolor             Ilosc       Ilosc.kolor
# Ford          Focus  srebrny-metallic  5691        1711
# Volkswagen    Passat srebrny-metallic  6883        1466
# Opel          Astra  srebrny-metallic  6348        1539
# BMW           320    srebrny-metallic  1791         441
# Renault       Megane srebrny-metallic  3759         733
# Mercedes-Benz C 220  srebrny-metallic   667         205
# Audi          A4     czarny-metallic   4280         853