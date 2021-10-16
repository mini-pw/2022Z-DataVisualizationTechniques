library(PogromcyDanych)
library(dplyr)
library(stringi)
library(tidyr)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

## 1. Z którego rocznika jest najwięcej aut i ile ich jest?
auta2012 %>% 
    group_by(Rok.produkcji) %>% 
    summarise(ctx = n()) %>% 
    top_n(1)

## Odp: 
# 2011r. -- 17418 szt.

## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?

auta2012 %>% 
    filter(Rok.produkcji == 2011) %>% 
    group_by(Marka) %>% 
    summarise(ctx = n()) %>% 
    top_n(1)
## Odp:
# Skoda

## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?
auta2012 %>% 
    filter(
        Rok.produkcji > 2004,
        Rok.produkcji < 2012,
        Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
    summarise(ctx = n())


## Odp:
# 59534

## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?
auta2012 %>% 
    filter(
        Rok.produkcji == 2011,
        Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
    group_by(Marka) %>% 
    summarise(avgCost = mean(Cena.w.PLN)) %>% 
    top_n(1)
## Odp:
# Porsche

## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?

auta2012 %>% 
    filter(
        Marka == "Skoda",
        Rok.produkcji == 2011) %>% 
    group_by(Model) %>% 
    summarise(avgCost = mean(Cena.w.PLN)) %>% 
    top_n(-1)
## Odp:
# Fabia

## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?
auta2012 %>% 
    filter(Liczba.drzwi == "2/3") %>% 
    mutate(stosunek.ceny = Cena.w.PLN/KM) %>% 
    filter(stosunek.ceny > 600) %>% 
    group_by(Skrzynia.biegow) %>% 
    summarise(ctx = n()) %>% 
    top_n(1)

## Odp: 
# automatyczna

## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?
benzyniaki = auta2012 %>% 
    filter(
        Marka == "Skoda",
        Rodzaj.paliwa == "benzyna") %>% 
    group_by(Model) %>% 
    summarise(avgCost = mean(Cena.w.PLN))

auta2012 %>% 
    filter(
        Marka == "Skoda",
        Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
    group_by(Model) %>% 
    summarise(avgCost = mean(Cena.w.PLN)) %>% 
    inner_join(benzyniaki, by = "Model",suffix = c("Diesel","Benzyna")) %>% 
    mutate(diff = abs(avgCostDiesel - avgCostBenzyna)) %>% 
    slice_min(diff,n=1)
## Odp: 
#Felicia

## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini

auta2012 %>% 
    filter(
        Marka == "Lamborghini") %>% 
    select(Wyposazenie.dodatkowe) %>% 
    separate_rows(1,sep = ", ") %>% 
    group_by(Wyposazenie.dodatkowe) %>% 
    summarise(ctx = n()) %>% 
    filter(ctx == max(ctx) | ctx == min(ctx))

## Odp: 
#najczsciej: ABS, alufelgi, wspomaganie kierownicy
#najrzadziej: blokada skrzyni biegAlw, klatka

## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi
assign <- function (x) {
    ifelse(
        stri_detect_regex(str = x, pattern = '^A\\d'), "A",
        ifelse(
            stri_detect_regex(str = x, pattern = '^S\\d'), "S",
            ifelse(
                stri_detect_regex(str = x, pattern = '^RS\\d'), "RS","other")))
}
auta2012 %>% 
    filter(Marka == "Audi",
           is.na(KM) == FALSE) %>% 
    mutate(
        grupaModeli = assign(Model),
        .keep = "all") %>% 
    filter(grupaModeli != "other") %>% 
    group_by(grupaModeli) %>% 
    summarise(avgPower = mean(KM), medianPower = median(KM))


## Odp: grupa srednia   mediana
#        A      160        140
#       RS      500        450
#       S       344        344

## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.
marki <- auta2012 %>% 
    group_by(Marka) %>% 
    summarise(ctx = n()) %>% 
    filter(ctx >10000)

modele <- auta2012 %>% 
    group_by(Marka, Model) %>% 
    summarise(ctx = n()) %>% 
    arrange(desc(ctx)) %>% 
    right_join(marki, by = "Marka") %>% 
    filter(row_number() == 1)

auta2012 %>% 
    right_join(modele, by = c("Model","Marka")) %>% 
    group_by(Marka,Model,Kolor) %>% 
    summarise(ctx = n()) %>% 
    arrange(desc(ctx)) %>% 
    filter(row_number() == 1)



## Odp: 
#Marka         Model  Kolor              ctx
#<fct>         <fct>  <fct>            <int>
#1 Ford          Focus  srebrny-metallic  1711
#2 Opel          Astra  srebrny-metallic  1539
#3 Volkswagen    Passat srebrny-metallic  1466
#4 Audi          A4     czarny-metallic    853
#5 Renault       Megane srebrny-metallic   733
#6 BMW           320    srebrny-metallic   441
#7 Mercedes-Benz C 220  srebrny-metallic   205