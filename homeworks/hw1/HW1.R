library(PogromcyDanych)
library(dplyr)
library(tidyr)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
tail(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))
df <- auta2012
## 1. Z którego rocznika jest najwięcej aut i ile ich jest?
df %>% 
  group_by(Rok.produkcji) %>% 
  summarise(n = n()) %>% 
  top_n(1,n)

## Odp: Rok == 2011, liczba aut == 17418


## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?
df %>% 
  filter(Rok.produkcji == 2011) %>% 
  group_by(Marka) %>% 
  summarise(n = n()) %>% 
  top_n(1,n)

## Odp: Skoda  1288


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?
df %>% 
  filter((Rok.produkcji > 2004) & (Rok.produkcji < 2012) & (Rodzaj.paliwa == "olej napedowy (diesel)")) %>% 
  summarise(n())

## Odp: 59534


## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?
df %>% 
  filter((Rok.produkcji == 2011) & (Rodzaj.paliwa == "olej napedowy (diesel)")) %>% 
  group_by(Marka) %>% 
  summarise(avg_price = mean(Cena.w.PLN)) %>% 
  top_n(1,avg_price)

## Odp: Porsche


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?
df %>% 
  filter((Rok.produkcji == 2011) & (Marka == "Skoda")) %>% 
  group_by(Model) %>% 
  summarise(avg_price = mean(Cena.w.PLN)) %>% 
  top_n(1,-avg_price)

## Odp: Fabia


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?
df %>% 
  filter(Liczba.drzwi == "2/3") %>% 
  mutate(price2hp = Cena.w.PLN/KM) %>% 
  filter(price2hp > 600) %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(n = n()) %>% 
  top_n(1,n)

## Odp: automatyczna


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?
df2 <- df %>% 
  filter(Marka == "Skoda") %>% 
  group_by(Model,Rodzaj.paliwa) %>% 
  summarise(avg_price = mean(Cena.w.PLN))
benzynowe <- df2 %>% 
  filter(Rodzaj.paliwa == "benzyna")
diesle <- df2 %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)")
inner_join(benzynowe,diesle,by = "Model") %>% 
  mutate(difference = abs(avg_price.x - avg_price.y)) %>% 
  arrange(difference) %>% 
  head(1)
  
## Odp: Felicia


## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini
library(stringr)
wyposazenie <- df %>% 
  filter(Marka == "Lamborghini") %>% 
  select(Wyposazenie.dodatkowe) %>% 
  separate_rows(1,sep=",") %>% 
  group_by(Wyposazenie.dodatkowe) %>% 
  summarise(n = n())
wyposazenie %>% 
  top_n(1,n)
wyposazenie %>% 
  top_n(1,-n)

## Odp: Najczęściej: alufelgi, wspomaganie kierownicy, ABS
# Najrzadziej: blokada skrzyni biegów, klatka


## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi
df %>% 
  filter(Marka == "Audi") %>%
  mutate(model_group = case_when(grepl("^A.$", Model) ~ "A",
                                 grepl("^S.$", Model) ~ "S",
                                 grepl("^RS.$", Model) ~ "RS")) %>%
  drop_na(model_group) %>% 
  group_by(model_group) %>% 
  summarise(median = median(KM,na.rm = TRUE), avg = mean(KM,na.rm = TRUE))

## Odp:  model_group median   avg
#       <chr>        <dbl> <dbl>
#       1 A              140  158.
#       2 RS             450  500.
#       3 S              344  344.


## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.
df %>% 
  select(Marka,Model,Kolor) %>% 
  group_by(Marka) %>% 
  filter(n() > 10000) %>% 
  group_by(Marka,Model) %>%
  summarise(cnt = n()) %>%
  slice_max(order_by = cnt, n=1, with_ties = FALSE) %>% 
  inner_join(auta2012) %>% 
  select(Marka,Model,Kolor) %>%
  slice(-(Kolor == "")) %>%
  group_by(Marka,Model,Kolor) %>% 
  summarise(cnt2 = n()) %>% 
  slice_max(order_by = cnt2, n=1) 

## Odp:     Marka         Model  Kolor              cnt
#          <fct>         <fct>  <fct>            <int>
#         1 Audi          A4     czarny-metallic    853
#         2 BMW           320    srebrny-metallic   441
#         3 Ford          Focus  srebrny-metallic  1711
#         4 Mercedes-Benz C 220  srebrny-metallic   205
#         5 Opel          Astra  srebrny-metallic  1539
#         6 Renault       Scenic srebrny-metallic   819
#         7 Volkswagen    Passat srebrny-metallic  1466


