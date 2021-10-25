library(PogromcyDanych)
library(dplyr)
library(tidyr)
library(stringr)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

## 1. Z którego rocznika jest najwięcej aut i ile ich jest?

auta_rok <- auta2012 %>%
  group_by(Rok.produkcji)  %>%
  summarise(count_rok = n()) %>%
  arrange(-count_rok)

head(auta_rok)

## Odp: Rok: 2011, liczba aut: 17418.


## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?

auta_2011 <- auta2012 %>%
  filter(Rok.produkcji == "2011") %>%
  group_by(Marka) %>%
  summarise(count_marki = n()) %>%
  arrange(-count_marki)

head(auta_2011)

## Odp: Skoda.


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?

auta_dies_05_11 <- auta2012 %>%
  filter(Rodzaj.paliwa == "olej napedowy (diesel)", Rok.produkcji >= 2005, Rok.produkcji <= 2011) %>%
  arrange(-Rok.produkcji)

head(auta_dies_05_11)
dim(auta_dies_05_11)

## Odp: 59534.


## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?

auta_dies_11 <- auta2012 %>%
  filter(Rodzaj.paliwa == "olej napedowy (diesel)", Rok.produkcji == 2011) %>%
  group_by(Marka) %>%
  summarise(mean_cena = mean(Cena.w.PLN)) %>%
  arrange(-mean_cena)

head(auta_dies_11)

## Odp: Porsche.


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?

auta_skoda_11 <- auta2012 %>%
  filter(Marka == "Skoda", Rok.produkcji == 2011) %>%
  group_by(Model) %>%
  summarise(mean_cena = mean(Cena.w.PLN)) %>%
  arrange(mean_cena)

head(auta_skoda_11)

## Odp: Fabia.


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?

auta_pln_km <- auta2012 %>%
  mutate(pln_km = Cena.w.PLN / KM) %>%
  filter(Liczba.drzwi == "2/3", pln_km > 600) %>%
  group_by(Skrzynia.biegow) %>%
  summarise(count_skb = n())

auta_pln_km

## Odp: automatyczna.


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?

auta_skoda <- auta2012 %>%
  filter(Marka == "Skoda", Rodzaj.paliwa == "olej napedowy (diesel)" | Rodzaj.paliwa == "benzyna") %>%
  group_by(Model,Rodzaj.paliwa) %>%
  summarise(mean_cena = mean(Cena.w.PLN)) %>%
  group_by(Model) %>%
  mutate(max_ = max(mean_cena), min_ = min(mean_cena), count = n()) %>%
  filter(count > 1) %>%
  distinct(dif = max_ - min_) %>%
  arrange(dif)

head(auta_skoda)

## Odp: Felicia.


## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini

# sp1
wypos_lambo <- auta2012 %>%
  filter(Marka == "Lamborghini") %>%
  select(Wyposazenie.dodatkowe) %>%
  separate(Wyposazenie.dodatkowe, sep = ", ", into = as.character(1:32)) %>% # sredni sposob
  pivot_longer(everything(),names_to = "lp", values_to = "wyposazenie") %>%
  group_by(wyposazenie) %>%
  summarise(count_wyp = n()) %>%
  arrange(count_wyp)

head(wypos_lambo)
tail(wypos_lambo)

# sp2
# wypos_lambo <- auta2012 %>%
#   filter(Marka == "Lamborghini", Wyposazenie.dodatkowe != "") %>%
#   select(Wyposazenie.dodatkowe)
# str_split(wypos_lambo$Wyposazenie.dodatkowe, ", ") %>%
#   unlist() %>%
#   table() %>%
#   sort()

## Odp: Najrzadziej - blokada skrzyni biegów, klatka; najczęściej - ABS, alufelgi, wspomaganie kierownicy.


## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi

auta_audi <- auta2012 %>%
  filter(Marka == "Audi") %>%
  mutate(grupa = case_when(str_detect(Model, "^A") ~ "A",
                           str_detect(Model, "^S") ~ "S",
                           str_detect(Model, "^RS") ~ "RS",
                           TRUE ~ "Other")) %>%
  group_by(grupa) %>%
  summarise(mean_KM = mean(KM, na.rm = TRUE), med_KM = median(KM, na.rm = TRUE)) %>%
  filter(grupa != "Other")

auta_audi

## Odp: grupa mean_KM med_KM
#       A     160.    140
#       RS    500.    450
#       S     344.    344


## 10. Znajdź marki, których auta występują w danych ponad 10 000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.

auta_m10000 <- auta2012 %>%
  group_by(Marka) %>%
  summarise(count_marki = n()) %>%
  filter(count_marki > 10000) 

#auta_m10000

auta_najpop <- inner_join(auta2012, auta_m10000, by="Marka") %>%
  group_by(Marka, Model) %>%
  summarise(count_model = n())

auta_najpop0 <- auta_najpop %>%
  summarise(max_model = max(count_model))

auta_najpop <- inner_join(auta_najpop, auta_najpop0, by=c("Marka"="Marka", "count_model"="max_model"))

#auta_najpop

auta_kolor <- inner_join(auta2012, auta_najpop, by=c("Marka","Model")) %>%
  group_by(Kolor,Marka, Model) %>%
  summarise(count_kolor = n())

auta_kolor0 <- auta_kolor %>%
  group_by(Model) %>%
  summarise(max_count_kolor = max(count_kolor))

auta_kolor <- inner_join(auta_kolor, auta_kolor0, by=c("Model"="Model", "count_kolor"="max_count_kolor")) %>%
  select(Marka,Model,Kolor,count_kolor)

auta_kolor

## Odp: Marka         Model  Kolor            count_kolor
#       Audi          A4     czarny-metallic          853
#       BMW           320    srebrny-metallic         441
#       Ford          Focus  srebrny-metallic        1711
#       Mercedes-Benz C 220  srebrny-metallic         205
#       Opel          Astra  srebrny-metallic        1539
#       Renault       Megane srebrny-metallic         733
#       Volkswagen    Passat srebrny-metallic        1466