library(PogromcyDanych)
library(dplyr)
library(stringr)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

auta <- auta2012 

## 1. Z którego rocznika jest najwięcej aut i ile ich jest?

auta %>% 
  select(Rok.produkcji) %>% 
  table() %>% 
  sort() %>% 
  tail(1)

## Odp: Najwięcej aut jest z 2011 i jest ich 17418


## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?

auta %>% 
  filter(Rok.produkcji == 2012) %>% 
  select(Marka) %>% 
  table() %>% 
  sort() %>% 
  tail(1)

## Odp:  Audi - 288 razy


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?

auta %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)" & Rok.produkcji>=2005 & Rok.produkcji<=2011) %>% 
  length()

## Odp: 21


## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?

auta %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)" &  Rok.produkcji==2011) %>%
  mutate(brutto.price = case_when(Brutto.netto == "netto" ~ Cena.w.PLN * 1.23,
                                  TRUE ~ Cena.w.PLN)) %>% 
  select(c(brutto.price, Marka)) %>% 
  group_by(Marka) %>%
  summarise(mean = mean(brutto.price, na.rm = TRUE)) %>% 
  arrange(-mean) %>% 
  head(1)


## Odp: Porsche - średnio kosztuje 401,201 pln brutto


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?

auta %>% 
  filter(Marka == "Skoda" &  Rok.produkcji==2011) %>%
  mutate(brutto.price = case_when(Brutto.netto == "netto" ~ Cena.w.PLN * 1.23,
                                  TRUE ~ Cena.w.PLN)) %>% 
  select(c(brutto.price, Model)) %>% 
  group_by(Model) %>% 
  summarise(mean = mean(brutto.price)) %>% 
  arrange(mean) %>% 
  head(1)
  
 
## Odp: Fabia - średnio kosztuje 42,544 pln brutto


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?

auta %>% 
  mutate(stos = Cena.w.PLN/KM) %>% 
  filter(Liczba.drzwi == "2/3" & stos > 600) %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  head(1)



## Odp: automatyczna - 708 rrazy


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?

auta %>% 
  filter(Marka == "Skoda") %>% 
  mutate(brutto.price = case_when(Brutto.netto == "netto" ~ Cena.w.PLN * 1.23,
                                                          TRUE ~ Cena.w.PLN)) %>%
  group_by(Model,Rodzaj.paliwa) %>% 
  summarise(m = mean(brutto.price)) %>% 
  filter(Rodzaj.paliwa == "benzyna" | Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  mutate(new_p = case_when(Rodzaj.paliwa == "benzyna" ~ m,
                           Rodzaj.paliwa == "olej napedowy (diesel)" ~ -m)) %>% 
  select(Model, new_p) %>% 
  group_by(Model) %>% 
  summarise(s = sum(new_p)) %>% 
  mutate(abs = abs(s)) %>% 
  arrange(abs) %>% 
  head(1)




## Odp: Felicia, różnica wynosi 104 pln dla cen brutto


## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini

auta %>% 
  filter(Marka == "Lamborghini") %>% 
  select(Wyposazenie.dodatkowe) -> w 
  
  as.character(w$Wyposazenie.dodatkowe) %>% 
  str_split(", ") %>% 
  unlist() %>% 
  table() %>% 
  sort() %>% 
  View()



## Odp: najrzadsze: blokada skrzyni biegów, klatka, - 1 raz
##      najczęstsze: alufelgi, wspomaganie kierownicy, ABS - 18 razy


## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi
  
  auta %>% 
    filter(Marka == "Audi") %>% 
    mutate(modele = case_when(grepl("^A.", Model) ~ "A",
                              grepl("^S.", Model) ~ "S",
                              grepl("^RS.", Model) ~ "RS",
                              TRUE ~ "WE")) %>% 
    filter(modele!="WE") %>%
    select(c(KM, modele)) %>%
    group_by(modele) %>% 
    summarise(sre = mean(KM, na.rm = TRUE), med = median(KM, na.rm = TRUE)) %>% View()
  
    

## Odp: Średnia - A : 159.5799, RS : 500.0282, S : 343.7371
##      Mediana - A : 140,      RS : 450,      S : 344


## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.
  
  auta %>% 
    select(Marka, Model, Kolor) %>% 
    group_by(Marka) %>%
    mutate(marki = n()) %>% 
    filter(marki > 10000) %>%
    filter(Marka %in% choos) %>% 
    group_by(Marka,Model) %>%
    mutate(model = n()) %>%
    group_by(Marka) %>% 
    filter(model == max(model)) %>%
    group_by(Marka, Kolor) %>% 
    mutate(kolor = n()) %>% 
    group_by(Marka) %>% 
    filter(kolor == max(kolor)) %>%  
    unique() -> x
    x[c('Marka','Model','Kolor')]

## Odp:   -Marka-    -Model-     -Kolor-
##      Ford          Focus  srebrny-metallic
##      Volkswagen    Passat srebrny-metallic
##      Opel          Astra  srebrny-metallic
##      BMW           320    srebrny-metallic
##      Renault       Megane srebrny-metallic
##      Mercedes-Benz C 220  srebrny-metallic
##      Audi          A4     czarny-metallic