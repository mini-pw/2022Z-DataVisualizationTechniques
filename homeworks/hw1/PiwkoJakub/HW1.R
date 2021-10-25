library(PogromcyDanych)
library(dplyr)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))


## 1. Z którego rocznika jest najwięcej aut i ile ich jest?

auta2012 %>% 
  group_by(Rok.produkcji) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  head(1)
## Odp: Najwięcej aut jest z rocznika 2011. Jest ich 17418.


## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?

auta2012 %>% 
  filter(Rok.produkcji == 2011) %>% 
  group_by(Marka) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  head(1)
## Odp: Najczęściej wsród samochodów z 2011 roku występuje Skoda.


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?

auta2012 %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)" 
         & Rok.produkcji >= 2005 
         & Rok.produkcji <= 2011) %>% 
  summarise(count = n())
## Odp: Takich aut jest 59534.


## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?

auta2012 %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)" & Rok.produkcji == 2011) %>% 
  group_by(Marka) %>% 
  summarise(srednia_cena = mean(Cena, na.rm = TRUE)) %>% 
  arrange(desc(srednia_cena)) %>% 
  head(1)
## Odp: Średnio najdroższe jest Volvo.


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?

auta2012 %>% 
  filter(Marka == "Skoda" & Rok.produkcji == 2011) %>% 
  group_by(Model) %>% 
  summarise(srednia_cena = mean(Cena, na.rm = TRUE)) %>% 
  arrange(srednia_cena) %>% 
  head(1)
## Odp: Jest to Skoda Fabia


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?

auta2012 %>% 
  filter(Cena.w.PLN / KM > 600 & Liczba.drzwi == "2/3") %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  head(1)
## Odp: Najczęściej w tej grupie występuje automatyczna skrzynia biegów.


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?

diesel <- auta2012 %>% 
  filter(Marka == "Skoda" & Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Model) %>% 
  summarise(srednia_d = mean(Cena))
benzyna <- auta2012 %>% 
  filter(Marka == "Skoda" & Rodzaj.paliwa == "benzyna") %>% 
  group_by(Model) %>% 
  summarise(srednia_b = mean(Cena))
inner_join(diesel, benzyna) %>% 
  mutate(roznica = abs(srednia_b - srednia_d)) %>% 
  arrange(roznica) %>% 
  head(1)
## Odp: Najmniejsza różnica między średnimi cenami występuje dla modelu Praktik.


## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini

library(stringr)
wyposazenie <- auta2012 %>%
  filter(Marka == "Lamborghini") %>% 
  select("Wyposazenie.dodatkowe")
wyposazenie <- as.data.frame(table(as.vector(
  str_split(wyposazenie$Wyposazenie.dodatkowe, ", ", simplify = TRUE)))) 
wyposazenie %>% 
  filter(Var1 != "") %>%  
  filter(Freq == max(Freq) | Freq == min(Freq))
## Odp: Najczęściej występującym wyposażeniem jest ABS, alufelgi
## i wspomaganie kierownicy, natomiast najrzadziej: 
## blokada skrzyni biegów i klatka.


## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi

auta <- auta2012 %>% 
  filter(Marka == "Audi") %>% 
  mutate(grupa = "")
grupy <- c("A", "S", "RS")
for (str in grupy) {
  kolumna <- str_detect(auta$Model, str_glue("{str}(.)"))
  for (k in seq(length(kolumna))) {
    if (kolumna[k]) {
      auta[k, "grupa"] <- str
    }
  }
}
auta %>% 
  filter(grupa != "") %>% 
  group_by(grupa) %>% 
  summarise(mediana = median(KM, na.rm = TRUE), srednia = mean(KM, na.rm = TRUE))
## Odp:
#Grupa:   Mediana:  Średnia:
#   A         140       160
#   RS        450       500
#   S         344       344
## W przypadku grupy modeli "A" i "RS" środkowa wartość mocy KM jest mniejsza
## od średniej, natomiast dla grupy S te wartości są równe. 


## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
## Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.

find_answ <- function(x) {
  ## Funkcja wypisująca najpopularniejszy koloru
  ## wśród najpopularniejszego modelu wśród marki samochodu 
  ## podanej w wektorze napisów (argument x).
  mod <- auta2012 %>% 
    filter(Marka == x) %>% 
    group_by(Model) %>% 
    summarise(count = n()) %>%
    filter(count == max(count))
  kol <- auta2012 %>% 
    filter(Marka == x & Model == mod$Model) %>% 
    group_by(Kolor) %>% 
    summarise(count = n()) %>% 
    filter(Kolor != "") %>% 
    filter(count == max(count))
  res <- c(x, levels(droplevels(mod$Model)), levels(droplevels(kol$Kolor)))
  print(res)
}

mar <- auta2012 %>% 
  group_by(Marka) %>% 
  summarise(count = n()) %>% 
  filter(count > 10000)
cars <- levels(droplevels(mar$Marka))

for (i in cars) {
  find_answ(i)
}
## Odp: 
#  Marka:            Model:              Kolor:
# "Audi"             "A4"               "czarny-metallic"
# "BMW"              "320"              "srebrny-metallic"
# "Ford"             "Focus"            "srebrny-metallic"
# "Mercedes-Benz"    "C 220"            "srebrny-metallic"
# "Opel"             "Astra"            "srebrny-metallic"
# "Renault"          "Megane"           "srebrny-metallic"
# "Volkswagen"       "Passat"           "srebrny-metallic"
