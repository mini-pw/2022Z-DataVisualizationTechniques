library(PogromcyDanych)
library(dplyr)
library(stringr)
library(stringi)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

## 1. Z którego rocznika jest najwięcej aut i ile ich jest?
df <- auta2012
df %>% 
  group_by(Rok.produkcji) %>% 
  summarise(ile=n()) %>% 
  arrange(-ile) %>% 
  head(1)
  
## Odp: 
#Z rocznika 2011, 17418 aut.

## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?
df %>% 
  filter(Rok.produkcji == 2011) %>% 
  group_by(Marka) %>% 
  summarise(ile=n()) %>% 
  arrange(-ile) %>% 
  head(1)

## Odp:
#Skoda

## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?
df %>% 
  filter(Rok.produkcji >= 2005, Rok.produkcji <= 2011) %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  count()


## Odp:
#59534

## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?
df %>% 
  filter(Rok.produkcji == 2011, Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Marka) %>% 
  summarise(srednia = mean(Cena.w.PLN)) %>% 
  arrange(-srednia) %>% 
  head(1)
## Odp:
#Porsche

## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?
df %>% 
  filter(Rok.produkcji == 2011, Marka == "Skoda") %>% 
  group_by(Model) %>% 
  summarise(srednia = mean(Cena.w.PLN)) %>% 
  arrange(srednia) %>% 
  head(1)
## Odp:
#Fabia

## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?
df %>% 
  mutate(stosunek_cena_moc = Cena.w.PLN/KM) %>% 
  filter(Liczba.drzwi == "2/3", stosunek_cena_moc > 600) %>% 
  group_by(Skrzynia.biegow) %>%
  summarise(ile_danej_skrzyni = n()) %>% 
  arrange(-ile_danej_skrzyni) %>% 
  head(1)
## Odp: 
#Automatyczna

## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?
benzyna <-  df %>% 
              filter(Marka == "Skoda", Rodzaj.paliwa==c("benzyna","benzyna+LPG")) %>% 
              group_by(Model) %>% 
              summarise(srednia = mean(Cena.w.PLN))
diesel <- df %>% 
            filter(Marka == "Skoda", Rodzaj.paliwa=="olej napedowy (diesel)") %>% 
            group_by(Model) %>% 
            summarise(srednia = mean(Cena.w.PLN))
inner_join(benzyna, diesel, by="Model") %>% 
  rename(srednia_benzyna=srednia.x, srednia_diesel=srednia.y) %>% 
  mutate(roznica = abs(srednia_benzyna - srednia_diesel)) %>% 
  arrange(roznica) %>% 
  select(Model, roznica) %>% 
  head(1)
## Odp: 
#Favorit

## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini
df %>% 
  filter(Marka=='Lamborghini') %>% 
  .$Wyposazenie.dodatkowe %>% 
  str_split(pattern = ",",simplify = TRUE) %>% 
  stri_remove_empty() %>% 
  table()

## Odp: 
#Wspomaganie kierownicy, ABS, alufelgi

## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi
audi<- df %>%
  filter(Marka=="Audi")
modele_z_a <- audi %>% 
  filter(str_detect(Model, "^A")) %>% 
  mutate(srednia=mean(KM, na.rm=TRUE)) %>% 
  mutate(mediana=median(KM, na.rm=TRUE))
modele_z_s <- audi %>% 
  filter(str_detect(Model, "^S")) %>% 
  mutate(srednia=mean(KM,na.rm=TRUE)) %>% 
  mutate(mediana=median(KM,na.rm=TRUE))
modele_z_rs <- audi %>% 
  filter(str_detect(Model, "^RS")) %>% 
  mutate(srednia=mean(KM,na.rm=TRUE)) %>% 
  mutate(mediana=median(KM,na.rm=TRUE))
modele_z_a      

## Odp:
#Dla modeli z grupy A: średnia=159.5799, mediana=140
#z grupy S: średnia=343.7371, mediana=344
#z grupy RS: średnia=500.0282, mediana=450

## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.
najpopularniejsze_modele<- df %>% 
  group_by(Marka) %>% 
  mutate(IleWystapienMarki = n()) %>% 
  filter(IleWystapienMarki > 10000) %>% 
  group_by(Model) %>% 
  mutate(IleModelu = n()) %>% 
  select(Marka, Model, IleModelu) %>% 
  ungroup(Model) %>% 
  group_by(Marka) %>% 
  mutate(NajpopularniejszyModel = max(IleModelu)) %>% 
  filter(IleModelu == NajpopularniejszyModel) %>% 
  distinct() %>% 
  select(Marka, Model, NajpopularniejszyModel, IleModelu)
df %>% 
  group_by(Marka, Model, Kolor) %>% 
  summarise(ile_danego_koloru = n()) %>% 
  arrange(-ile_danego_koloru) %>% 
  inner_join(y = najpopularniejsze_modele, by = 'Model') %>% 
  as.data.frame()
## Odp: 
# Ford Focus, Opel Astra, Volkswagen Passat, Renault Megane, BMW 320, Mecedes-Benz 320: srebrny-metallic
# Audi A4: czarny-metallic