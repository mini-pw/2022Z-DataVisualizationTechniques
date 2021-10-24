library(PogromcyDanych)
library(dplyr)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

## 1. Z którego rocznika jest najwięcej aut i ile ich jest?
auta2012 %>% 
  group_by(Rok.produkcji) %>% 
  summarise(ilosc=n()) %>% 
  arrange(-ilosc) %>% 
  head(1)

## Odp: 2011, 17418


## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?
auta2012 %>% 
  filter(auta2012$Rok.produkcji==2011) %>% 
  group_by(Marka) %>% 
  summarise(ilosc=n()) %>% 
  arrange(-ilosc) %>% 
  head(1)

## Odp: Skoda


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?
auta2012 %>% 
  filter(Rok.produkcji %in% 2005:2011, grepl('diesel', Rodzaj.paliwa, fixed = TRUE)) %>% 
  count()


## Odp: 59534


## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?
auta2012 %>% 
  filter(Rok.produkcji == 2011, grepl('diesel', Rodzaj.paliwa, fixed = TRUE)) %>% 
  mutate(cena.brutto = ifelse(Brutto.netto=='netto', Cena.w.PLN*1.23, Cena.w.PLN)) %>% 
  select(Marka, cena.brutto) %>% 
  group_by(Marka) %>% 
  summarise(srednia=mean(cena.brutto, na.rm=T)) %>%
  arrange(-srednia)

## Odp: Porsche


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?
auta2012 %>% 
  filter(Rok.produkcji == 2011, Marka == 'Skoda') %>% 
  mutate(cena.brutto = ifelse(Brutto.netto=='netto', Cena.w.PLN*1.23, Cena.w.PLN)) %>% 
  select(Model, cena.brutto) %>% 
  group_by(Model) %>% 
  summarise(srednia=mean(cena.brutto, na.rm=T)) %>% 
  arrange(srednia)

## Odp: Fabia


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?
auta2012 %>% 
  filter(Liczba.drzwi == '2/3' | Liczba.drzwi %in% 2:3) %>% 
  na.omit(Cena.w.PLN, KM) %>% 
  mutate(cena.brutto = ifelse(Brutto.netto=='netto', Cena.w.PLN*1.23, Cena.w.PLN)) %>% 
  mutate(stosunek = cena.brutto/KM) %>% 
  filter(stosunek > 600) %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(ilosc=n()) %>% 
  arrange(-ilosc)

## Odp: automatyczna


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?
tmp7_1 <- auta2012 %>% 
  filter(Marka == 'Skoda', grepl('diesel', Rodzaj.paliwa, fixed = TRUE)) %>% 
  mutate(cena.brutto = ifelse(Brutto.netto=='netto', Cena.w.PLN*1.23, Cena.w.PLN)) %>% 
  select(Model, cena.brutto) %>% 
  group_by(Model) %>% 
  summarise(srednia_diesel=mean(cena.brutto, na.rm=T))

tmp7_2 <- auta2012 %>% 
  filter(Marka == 'Skoda', Rodzaj.paliwa=='benzyna') %>% 
  mutate(cena.brutto = ifelse(Brutto.netto=='netto', Cena.w.PLN*1.23, Cena.w.PLN)) %>% 
  select(Model, cena.brutto) %>% 
  group_by(Model) %>% 
  summarise(srednia_benzyna=mean(cena.brutto, na.rm=T))

inner_join(tmp7_1, tmp7_2, by='Model') %>% 
  mutate(roznica = abs(srednia_diesel-srednia_benzyna)) %>% 
  select(Model, roznica) %>% 
  arrange(roznica)

#2spos
library(tidyr)
auta2012 %>% 
  filter(Marka == 'Skoda') %>% 
  mutate(cena.brutto = ifelse(Brutto.netto=='netto', Cena.w.PLN*1.23, Cena.w.PLN)) %>% 
  group_by(Model, Rodzaj.paliwa) %>% 
  summarise(sr_cena = mean(cena.brutto)) %>% 
  pivot_wider(names_from = Rodzaj.paliwa, values_from = sr_cena, values_fill = 0) %>% 
  mutate(roznica = abs(`olej napedowy (diesel)` - `benzyna`)) %>% 
  select(Model, roznica) %>% 
  arrange(roznica)


## Odp: Felicia


## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini

library(stringr)
auta2012_tmp <- auta2012 %>% 
  filter(Marka == "Lamborghini", Wyposazenie.dodatkowe!='')
tmp_8 <- str_split(auta2012_tmp$Wyposazenie.dodatkowe,", ")
unlist(tmp_8) %>% table %>% sort

## Odp: najczesciej: ABS, alufelgi, wspomaganie kierownicy
##      najrzadziej: blokata skrzyni biegow, klatka


## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi

auta2012 %>% 
  filter(Marka == 'Audi') %>% 
  mutate(seria = ifelse(substr(Model, 2, 2) == 'S', substr(Model, 1, 2), substr(Model, 1, 1))) %>%
  group_by(seria) %>%
  filter(seria == 'A' | seria == 'S' | seria == 'RS') %>%
  summarise(Srednia.moc = mean(KM, na.rm = T), Mediana.moc = median(KM, na.rm = T))

##                  Srednia      Mediana
## Odp: A            160.         140
##      RS           500.         450
##      S            344.         344


## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.

tmp_10_1 <- auta2012 %>% 
  group_by(Marka) %>% 
  count() %>% 
  filter(n > 10000)
tmp_10_1 <- pull(tmp_10_1[,'Marka'])

tmp_10_2 <- auta2012 %>% 
  select(Marka, Model) %>% 
  filter(Marka %in% tmp_10_1) %>% 
  group_by(Marka, Model) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(Marka) %>% 
  filter(n == max(n))

tmp_10_2 <- pull(tmp_10_2[,'Model'])

auta2012 %>% 
  select(Marka, Model, Kolor) %>% 
  filter(Marka %in% tmp_10_1, Model %in% tmp_10_2, Kolor != '') %>% 
  group_by(Marka, Model, Kolor) %>% 
  count() %>% 
  ungroup() %>%
  group_by(Marka) %>%
  filter(n == max(n))

## Odp:
#   Marka         Model     Kolor                
# 1 Audi          A4     czarny-metallic   
# 2 BMW           320    srebrny-metallic  
# 3 Ford          Focus  srebrny-metallic  
# 4 Mercedes-Benz C 220  srebrny-metallic  
# 5 Opel          Astra  srebrny-metallic 
# 6 Renault       Megane srebrny-metallic  
# 7 Volkswagen    Passat srebrny-metallic  