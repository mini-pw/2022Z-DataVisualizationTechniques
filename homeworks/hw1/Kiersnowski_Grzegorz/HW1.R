library(PogromcyDanych)
library(dplyr)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

## 1. Z którego rocznika jest najwiecej aut i ile ich jest?
auta2012 %>% 
  count(Rok.produkcji) %>% 
  arrange(desc(n)) %>% 
  head()

## Odp: 
#2011r 17418 aut

## 2. Która marka samochodu wystepuje najczesciej wsród aut wyprodukowanych w 2011 roku?
auta2012 %>% 
  filter(Rok.produkcji==2011) %>% 
  count(Marka) %>% 
  arrange(desc(n)) %>% 
  head()
  

## Odp:
#Skoda 1288

## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?
auta2012 %>% 
  filter(Rok.produkcji >= 2005,
         Rok.produkcji <= 2011, 
         Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  count()

## Odp: 59534


## 4. Sposród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest srednio najdrozsza?
auta2012 %>% 
  filter(Rok.produkcji==2011,
         Rodzaj.paliwa == "olej napedowy (diesel)") %>%
  group_by(Marka) %>% 
  summarize(avg.prize = mean(Cena.w.PLN)) %>% 
  arrange(desc(avg.prize))

## Odp: Porsche         345669 zl


## 5. Sposród aut marki Skoda wyprodukowanych w 2011 roku, który model jest srednio najtanszy?
auta2012 %>% 
  filter(Rok.produkcji==2011,
         Marka=='Skoda') %>% 
  group_by(Model) %>% 
  summarize(avg.prize = mean(Cena.w.PLN)) %>% 
  arrange(desc(-avg.prize))

## Odp: Fabia       42015.


## 6. Która skrzynia biegów wystepuje najczesciej wsród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?
auta2012 %>% 
  filter(Liczba.drzwi == "2/3") %>% 
  summarize(Skrzynia.biegow, 
            wazna.kolumna = Cena.w.PLN/KM) %>% 
  filter(wazna.kolumna > 600) %>% 
  count(Skrzynia.biegow)

  

## Odp: automatyczna 708


## 7. Sposród aut marki Skoda, który model ma najmniejsza róznice srednich cen 
##    miedzy samochodami z silnikiem benzynowym, a diesel?
auta2012 %>% 
  filter(Marka == "Skoda") %>% 
  mutate(Diesel = (Rodzaj.paliwa == 'olej napedowy (diesel)'),
         benzyna = (Rodzaj.paliwa == 'benzyna')) %>% 
  group_by(Model) %>%
  summarize(abs(sum(Cena*Diesel)/sum(Diesel)-
      sum(Cena*benzyna)/sum(benzyna)))
  
  

## Odp:  Praktik                                                        2060 


## 8. Znajdz najrzadziej i najczesciej wystepujace wyposazenie/a dodatkowe 
##    samochodów marki Lamborghini
tab <- auta2012 %>% 
  filter(Marka == "Lamborghini") %>% 
  select(Wyposazenie.dodatkowe) %>% 
  apply(1,FUN = as.character)
A = ""
for(i in tab){
  A <- paste(A, i, sep = ', ')
}
dflambo <- as.data.frame(strsplit(A, ', '))
colnames(dflambo)<-"dane"  
count(dflambo, dane) %>% 
  arrange(desc(n))



## Odp: 
#1                        ABS 18
#2                   alufelgi 18
#3     wspomaganie kierownicy 18
#32   blokada skrzyni biegAlw  1
#33                    klatka  1


## 9. Porównaj srednia i mediane mocy KM miedzy grupami modeli A, S i RS 
##    samochodów marki Audi
auta2012 %>% 
  filter(Marka == 'Audi',
         substring(Model, 1, 1) == 'A' |
           substring(Model, 1, 1) == 'S' |
           substring(Model,1,2) == "RS") %>% 
  mutate(Grupa.Modeli = substring(Model,1,1)) %>% 
  select(Grupa.Modeli, KM) %>%
  na.omit() %>% 
  group_by(Grupa.Modeli) %>%
  summarize(median(KM), mean(KM))


## Odp:
#Grupa.Modeli `median(KM)` `mean(KM)`
#<chr>               <dbl>      <dbl>
# 1 A                     140       160.
# 2 RS                    450       500.
# 3 S                     344       344.

## 10. Znajdz marki, których auta wystepuja w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla kazdej z tych marek.
options(stringsAsFactors=FALSE)
Marki <- auta2012 %>% 
  count(Marka) %>% 
  filter(n>10000) %>%
  select(Marka) %>% 
  apply(1,FUN = as.character)

for(Marka1 in Marki){
  Model1 <- (auta2012 %>% 
               filter(Marka == Marka1) %>% 
               count(Model) %>% 
               arrange(desc(n)) %>% 
               head(1) %>% 
               apply(1,FUN = as.character))[[1]]
  Kolor1 <- (auta2012 %>% 
               filter(Marka == Marka1, Model == Model1) %>% 
               count(Kolor) %>% 
               arrange(desc(n)) %>% 
               head(1) %>% 
               apply(1,FUN = as.character))[[1]]
  cat(Marka1, '\t', Model1, '\t', Kolor1, '\n')
}


## Odp: 
#Audi 	 A4 	 czarny-metallic 
#BMW 	 320 	 srebrny-metallic 
#Ford 	 Focus 	 srebrny-metallic 
#Mercedes-Benz 	 C 220 	 srebrny-metallic 
#Opel 	 Astra 	 srebrny-metallic 
#Renault 	 Megane 	 srebrny-metallic 
#Volkswagen 	 Passat 	 srebrny-metallic 
