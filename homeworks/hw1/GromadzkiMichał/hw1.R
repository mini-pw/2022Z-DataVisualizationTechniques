library(PogromcyDanych)
library(dplyr)
library(stringr)
colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

## 1. Z którego rocznika jest najwięcej aut i ile ich jest?
auta2012 %>% 
  group_by(Rok.produkcji) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  head(1)

## Odp: Rok 2011, 17418


## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?
auta2012 %>% 
  filter(Rok.produkcji==2011) %>% 
  group_by(Marka) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  head(1)


## Odp: Skoda


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?
auta2012 %>% 
  filter(Rok.produkcji>=2005 & Rok.produkcji<=2011 & Rodzaj.paliwa=="olej napedowy (diesel)") %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))


## Odp: 59534


## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?
auta2012 %>% 
  filter(Rodzaj.paliwa=="olej napedowy (diesel)") %>% 
  group_by(Marka) %>% 
  summarise(avg_price=mean(Cena.w.PLN)) %>% 
  arrange(desc(avg_price)) %>% 
  head(1)

## Odp:Porsche


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?
auta2012 %>% 
  filter(Marka=="Skoda" & Rok.produkcji==2011) %>% 
  group_by(Model) %>% 
  summarise(avg_price=mean(Cena.w.PLN)) %>% 
  arrange(avg_price) %>% 
  head(1)
  

## Odp:Fabia


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?
auta2012 %>% 
  filter(Liczba.drzwi=="2/3" & Cena.w.PLN/KM>600) %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  head(1)

## Odp:automatyczna


## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?

avg_benz <- auta2012 %>% 
  filter(Marka=="Skoda" & Rodzaj.paliwa=="benzyna") %>% 
  group_by(Model) %>% 
  summarise(avg_benz=mean(Cena.w.PLN,na.rm=TRUE))

avg_dis <- auta2012 %>% 
  filter(Marka=="Skoda" & Rodzaj.paliwa=="olej napedowy (diesel)") %>% 
  group_by(Model) %>% 
  summarise(avg_dis=mean(Cena.w.PLN,na.rm=TRUE))

final <- inner_join(avg_benz,avg_dis, by="Model")

final %>% 
  mutate(diff=abs(avg_benz-avg_dis)) %>% 
  arrange(diff) %>% 
  head(1)

 
## Odp:Felicia


## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini

Wyposazeniedod <- auta2012 %>% 
  filter(Marka=="Lamborghini") %>% 
  select(Wyposazenie.dodatkowe)

sort(table(unlist(str_split(unlist(Wyposazeniedod), ", "))))

## Odp:
## Najrzadziej blokada skrzyni biegów, klatka
## Najczęściej ABS, alufelgi, wspomaganie kierownicy


## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi

auta2012 %>% 
  filter(Marka=="Audi") %>% 
  mutate(
  modelgroup=case_when(str_detect(Model, "^A") ~ "A",
                       str_detect(Model, "^S") ~ "S",
                       str_detect(Model, "^RS") ~ "RS",
                       TRUE ~ "Z")) %>% 
  group_by(modelgroup) %>% 
  summarise(mean=mean(KM, na.rm=TRUE),median=median(KM, na.rm=TRUE)) %>% 
  head(3)


## Odp: 
## modelgroup  mean   median
##1 A           160.    140
##2 RS          500.    450
##3 S           344.    344


## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.
  
over1000<- auta2012 %>% 
    group_by(Marka) %>% 
    summarise(n=n()) %>%
    filter(n>10000) %>% 
    arrange(desc(n))
 
    

topmodel <- inner_join(over1000,auta2012) %>% 
  group_by(Marka,Model) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  filter(row_number()==1)

topkolor <- inner_join(topmodel,auta2012) %>% 
  group_by(Marka,Model,Kolor) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  filter(row_number() == 1)

topkolor

## Odp: 
## Ford           Focus       srebrny-metallic
## Opel           Astra       srebrny-metallic
## Volkswagen     Passat      srebrny-metallic
## Audi           A4          czarny-metallic
## Renault        Meganes     rebrny-metallic
## BMW            320         srebrny-metallic
## Mercedes-Benz  C 220       srebrny-metallic      