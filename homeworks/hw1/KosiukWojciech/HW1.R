library(PogromcyDanych)
library(dplyr)

colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

## 1. Z którego rocznika jest najwięcej aut i ile ich jest?


## Odp: 2011, 17418
head(count(auta2012,Rok.produkcji,sort = T,name = "Ilosc"),1)

## 2. Która marka samochodu występuje najczęściej wśród aut wyprodukowanych w 2011 roku?


## Odp: Skoda
auta2012 %>%
  filter(Rok.produkcji == 2011) %>%
  count(Marka,sort = T) %>%
  head(1)

## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?


## Odp: 59534
auta2012 %>%
  filter(Rodzaj.paliwa == "olej napedowy (diesel)",Rok.produkcji >= 2005,
         Rok.produkcji <= 2011) %>%
  count()

## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest średnio najdroższa?


## Odp: Porsche
auta2012 %>%
  filter(Rodzaj.paliwa == "olej napedowy (diesel)",Rok.produkcji == 2011) %>%
  group_by(Marka) %>%
  summarise(mean = mean(Cena.w.PLN)) %>%
  arrange(-mean) %>%
  head(1)


## 5. Spośród aut marki Skoda wyprodukowanych w 2011 roku, który model jest średnio najtańszy?


## Odp: Skoda Fabia
auta2012 %>%
  filter(Rok.produkcji==2011,Marka=="Skoda") %>%
  group_by(Model) %>%
  summarise(mean=mean(Cena.w.PLN)) %>%
  arrange(mean) %>%
  head(1)


## 6. Która skrzynia biegów występuje najczęściej wśród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?


## Odp: automatyczna 
auta2012 %>%
  filter(Liczba.drzwi=="2/3",Cena.w.PLN/KM > 600) %>%
  count(Skrzynia.biegow) %>%
  top_n(1)

## 7. Spośród aut marki Skoda, który model ma najmniejszą różnicę średnich cen 
##    między samochodami z silnikiem benzynowym, a diesel?


## Odp: Skoda Felicia

  
auta2012_new <- auta2012 %>%
  filter(Marka == "Skoda") %>%
  group_by(Model,Rodzaj.paliwa) %>%
  summarise(mean=mean(Cena.w.PLN))
 
auta2012_benzyna <- auta2012_new %>%
  filter(Rodzaj.paliwa == "benzyna")
auta2012_diesel <- auta2012_new %>%
  filter(Rodzaj.paliwa == "olej napedowy (diesel)")

left_join(auta2012_benzyna,auta2012_diesel,by = 'Model') %>%
  select(Model,mean.x,mean.y) %>%
  mutate(diff = abs(mean.x-mean.y)) %>%
  arrange(diff) %>%
  head(1)

## 8. Znajdź najrzadziej i najczęściej występujące wyposażenie/a dodatkowe 
##    samochodów marki Lamborghini


## Odp: najrzadziej: blokada skrzyni biegów, klatka - 1 raz,
# najczęściej: ABS, wspomaganie kierownicy, alufelgi - 18 razy

wyposazenie <- auta2012 %>% 
  filter(Marka == "Lamborghini") %>%
  select(Wyposazenie.dodatkowe)

x <- c()
for (wyp in wyposazenie){
  x <- c(x,strsplit(as.character(wyp),","))
}
x <- unlist(x)
sort(table(x))



## 9. Porównaj średnią i medianę mocy KM między grupami modeli A, S i RS 
##    samochodów marki Audi


## Odp:  
#             A         S        RS
# Średnia 159.5799  340.4772   493.2703
# Mediana  140        340        450
# Różnica  19.5799      0.4772    43.2703
Audi_A <- auta2012 %>%
  filter(Marka=="Audi",grepl("A",auta2012$Model,ignore.case = F)) %>%
  summarise(mean=mean(KM,na.rm=T),median=median(KM,na.rm=T))

Audi_RS <- auta2012 %>%
  filter(Marka=="Audi",grepl("RS",auta2012$Model,ignore.case = F)) %>%
  summarise(mean=mean(KM,na.rm=T),median=median(KM,na.rm=T))

Audi_S <- auta2012 %>%
  filter(Marka=="Audi",grepl("S",auta2012$Model,ignore.case = F),!grepl("RS",auta2012$Model,ignore.case = F)) %>%
  summarise(mean=mean(KM,na.rm=T),median=median(KM,na.rm=T))

summary <- rbind(Audi_A,Audi_S,Audi_RS) %>%
  mutate(diff = mean - median, model = c('A','S','RS'))
summary

## 10. Znajdź marki, których auta występują w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla każdej z tych marek.
marki <- auta2012 %>%
  count(Marka) %>%
  filter(n>10000)

for (marka in marki$Marka){
 model <- auta2012 %>%
    filter(Marka == marka) %>%
    count(Model) %>%
    arrange(-n) %>%
    head(1) %>%
   select(Model) 
 
 cat(c(marka,as.character(model[[1]])))
 auta2012 %>% 
   filter(Marka==marka,Model==model[[1]]) %>%
   count(Kolor) %>%
   arrange(-n) %>%
   head(1) %>%
   select(Kolor) %>%
   print()
}  


## Odp: Audi A4 - czarny metallic, BMW 320 - srebrny metallic, Ford Focus - srebrny metallic,
# Mercedes-Benz C 220 - srebrny metallic, Opel Astra - srebrny metallic, Renault Megane - srebrny metallic,
# Volkswagen Passat - srebrny metallic

