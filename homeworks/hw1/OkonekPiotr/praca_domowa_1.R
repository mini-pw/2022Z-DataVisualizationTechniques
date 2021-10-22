library(PogromcyDanych)
library(dplyr)
library(tidyr)
library(tidyverse)
colnames(auta2012)
dim(auta2012)
head(auta2012[,-ncol(auta2012)])
sum(is.na(auta2012))

## 1. Z którego rocznika jest najwiêcej aut i ile ich jest?
auta2012 %>% 
  group_by(Rok.produkcji) %>% 
  summarise(n=n()) %>% 
  top_n(1,n)

## Odp: Z 2011, 17418 samochodow

## 2. Która marka samochodu wystêpuje najczêœciej wœród aut wyprodukowanych w 2011 roku?
auta2012 %>% 
  filter(Rok.produkcji==2011) %>% 
  group_by(Marka) %>% 
  summarise(n=n()) %>% 
  top_n(1,n)

## Odp:Skoda 


## 3. Ile jest aut z silnikiem diesla wyprodukowanych w latach 2005-2011?
auta2012 %>% 
  filter(Rok.produkcji>=2005 & Rok.produkcji<=2011&Rodzaj.paliwa=="olej napedowy (diesel)") %>% 
  summarise(n=n())
  

## Odp:59534


## 4. Spoœród aut z silnikiem diesla wyprodukowanych w 2011 roku, która marka jest œrednio najdro¿sza?
  auta2012 %>% 
  filter(Rok.produkcji==2011&Rodzaj.paliwa=="olej napedowy (diesel)") %>%
  group_by(Marka) %>% 
  summarise(srednia=mean(Cena,na.rm=TRUE)) %>% 
  top_n(1,srednia)
## Odp: Volvo


## 5. Spoœród aut marki Skoda wyprodukowanych w 2011 roku, który model jest œrednio najtañszy?
auta2012 %>% 
  filter(Rok.produkcji==2011&Marka=="Skoda") %>%
  group_by(Model) %>% 
  summarise(srednia=mean(Cena,na.rm=TRUE)) %>% 
  arrange(srednia) %>% 
  head(1)

## Odp: Fabia


## 6. Która skrzynia biegów wystêpuje najczêœciej wœród 2/3-drzwiowych aut,
##    których stosunek ceny w PLN do KM wynosi ponad 600?
auta2012 %>% 
  filter(Liczba.drzwi=="2/3") %>% 
 mutate(stosunek=Cena.w.PLN/KM) %>% 
  filter(stosunek>600) %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(n=n()) %>% 
  top_n(1,n)

## Odp: automatyczna


## 7. Spoœród  aut marki Skoda, który model ma najmniejsz¹ ró¿nicê œrednich cen 
##    miêdzy samochodami z silnikiem benzynowym, a diesel?

Zadanie7_a<-auta2012 %>%                        ## Samochody marki Skoda na benzyne z ich srednia cena
    filter(Rodzaj.paliwa=="benzyna"&Marka=="Skoda") %>% 
    group_by(Model) %>% 
    mutate(srednia1=mean(Cena.w.PLN))%>% 
  select(Model,srednia1) %>% 
  rename(NULL)
  
Zadanie7_b<-auta2012 %>%                        ## Samochody marki Skoda na diesel z ich srednia cena
  filter(Rodzaj.paliwa=="olej napedowy (diesel)"& Marka=="Skoda") %>% 
  group_by(Model) %>% 
  mutate(srednia2=mean(Cena.w.PLN))%>% 
  select(Model,srednia2) %>% 
  rename(NULL)

Zadanie7_a<-Zadanie7_a[!duplicated(t(apply(Zadanie7_a, 1, sort))),] ## Odrzucam powtarzajace sie wyniki
Zadanie7_b<-Zadanie7_b[!duplicated(t(apply(Zadanie7_b, 1, sort))),]

merge(Zadanie7_a,Zadanie7_b,by="Model") %>% 
  mutate(wynik=abs(srednia1-srednia2)) %>%         ## Lacze tabele po modelu, tworze roznice srednich
  top_n(-1,wynik)

## Odp: Felicia


## 8. ZnajdŸ najrzadziej i najczêœciej wystêpuj¹ce wyposa¿enie/a dodatkowe 
##    samochodów marki Lamborghini

auta2012 %>%
  filter(Marka=="Lamborghini") %>% 
  separate_rows(Wyposazenie.dodatkowe,sep = ",") %>%    ##rodzielam kolumne wzgledem przecinka
  drop_na()%>% 
  group_by(Wyposazenie.dodatkowe) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))

## Odp: Najczesciej: alufelgi, wspomaganie kierownicy, ABS
## Najrzadziej: blokada skrzyni biegow, klatka, pod. przednia szyba

## 9. Porównaj œredni¹ i medianê mocy KM miêdzy grupami modeli A, S i RS 
##    samochodów marki Audi

 
ZAdanie9a<-auta2012 %>%                        
  filter(Marka=="Audi",substr(Model, 1, 1) == "S")

mean(ZAdanie9a$KM,na.rm = TRUE)  
median(ZAdanie9a$KM,na.rm = TRUE)

ZAdanie9b<-auta2012 %>% 
  filter(Marka=="Audi",substr(Model, 1, 1) == "A") 

mean(ZAdanie9b$KM,na.rm = TRUE)  
median(ZAdanie9b$KM,na.rm = TRUE)

ZAdanie9c<-auta2012 %>% 
  filter(Marka=="Audi",substr(Model, 1, 2) == "RS") 

mean(ZAdanie9c$KM,na.rm = TRUE)  
median(ZAdanie9c$KM,na.rm = TRUE)


## Odp:Srednia moc:  S-343.7371   A-159.5799   RS-500.0282 
#      Mediana mocy: S-344   A-140   RS-450 

## 10. ZnajdŸ marki, których auta wystêpuj¹ w danych ponad 10000 razy.
##     Podaj najpopularniejszy kolor najpopularniejszego modelu dla ka¿dej z tych marek.


Pomocnicza1<-auta2012 %>%      ##Marki wiêcej ni¿ 10000
  group_by(Marka) %>% 
  summarise(n=n(),na.rm=TRUE) %>% 
  filter(n>10000)

Vector<-as.vector(unlist(Pomocnicza1[,1]))     ## Wektor z markami


Pomocnicza2<-auta2012 %>% 
  filter(Marka %in% Vector) %>%         ## Wybrane marki, modele i kolory
  select(Marka,Model,Kolor)


  for(i in 1:length(Vector) ) 
     y[i]<-(Pomocnicza2 %>%
     filter(Marka==Vector[i]) %>% 
     group_by(Model) %>%                   ## Dla ka¿dej marki szukam najpopularniejszego modelu
     summarise(m=n(),na.rm=TRUE)%>%        ## Zapisuje w tablicy y ten model
    top_n(1,m))[1]
   

 
Pomocnicza2%>%                             ##Szukam modeli z tablicy y
  filter(Model %in% y) %>%                 ## Wybieram najpopularniejszy kolor     
    group_by(Model,Kolor) %>% 
summarise(m=n(),na.rm=TRUE)%>%
  top_n(1,m)

## Odp: Audi,BMW,Ford,Mercedes,Opel,Renault,Volkswagen
##  320    srebrny-metallic    
## A4     czarny-metallic     
## Astra  srebrny-metallic    
##  C 220  srebrny-metallic 
## Focus  srebrny-metallic   
## Megane srebrny-metallic   
## Passat srebrny-metallic