library(dplyr)
library(ggplot2)

# Najpierw zamierzam wygenerować 
# dane i sprawdzić ich poprawność 
# odtwarzając wykresy z raportu

pit <- function(dochod){
  k <- 0
  if(dochod<85528){
    k = 0.17*dochod
  }
  else{
    k = 0.32 * (dochod - 85528) + 15181
  }
  if(dochod<8001){
    k = 0
  }
  else if(dochod<13000){
    k = k - (1360 - 835*(dochod-8000)/5000)
  }
  else if(dochod<85528){
    k = k - 525
  }
  else if(dochod<127000){
    k = k - (525 - 525*(dochod-85528)/41472)
  }
  k
}

zapomoga <-function(ktore_dziecko, dochod){
  if(ktore_dziecko==1){
    k <- max(0, min(116, (674*3-dochod+116)))
  }
  else{
    k <- max(0, min(116*ktore_dziecko, 674*(2+ktore_dziecko)-dochod+116*ktore_dziecko))
    k <- k-zapomoga(ktore_dziecko-1, dochod)
  }
  
  
  k
}


pitnl <- function(dochod){
  k <- 0
  if(dochod<120000){
    k = 0.17*dochod
  }
  else{
    k = 0.32 * (dochod - 120000) + 120000*0.17
  }
  if(dochod<30000){
    k = 0
  }
  else{
    k = k - 30000*0.17
  }
  k
}
  


System_Bazowy <- data.frame(seq(0,30000,5)) %>% 
  cbind(Pierwsze_dziecko = 585,
        Drugie_dziecko = 585,
        Trzecie_dziecko = 585,
        Czwarte_dziecko = 585) # Initialize with 500+, 300+, 'becikowe', 'świadczenie rodzicielskie'

colnames(System_Bazowy)[[1]] <- "Dochod_w_zl"
### PODATKI I ZAPOMOGI
System_Bazowy <- System_Bazowy %>% 
  rowwise() %>% 
  mutate(Pierwsze_dziecko = Pierwsze_dziecko + min(pit(Dochod_w_zl*12)/12, as.integer(Dochod_w_zl*12<112000)*93),
         Drugie_dziecko = Drugie_dziecko + max(0, min(93+as.integer(Dochod_w_zl*12>=112000)*93, pit(Dochod_w_zl*12)/12-93)),
         Trzecie_dziecko = Trzecie_dziecko + max(0, min(167, pit(Dochod_w_zl*12)/12-93*2)),
         Czwarte_dziecko = Czwarte_dziecko + max(0, min(225, pit(Dochod_w_zl*12)/12-93*2-167))) %>% 
  rowwise() %>% # DODAJE TERAZ ZASIŁEK RODZINNY                                                                                                                                                                                                         KOFEINA SPRAWIA ZE NIE CZUJE JUZ NICZEGO POZA ARYTMIA SERCA I DRZENIEM DLONI CHCE JUZ TO SKONCZYC DLACZEGO WYBRALEM TEN WYKRES JAK UMRE NAPISZCIE NA GROBIE ZE TO PRZEZ TEN PRZEDMIOT I DLATEGO ZE JESTEM GLUPI I PODEJMUJE ZLE DECYZJE ZYCIOWE
  mutate(Pierwsze_dziecko = Pierwsze_dziecko + zapomoga(1, Dochod_w_zl),
         Drugie_dziecko = Drugie_dziecko + zapomoga(2, Dochod_w_zl),
         Trzecie_dziecko = Trzecie_dziecko + zapomoga(3, Dochod_w_zl),
         Czwarte_dziecko = Czwarte_dziecko + zapomoga(4, Dochod_w_zl))



ggplot(System_Bazowy, aes(x=Dochod_w_zl))+
  geom_line(aes(y = Pierwsze_dziecko, color = 'Pierwsze'))+
  geom_line(aes(y = Drugie_dziecko, color = 'Drugie'))+
  geom_line(aes(y = Trzecie_dziecko, color = 'Trzecie'))+
  geom_line(aes(y = Czwarte_dziecko, color = 'Czwarte'))+
  ylim(0,4000) +
  ylab('Wsparcie pańwstwa')


# Nie ogarnąłem do końca jak im tam wyszedł ten drugi wykres, więc zrobiłem coś bardzo podobnego (nie wpływa na wizualizację)
Nowy_Lad <- System_Bazowy %>% 
  rowwise() %>% 
  mutate(Czwarte_dziecko= Czwarte_dziecko + pitnl(min((Dochod_w_zl-3500)*12, 160000))/12)

ggplot(Nowy_Lad, aes(x=Dochod_w_zl))+
  geom_line(aes(y = Pierwsze_dziecko, color = 'Pierwsze'))+
  geom_line(aes(y = Drugie_dziecko, color = 'Drugie'))+
  geom_line(aes(y = Trzecie_dziecko, color = 'Trzecie'))+
  geom_line(aes(y = Czwarte_dziecko, color = 'Czwarte'))+
  ylim(0,4000)+
  ylab('Wsparcie państwa')

Plotbazowy <- System_Bazowy %>% 
  mutate(Drugie_dziecko = Drugie_dziecko + Pierwsze_dziecko) %>% 
  mutate(Trzecie_dziecko = Trzecie_dziecko + Drugie_dziecko) %>% 
  mutate(Czwarte_dziecko = Czwarte_dziecko + Trzecie_dziecko) %>% 
  ggplot(aes(x = Dochod_w_zl))+
  geom_area(aes(y = Czwarte_dziecko, fill = '4 - Czwarte dziecko'), color = 'black')+ 
  geom_area(aes(y = Trzecie_dziecko, fill = '3 - Trzecie dziecko'), color = 'black')+
  geom_area(aes(y = Drugie_dziecko, fill = '2 - Drugie dziecko'), color = 'black')+
  geom_area(aes(y = Pierwsze_dziecko, fill = '1 - Pierwsze dziecko'), color = 'black')+
  ylab('Wsparcie Państwa')+
  xlab('Dochód rodziny')+
  theme(legend.title = element_blank(),
        legend.position = c(0.7,0.8))+
  ylim(0,5500)+
  ggtitle('Obecna sytuacja')


PlotNL <- Nowy_Lad %>% 
  mutate(Drugie_dziecko = Drugie_dziecko + Pierwsze_dziecko) %>% 
  mutate(Trzecie_dziecko = Trzecie_dziecko + Drugie_dziecko) %>% 
  mutate(Czwarte_dziecko = Czwarte_dziecko + Trzecie_dziecko) %>% 
  ggplot(aes(x = Dochod_w_zl))+
  geom_area(aes(y = Czwarte_dziecko, fill = '4Czworo dzieci'), color = 'black')+ 
  geom_area(aes(y = Trzecie_dziecko, fill = '3Troje dzieci'), color = 'black')+
  geom_area(aes(y = Drugie_dziecko, fill = '2Dwoje dzieci'), color = 'black')+
  geom_area(aes(y = Pierwsze_dziecko, fill = '1Jedno dziecko'), color = 'black')+
  ylab('Wsparcie Państwa')+
  xlab('Dochód rodziny')+
  theme(legend.position = 'none')+
  ylim(0,5500)+
  ggtitle('Nowy Ład')
  
library(gridExtra)
ggsave(file = 'wykres.jpg',
  arrangeGrob(Plotbazowy, PlotNL, nrow = 1),
  width = 10,
  height = 5)
