library(dplyr)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(stringr)
library(tidyr)
library(lubridate)
library(padr)
library(zoo)

#read.csv#########################################################################################################################

df_tymekkk_blitz_black_games <- read.csv("dane_pierwotne/tymekkk_blitz_black_games.csv", na.strings=c("","NA"))
df_tymekkk_blitz_black_moves <- read.csv("dane_pierwotne/tymekkk_blitz_black_moves.csv", na.strings=c("","NA"))
df_tymekkk_blitz_white_games<- read.csv("dane_pierwotne/tymekkk_blitz_white_games.csv", na.strings=c("","NA"))
df_tymekkk_blitz_white_moves <- read.csv("dane_pierwotne/tymekkk_blitz_white_moves.csv", na.strings=c("","NA"))
df_Mlodziak77_rapid_black_games <- read.csv("dane_pierwotne/Mlodziak77_rapid_black_games.csv", na.strings=c("","NA"))
df_Mlodziak77_rapid_black_moves <- read.csv("dane_pierwotne/Mlodziak77_rapid_black_moves.csv", na.strings=c("","NA"))
df_Mlodziak77_rapid_white_games<- read.csv("dane_pierwotne/Mlodziak77_rapid_white_games.csv", na.strings=c("","NA"))
df_Mlodziak77_rapid_white_moves <- read.csv("dane_pierwotne/Mlodziak77_rapid_white_moves.csv", na.strings=c("","NA"))
df_Mlodziak77_blitz_black_games <- read.csv("dane_pierwotne/Mlodziak77_blitz_black_games.csv", na.strings=c("","NA"))
df_Mlodziak77_blitz_black_moves <- read.csv("dane_pierwotne/Mlodziak77_blitz_black_moves.csv", na.strings=c("","NA"))
df_Mlodziak77_blitz_white_games<- read.csv("dane_pierwotne/Mlodziak77_blitz_white_games.csv", na.strings=c("","NA"))
df_Mlodziak77_blitz_white_moves <- read.csv("dane_pierwotne/Mlodziak77_blitz_white_moves.csv", na.strings=c("","NA"))
df_Mlodziak77_bullet_black_games <- read.csv("dane_pierwotne/Mlodziak77_bullet_black_games.csv", na.strings=c("","NA"))
df_Mlodziak77_bullet_black_moves <- read.csv("dane_pierwotne/Mlodziak77_bullet_black_moves.csv", na.strings=c("","NA"))
df_Mlodziak77_bullet_white_games<- read.csv("dane_pierwotne/Mlodziak77_bullet_white_games.csv", na.strings=c("","NA"))
df_Mlodziak77_bullet_white_moves <- read.csv("dane_pierwotne/Mlodziak77_bullet_white_moves.csv", na.strings=c("","NA"))
df_grooney_rapid_black_games <- read.csv("dane_pierwotne/grooney_rapid_black_games.csv", na.strings=c("","NA"))
df_grooney_rapid_black_moves <- read.csv("dane_pierwotne/grooney_rapid_black_moves.csv", na.strings=c("","NA"))
df_grooney_rapid_white_games<- read.csv("dane_pierwotne/grooney_rapid_white_games.csv", na.strings=c("","NA"))
df_grooney_rapid_white_moves <- read.csv("dane_pierwotne/grooney_rapid_white_moves.csv", na.strings=c("","NA"))
df_grooney_blitz_black_games <- read.csv("dane_pierwotne/grooney_blitz_black_games.csv", na.strings=c("","NA"))
df_grooney_blitz_black_moves <- read.csv("dane_pierwotne/grooney_blitz_black_moves.csv", na.strings=c("","NA"))
df_grooney_blitz_white_games<- read.csv("dane_pierwotne/grooney_blitz_white_games.csv", na.strings=c("","NA"))
df_grooney_blitz_white_moves <- read.csv("dane_pierwotne/grooney_blitz_white_moves.csv", na.strings=c("","NA"))
df_grooney_bullet_black_games <- read.csv("dane_pierwotne/grooney_bullet_black_games.csv", na.strings=c("","NA"))
df_grooney_bullet_black_moves <- read.csv("dane_pierwotne/grooney_bullet_black_moves.csv", na.strings=c("","NA"))
df_grooney_bullet_white_games<- read.csv("dane_pierwotne/grooney_bullet_white_games.csv", na.strings=c("","NA"))
df_grooney_bullet_white_moves <- read.csv("dane_pierwotne/grooney_bullet_white_moves.csv", na.strings=c("","NA"))



#heatmap###########################################################################################################

df_grooney_blitz_both_games <- bind_rows(df_grooney_blitz_black_games,df_grooney_blitz_white_games) %>%
  mutate(color=ifelse(white=="grooney","white","black")) %>% 
  select(date,time,color,hero.points) %>%
  mutate(gracz="grooney",rodzaj="blitz") %>% 
  mutate(weekday = weekdays(as.Date(date, "%Y.%m.%d"))) %>% 
  mutate(time_hour = as.numeric(str_extract(time, "\\d+")))
df_grooney_rapid_both_games <- bind_rows(df_grooney_rapid_black_games,df_grooney_rapid_white_games) %>%
  mutate(color=ifelse(white=="grooney","white","black")) %>% 
  select(date,time,color,hero.points) %>%
  mutate(gracz="grooney",rodzaj="rapid") %>% 
  mutate(weekday = weekdays(as.Date(date, "%Y.%m.%d"))) %>% 
  mutate(time_hour = as.numeric(str_extract(time, "\\d+")))
df_grooney_bullet_both_games <- bind_rows(df_grooney_bullet_black_games,df_grooney_bullet_white_games) %>%
  mutate(color=ifelse(white=="grooney","white","black")) %>% 
  select(date,time,color,hero.points) %>%
  mutate(gracz="grooney",rodzaj="bullet") %>% 
  mutate(weekday = weekdays(as.Date(date, "%Y.%m.%d"))) %>% 
  mutate(time_hour = as.numeric(str_extract(time, "\\d+")))

df_Mlodziak77_blitz_both_games <- bind_rows(df_Mlodziak77_blitz_black_games,df_Mlodziak77_blitz_white_games) %>%
  mutate(color=ifelse(white=="Mlodziak77","white","black")) %>% 
  select(date,time,color,hero.points) %>%
  mutate(gracz="Mlodziak77",rodzaj="blitz") %>% 
  mutate(weekday = weekdays(as.Date(date, "%Y.%m.%d"))) %>% 
  mutate(time_hour = as.numeric(str_extract(time, "\\d+")))
df_Mlodziak77_rapid_both_games <- bind_rows(df_Mlodziak77_rapid_black_games,df_Mlodziak77_rapid_white_games) %>%
  mutate(color=ifelse(white=="Mlodziak77","white","black")) %>% 
  select(date,time,color,hero.points) %>%
  mutate(gracz="Mlodziak77",rodzaj="rapid") %>% 
  mutate(weekday = weekdays(as.Date(date, "%Y.%m.%d"))) %>% 
  mutate(time_hour = as.numeric(str_extract(time, "\\d+")))
df_Mlodziak77_bullet_both_games <- bind_rows(df_Mlodziak77_bullet_black_games,df_Mlodziak77_bullet_white_games) %>%
  mutate(color=ifelse(white=="Mlodziak77","white","black")) %>% 
  select(date,time,color,hero.points) %>%
  mutate(gracz="Mlodziak77",rodzaj="bullet") %>% 
  mutate(weekday = weekdays(as.Date(date, "%Y.%m.%d"))) %>% 
  mutate(time_hour = as.numeric(str_extract(time, "\\d+")))

df_tymekkk_blitz_both_games <- bind_rows(df_tymekkk_blitz_black_games,df_tymekkk_blitz_white_games) %>%
  mutate(color=ifelse(white=="tymekkk","white","black")) %>% 
  select(date,time,color,hero.points) %>% 
  mutate(gracz="tymekkk",rodzaj="blitz") %>% 
  mutate(weekday = weekdays(as.Date(date, "%Y.%m.%d"))) %>% 
  mutate(time_hour = as.numeric(str_extract(time, "\\d+")))

df_heatmap<-rbind(df_tymekkk_blitz_both_games,
                  df_grooney_blitz_both_games,
                  df_Mlodziak77_blitz_both_games,
                  df_grooney_rapid_both_games,
                  df_Mlodziak77_rapid_both_games,
                  df_grooney_bullet_both_games,
                  df_Mlodziak77_bullet_both_games)

df_heatmap<-rbind(df_heatmap,mutate(df_heatmap,color="both")) %>% 
  group_by(weekday, time_hour, color,gracz,rodzaj) %>%
  summarise(sum = sum(hero.points, na.rm = TRUE), n = n()) %>% 
  mutate(mean=sum/n) %>% 
  mutate(day=case_when(weekday=="poniedzia�ek" ~ 1,
                       weekday=="wtorek"~ 2,
                       weekday=="�roda"~ 3,
                       weekday=="czwartek"~ 4,
                       weekday=="pi�tek"~ 5,
                       weekday=="sobota"~ 6,
                       weekday=="niedziela"~ 7),
         hour=time_hour) %>% 
  ungroup() %>% 
  select(-c(weekday,time_hour,sum))

gracz<-rep(c("grooney","Mlodziak77","tymekkk"),times=1,each=1512)
rodzaj<-rep(c("blitz","bullet","rapid"),times=3,each=504)
color<- rep(c("white", "black","both"),times=9,each=168)
day <- rep(c(1:7),times=27,each=24)
hour <- rep(c(0:23),times=189,each=1)
n <- rep(0,4536)
mean <- rep(0,4536)
df_heatmap_0<-data.frame(gracz,rodzaj,color,day,hour,n,mean)
df_heatmap<-rbind(df_heatmap,df_heatmap_0) %>% 
  distinct(gracz,rodzaj,color,day,hour,.keep_all = TRUE)

df_heatmap_wynik <- df_heatmap %>% 
  filter(color=="black",gracz=="grooney",rodzaj=="bullet") %>% 
  select(day, hour, mean) 


p<-plot_ly(
  x = c(0:23),
  y = c("Sunday", "Saturday", "Friday", "Thursday", "Wednesday", "Tuesday", "Monday"),
  z = as.matrix(df_heatmap_wynik),
  type = "heatmap",
  colors = colorRamp(c("white", "blue"))) %>% 
  layout(
    xaxis = list(
      dtick = 1, 
      tick0 = 0, 
      tickmode = "linear"
    )) 
# df_heatmap_wynik<-df_heatmap_wynik %>%
#   mutate(day=case_when(weekday==1 ~ "Monday",
#                      weekday==2~ "Tuesday",
#                      weekday==3~ "Wednesday",
#                      weekday==4~ "Thursday",
#                      weekday==5~ "Friday",
#                      weekday==6~ "Saturday",
#                      weekday==7~ "Sunday"))
# 
# p <- ggplot(df_heatmap_wynik, aes(x=hour, y=day, fill= mean)) +
#   geom_tile()+
#   scale_fill_gradient(high = "#f6e8c3",
#                       low = "#543005")+
#   theme_ipsum() +
#   labs(x = element_blank(), y = element_blank())+ 
#   scale_fill_continuous(limits=c(0, 1), breaks=seq(0,1,by=0.25))+
#   scale_x_continuous( breaks=seq(0,23,by=1))
# 
# p <- ggplotly(p) %>% layout(
#   title = "Where do we move pices across different opennings?"
#   )
# 
# p
# if(input$category=="mean"){fig+ scale_fill_continuous(limits=c(0, 1), breaks=seq(0,1,by=0.25))}
#plot2######################################################################################################################

df_grooney_bullet_white_plot2 <-df_grooney_bullet_white_moves %>% 
  select(move=white) %>% 
  mutate(color="white",gracz="grooney",rodzaj="bullet")
df_grooney_blitz_white_plot2 <-df_grooney_blitz_white_moves %>% 
  select(move=white) %>% 
  mutate(color="white",gracz="grooney",rodzaj="blitz")
df_grooney_rapid_white_plot2 <-df_grooney_rapid_white_moves %>% 
  select(move=white) %>% 
  mutate(color="white",gracz="grooney",rodzaj="rapid")
df_grooney_bullet_black_plot2 <-df_grooney_bullet_black_moves %>% 
  select(move=black) %>% 
  mutate(color="black",gracz="grooney",rodzaj="bullet")
df_grooney_blitz_black_plot2 <-df_grooney_blitz_black_moves %>% 
  select(move=black) %>% 
  mutate(color="black",gracz="grooney",rodzaj="blitz")
df_grooney_rapid_black_plot2 <-df_grooney_rapid_black_moves %>% 
  select(move=black) %>% 
  mutate(color="black",gracz="grooney",rodzaj="rapid")

df_Mlodziak77_bullet_white_plot2 <-df_Mlodziak77_bullet_white_moves %>% 
  select(move=white) %>% 
  mutate(color="white",gracz="Mlodziak77",rodzaj="bullet")
df_Mlodziak77_blitz_white_plot2 <-df_Mlodziak77_blitz_white_moves %>% 
  select(move=white) %>% 
  mutate(color="white",gracz="Mlodziak77",rodzaj="blitz")
df_Mlodziak77_rapid_white_plot2 <-df_Mlodziak77_rapid_white_moves %>% 
  select(move=white) %>% 
  mutate(color="white",gracz="Mlodziak77",rodzaj="rapid")
df_Mlodziak77_bullet_black_plot2 <-df_Mlodziak77_bullet_black_moves %>% 
  select(move=black) %>% 
  mutate(color="black",gracz="Mlodziak77",rodzaj="bullet")
df_Mlodziak77_blitz_black_plot2 <-df_Mlodziak77_blitz_black_moves %>% 
  select(move=black) %>% 
  mutate(color="black",gracz="Mlodziak77",rodzaj="blitz")
df_Mlodziak77_rapid_black_plot2 <-df_Mlodziak77_rapid_black_moves %>% 
  select(move=black) %>% 
  mutate(color="black",gracz="Mlodziak77",rodzaj="rapid")

df_tymekkk_blitz_white_plot2 <-df_tymekkk_blitz_white_moves %>% 
  select(move=white) %>% 
  mutate(color="white",gracz="tymekkk",rodzaj="blitz")
df_tymekkk_blitz_black_plot2 <-df_tymekkk_blitz_black_moves %>% 
  select(move=black) %>% 
  mutate(color="black",gracz="tymekkk",rodzaj="blitz")

df_plot2<-rbind(df_tymekkk_blitz_white_plot2,
                df_grooney_blitz_white_plot2,
                df_Mlodziak77_blitz_white_plot2,
                df_grooney_rapid_white_plot2,
                df_Mlodziak77_rapid_white_plot2,
                df_grooney_bullet_white_plot2,
                df_Mlodziak77_bullet_white_plot2,
                df_tymekkk_blitz_black_plot2,
                df_grooney_blitz_black_plot2,
                df_Mlodziak77_blitz_black_plot2,
                df_grooney_rapid_black_plot2,
                df_Mlodziak77_rapid_black_plot2,
                df_grooney_bullet_black_plot2,
                df_Mlodziak77_bullet_black_plot2)

df_plot2<-rbind(df_plot2,mutate(df_plot2,color="both")) %>%
  filter(move!=".") %>% 
  filter(substr(move, 1, 1)!="O") %>% 
  na.omit()%>% 
  mutate(Pawns_moves = ifelse(substr(move, 1, 1) != toupper(substr(move, 1, 1)),substr(move, 1, 1),NA),
         Pawns_captures = ifelse(substr(move, 1, 1) != toupper(substr(move, 1, 1)) & grepl("x", move),substr(move, 1, 1),NA),
         Pawns_promotions = ifelse(substr(move, 1, 1) != toupper(substr(move, 1, 1)) & grepl("=", move),substr(move, 1, 1),NA),
         Pieces_moves = ifelse(substr(move, 1, 1) == toupper(substr(move, 1, 1)),substr(move, 1, 1),NA),
         Pieces_captures = ifelse(substr(move, 1, 1) == toupper(substr(move, 1, 1)) & grepl("x", move),substr(move, 1, 1),NA)) 
df_plot2_Pawns_moves<- df_plot2 %>%  
  group_by(rodzaj,gracz,color,Pawns_moves)%>%
  summarise(n = n())%>% 
  na.omit()
df_plot2_Pawns_captures<- df_plot2 %>%  
  group_by(rodzaj,gracz,color,Pawns_captures)%>%
  summarise(n = n())%>% 
  na.omit()
df_plot2_Pawns_promotions<- df_plot2 %>%  
  group_by(rodzaj,gracz,color,Pawns_promotions)%>%
  summarise(n = n())%>% 
  na.omit()
df_plot2_Pieces_moves<- df_plot2 %>%  
  group_by(rodzaj,gracz,color,Pieces_moves)%>%
  summarise(n = n())%>% 
  na.omit()
df_plot2_Pieces_captures<- df_plot2 %>%  
  group_by(rodzaj,gracz,color,Pieces_captures)%>%
  summarise(n = n())%>% 
  na.omit()
Pieces_moves <- rep(c("R", "N", "B", "K", "Q", "B ", "N ", "R "),times=27,each=1)
Pieces_captures <- rep(c("R", "N", "B", "K", "Q", "B ", "N ", "R "),times=27,each=1)
Pawns_moves <- rep(letters[1:8],times=27,each=1)
Pawns_captures <- rep(letters[1:8],times=27,each=1)
Pawns_promotions <- rep(letters[1:8],times=27,each=1)
rodzaj<-rep(c("blitz","bullet","rapid"),times=1,each=72)
gracz<-rep(c("grooney","Mlodziak77","tymekkk"),times=3,each=24)
color<-rep(c("black","white","both"),each=8,times=9)
n<-rep(0,216)

df_plot2_Pawns_moves_0<-data.frame(rodzaj,gracz,color,Pawns_moves,n)
df_plot2_Pawns_captures_0<-data.frame(rodzaj,gracz,color,Pawns_captures,n)
df_plot2_Pawns_promotions_0<-data.frame(rodzaj,gracz,color,Pawns_promotions,n)
df_plot2_Pieces_moves_0<-data.frame(rodzaj,gracz,color,Pieces_moves,n)
df_plot2_Pieces_captures_0<-data.frame(rodzaj,gracz,color,Pieces_captures,n)

df_plot2_Pawns_moves<-rbind(df_plot2_Pawns_moves,df_plot2_Pawns_moves_0) %>% 
  distinct(rodzaj,gracz,color,Pawns_moves,.keep_all = TRUE)%>% 
  mutate(typ="Pawns_moves") %>% 
  rename(nazwa=Pawns_moves)
df_plot2_Pawns_captures<-rbind(df_plot2_Pawns_captures,df_plot2_Pawns_captures_0) %>% 
  distinct(rodzaj,gracz,color,Pawns_captures,.keep_all = TRUE)%>% 
  mutate(typ="Pawns_captures")%>% 
  rename(nazwa=Pawns_captures)
df_plot2_Pawns_promotions<-rbind(df_plot2_Pawns_promotions,df_plot2_Pawns_promotions_0) %>% 
  distinct(rodzaj,gracz,color,Pawns_promotions,.keep_all = TRUE)%>% 
  mutate(typ="Pawns_promotions")%>% 
  rename(nazwa=Pawns_promotions)

df_plot2_Pieces_moves<-rbind(df_plot2_Pieces_moves,df_plot2_Pieces_moves_0) %>% 
  distinct(rodzaj,gracz,color,Pieces_moves,.keep_all = TRUE)
df_plot2_Pieces_captures<-rbind(df_plot2_Pieces_captures,df_plot2_Pieces_captures_0) %>% 
  distinct(rodzaj,gracz,color,Pieces_captures,.keep_all = TRUE)

df_plot2_Pieces_moves<-df_plot2_Pieces_moves %>% 
  mutate(Pieces_moves1=substr(Pieces_moves, 1, 1)) %>% 
  group_by(rodzaj,gracz,color,Pieces_moves1)%>%
  mutate(n1=max(n))%>%
  mutate(n=ifelse(Pieces_moves1=='R' |
                    Pieces_moves1=='N' |
                    Pieces_moves1=='B',
                  floor(n1/2),
                  n)) %>% 
  ungroup() %>%
  select(-c(Pieces_moves1,n1)) %>% 
  mutate(typ="Pieces_moves")%>% 
  rename(nazwa=Pieces_moves) 


df_plot2_Pieces_captures<-df_plot2_Pieces_captures %>% 
  mutate(Pieces_captures1=substr(Pieces_captures, 1, 1)) %>% 
  group_by(rodzaj,gracz,color,Pieces_captures1) %>%
  mutate(n1=max(n))%>%
  mutate(n=ifelse(Pieces_captures1=='R' |
                    Pieces_captures1=='N' |
                    Pieces_captures1=='B',
                  floor(n1/2),
                  n)) %>%
  ungroup() %>% 
  select(-c(Pieces_captures1,n1))%>% 
  mutate(typ="Pieces_captures")%>% 
  rename(nazwa=Pieces_captures)

df_plot2<-rbind(df_plot2_Pawns_moves,
                df_plot2_Pawns_captures,
                df_plot2_Pawns_promotions,
                df_plot2_Pieces_moves,
                df_plot2_Pieces_captures)


#plot1##############################################################################

df_grooney_bullet_white_plot1 <-df_grooney_bullet_white_games %>% 
  select(hero=white.elo,  date) %>% 
  mutate(gracz="grooney",rodzaj="bullet")
df_grooney_blitz_white_plot1 <-df_grooney_blitz_white_games %>% 
  select(hero=white.elo,  date) %>% 
  mutate(gracz="grooney",rodzaj="blitz")
df_grooney_rapid_white_plot1 <-df_grooney_rapid_white_games %>% 
  select(hero=white.elo,  date) %>% 
  mutate(gracz="grooney",rodzaj="rapid")
df_grooney_bullet_black_plot1 <-df_grooney_bullet_black_games %>% 
  select(hero=black.elo,  date) %>% 
  mutate(gracz="grooney",rodzaj="bullet")
df_grooney_blitz_black_plot1 <-df_grooney_blitz_black_games %>% 
  select(hero=black.elo,  date) %>% 
  mutate(gracz="grooney",rodzaj="blitz")
df_grooney_rapid_black_plot1 <-df_grooney_rapid_black_games %>% 
  select(hero=black.elo,  date) %>% 
  mutate(gracz="grooney",rodzaj="rapid")

df_Mlodziak77_bullet_white_plot1 <-df_Mlodziak77_bullet_white_games %>% 
  select(hero=white.elo,  date) %>% 
  mutate(gracz="Mlodziak77",rodzaj="bullet")
df_Mlodziak77_blitz_white_plot1 <-df_Mlodziak77_blitz_white_games %>% 
  select(hero=white.elo,  date) %>% 
  mutate(gracz="Mlodziak77",rodzaj="blitz")
df_Mlodziak77_rapid_white_plot1 <-df_Mlodziak77_rapid_white_games %>% 
  select(hero=white.elo,  date) %>% 
  mutate(gracz="Mlodziak77",rodzaj="rapid")
df_Mlodziak77_bullet_black_plot1 <-df_Mlodziak77_bullet_black_games %>% 
  select(hero=black.elo,  date) %>% 
  mutate(gracz="Mlodziak77",rodzaj="bullet")
df_Mlodziak77_blitz_black_plot1 <-df_Mlodziak77_blitz_black_games %>% 
  select(hero=black.elo,  date) %>% 
  mutate(gracz="Mlodziak77",rodzaj="blitz")
df_Mlodziak77_rapid_black_plot1 <-df_Mlodziak77_rapid_black_games %>% 
  select(hero=black.elo,  date) %>% 
  mutate(gracz="Mlodziak77",rodzaj="rapid")

df_tymekkk_blitz_white_plot1 <-df_tymekkk_blitz_white_games %>% 
  select(hero=white.elo,  date) %>% 
  mutate(gracz="tymekkk",rodzaj="blitz")
df_tymekkk_blitz_black_plot1 <-df_tymekkk_blitz_black_games %>% 
  select(hero=black.elo,  date) %>% 
  mutate(gracz="tymekkk",rodzaj="blitz")
df_tymekkk_blitz_plot1 <- rbind(df_tymekkk_blitz_white_plot1, df_tymekkk_blitz_black_plot1)%>%
  group_by(date) %>%
  summarise(rating = max(hero)) %>% 
  mutate(date=as_datetime(date)) %>% 
  pad() %>% 
  na.locf(fromLast = TRUE) %>% 
  mutate(date= as.Date(date))
df_Mlodziak77_bullet_plot1 <- rbind(df_Mlodziak77_bullet_white_plot1, df_Mlodziak77_bullet_black_plot1)%>%
  group_by(date) %>%
  summarise(rating = max(hero)) %>% 
  mutate(date=as_datetime(date)) %>% 
  pad() %>% 
  na.locf(fromLast = TRUE) %>% 
  mutate(date= as.Date(date))
df_Mlodziak77_blitz_plot1 <- rbind(df_Mlodziak77_blitz_white_plot1, df_Mlodziak77_blitz_black_plot1)%>%
  group_by(date) %>%
  summarise(rating = max(hero)) %>% 
  mutate(date=as_datetime(date)) %>% 
  pad() %>% 
  na.locf(fromLast = TRUE) %>% 
  mutate(date= as.Date(date))
df_Mlodziak77_rapid_plot1 <- rbind(df_Mlodziak77_rapid_white_plot1, df_Mlodziak77_rapid_black_plot1) %>%
  group_by(date) %>%
  summarise(rating = max(hero)) %>% 
  mutate(date=as_datetime(date)) %>% 
  pad() %>% 
  na.locf(fromLast = TRUE) %>% 
  mutate(date= as.Date(date))
df_grooney_rapid_plot1 <- rbind(df_grooney_rapid_white_plot1, df_grooney_rapid_black_plot1)%>%
  group_by(date) %>%
  summarise(rating = max(hero)) %>% 
  mutate(date=as_datetime(date)) %>% 
  pad() %>% 
  na.locf(fromLast = TRUE) %>% 
  mutate(date= as.Date(date))
df_grooney_blitz_plot1 <- rbind(df_grooney_blitz_white_plot1, df_grooney_blitz_black_plot1)%>%
  group_by(date) %>%
  summarise(rating = max(hero)) %>% 
  mutate(date=as_datetime(date)) %>% 
  pad() %>% 
  na.locf(fromLast = TRUE) %>% 
  mutate(date= as.Date(date))
df_grooney_bullet_plot1 <- rbind(df_grooney_bullet_white_plot1, df_grooney_bullet_black_plot1) %>%
  group_by(date) %>%
  summarise(rating = max(hero)) %>% 
  mutate(date=as_datetime(date)) %>% 
  pad() %>% 
  na.locf(fromLast = TRUE) %>% 
  mutate(date= as.Date(date))
df_blitz_plot1 <- merge(merge(df_grooney_blitz_plot1, df_Mlodziak77_blitz_plot1, by = "date", all = TRUE),
                        df_tymekkk_blitz_plot1, by = "date", all = TRUE) %>% 
  rename(grooney=rating.x,Mlodziak77=rating.y,tymekkk=rating) %>% 
  mutate(rodzaj="blitz")

df_bullet_plot1 <- merge(df_grooney_bullet_plot1, df_Mlodziak77_bullet_plot1, by = "date", all = TRUE) %>% 
  rename(grooney=rating.x,Mlodziak77=rating.y)%>% 
  mutate(tymekkk=NA,rodzaj="bullet")

df_rapid_plot1 <- merge(df_grooney_rapid_plot1, df_Mlodziak77_rapid_plot1, by = "date", all = TRUE) %>% 
  rename(grooney=rating.x,Mlodziak77=rating.y)%>% 
  mutate(tymekkk=NA,rodzaj="rapid")
df_plot1<-rbind(df_blitz_plot1,
                df_bullet_plot1,
                df_rapid_plot1)%>% 
  mutate(date= as.Date(date),tymekkk=tymekkk+200) 



#scatter############################################################################################################

df_grooney_bullet_white_scatter <-df_grooney_bullet_white_games %>% 
  select(ranking_gracz=white.elo, ranking_przeciwnik=black.elo, wynik=hero.points, link=link,data=date) %>% 
  mutate(color="white",gracz="grooney",rodzaj="bullet")
df_grooney_blitz_white_scatter <-df_grooney_blitz_white_games %>% 
  select(ranking_gracz=white.elo, ranking_przeciwnik=black.elo, wynik=hero.points, link=link,data=date) %>% 
  mutate(color="white",gracz="grooney",rodzaj="blitz")
df_grooney_rapid_white_scatter <-df_grooney_rapid_white_games %>% 
  select(ranking_gracz=white.elo, ranking_przeciwnik=black.elo, wynik=hero.points, link=link,data=date) %>% 
  mutate(color="white",gracz="grooney",rodzaj="rapid")
df_grooney_bullet_black_scatter <-df_grooney_bullet_black_games %>% 
  select(ranking_gracz=black.elo, ranking_przeciwnik=white.elo, wynik=hero.points, link=link,data=date) %>% 
  mutate(color="black",gracz="grooney",rodzaj="bullet")
df_grooney_blitz_black_scatter <-df_grooney_blitz_black_games %>% 
  select(ranking_gracz=black.elo, ranking_przeciwnik=white.elo, wynik=hero.points, link=link,data=date) %>% 
  mutate(color="black",gracz="grooney",rodzaj="blitz")
df_grooney_rapid_black_scatter <-df_grooney_rapid_black_games %>% 
  select(ranking_gracz=black.elo, ranking_przeciwnik=white.elo, wynik=hero.points, link=link,data=date) %>% 
  mutate(color="black",gracz="grooney",rodzaj="rapid")

df_Mlodziak77_bullet_white_scatter <-df_Mlodziak77_bullet_white_games %>% 
  select(ranking_gracz=white.elo, ranking_przeciwnik=black.elo, wynik=hero.points, link=link,data=date) %>% 
  mutate(color="white",gracz="Mlodziak77",rodzaj="bullet")
df_Mlodziak77_blitz_white_scatter <-df_Mlodziak77_blitz_white_games %>% 
  select(ranking_gracz=white.elo, ranking_przeciwnik=black.elo, wynik=hero.points, link=link,data=date) %>% 
  mutate(color="white",gracz="Mlodziak77",rodzaj="blitz")
df_Mlodziak77_rapid_white_scatter <-df_Mlodziak77_rapid_white_games %>% 
  select(ranking_gracz=white.elo, ranking_przeciwnik=black.elo, wynik=hero.points, link=link,data=date) %>% 
  mutate(color="white",gracz="Mlodziak77",rodzaj="rapid")
df_Mlodziak77_bullet_black_scatter <-df_Mlodziak77_bullet_black_games %>% 
  select(ranking_gracz=black.elo, ranking_przeciwnik=white.elo, wynik=hero.points, link=link,data=date) %>% 
  mutate(color="black",gracz="Mlodziak77",rodzaj="bullet")
df_Mlodziak77_blitz_black_scatter <-df_Mlodziak77_blitz_black_games %>% 
  select(ranking_gracz=black.elo, ranking_przeciwnik=white.elo, wynik=hero.points, link=link,data=date) %>% 
  mutate(color="black",gracz="Mlodziak77",rodzaj="blitz")
df_Mlodziak77_rapid_black_scatter <-df_Mlodziak77_rapid_black_games %>% 
  select(ranking_gracz=black.elo, ranking_przeciwnik=white.elo, wynik=hero.points, link=link,data=date) %>% 
  mutate(color="black",gracz="Mlodziak77",rodzaj="rapid")

df_tymekkk_blitz_white_scatter <-df_tymekkk_blitz_white_games %>% 
  select(ranking_gracz=white.elo, ranking_przeciwnik=black.elo, wynik=hero.points, link=link,data=date) %>% 
  mutate(color="white",gracz="tymekkk",rodzaj="blitz")
df_tymekkk_blitz_black_scatter <-df_tymekkk_blitz_black_games %>% 
  select(ranking_gracz=black.elo, ranking_przeciwnik=white.elo, wynik=hero.points, link=link,data=date) %>% 
  mutate(color="black",gracz="tymekkk",rodzaj="blitz")

df_scatter<-rbind(df_tymekkk_blitz_white_scatter,
                  df_grooney_blitz_white_scatter,
                  df_Mlodziak77_blitz_white_scatter,
                  df_grooney_rapid_white_scatter,
                  df_Mlodziak77_rapid_white_scatter,
                  df_grooney_bullet_white_scatter,
                  df_Mlodziak77_bullet_white_scatter,
                  df_tymekkk_blitz_black_scatter,
                  df_grooney_blitz_black_scatter,
                  df_Mlodziak77_blitz_black_scatter,
                  df_grooney_rapid_black_scatter,
                  df_Mlodziak77_rapid_black_scatter,
                  df_grooney_bullet_black_scatter,
                  df_Mlodziak77_bullet_black_scatter)



#Arek######################################################################################################################
df_grooney_bullet_both_games <- bind_rows(df_grooney_bullet_black_games,df_grooney_bullet_white_games) %>%
  mutate(opening_name = ifelse(grepl(":", opening.name), substr(opening.name, 1, unlist(gregexpr(":", opening.name)) -1), opening.name)) %>%   
  mutate(opening_name=ifelse(opening_name=="GrÄ‚Ä1nfeld Defense","Grunfeld Defense",opening_name))%>%
  mutate(color=ifelse(white=="grooney","white","black")) %>% 
  select(id,opening_name,color,hero.points)

df_grooney_bullet_black_moves<-df_grooney_bullet_black_moves %>% 
  select(game.id,black) %>% 
  mutate(color="black") %>% 
  rename(move=black)
df_grooney_bullet_white_moves<-df_grooney_bullet_white_moves %>% 
  select(game.id,white) %>% 
  mutate(color="white") %>% 
  rename(move=white)
df_grooney_bullet_both_moves<-bind_rows(df_grooney_bullet_black_moves,df_grooney_bullet_white_moves)

df_grooney_bullet_0 <- df_grooney_bullet_both_games %>%
  group_by(color,opening_name) %>%
  summarise(n = n(),avg_points = mean(hero.points)) %>%
  arrange(-n) %>%
  mutate(opening_name = forcats::fct_reorder(opening_name, n)) 

df_grooney_bullet_ruchygracza<-inner_join(df_grooney_bullet_both_games,df_grooney_bullet_both_moves,by = c("id" = "game.id","color"="color")) %>% 
  mutate(move=case_when(is.na(as.numeric(substr(move,nchar(move),nchar(move)))) == FALSE ~ substr(move,nchar(move)-1,nchar(move)),
                        (substr(move,1,5) == "O-O-O" & color=="black") ~ "c8d8",
                        (substr(move,1,5) == "O-O-O" & color=="white") ~ "c1d1",
                        (substr(move,1,3) == "O-O" & color=="black") ~ "g8f8",
                        (substr(move,1,3) == "O-O" & color=="white") ~ "g1f1",
                        substr(move,nchar(move)-1,nchar(move)-1) == "=" ~ substr(move,nchar(move)-3,nchar(move)-2),
                        substr(move,nchar(move)-2,nchar(move)-2) == "=" ~ substr(move,nchar(move)-4,nchar(move)-3),
                        substr(move,nchar(move),nchar(move)) == "+" ~ substr(move,nchar(move)-2,nchar(move)-1),
                        substr(move,nchar(move),nchar(move)) == "#" ~ substr(move,nchar(move)-2,nchar(move)-1),
                        TRUE ~ "nie"))

df_grooney_bullet_ruchygracza_add1<- filter(df_grooney_bullet_ruchygracza,nchar(move)==4) %>% 
  mutate(move=substr(move,1,2))
df_grooney_bullet_ruchygracza_add2<- filter(df_grooney_bullet_ruchygracza,nchar(move)==4) %>% 
  mutate(move=substr(move,3,4))

df_grooney_bullet_ruchygracza<-rbind(filter(df_grooney_bullet_ruchygracza,nchar(move)==2),df_grooney_bullet_ruchygracza_add1,df_grooney_bullet_ruchygracza_add2)%>% 
  group_by(opening_name,color,move) %>%
  summarise(count = n())

df_grooney_bullet_0<-df_grooney_bullet_0 %>% 
  mutate(gracz="grooney",rodzaj="bullet")
df_grooney_bullet_ruchygracza<-df_grooney_bullet_ruchygracza %>% 
  mutate(gracz="grooney",rodzaj="bullet")

df_grooney_blitz_both_games <- bind_rows(df_grooney_blitz_black_games,df_grooney_blitz_white_games) %>%
  mutate(opening_name = ifelse(grepl(":", opening.name), substr(opening.name, 1, unlist(gregexpr(":", opening.name)) -1), opening.name)) %>%
  mutate(opening_name=ifelse(opening_name=="GrÄ‚Ä1nfeld Defense","Grunfeld Defense",opening_name))%>% 
  mutate(color=ifelse(white=="grooney","white","black")) %>% 
  select(id,opening_name,color,hero.points)

df_grooney_blitz_black_moves<-df_grooney_blitz_black_moves %>% 
  select(game.id,black) %>% 
  mutate(color="black") %>% 
  rename(move=black)
df_grooney_blitz_white_moves<-df_grooney_blitz_white_moves %>% 
  select(game.id,white) %>% 
  mutate(color="white") %>% 
  rename(move=white)
df_grooney_blitz_both_moves<-bind_rows(df_grooney_blitz_black_moves,df_grooney_blitz_white_moves)

df_grooney_blitz_0 <- df_grooney_blitz_both_games %>%
  group_by(color,opening_name) %>%
  summarise(n = n(),avg_points = mean(hero.points)) %>%
  arrange(-n) %>%
  mutate(opening_name = forcats::fct_reorder(opening_name, n)) 

df_grooney_blitz_ruchygracza<-inner_join(df_grooney_blitz_both_games,df_grooney_blitz_both_moves,by = c("id" = "game.id","color"="color")) %>% 
  mutate(move=case_when(is.na(as.numeric(substr(move,nchar(move),nchar(move)))) == FALSE ~ substr(move,nchar(move)-1,nchar(move)),
                        (substr(move,1,5) == "O-O-O" & color=="black") ~ "c8d8",
                        (substr(move,1,5) == "O-O-O" & color=="white") ~ "c1d1",
                        (substr(move,1,3) == "O-O" & color=="black") ~ "g8f8",
                        (substr(move,1,3) == "O-O" & color=="white") ~ "g1f1",
                        substr(move,nchar(move)-1,nchar(move)-1) == "=" ~ substr(move,nchar(move)-3,nchar(move)-2),
                        substr(move,nchar(move)-2,nchar(move)-2) == "=" ~ substr(move,nchar(move)-4,nchar(move)-3),
                        substr(move,nchar(move),nchar(move)) == "+" ~ substr(move,nchar(move)-2,nchar(move)-1),
                        substr(move,nchar(move),nchar(move)) == "#" ~ substr(move,nchar(move)-2,nchar(move)-1),
                        TRUE ~ "nie"))

df_grooney_blitz_ruchygracza_add1<- filter(df_grooney_blitz_ruchygracza,nchar(move)==4) %>% 
  mutate(move=substr(move,1,2))
df_grooney_blitz_ruchygracza_add2<- filter(df_grooney_blitz_ruchygracza,nchar(move)==4) %>% 
  mutate(move=substr(move,3,4))

df_grooney_blitz_ruchygracza<-rbind(filter(df_grooney_blitz_ruchygracza,nchar(move)==2),df_grooney_blitz_ruchygracza_add1,df_grooney_blitz_ruchygracza_add2)%>% 
  group_by(opening_name,color,move) %>%
  summarise(count = n())

df_grooney_blitz_0<-df_grooney_blitz_0 %>% 
  mutate(gracz="grooney",rodzaj="blitz")
df_grooney_blitz_ruchygracza<-df_grooney_blitz_ruchygracza %>% 
  mutate(gracz="grooney",rodzaj="blitz")

df_grooney_rapid_both_games <- bind_rows(df_grooney_rapid_black_games,df_grooney_rapid_white_games) %>%
  mutate(opening_name = ifelse(grepl(":", opening.name), substr(opening.name, 1, unlist(gregexpr(":", opening.name)) -1), opening.name)) %>% 
  mutate(opening_name=ifelse(opening_name=="GrÄ‚Ä1nfeld Defense","Grunfeld Defense",opening_name))%>% 
  mutate(color=ifelse(white=="grooney","white","black")) %>% 
  select(id,opening_name,color,hero.points)

df_grooney_rapid_black_moves<-df_grooney_rapid_black_moves %>% 
  select(game.id,black) %>% 
  mutate(color="black") %>% 
  rename(move=black)
df_grooney_rapid_white_moves<-df_grooney_rapid_white_moves %>% 
  select(game.id,white) %>% 
  mutate(color="white") %>% 
  rename(move=white)
df_grooney_rapid_both_moves<-bind_rows(df_grooney_rapid_black_moves,df_grooney_rapid_white_moves)

df_grooney_rapid_0 <- df_grooney_rapid_both_games %>%
  group_by(color,opening_name) %>%
  summarise(n = n(),avg_points = mean(hero.points)) %>%
  arrange(-n) %>%
  mutate(opening_name = forcats::fct_reorder(opening_name, n)) 

df_grooney_rapid_ruchygracza<-inner_join(df_grooney_rapid_both_games,df_grooney_rapid_both_moves,by = c("id" = "game.id","color"="color")) %>% 
  mutate(move=case_when(is.na(as.numeric(substr(move,nchar(move),nchar(move)))) == FALSE ~ substr(move,nchar(move)-1,nchar(move)),
                        (substr(move,1,5) == "O-O-O" & color=="black") ~ "c8d8",
                        (substr(move,1,5) == "O-O-O" & color=="white") ~ "c1d1",
                        (substr(move,1,3) == "O-O" & color=="black") ~ "g8f8",
                        (substr(move,1,3) == "O-O" & color=="white") ~ "g1f1",
                        substr(move,nchar(move)-1,nchar(move)-1) == "=" ~ substr(move,nchar(move)-3,nchar(move)-2),
                        substr(move,nchar(move)-2,nchar(move)-2) == "=" ~ substr(move,nchar(move)-4,nchar(move)-3),
                        substr(move,nchar(move),nchar(move)) == "+" ~ substr(move,nchar(move)-2,nchar(move)-1),
                        substr(move,nchar(move),nchar(move)) == "#" ~ substr(move,nchar(move)-2,nchar(move)-1),
                        TRUE ~ "nie"))

df_grooney_rapid_ruchygracza_add1<- filter(df_grooney_rapid_ruchygracza,nchar(move)==4) %>% 
  mutate(move=substr(move,1,2))
df_grooney_rapid_ruchygracza_add2<- filter(df_grooney_rapid_ruchygracza,nchar(move)==4) %>% 
  mutate(move=substr(move,3,4))

df_grooney_rapid_ruchygracza<-rbind(filter(df_grooney_rapid_ruchygracza,nchar(move)==2),df_grooney_rapid_ruchygracza_add1,df_grooney_rapid_ruchygracza_add2)%>% 
  group_by(opening_name,color,move) %>%
  summarise(count = n())


df_grooney_rapid_0<-df_grooney_rapid_0 %>% 
  mutate(gracz="grooney",rodzaj="rapid")
df_grooney_rapid_ruchygracza<-df_grooney_rapid_ruchygracza %>% 
  mutate(gracz="grooney",rodzaj="rapid")


df_Mlodziak77_bullet_both_games <- bind_rows(df_Mlodziak77_bullet_black_games,df_Mlodziak77_bullet_white_games) %>%
  mutate(opening_name = ifelse(grepl(":", opening.name), substr(opening.name, 1, unlist(gregexpr(":", opening.name)) -1), opening.name)) %>% 
  mutate(opening_name=ifelse(opening_name=="GrÄ‚Ä1nfeld Defense","Grunfeld Defense",opening_name))%>% 
  mutate(color=ifelse(white=="Mlodziak77","white","black")) %>% 
  select(id,opening_name,color,hero.points)

df_Mlodziak77_bullet_black_moves<-df_Mlodziak77_bullet_black_moves %>% 
  select(game.id,black) %>% 
  mutate(color="black") %>% 
  rename(move=black)
df_Mlodziak77_bullet_white_moves<-df_Mlodziak77_bullet_white_moves %>% 
  select(game.id,white) %>% 
  mutate(color="white") %>% 
  rename(move=white)
df_Mlodziak77_bullet_both_moves<-bind_rows(df_Mlodziak77_bullet_black_moves,df_Mlodziak77_bullet_white_moves)

df_Mlodziak77_bullet_0 <- df_Mlodziak77_bullet_both_games %>%
  group_by(color,opening_name) %>%
  summarise(n = n(),avg_points = mean(hero.points)) %>%
  arrange(-n) %>%
  mutate(opening_name = forcats::fct_reorder(opening_name, n)) 



df_Mlodziak77_bullet_ruchygracza<-inner_join(df_Mlodziak77_bullet_both_games,df_Mlodziak77_bullet_both_moves,by = c("id" = "game.id","color"="color")) %>% 
  mutate(move=case_when(is.na(as.numeric(substr(move,nchar(move),nchar(move)))) == FALSE ~ substr(move,nchar(move)-1,nchar(move)),
                        (substr(move,1,5) == "O-O-O" & color=="black") ~ "c8d8",
                        (substr(move,1,5) == "O-O-O" & color=="white") ~ "c1d1",
                        (substr(move,1,3) == "O-O" & color=="black") ~ "g8f8",
                        (substr(move,1,3) == "O-O" & color=="white") ~ "g1f1",
                        substr(move,nchar(move)-1,nchar(move)-1) == "=" ~ substr(move,nchar(move)-3,nchar(move)-2),
                        substr(move,nchar(move)-2,nchar(move)-2) == "=" ~ substr(move,nchar(move)-4,nchar(move)-3),
                        substr(move,nchar(move),nchar(move)) == "+" ~ substr(move,nchar(move)-2,nchar(move)-1),
                        substr(move,nchar(move),nchar(move)) == "#" ~ substr(move,nchar(move)-2,nchar(move)-1),
                        TRUE ~ "nie"))

df_Mlodziak77_bullet_ruchygracza_add1<- filter(df_Mlodziak77_bullet_ruchygracza,nchar(move)==4) %>% 
  mutate(move=substr(move,1,2))
df_Mlodziak77_bullet_ruchygracza_add2<- filter(df_Mlodziak77_bullet_ruchygracza,nchar(move)==4) %>% 
  mutate(move=substr(move,3,4))

df_Mlodziak77_bullet_ruchygracza<-rbind(filter(df_Mlodziak77_bullet_ruchygracza,nchar(move)==2),df_Mlodziak77_bullet_ruchygracza_add1,df_Mlodziak77_bullet_ruchygracza_add2)%>% 
  group_by(opening_name,color,move) %>%
  summarise(count = n())

df_Mlodziak77_bullet_0<-df_Mlodziak77_bullet_0 %>% 
  mutate(gracz="Mlodziak77",rodzaj="bullet")
df_Mlodziak77_bullet_ruchygracza<-df_Mlodziak77_bullet_ruchygracza %>% 
  mutate(gracz="Mlodziak77",rodzaj="bullet")


df_Mlodziak77_blitz_both_games <- bind_rows(df_Mlodziak77_blitz_black_games,df_Mlodziak77_blitz_white_games) %>%
  mutate(opening_name = ifelse(grepl(":", opening.name), substr(opening.name, 1, unlist(gregexpr(":", opening.name)) -1), opening.name)) %>% 
  mutate(opening_name=ifelse(opening_name=="GrÄ‚Ä1nfeld Defense","Grunfeld Defense",opening_name))%>% 
  mutate(color=ifelse(white=="Mlodziak77","white","black")) %>% 
  select(id,opening_name,color,hero.points)

df_Mlodziak77_blitz_black_moves<-df_Mlodziak77_blitz_black_moves %>% 
  select(game.id,black) %>% 
  mutate(color="black") %>% 
  rename(move=black)
df_Mlodziak77_blitz_white_moves<-df_Mlodziak77_blitz_white_moves %>% 
  select(game.id,white) %>% 
  mutate(color="white") %>% 
  rename(move=white)
df_Mlodziak77_blitz_both_moves<-bind_rows(df_Mlodziak77_blitz_black_moves,df_Mlodziak77_blitz_white_moves)

df_Mlodziak77_blitz_0 <- df_Mlodziak77_blitz_both_games %>%
  group_by(color,opening_name) %>%
  summarise(n = n(),avg_points = mean(hero.points)) %>%
  arrange(-n) %>%
  mutate(opening_name = forcats::fct_reorder(opening_name, n)) 



df_Mlodziak77_blitz_ruchygracza<-inner_join(df_Mlodziak77_blitz_both_games,df_Mlodziak77_blitz_both_moves,by = c("id" = "game.id","color"="color")) %>% 
  mutate(move=case_when(is.na(as.numeric(substr(move,nchar(move),nchar(move)))) == FALSE ~ substr(move,nchar(move)-1,nchar(move)),
                        (substr(move,1,5) == "O-O-O" & color=="black") ~ "c8d8",
                        (substr(move,1,5) == "O-O-O" & color=="white") ~ "c1d1",
                        (substr(move,1,3) == "O-O" & color=="black") ~ "g8f8",
                        (substr(move,1,3) == "O-O" & color=="white") ~ "g1f1",
                        substr(move,nchar(move)-1,nchar(move)-1) == "=" ~ substr(move,nchar(move)-3,nchar(move)-2),
                        substr(move,nchar(move)-2,nchar(move)-2) == "=" ~ substr(move,nchar(move)-4,nchar(move)-3),
                        substr(move,nchar(move),nchar(move)) == "+" ~ substr(move,nchar(move)-2,nchar(move)-1),
                        substr(move,nchar(move),nchar(move)) == "#" ~ substr(move,nchar(move)-2,nchar(move)-1),
                        TRUE ~ "nie"))

df_Mlodziak77_blitz_ruchygracza_add1<- filter(df_Mlodziak77_blitz_ruchygracza,nchar(move)==4) %>% 
  mutate(move=substr(move,1,2))
df_Mlodziak77_blitz_ruchygracza_add2<- filter(df_Mlodziak77_blitz_ruchygracza,nchar(move)==4) %>% 
  mutate(move=substr(move,3,4))

df_Mlodziak77_blitz_ruchygracza<-rbind(filter(df_Mlodziak77_blitz_ruchygracza,nchar(move)==2),df_Mlodziak77_blitz_ruchygracza_add1,df_Mlodziak77_blitz_ruchygracza_add2)%>% 
  group_by(opening_name,color,move) %>%
  summarise(count = n())

df_Mlodziak77_blitz_0<-df_Mlodziak77_blitz_0 %>% 
  mutate(gracz="Mlodziak77",rodzaj="blitz")
df_Mlodziak77_blitz_ruchygracza<-df_Mlodziak77_blitz_ruchygracza %>% 
  mutate(gracz="Mlodziak77",rodzaj="blitz")


df_Mlodziak77_rapid_both_games <- bind_rows(df_Mlodziak77_rapid_black_games,df_Mlodziak77_rapid_white_games) %>%
  mutate(opening_name = ifelse(grepl(":", opening.name), substr(opening.name, 1, unlist(gregexpr(":", opening.name)) -1), opening.name)) %>% 
  mutate(opening_name=ifelse(opening_name=="GrÄ‚Ä1nfeld Defense","Grunfeld Defense",opening_name))%>% 
  mutate(color=ifelse(white=="Mlodziak77","white","black")) %>% 
  select(id,opening_name,color,hero.points)

df_Mlodziak77_rapid_black_moves<-df_Mlodziak77_rapid_black_moves %>% 
  select(game.id,black) %>% 
  mutate(color="black") %>% 
  rename(move=black)
df_Mlodziak77_rapid_white_moves<-df_Mlodziak77_rapid_white_moves %>% 
  select(game.id,white) %>% 
  mutate(color="white") %>% 
  rename(move=white)
df_Mlodziak77_rapid_both_moves<-bind_rows(df_Mlodziak77_rapid_black_moves,df_Mlodziak77_rapid_white_moves)

df_Mlodziak77_rapid_0 <- df_Mlodziak77_rapid_both_games %>%
  group_by(color,opening_name) %>%
  summarise(n = n(),avg_points = mean(hero.points)) %>%
  arrange(-n) %>%
  mutate(opening_name = forcats::fct_reorder(opening_name, n)) 



df_Mlodziak77_rapid_ruchygracza<-inner_join(df_Mlodziak77_rapid_both_games,df_Mlodziak77_rapid_both_moves,by = c("id" = "game.id","color"="color")) %>% 
  mutate(move=case_when(is.na(as.numeric(substr(move,nchar(move),nchar(move)))) == FALSE ~ substr(move,nchar(move)-1,nchar(move)),
                        (substr(move,1,5) == "O-O-O" & color=="black") ~ "c8d8",
                        (substr(move,1,5) == "O-O-O" & color=="white") ~ "c1d1",
                        (substr(move,1,3) == "O-O" & color=="black") ~ "g8f8",
                        (substr(move,1,3) == "O-O" & color=="white") ~ "g1f1",
                        substr(move,nchar(move)-1,nchar(move)-1) == "=" ~ substr(move,nchar(move)-3,nchar(move)-2),
                        substr(move,nchar(move)-2,nchar(move)-2) == "=" ~ substr(move,nchar(move)-4,nchar(move)-3),
                        substr(move,nchar(move),nchar(move)) == "+" ~ substr(move,nchar(move)-2,nchar(move)-1),
                        substr(move,nchar(move),nchar(move)) == "#" ~ substr(move,nchar(move)-2,nchar(move)-1),
                        TRUE ~ "nie"))

df_Mlodziak77_rapid_ruchygracza_add1<- filter(df_Mlodziak77_rapid_ruchygracza,nchar(move)==4) %>% 
  mutate(move=substr(move,1,2))
df_Mlodziak77_rapid_ruchygracza_add2<- filter(df_Mlodziak77_rapid_ruchygracza,nchar(move)==4) %>% 
  mutate(move=substr(move,3,4))

df_Mlodziak77_rapid_ruchygracza<-rbind(filter(df_Mlodziak77_rapid_ruchygracza,nchar(move)==2),df_Mlodziak77_rapid_ruchygracza_add1,df_Mlodziak77_rapid_ruchygracza_add2)%>% 
  group_by(opening_name,color,move) %>%
  summarise(count = n())

df_Mlodziak77_rapid_0<-df_Mlodziak77_rapid_0 %>% 
  mutate(gracz="Mlodziak77",rodzaj="rapid")
df_Mlodziak77_rapid_ruchygracza<-df_Mlodziak77_rapid_ruchygracza %>% 
  mutate(gracz="Mlodziak77",rodzaj="rapid")


df_tymekkk_blitz_both_games <- bind_rows(df_tymekkk_blitz_black_games,df_tymekkk_blitz_white_games) %>%
  mutate(opening_name = ifelse(grepl(":", opening.name), substr(opening.name, 1, unlist(gregexpr(":", opening.name)) -1), opening.name)) %>%
  mutate(opening_name=ifelse(opening_name=="GrÄ‚Ä1nfeld Defense","Grunfeld Defense",opening_name))%>% 
  mutate(color=ifelse(white=="tymekkk","white","black")) %>% 
  select(id,opening_name,color,hero.points)

df_tymekkk_blitz_black_moves<-df_tymekkk_blitz_black_moves %>% 
  select(game.id,black) %>% 
  mutate(color="black") %>% 
  rename(move=black)
df_tymekkk_blitz_white_moves<-df_tymekkk_blitz_white_moves %>% 
  select(game.id,white) %>% 
  mutate(color="white") %>% 
  rename(move=white)
df_tymekkk_blitz_both_moves<-bind_rows(df_tymekkk_blitz_black_moves,df_tymekkk_blitz_white_moves)

df_tymekkk_blitz_0 <- df_tymekkk_blitz_both_games %>%
  group_by(color,opening_name) %>%
  summarise(n = n(),avg_points = mean(hero.points)) %>%
  arrange(-n) %>%
  mutate(opening_name = forcats::fct_reorder(opening_name, n)) 



df_tymekkk_blitz_ruchygracza<-inner_join(df_tymekkk_blitz_both_games,df_tymekkk_blitz_both_moves,by = c("id" = "game.id","color"="color")) %>% 
  mutate(move=case_when(is.na(as.numeric(substr(move,nchar(move),nchar(move)))) == FALSE ~ substr(move,nchar(move)-1,nchar(move)),
                        (substr(move,1,5) == "O-O-O" & color=="black") ~ "c8d8",
                        (substr(move,1,5) == "O-O-O" & color=="white") ~ "c1d1",
                        (substr(move,1,3) == "O-O" & color=="black") ~ "g8f8",
                        (substr(move,1,3) == "O-O" & color=="white") ~ "g1f1",
                        substr(move,nchar(move)-1,nchar(move)-1) == "=" ~ substr(move,nchar(move)-3,nchar(move)-2),
                        substr(move,nchar(move)-2,nchar(move)-2) == "=" ~ substr(move,nchar(move)-4,nchar(move)-3),
                        substr(move,nchar(move),nchar(move)) == "+" ~ substr(move,nchar(move)-2,nchar(move)-1),
                        substr(move,nchar(move),nchar(move)) == "#" ~ substr(move,nchar(move)-2,nchar(move)-1),
                        TRUE ~ "nie")) %>% 
  filter(move!="nie")

df_tymekkk_blitz_ruchygracza_add1<- filter(df_tymekkk_blitz_ruchygracza,nchar(move)==4) %>% 
  mutate(move=substr(move,1,2))
df_tymekkk_blitz_ruchygracza_add2<- filter(df_tymekkk_blitz_ruchygracza,nchar(move)==4) %>% 
  mutate(move=substr(move,3,4))

df_tymekkk_blitz_ruchygracza<-rbind(filter(df_tymekkk_blitz_ruchygracza,nchar(move)==2),df_tymekkk_blitz_ruchygracza_add1,df_tymekkk_blitz_ruchygracza_add2)%>% 
  group_by(opening_name,color,move) %>%
  summarise(count = n())

df_tymekkk_blitz_0<-df_tymekkk_blitz_0 %>% 
  mutate(gracz="tymekkk",rodzaj="blitz")
df_tymekkk_blitz_ruchygracza<-df_tymekkk_blitz_ruchygracza %>% 
  mutate(gracz="tymekkk",rodzaj="blitz")

#heatmap_opening##########################################

df_0<-rbind(df_tymekkk_blitz_0,
            df_grooney_blitz_0,
            df_Mlodziak77_blitz_0,
            df_grooney_rapid_0,
            df_Mlodziak77_rapid_0,
            df_grooney_bullet_0,
            df_Mlodziak77_bullet_0) 

df_ruchygracza<-rbind(df_tymekkk_blitz_ruchygracza,
                      df_grooney_blitz_ruchygracza,
                      df_Mlodziak77_blitz_ruchygracza,
                      df_grooney_rapid_ruchygracza,
                      df_Mlodziak77_rapid_ruchygracza,
                      df_grooney_bullet_ruchygracza,
                      df_Mlodziak77_bullet_ruchygracza) 


openingi<-df_ruchygracza%>% 
  ungroup() %>% 
  distinct(opening_name) %>% 
  arrange(opening_name) %>% 
  pull(opening_name)

d<-length(openingi)

gracz<-rep(c("grooney","Mlodziak77","tymekkk"),times=1,each=384*d)
rodzaj<-rep(c("blitz","bullet","rapid")       ,times=3,each=128*d)
color<- rep(c("white", "black")               ,times=9,each=64*d)
opening_name<-rep(openingi                    ,times=18,each=64)
X<-rep(letters[1:8]                           ,times=18*d,each=8)
Y<-rep(as.character(1:8)                      ,times=144*d,each=1)
count<-rep(0                                ,times=1152*d,each=1)
df_heatmap_0_openingi<-data.frame(gracz,rodzaj,color,opening_name,X,Y,count)


df_ruchygracza<-df_ruchygracza %>% 
  mutate(X=substr(move,1,1),Y=substr(move,2,2)) %>% 
  select(gracz,rodzaj,color,opening_name,X,Y,count) %>% 
  rbind(df_heatmap_0_openingi) %>% 
  distinct(gracz,rodzaj,color,opening_name,X,Y,.keep_all = TRUE) %>% 
  group_by(gracz,rodzaj,color,opening_name) %>%
  mutate(suma=sum(count)) %>% 
  filter(suma!=0) %>% 
  select(-suma)


#write.csv########################################################################################
write.csv(df_plot1,"dane_obrobione/df_plot1.csv",row.names = FALSE)
write.csv(df_plot2,"dane_obrobione/df_plot2.csv",row.names = FALSE)
write.csv(df_heatmap,"dane_obrobione/df_heatmap.csv",row.names = FALSE)
write.csv(df_scatter,"dane_obrobione/df_scatter.csv",row.names = FALSE)
write.csv(df_0,"dane_obrobione/df_0.csv",row.names = FALSE)
write.csv(df_ruchygracza,"dane_obrobione/df_ruchygracza.csv",row.names = FALSE)
write.csv(openingi,"dane_obrobione/openingi.csv",row.names = FALSE)
