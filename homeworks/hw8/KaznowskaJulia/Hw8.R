library(dplyr)
library(ggplot2)
library(dlookr)
library("DataExplorer")
library(patchwork)
library(xtable)

# rozmiary ramki
# ile kolumn ile wierszy
# jakie kolumny
# co oznaczają ?(weż stamtąd?)
# braki w danych
# liczba unikalnych wartości
# barploty żeby zobaczyć czy czegoś jest dużo gdzieś
# bruises, it is probably edible



#setwd("C:/Users/farbo/Desktop/Studenckie życie/Semestr 3/Techniki Wizualizacji Danych/Hw8/data")
mushrooms <- read.csv("./mushrooms.csv")

# wstępna analiza
dim(mushrooms)
str(mushrooms)
lapply(mushrooms, unique)
lapply(lapply(mushrooms, unique), length)
mushrooms %>% 
  filter(stalk.root == "?") %>% 
  summarise(count = n())

2480/(dim(mushrooms)[1])



# tabelka licząca
kolumna - liczba - procent


tab <- function(x) {
  len = dim(mushrooms)[1]
  wynik <- mushrooms %>% 
    group_by(!! rlang::ensym(x)) %>% 
    summarise(count = n()) %>% 
    mutate(percentage = count/len*100) %>% 
    arrange(-percentage)
}

# wykresy
plttt <- function(z) {
  mushrooms %>% 
    select(!! rlang::ensym(z)) %>% 
    ggplot() +
    geom_bar(aes(x = !! rlang::ensym(z)))
}

bob <- names(mushrooms)

for(i in 1:length(bob)){
  col <- bob[i]
  nam <- paste0(col, "_tab", sep = "")
  assign(nam, tab(!! rlang::ensym(col)))
  nam2 <- paste0("plot", i, sep="")
  assign(nam2, plttt(!! rlang::ensym(col)))
}

plot2

# łączenie wykresów
p1 <- plot1 | plot2 | plot3 
p2 <- plot4 | plot5 | plot6
p3 <- plot7 | plot8 | plot9
p4 <- plot10 | plot11 | plot12
p5 <- plot13 | plot14 | plot15
p6 <- plot16 | plot17 | plot18
p7 <- plot19 | plot20 | plot21
p8 <- plot22 | plot23

p <- p1 / p2 / p3 / p4 
pp <- p5 / p6 / p7 / p8

p
pp



# odor
ggplot(mushrooms) + 
  geom_bar(aes(x = odor, fill = class),position = "dodge") +
  labs(title = "Odor and poisonousness of mushrooms") +
  scale_fill_manual(labels = c("edible", "poisonous"), values = c("skyblue", "coral1"))

# gill.size
ggplot(mushrooms) + 
  geom_bar(aes(x = gill.size, fill = class),position = "dodge") +
  labs(title = "Gill size and poisonousness of mushrooms") +
  scale_fill_manual(labels = c("edible", "poisonous"), values = c("skyblue", "coral1"))

# gill.color
ggplot(mushrooms) + 
  geom_bar(aes(x = gill.color, fill = class),position = "dodge") +
  labs(title = "Gill colour and poisonousness of mushrooms") +
  scale_fill_manual(labels = c("edible", "poisonous"), values = c("skyblue", "coral1"))

# stalk.root
ggplot(mushrooms) + 
  geom_bar(aes(x = stalk.root, fill = class),position = "dodge") +
  labs(title = "Stalk root and poisonousness of mushrooms") +
  scale_fill_manual(labels = c("edible", "poisonous"), values = c("skyblue", "coral1"))


# stalk.surface
stalks <- mushrooms %>% 
  group_by(stalk.surface.above.ring, stalk.surface.below.ring) %>% 
  summarise(count = n()) %>% 
  arrange(-count)

# sum same
sim1 <- stalks %>% 
  filter(stalk.surface.above.ring == stalk.surface.below.ring)
sum(sim1$count)
sum(sim1$count)/dim(mushrooms)[1]*100

print(xtable(stalks, type = "latex"), file = "stalks.tex")


# stalk.surface
stalks2 <- mushrooms %>% 
  group_by(stalk.color.above.ring, stalk.color.below.ring) %>% 
  summarise(count = n()) %>% 
  arrange(-count)

# sum same
sim2 <- stalks2 %>% 
  filter(stalk.color.above.ring == stalk.color.below.ring)
sum(sim2$count)
sum(sim2$count)/dim(mushrooms)[1]*100

print(xtable(stalks2, type = "latex"), file = "stalks2.tex")

# surf above
ggplot(mushrooms) + 
  geom_bar(aes(x = stalk.surface.above.ring, fill = class),position = "dodge") +
  labs(title = "Stalk surface above ring and poisonousness") +
  scale_fill_manual(labels = c("edible", "poisonous"), values = c("skyblue", "coral1"))

# surf below
ggplot(mushrooms) + 
  geom_bar(aes(x = stalk.surface.below.ring, fill = class),position = "dodge") +
  labs(title = "Stalk surface below ring and poisonousness") +
  scale_fill_manual(labels = c("edible", "poisonous"), values = c("skyblue", "coral1"))

# ring type
ggplot(mushrooms) + 
  geom_bar(aes(x = ring.type, fill = class),position = "dodge") +
  labs(title = "Ring type and poisonousness of mushrooms") +
  scale_fill_manual(labels = c("edible", "poisonous"), values = c("skyblue", "coral1"))

# spore print
ggplot(mushrooms) + 
  geom_bar(aes(x = spore.print.color, fill = class),position = "dodge") +
  labs(title = "Spore print color and poisonousness") +
  scale_fill_manual(labels = c("edible", "poisonous"), values = c("skyblue", "coral1"))

# dobre wg wskazówek
good <- mushrooms %>% 
  filter(odor == "n") %>% 
  filter(gill.color == "n") %>% 
  filter(ring.type == "p") %>% 
  filter(spore.print.color %in% c("k", "n")) %>% 
  group_by(class) %>% 
  summarise(count = n())

allgood <- mushrooms %>% 
  filter(class == "e") %>% 
  summarise(count = n())
100 - 472/4208*100


# złe wg wskazówek
bad <- mushrooms %>% 
  filter(odor == "f") %>% 
  filter(gill.size == "n") %>% 
  filter(gill.color == "b") %>% 
  filter(stalk.surface.above.ring == "k") %>% 
  filter(ring.type == "l") %>% 
  filter(spore.print.color %in% c("h", "w")) %>% 
  group_by(class) %>% 
  summarise(count = n())
  