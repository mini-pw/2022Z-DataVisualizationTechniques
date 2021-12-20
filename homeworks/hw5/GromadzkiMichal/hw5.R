library(tidyverse)
library(plotly)
library(dplyr)

df <- read.csv("complete.csv")
unique(df$category)

# Zliczenie nagród oraz dodanie dodatkowych wierszy, aby zachowaæ ci¹g³oœæ wizualizacji

dfPeace <- df %>% 
  filter(category=="Peace") %>% 
  group_by(awardYear) %>% 
  summarise(n=n()) %>% 
  mutate(csum = cumsum(n),category="Peace")

for (i in 1:nrow(dfPeace)){
  if(dfPeace[i,"awardYear"]!=1900+i){
    dfPeace <- dfPeace %>%
      add_row(awardYear=1900+i,n=0,csum=as.numeric(dfPeace[i-1,"csum"]),category="Peace") %>% 
      arrange(awardYear)
  }
}

dfPeace <- dfPeace %>%  add_row(awardYear=1900,n=0,csum=0,category="Peace") %>% arrange(awardYear)


dfPhysics <- df %>% 
  filter(category=="Physics") %>% 
  group_by(awardYear) %>% 
  summarise(n=n()) %>% 
  mutate(csum = cumsum(n),category="Physics")

for (i in 1:nrow(dfPhysics)){
  if(dfPhysics[i,"awardYear"]!=1900+i){
    dfPhysics <- dfPhysics %>%
      add_row(awardYear=1900+i,n=0,csum=as.numeric(dfPhysics[i-1,"csum"]),category="Physics") %>% 
      arrange(awardYear)
  }
}
dfPhysics <- dfPhysics %>%  add_row(awardYear=1900,n=0,csum=0,category="Physics") %>% arrange(awardYear)


dfLiterature <- df %>% 
  filter(category=="Literature") %>% 
  group_by(awardYear) %>% 
  summarise(n=n()) %>% 
  mutate(csum = cumsum(n),category="Literature")

for (i in 1:nrow(dfLiterature)){
  if(dfLiterature[i,"awardYear"]!=1900+i){
    dfLiterature <- dfLiterature %>%
      add_row(awardYear=1900+i,n=0,csum=as.numeric(dfLiterature[i-1,"csum"]),category="Literature") %>% 
      arrange(awardYear)
  }
}
dfLiterature <- dfLiterature %>%  add_row(awardYear=1900,n=0,csum=0,category="Literature") %>% arrange(awardYear)


dfChemistry <- df %>% 
  filter(category=="Chemistry") %>% 
  group_by(awardYear) %>% 
  summarise(n=n()) %>% 
  mutate(csum = cumsum(n),category="Chemistry")

for (i in 1:nrow(dfChemistry)){
  if(dfChemistry[i,"awardYear"]!=1900+i){
    dfChemistry <- dfChemistry %>%
      add_row(awardYear=1900+i,n=0,csum=as.numeric(dfChemistry[i-1,"csum"]),category="Chemistry") %>% 
      arrange(awardYear)
  }
}
dfChemistry <- dfChemistry %>%  add_row(awardYear=1900,n=0,csum=0,category="Chemistry") %>% arrange(awardYear)


dfES <- df %>% 
  filter(category=="Economic Sciences") %>% 
  group_by(awardYear) %>% 
  summarise(n=n()) %>% 
  mutate(csum = cumsum(n),category="Economic Sciences")

for (i in 1:nrow(dfES)){
  if(dfES[i,"awardYear"]!=1968+i){
    dfES <- dfES %>%
      add_row(awardYear=1968+i,n=0,csum=as.numeric(dfES[i-1,"csum"]),category="Economic Sciences") %>% 
      arrange(awardYear)
  }
}


dfPorM <- df %>% 
  filter(category=="Physiology or Medicine") %>% 
  group_by(awardYear) %>% 
  summarise(n=n()) %>% 
  mutate(csum = cumsum(n),category="Physiology or Medicine")

for (i in 1:nrow(dfPorM)){
  if(dfPorM[i,"awardYear"]!=1900+i){
    dfPorM <- dfPorM %>%
      add_row(awardYear=1900+i,n=0,csum=as.numeric(dfPorM[i-1,"csum"]),category="Physiology or Medicine") %>% 
      arrange(awardYear)
  }
}
dfPorM <- dfPorM %>%  add_row(awardYear=1900,n=0,csum=0,category="Physiology or Medicine") %>% arrange(awardYear)


dane <- rbind(dfPeace,dfPhysics,dfLiterature,dfChemistry,dfES,dfPorM)

# Tworzenie wizualizacji

plot <- plot_ly(data = dane, x = ~csum,y=~category, frame = ~awardYear, type = "bar") %>% 
  layout(
    title="The sum of Nobel prizes in each category by year",
    xaxis=list(title="Number of Nobel prizes"),
    yaxis=list(title="Category")
  )

plot %>% 
  animation_opts(200) %>%
  animation_button(x = 0.05, y = 0.05) %>%
  animation_slider(currentvalue = list(prefix = "Year: ",font = list(color="black")))
