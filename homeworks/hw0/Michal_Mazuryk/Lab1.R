###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 1            ###
###########################################

## 0) ProwadzÄcy.
# Hubert Baniecki/Anna Kozak
# Kontakt: MS Teams lub mail
# a.kozak@mini.pw.edu.pl

## 1) MateriaÅy
# Repozytorium na GitHub
# https://github.com/mini-pw/2022Z-DataVisualizationTechniques 

## 2) Jak dziaÅa GitHub?
# Jak zgÅosiÄ pracÄ domowÄ/projekt? (fork, commit, pull request)
# https://rogerdudler.github.io/git-guide/

## 3) Podstawy R - rozgrzewka 
data(mtcars)
head(mtcars)

?mtcars
# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?
mtcars[2:3,4:5]

# Pierwszy wiersz, pierwsza kolumna?
mtcars[1,1]

# 10 pierszych wierszy, 2 i 3 kolumna?
mtcars[1:10,2:3]

# Wszytskie wiersze i kolumny w kolejnoÅci "am", "wt", "mpg"?
mtcars[,c('am','wt','mpg')]

# Jak wybieraÄ jednÄ kolumnÄ?
mtcars$hp

# Pytania
ctrl shift m %>% 
  alt + -
  
# 1. Wymiar ramki danych
dim(mtcars)

# 2. Jakie sÄ typy zmiennych?
str(mtcars)

# 3. Ile jest unikalnych wartoÅci zmiennej "cyl" i jakie to sÄ wartoÅci?
unique(mtcars$cyl)

# 4. Jaka jest Årednia wartoÅÄ zmiennej "drat" dla samochodÃ³w o wartoÅci zmiennej "cyl" rÃ³wnej 4?
mean(mtcars$drat[mtcars$cyl==4])
mean(mtcars[mtcars$cyl==4, 'drat'])

# Prosty wykres

# ZaleÅ¼noÅÄ "mpg" i "hp" - scatter plot
plot(mtcars$mpg,mtcars$hp)

# Zmienna "cyl" - barplot
barplot(table(mtcars$cyl))

## 4) Gra proton, naleÅ¼y stworzyÄ plik R z kodami do rozwiÄzania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()

## 5) Umieszczamy rozwiÄzanie na repozytorium.
#1
employees[employees$surname=='Insecure',]
proton(action = "login", login="johnins")
#2

for(i in top1000passwords){proton(action = "login", login="johnins", password=i)}

#3
employees[employees$surname=='Pietraszko',]
table(logs[logs$login=='slap','host'])==max(table(logs[logs$login=='slap','host']))
proton(action = "server", host="194.29.178.16")

#4

for(i in strsplit(bash_history," ")
){proton(action = "login", login="slap", password=i)}