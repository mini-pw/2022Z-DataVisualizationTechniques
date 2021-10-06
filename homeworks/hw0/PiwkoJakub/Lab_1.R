###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 1            ###
###########################################

## 0) ProwadzÄ…cy.
# Hubert Baniecki/Anna Kozak
# Kontakt: MS Teams lub mail
# a.kozak@mini.pw.edu.pl

## 1) MateriaĹ‚y
# Repozytorium na GitHub
# https://github.com/mini-pw/2022Z-DataVisualizationTechniques 

## 2) Jak dziaĹ‚a GitHub?
# Jak zgĹ‚osiÄ‡ pracÄ™ domowÄ…/projekt? (fork, commit, pull request)
# https://rogerdudler.github.io/git-guide/

## 3) Podstawy R - rozgrzewka 

data(mtcars)
head(mtcars)

?mtcars

# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?

#wiersze
mtcars[30, ]
#kolumny
mtcars[, "cyl"]

# Pierwszy wiersz, pierwsza kolumna?

mtcars[1,1]

# 10 pierszych wierszy, 2 i 3 kolumna?

mtcars[1:10, 2:3]

# Wszytskie wiersze i kolumny w kolejnoĹ›ci "am", "wt", "mpg"?

mtcars[, c("am", "wt", "mpg")]

# Jak wybieraÄ‡ jednÄ… kolumnÄ™?

mtcars[, 2]

# Pytania

# 1. Wymiar ramki danych

dim(mtcars)

# 2. Jakie sÄ… typy zmiennych?

str(mtcars)

# 3. Ile jest unikalnych wartoĹ›ci zmiennej "cyl" i jakie to sÄ… wartoĹ›ci

length(unique(mtcars$cyl))

# 4. Jaka jest Ĺ›rednia wartoĹ›Ä‡ zmiennej "drat" dla samochodĂłw o wartoĹ›ci zmiennej "cyl" rĂłwnej 4?

mean(mtcars[mtcars$cyl == 4, "drat"])

# Prosty wykres

# ZaleĹĽnoĹ›Ä‡ "mpg" i "hp" - scatter plot

plot(mtcars$mpg, mtcars$hp)

# Zmienna "cyl" - barplot

barplot(mtcars$cyl)
barplot(table(mtcars$cyl))

## 4) Gra proton, naleĹĽy stworzyÄ‡ plik R z kodami do rozwiÄ…zania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()

head(employees)
employees[employees$name == "John" & employees$surname == "Insecure", ]
#johnins

proton(action = "login", login="johnins")

head(top1000passwords)

for (pwd in top1000passwords) {
  proton(action = "login", login = "johnins", password = pwd)
}
  
head(logs)
employees[employees$surname == "Pietraszko", "login"]
# login: slap

l <- logs[logs$login == "slap",]
l <- as.data.frame(table(l$host))
l[l$Freq == max(l$Freq),]
# host: 194.29.178.16

proton(action = "server", host="194.29.178.16")

head(bash_history)
library(stringr)
df <- stringr::str_split(bash_history, " ", simplify = T)
comands <- df[,1]

for (command in unique(comands))
  proton(action = "login", login = "slap", password = command)
  

## 5) Umieszczamy rozwiÄ…zanie na repozytorium.
