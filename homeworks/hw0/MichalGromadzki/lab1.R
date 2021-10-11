###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 1            ###
###########################################

## 0) Prowadzacy.
# Hubert Baniecki/Anna Kozak
# Kontakt: MS Teams lub mail
# a.kozak@mini.pw.edu.pl

## 1) Materialy
# Repozytorium na GitHub
# https://github.com/mini-pw/2022Z-DataVisualizationTechniques 

## 2) Jak dziala GitHub?
# Jak zglosic prace domowa/projekt? (fork, commit, pull request)
# https://rogerdudler.github.io/git-guide/

## 3) Podstawy R - rozgrzewka 

data(mtcars)
head(mtcars)

?mtcars


# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?
mtcars[1,1]
mtcars[4:5,2:3]
# Pierwszy wiersz, pierwsza kolumna?

# 10 pierszych wierszy, 2 i 3 kolumna?
mtcras[1:10,]
head(mtcars[,2:3],10)
# Wszytskie wiersze i kolumny w kolejnosci "am", "wt", "mpg"?
mtcars[,c("am","wt","mpg")]
# Jak wybierac jedna kolumne?
mtcras[,2]
mtcras[,"am"]
mtcars$carb
# Pytania

# 1. Wymiar ramki danych
dim(mtcars)
# 2. Jakie sa typy zmiennych?
str(mtcars)
# 3. Ile jest unikalnych wartosci zmiennej "cyl" i jakie to sa wartosci?
a <- unique(mtcars$cyl)
length(a)
# 4. Jaka jest srednia wartosc zmiennej "drat" dla samochodów o wartosci zmiennej "cyl" równej 4?
mean(mtcars[mtcars$cyl==4,"drat"])
median(mtcars[mtcars$cyl==4,"drat"])
# Prosty wykres

# Zaleznosc "mpg" i "hp" - scatter plot
plot(mtcars$mpg, mtcars$hp)

# Zmienna "cyl" - barplot
barplot(mtcars$cyl)
barplot(table(mtcars$cyl))

## 4) Gra proton, nalezy stworzyc plik R z kodami do rozwiazania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()
tab <- employees
tab[tab$name == "John",]
proton(action = "login", login="johnins")
for(i in 1:1000){
  proton(action = "login", login="johnins",password=top1000passwords[i])
}
tab[tab$surname=="Pietraszko",] ##slap
logs[logs$login=="slap", "host"]
proton(action = "server", host="194.29.178.16")
b <- strsplit(bash_history,"")
for(x in b){
  comads <- c(comads,x[[1]])
}
for(comads in unique(comads)){
  proton(action="login", login="slap", password = comads)
}
## 5) Umieszczamy rozwiazanie na repozytorium.