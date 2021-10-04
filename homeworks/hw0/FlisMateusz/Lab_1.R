###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 1            ###
###########################################

## 0) Prowadzący.
# Hubert Baniecki/Anna Kozak
# Kontakt: MS Teams lub mail
# a.kozak@mini.pw.edu.pl

## 1) Materiały
# Repozytorium na GitHub
# https://github.com/mini-pw/2022Z-DataVisualizationTechniques 

## 2) Jak działa GitHub?
# Jak zgłosić pracę domową/projekt? (fork, commit, pull request)
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
mtcars[1:10,]
head(mtcars[,2:3],10)
# Wszytskie wiersze i kolumny w kolejności "am", "wt", "mpg"?
mtcars[,c("am","wt","mpg")]
# Jak wybierać jedną kolumnę?
mtcars[,2]
mtcars$disp
mtcars[,"am"]
# Pytania

# 1. Wymiar ramki danych
dim(mtcars)
# 2. Jakie są typy zmiennych?
str(mtcars)
# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?
a <- unique(mtcars$cyl)
length(a)
# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?
mean(mtcars[mtcars$cyl==4,"drat"])

# Prosty wykres

# Zależność "mpg" i "hp" - scatter plot

plot(mtcars$mpg,mtcars$hp)

# Zmienna "cyl" - barplot

barplot(mtcars$cyl)
table(mtcars$cyl)
barplot(table(mtcars$cyl))
## 4) Gra proton, nalezy stworzyc plik R z kodami do rozwiązania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()
employees[employees$name == "John",]
proton(action = "login", login="johnins")
sort(top1000passwords)
i <- 0
while (i<1000) {
  proton(action = "login", login="johnins", password=top1000passwords[i])
  i <- i+1
}
logs[logs$login=="slap",]
table(logs)

logs1 <- logs[logs$login=="slap",]
employees[employees$surname=="Pietraszko",]
sort(table(logs1$host))
proton(action = "server", host="194.29.178.16")
bash_history
splitbashhistory <- strsplit(bash_history," ")
for (x in splitbashhistory) {
  comands <- c(comands,x[[1]])
}
for (comands in unique(comands)) {
  proton(action="login",login="slap",password = comands)
}
## 5) Umieszczamy rozwiązanie na repozytorium.