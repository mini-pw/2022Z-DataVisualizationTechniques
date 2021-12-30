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
mtcars[1:10,]
head(mtcars[,2:3], 10)

# Pierwszy wiersz, pierwsza kolumna?

# 10 pierszych wierszy, 2 i 3 kolumna?

# Wszytskie wiersze i kolumny w kolejności "am", "wt", "mpg"?
mtcars[, c("am", "wt", "mpg")]


# Jak wybierać jedną kolumnę?
mtcars[, 2]
mtcars[ , c("am")]
mtcars$am 


# Pytania

# 1. Wymiar ramki danych
dim(mtcars)

# 2. Jakie są typy zmiennych?
str(mtcars)

# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?
a <-  unique(mtcars$cyl)
length(a)

# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?
mean(mtcars[mtcars$cyl == 4, "drat"])

# Prosty wykres


# Zależność "mpg" i "hp" - scatter plot

plot(mtcars$mpg, mtcars$hp)


# Zmienna "cyl" - barplot
barplot(mtcars$cyl)

barplot(table(mtcars$cyl))


## 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()

employees[employees$name == "John",]
proton(action = "login", login="johnins")
head(top1000passwords)

for(i in top1000passwords){
  proton(action = "login", login="johnins", password=i)
}

proton(action = "server", host="XYZ")
head(logs)

employees[employees$surname == "Pietraszko",]
name = "Slawomir"
login = "slap"


Slap_logs  <-  (logs[logs$login == "slap",])
table(Slap_logs$host) 
sort(table(Slap_logs$host))

proton(action = "server", host="194.29.178.16")


bash_history
split_bash <- strsplit(bash_history, " ")

for(x in split_bash){
  comads <-  c(comads, x[[1]])
}

for(comads in unique(comads)){
  proton(action = "login", login = " slap", password = comads)
}


## 5) Umieszczamy rozwiązanie na repozytorium.