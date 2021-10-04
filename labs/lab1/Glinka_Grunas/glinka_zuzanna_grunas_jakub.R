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
mtcars[,1]
# Pierwszy wiersz, pierwsza kolumna?
mtcars[1,1]
# 10 pierszych wierszy, 2 i 3 kolumna?
mtcars[1:10, 2:3]
mtcars[1:10, c(2,4)]
# Wszytskie wiersze i kolumny w kolejności "am", "wt", "mpg"?
mtcars[, c("am", "wt", "mpg")]
# Jak wybierać jedną kolumnę?
mtcars$am
# Pytania

# 1. Wymiar ramki danych
dim(mtcars[1:10,])
# 2. Jakie są typy zmiennych?
str(mtcars)
# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?

unique(mtcars$cyl)
length(unique(mtcars$cyl))
table(mtcars$cyl)
# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?
mean(mtcars[mtcars$cyl == 4, "drat"])

# Prosty wykres
plot(mtcars$mpg, mtcars$hp)
# Zależność "mpg" i "hp" - scatter plot
barplot(mtcars$cyl)
barplot(table(mtcars$cyl))

# Zmienna "cyl" - barplot


## 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()
employees[employees$surname == 'Insecure','login']
proton(action = "login", login="johnins")
top1000passwords
for (pwd in top1000passwords)
{proton(action = "login", login="johnins", password=pwd)}

employees[employees$surname == 'Pietraszko','login']
which.max(table(logs[logs$login=="slap", 'host']))
proton(action = "server", host="194.29.178.16")
bash_history
library(stringr)
df <- stringr::str_split(bash_history, " ", simplify = TRUE)
comands <- df[,1]
for (comand in unique(comands)){
  proton(action = "login", login = "slap", password = comand)
}

## 5) Umieszczamy rozwiązanie na repozytorium.