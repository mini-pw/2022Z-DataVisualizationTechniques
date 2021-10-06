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
tail(mtcars, 2)
str(mtcars)

D <- mtcars

?mtcars


# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?
mtcars[["cyl"]]
mtcars$cyl
mtcars[1,]


# Pierwszy wiersz, pierwsza kolumna?
mtcars[1,1]


# 10 pierszych wierszy, 2 i 3 kolumna?
mtcars[1:10,2:3]
mtcars[1:10,c(2,4)]

# Wszytskie wiersze i kolumny w kolejności "am", "wt", "mpg"?
mtcars[,c('am', 'wt', 'mpg')]

# Jak wybierać jedną kolumnę?
mtcars["cyl"]
mtcars[["cyl"]]

# Pytania

# 1. Wymiar ramki danych
dim(mtcars)


# 2. Jakie są typy zmiennych?
str(mtcars)
str(list(a=1))

# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?

length(unique(mtcars$cyl))
unique(mtcars$cyl)
table(mtcars$cyl)

# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?

mean(mtcars[mtcars$cyl == 4,"drat"])


# Prosty wykres

# Zależność "mpg" i "hp" - scatter plot

plot(mtcars[['mpg']], mtcars[['hp']])


# Zmienna "cyl" - barplot
barplot(mtcars[['cyl']])


## 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()
employees
D <- employees[employees$name == "John",]
D <- D[D$surname == "Insecure"]
proton(action = "login", login = "johnins")

top1000passwords

for (x in top1000passwords)
{
  proton(action = "login", login = "johnins", password=x)
}
employees
employees[employees$surname == "Pietraszko",]

E <- logs[logs$login == "slap",]
sort(table(E$host))

proton(action = "server", host="194.29.178.16")




bash_history

F <-  strsplit(bash_history, "\\s+")
typeof(F)
typeof(F[1])
F[1]


## 5) Umieszczamy rozwiązanie na repozytorium.