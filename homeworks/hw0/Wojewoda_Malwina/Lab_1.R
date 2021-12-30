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

D <- mtcars
head(mtcars)
tail(mtcars, 3)
str(mtcars)
?mtcars
typeof(D)

# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?

D["wt"]
D[, "wt"]
D[["wt"]]
D$wt
D[1,]

# Pierwszy wiersz, pierwsza kolumna?

D[1,1]

# 10 pierszych wierszy, 2 i 3 kolumna?

D[1:10, 2:3]
D[, c(2,4)]

# Wszytskie wiersze i kolumny w kolejności "am", "wt", "mpg"?

D[, c("am", "wt", "mpg")]


# Jak wybierać jedną kolumnę?

D["cyl"]
D[, "cyl"]

# Pytania

# 1. Wymiar ramki danych

dim(D)

# 2. Jakie są typy zmiennych?

str(D)

# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?

length(unique(mtcars$cyl))
table(mtcars$cyl)
table(mtcars$cyl, mtcars$gear)

# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?

mean(mtcars[mtcars$cyl==4, "drat"])

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

x<- employees[employees$name == "John",  ]
x[x$surname == "Insecure", ]

proton(action = "login", login = "johnins")


for (pass in top1000passwords){
    proton(action = "login", login="johnins", password=pass)
}

employees[employees$surname == "Pietraszko", ]
temp <- logs[logs$login == "slap", ]
temp$host <- as.vector(temp$host)
temp <- table(temp$host)
which.max(temp)

proton(action = "server", host="194.29.178.16")

bash_history

## 5) Umieszczamy rozwiązanie na repozytorium.