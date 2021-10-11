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
# tail
# str noice
str(mtcars)
?mtcars

D <- mtcars
D
class(D)
str(D)
typeof(D)

# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?

# Pierwszy wiersz, pierwsza kolumna?
D[1,1]

# 10 pierszych wierszy, 2 i 3 kolumna?
D[1:10, c(2,3)]

# Wszytskie wiersze i kolumny w kolejności "am", "wt", "mpg"?
D[, c("am", "wt", "mpg")]

# Jak wybierać jedną kolumnę?
D[,2]

# Pytania

# 1. Wymiar ramki danych
dim(D)

# 2. Jakie są typy zmiennych?

# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?
length(unique(D[, "cyl"]))

# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?
mean(D[D$cyl == 4, "drat"])


# Prosty wykres

# Zależność "mpg" i "hp" - scatter plot
plot(D[, "mpg"], D[, "hp"])


# Zmienna "cyl" - barplot
barplot(table(D[,"cyl"]))


## 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()

E <- employees
#John Insecure
Insecure <- E[E$surname == "Insecure", ]
Insecure
proton(action = "login", login="johnins")

#################################

top1000passwords
for (i in 1:length(top1000passwords)){
  proton(action = "login", login="johnins", password=top1000passwords[i])
}

#################################

logs
#who, when and from which computer logged
#Check from which server Pietraszko logs into the Proton server most often
PietrLogin <- E[E$surname=="Pietraszko", ]
PietrLogin

PietrLogs <- logs[logs$login == "slap", ]
s <- summary(as.factor(PietrLogs$host))
s

proton(action = "server", host="194.29.178.16")

#################################

bash_history
stri <- unique(sapply(strsplit(bash_history, " "), "[", 1))
stri
proton(action = "login", login="slap", password="DHbb7QXppuHnaXGN")

## 5) Umieszczamy rozwiązanie na repozytorium.