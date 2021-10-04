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
mtcars[1,]

# Pierwszy wiersz, pierwsza kolumna?

# 10 pierszych wierszy, 2 i 3 kolumna?
mtcars[1:10,c(2,4)]

# Wszytskie wiersze i kolumny w kolejności "am", "wt", "mpg"?
mtcars[, c("am", "wt")]

# Jak wybierać jedną kolumnę?
mtcars$am

# Pytania

# 1. Wymiar ramki danych
dim(mtcars)

# 2. Jakie są typy zmiennych?

str(mtcars)

# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?
unique(mtcars$cyl)
table(mtcars$cyl)
# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?
mean(mtcars[mtcars$cyl == 4, "drat"])

# Prosty wykres

# Zależność "mpg" i "hp" - scatter plot
plot(mtcars$mpg, mtcars$hp)



# Zmienna "cyl" - barplot
barplot(table(mtcars$cyl))

## 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()

head(employees)

john_login <- employees[employees$surname == "Insecure", "login"]
proton(action = "login", login=john_login)

head(top1000passwords)
length(top1000passwords)

for(password in top1000passwords) {
  proton(action = "login", login=john_login, password=password)
}

head(logs)
pietraszko_login <- employees[employees$surname == "Pietraszko", "login"]
pietraszko_login
ip_table <- table(logs[logs$login == pietraszko_login, "host"])
ip <- names(which.max(ip_table))

proton(action = "server", host=ip)

head(bash_history)

library(stringr)
df <- stringr::str_split(bash_history, " ", simplify = T)
commands <- df[,1]

for(command in unique(commands)){
  proton(action="login", login=pietraszko_login, password=command)
}

str_extra
## 5) Umieszczamy rozwiązanie na repozytorium.