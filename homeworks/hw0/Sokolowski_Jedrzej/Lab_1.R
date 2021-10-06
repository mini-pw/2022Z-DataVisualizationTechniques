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
tail(mtcars)
str(mtcars)

D <- mtcars

?mtcars


# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?
mtcars["cyl"]
mtcars[,"cyl"]
mtcars[["cyl"]]
mtcars$cyl
mtcars[1,]

# Pierwszy wiersz, pierwsza kolumna?

mtcars[1,1]

# 10 pierszych wierszy, 2 i 3 kolumna?

mtcars[1:10, 2:3]
mtcars[,c(2,4)]

# Wszytskie wiersze i kolumny w kolejności "am", "wt", "mpg"?

mtcars[,c("am", "wt", "mpg")]

# Jak wybierać jedną kolumnę?

mtcars["cyl"]

# Pytania

# 1. Wymiar ramki danych
dim(mtcars)

# 2. Jakie są typy zmiennych?
str(mtcars)

# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?
unique(mtcars[,"cyl"])
length(unique(mtcars[,"cyl"]))
table(mtcars$cyl)

# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?

mean(mtcars[mtcars$cyl == 4, "drat"])


# Prosty wykres

# Zależność "mpg" i "hp" - scatter plot

plot(mtcars$mpg, mtcars$hp)

# Zmienna "cyl" - barplot

barplot(mtcars$cyl)
barplot(table(mtcars$cyl))


## 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).

library(proton)
proton()


## 5) Umieszczamy rozwiązanie na repozytorium.

# Etap 1

head(employees)
johns = employees[employees$name == "John", c("surname", "login")]
his_login = johns[johns$surname == 'Insecure',2]



# Etap 2

for (pass in top1000passwords){
  proton(action = "login", login=his_login, password=pass)
}

# Etap 3

pietraszko_login = employees[employees$surname == "Pietraszko", "login"]
his_logins <- logs[logs$login == "slap", "host"]
result <- table(his_logins)
print(sort(result, decreasing = TRUE)[1])
print(result)


bash_hist_list <- list(str(bash_history))
bash_hist_list <- unlist(bash_hist_list)



for (p_pass in bash_hist_list){
  print()
}

