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
nrow(mtcars)
names(mtcars)
str(mtcars)

?mtcars

df <- mtcars
typeof(df)

# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?
df["mpg"] # lista
df[, "mpg", drop=F]
df[, "mpg"] # wektor
df[["mpg"]]
df$mpg

# Pierwszy wiersz, pierwsza kolumna?
df[1, 1]

# 10 pierszych wierszy, 2 i 3 kolumna?
df[1:10, 2:3]
df[1:10, c(2, 4)]

# Wszytskie wiersze i kolumny w kolejności "am", "wt", "mpg"?
df[, c("am", "wt", "mpg")] # indeksowanie kolumn nazwami

# Jak wybierać jedną kolumnę?
df["mpg"] # lista
df[["mpg"]] # wektor


# Pytania

# 1. Wymiar ramki danych
dim(df)

# 2. Jakie są typy zmiennych?
str(df)

# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?
length(unique(df$cyl))
unique(df$cyl)
table(df$cyl)

?table
table(df$cyl, df$gear)
table(df$cyl, df$gear, df$drat)

  # 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?
mean(df[df$cyl == 4, "drat"])


# Prosty wykres

# Zależność "mpg" i "hp" - scatter plot
plot(df$mpg, df$hp)



# Zmienna "cyl" - barplot
barplot(df$cyl)
barplot(table(df$cyl))


## 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()

head(employees)
dim(employees)
str(employees)

login <- employees[employees$name=="John" & employees$surname=="Insecure", "login"]
proton(action = "login", login=login)

# 2
head(top1000passwords, 100)


for (password in top1000passwords) {
  proton(action = "login", login=login, password=password)
}

# 3
head(logs)
str(logs)

pietraszko_login <- employees[employees$name=="Slawomir" & employees$surname=="Pietraszko", "login"]
pietraszko_logs <- logs[logs$login==pietraszko_login, ]

str(pietraszko_logs)
hosts <- pietraszko_logs$host
str(hosts)
hosts <- as.vector(hosts)
unique(hosts)
length(unique(hosts))
table(hosts)
str(table(hosts)) 
df_hosts <- as.data.frame(table(hosts))
# which.min(table(hosts))
most_frequent <- df_hosts[which.max(df_hosts$Freq), ] 
most_frequent <- "194.29.178.1" 
proton(action = "server", host=most_frequent)


# 4
head(bash_history)
str(bash_history)

library(stringr)


## 5) Umieszczamy rozwiązanie na repozytorium.