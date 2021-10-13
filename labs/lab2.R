###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 2            ###
###########################################
library(dplyr)
# Dane
starwars
starwars_new <- starwars[, c(1, 2, 3, 4, 5, 7, 8)]

# Informacje o zbiorze danych
dim(starwars)
sum(is.na(starwars))
apply(starwars, 2, function(x) sum(is.na(x)))
# Podgląd tabeli


# Określamy typy zmiennych:
# name - jakosciowa, nominalna
# height - ilosciowy, ilorazowy
# mass - ilosciowy, ilorazowy
# hair_color - jakosciowa, nominalna
# skin_color - jakosciowa, nominalna
# birth_year - ilosciowa, przedzialowa
# sex - jakosciowa, nominalna

# 1) Wybór wierszy i kolumn w dplyr
library(dplyr) # https://dplyr.tidyverse.org/

# a) wybór kolumn ---> select()
select(starwars, "mass")
select(starwars, mass)
mass
select(starwars, c("mass","height"))
unname(unlist(select(starwars,"mass")))
# b) wybór wierszy ---> filter()
filter(starwars, height>160)
filter(starwars,"height">160)#uwaga bo alwys true

# 2) pipes %>% (skrót Ctrl + Shift + m)
starwars %>% 
  select(mass) %>% 
  filter(mass > 50)

starwars %>% 
  select(., mass)

# Zadanie 1
# Używając funkcji z pakietu dplyr() wybierz te postacie, których gatunek to Droid, 
# a ich wysokość jest większa niż 100.


# Zadanie 2 
# Używając funkcji z pakietu dplyr() wybierz te postacie, które nie mają określonego koloru włosów.
starwars %>% 
  filter(is.na(hair_color))

# c) sortowanie wierszy ---> arrange()
starwars %>% 
  arrange(mass)
starwars %>% 
  arrange(-mass)
starwars %>% 
  arrange(desc(mass))
starwars %>% 
  arrange(desc(mass),height)
# Zadanie 3
# Używając funkcji z pakietu dplyr() wybierz postać o największej masie.
starwars %>% 
  arrange(-mass) %>% 
  head(1)


starwars %>% 
  filter(mass == max(mass, na.rm = TRUE))


starwars %>% 
  slice_max(mass)
# d) transformacja zmiennych ---> mutate()
starwars %>% 
  mutate(is_na_hair_color = is.na(hair_color)) %>% 
  select(is_na_hair_color)

# e) transformacja zmiennych ---> transmute()
starwars %>% 
  transmute(is_na_hair_color = is.na(hair_color))

# Zadanie 4
# Używając funkcji z pakietu dplyr() wylicz wskaźnik BMI (kg/m^2) i wskaż postać, która ma największy wskaźnik.
starwars %>% 
  mutate(bmi = mass/((height/100)**2),
    .before = "name")
# f) kolejność kolumn ---> relocate()
starwars %>% 
  relocate(hair_color, skin_color, .before = "name") %>% 
  relocate(eye_color, .after = "skin_color")

# g) dyskretyzacja ---> ifelse(), case_when()
ifelse(starwars$mass >50, 1, "b")
starwars %>% 
  mutate(species_new = case_when(species == "Human" ~ "Human",
                                 species == "Droid" ~ "Droid",
                                 TRUE ~ "Other")) %>% 
  select(name, species, species_new) %>% 
  tail()

# h) funkcje agregujące ---> summarise(), n(), mean, median, min, max, sum, sd, quantile
starwars %>% 
  filter(height > 150) %>% 
  summarise(count = n(),
            srednia_mass = mean(mass, na.rm = TRUE),
            minimum_mass = min(mass, na.rm = TRUE))

# i) grupowanie ---> group_by() + summarise()
starwars %>% 
  group_by(hair_color) %>% 
  summarise(count = n())
table(starwars$hair_color)
# 3) Przekształcenie ramki danych w tidyr
library(tidyr) # https://tidyr.tidyverse.org

# j) pivot_longer()

relig_income
relig_income %>% 
  pivot_longer(!religion,
               names_to = "income",
               values_to = "count")
# k) pivot_wider()

?fish_encounters

fish_encounters %>% 
  pivot_wider(names_from = station,
              values_from = seen)

fish_encounters %>% 
  pivot_wider(names_from = station,
              values_from = seen,
              values_fill = 0)


# 4) Praca z faktorami (szczególnie w wizualizacji)
library(forcats) # https://forcats.tidyverse.org
library(ggplot2) # https://ggplot2.tidyverse.org


# l) kolejność poziomów ---> fct_infreq()
factor(starwars$eye_color)
ggplot(starwars, aes(x = eye_color)) +
  geom_bar()+
  coord_flip()

# m) scalanie poziomów ---> fct_lump()


# n) kolejność poziomów na podstawie innej zmiennej ---> fct_reorder()


# 4) Praca z stringami
# Zaawansowane: https://stringi.gagolewski.com
library(stringr) # https://stringr.tidyverse.org

x <- paste0(letters[1:5], "=", 1:5, "__", letters[6:10], "=", 6:10)
x
# o) podział stringów ---> str_split()
str_split(x, "__")
str_split(x, "_", n = 1)
str_split(x, "_", simplify = T)
str_split(x, "_", simplify = T)[,1]

# p) usunięcie/zastąpienie znaków ---> str_remove(), str_replace()
x %>% 
  str_remove("=")


x %>% str_remove_all("=")

x %>% str_replace("=","+")

x %>% str_replace_all("=","+")



# 5) https://www.tidyverse.org/packages
# Bezpieczne wczytywanie danych w R https://readr.tidyverse.org