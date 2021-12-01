#
# Skrypt rysuje wykres na podstawie danych z google trends
#
# Dane umieszczone są w folderze 'data'
# Można dowolnie dodawać i usuwać pliki .csv z tego folderu
#
# w google trends ustawiamy opcję "cały świat"
# od 1 stycznia 2018 roku

library(tidyverse)
## Czytamy dane

# Folder z danymi
dir_name <- file.path("google_trends_data")

csv_files <- list.files(dir_name)

read_csv_file <- function(file_name) {
  readr::read_csv(file.path(dir_name, file_name), skip = 2)
}

# wektor wczytanych ramek danych
df_vector <- lapply(csv_files, read_csv_file)

# łączymy w jedną
df <- bind_cols(df_vector, .name_repair = "minimal")

# wybieramy kolumny poza zduplikowanymi kolumnami "Tydzień"
df <- df %>%
  select(-seq(3,ncol(df),2))

# zamiana wartości "<1" na 0
replace_chars_with_double <- function(column) {
  if (is.character(column)) {
    column <- replace(column, column == "<1", "0")
    return(as.double(column))
  } else {
    return(column)
  }
}

df <- df %>%
  mutate(across(!Tydzień, replace_chars_with_double))

# czyścimy nazwy
df <- df %>%
  rename_with(function(names) {
    sapply(str_split(names, ":"), function(a) a[[1]])
  })

## Wykres

speeches_dated <- readr::read_csv(file.path("speeches_dates.csv"))

df <- left_join(df, speeches_dated)


df <- df %>% pivot_longer(-Tydzień, names_to = "Trend")
# nie testowane jeszcze zmienione kolory

df <- df %>% mutate(
  size = ifelse(Trend == "Greta Thunberg", 2, 1)
)

ggplot(df[df$Trend == "Przemówienie", ]) +
  ggrepel::geom_text_repel(aes(Tydzień, value, label = as.character(Tydzień)), max.overlaps = 2000)

ggplot(df, aes(x = Tydzień, y = value, label = Tydzień)) +
  geom_line(data = df[df$Trend != "Greta Thunberg", ], aes(color = Trend), na.rm = TRUE) +
  geom_line(data = df[df$Trend == "Greta Thunberg", ], aes(color = Trend), size = 2, na.rm = TRUE) +
  geom_point(data = df[df$Trend == "Przemówienie", ], aes(shape = Trend),  size = 3, color = "#8f0000", na.rm = TRUE) +
  ggrepel::geom_text_repel(
    data = df[df$Trend == "Przemówienie", ], 
    aes(label = as.character(Tydzień)),
      size = 5,
    nudge_y = -5,
    nudge_x = -1,
    angle = 80,
    na.rm = TRUE) +
  scale_color_manual(name = "Trend",
    values = c(
      "Greta Thunberg" = "#FF0000",
       "Zmiana klimatu" = "#2274A5",
       "Klimat" = "#F1C40F",
       "Climate crisis" = "#00CC66",
       "Paliwa kopalne" = "#613F75",
       "Zanieczyszczenie powietrza" = "#C2AFF0")
      ) +
  labs(
    y = NULL,
    x = NULL
  ) +
  scale_y_continuous(expand = c(0.15, 0)) +
  theme(text = element_text(size=70))


