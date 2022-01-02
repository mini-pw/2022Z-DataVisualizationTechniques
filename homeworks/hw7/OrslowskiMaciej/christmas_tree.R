library(tidyverse)
library(googleVis)

# pakiet do pobierania informacji o lokalizacji na podstawie współrzędnych geograficznych
# devtools::install_github("mhudecheck/revgeo")
library(revgeo)


lat = rep(c(
  rep(-10, 2),  # podstawa pienia
  seq(-10, 10, len = 5),  # pien
  rep(10, 10),  # podstawa pierwszego "trojkata"
  seq(10, 30, len = 8),  # ramie pierwszego "trojkata"
  rep(30, 5),  # podstawa drugiego "trojkata"
  seq(30, 50, len = 8),  # ramie drugiego "trojkata"
  rep(50, 5),  # podstawa trzeciego "trojkata"
  seq(50, 70, len = 8)  # ramie trzeciego "trojkata"
), 2) / 20
long = c(
  seq(0, -5, len = 2),  # podstawa pienia
  rep(-5, 5),  # pien
  seq(-5, -50, len = 10),  # podstawa pierwszego "trojkata"
  seq(-50, -20, len = 8),  # ramie pierwszego "trojkata"
  seq(-20, -40, len = 5),  # podstawa drugiego "trojkata"
  seq(-40, -10, len = 8),  # ramie drugiego "trojkata"
  seq(-10, -30, len = 5),  # podstawa trzeciego "trojkata"
  seq(-30, 0, len = 8)  # ramie trzeciego "trojkata"
) / 20
long = c(long, -long) + 20.0785
lat = lat + 50.0078

df <- data.frame(
  lat = lat,
  long = long
) %>%
  mutate(location = paste(lat, long, sep = ":"),
         city = revgeo(
           longitude = long,
           latitude = lat,
           provider = "photon",
           output = "frame")$city)

christmas_tree <- gvisMap(
  data = df,
  locationvar = "location",
  tipvar = "city"
)

plot(christmas_tree)

