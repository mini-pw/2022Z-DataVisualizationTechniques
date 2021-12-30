library(readr)
library(stringr)
library(ggplot2)
library(dplyr)

## Loading Data

ozone_data <- read_tsv("Dane/Ozone_hole_area_data.txt", col_names = TRUE)

## Data Transformation

ozone_data <- str_split_fixed(ozone_data$`Year      Data   Minimum   Maximum`, "\ +", 4)

ozone_data <- as.data.frame(apply(ozone_data, 2, as.numeric))

colnames(ozone_data) <- c("Year", "Data", "Minimum", "Maximum")

## Plotting

ggplot(data = ozone_data, aes(x = Year, y = Data)) +
  geom_line(color = "dark red", size = 1) +
  geom_vline(xintercept = 1989, linetype = "dashed") +
  geom_smooth(data = filter(ozone_data, Year >= 1989), method = "lm", color = "black") +
  labs(title = "Average area of the ozone hole in years 1979 – 2021") +
  ylab(expression(paste("Average ozone layer area in  ", "km"^"2", " * 10"^"6"))) +
  theme_bw() +
  annotate(geom = "text", label = "Montreal Protocol", x = 1988.3, y = 5, angle = 90, size = 4, fontface = "italic") +
  annotate(geom = "text", label = "1989", x = 1989.5, y = 4.8, angle = 90, size = 4, fontface = "italic")
