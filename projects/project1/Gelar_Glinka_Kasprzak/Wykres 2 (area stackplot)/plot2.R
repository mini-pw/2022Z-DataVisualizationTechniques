library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library("RColorBrewer")

## Loading Data

data_ozone <- read.csv("Dane/ozone-depleting-substance-consumption.csv")

## Data Transformation

suma_produkcji_lata1 <- data_ozone %>% filter(Code != "OWID_WRL") %>%  filter(Code != "") %>% group_by(Year) %>%  summarise(
  "Methyl Chloroform" = sum(Consumption.of.Ozone.Depleting.Substances...Methyl.Chloroform, na.rm = TRUE),
  "Methyl Bromide" = sum(Consumption.of.Ozone.Depleting.Substances...Methyl.Bromide, na.rm = TRUE),
  "Hydrochlorofluorocarbons-HCFCs" = sum(Consumption.of.Ozone.Depleting.Substances...Hydrochlorofluorocarbons..HCFCs., na.rm = TRUE),
  "Carbon Tetrachloride" = sum(Consumption.of.Ozone.Depleting.Substances...Carbon.Tetrachloride, na.rm = TRUE),
  "Halons" = sum(Consumption.of.Ozone.Depleting.Substances...Halons, na.rm = TRUE),
  "Chlorofluorocarbons CFCs" = sum(Consumption.of.Ozone.Depleting.Substances...Chlorofluorocarbons..CFCs., na.rm = TRUE))


suma_produkcji_2 <- suma_produkcji_lata1 %>% 
  gather(Substances, Tones, -Year) %>% mutate(Tones = ifelse(Tones < 0, 0, Tones))


ylab <- c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5)
xlab <- c(1989, 1995, 2000, 2005, 2010, 2014)

## Plotting

ggplot(suma_produkcji_2, aes(x=Year, y=Tones, fill=Substances))+scale_fill_manual(values=c("#A50026", "#FFFFBF", "#FDAE61", "#A6D96A", "#006837", "#D73027"))+
  scale_y_continuous(labels = paste0(ylab),
                     breaks = 10^6 * ylab
  )+scale_x_continuous(labels = paste0(xlab), breaks = xlab
  )+ylab("Yearly consumption [mln tons]")+xlab("Year")+theme_bw()+theme(legend.position = c(.95, .95),
                                                                        legend.justification = c("right", "top"),
                                                                        legend.box.just = "right",
                                                                        legend.margin = margin(6, 6, 6, 6))+
  labs(title = "Ozone-depleting substances consumption")+
  geom_area()
