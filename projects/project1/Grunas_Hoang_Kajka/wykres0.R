setwd("C:/Users/PK/Desktop/TWD/projekt_1")
library(RColorBrewer)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)

Europe <- read.csv("Europa.csv", sep = ";")
Europe$OS = NULL
Europe$DA = NULL
Europe$CD = NULL
Europe$FA = NULL
Europe$PA = NULL
Europe$Waste = NULL
Europe$DW = NULL

Europe <- Europe %>% arrange(desc(PM2.5))

c <- c("Turcja", "Polska", "S³owacja", "S³owenia", "W³ochy", "Wêgry", "Grecja", "Czechy", "£otwa", "Belgia", "Austria", "Holandia", "Litwa", "Niemcy", "Francja", "Wielka Brytania", "Szwajcaria", "Hiszpania", "Luksemburg", "Dania", "Norwegia", "Finlandia", "Estonia", "Szwecja", "Islandia")
Europe$Panstwa <- c
Europe

p1<- ggplot(Europe, aes(x = reorder(Panstwa, PM2.5) , 
                        y = PM2.5)) + 
     geom_col(fill = c("#2b8cbe", "#fd8d3c", rep("#2b8cbe", times = 23))) +
     labs(x = "Pañstwa", 
          y = "Œredni poziom PM 2.5") +
     theme(axis.title.x = element_text(size = 14, colour = "#403f3f"), 
           axis.title.y = element_text(size = 14, colour = "#403f3f"), 
           axis.text.x = element_text(size = 11, colour = "#403f3f"), 
           axis.text.y = element_text(size = 11, colour = "#403f3f"), 
           panel.background = element_blank(),
           plot.background = element_blank()) + 
    coord_flip()
p1
ggsave("Europa.png", p1)

