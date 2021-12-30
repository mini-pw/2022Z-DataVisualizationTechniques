setwd("C:/Users/PK/Desktop/TWD/projekt_1")
library(RColorBrewer)

PM25_1g_2019 <- read.csv("./2019/2019_PM25_1g.csv", sep=';', stringsAsFactors = TRUE)
PM25_1g_2020 <- read.csv("./2020/2020_PM25_1g.csv", sep=';', stringsAsFactors = TRUE)

library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)

PM25_1g_2019$ï.¿Data <- dmy_hm(PM25_1g_2019$ï.¿Data)
PM25_1g_2020$ï.¿Data <- dmy_hm(PM25_1g_2020$ï.¿Data)



for (variable in c(2:140)) {
  PM25_1g_2019[,variable] <- sub(",", ".", PM25_1g_2019[,variable])
  PM25_1g_2019[,variable] <- sub("^$", 0, PM25_1g_2019[,variable])
  PM25_1g_2019[,variable] <- as.numeric(PM25_1g_2019[,variable])
}

for (variable in c(2:140)) {
  PM25_1g_2020[,variable] <- sub(",", ".", PM25_1g_2020[,variable])
  PM25_1g_2020[,variable] <- sub("^$", 0, PM25_1g_2020[,variable])
  PM25_1g_2020[,variable] <- as.numeric(PM25_1g_2020[,variable])
}




PM25_1g_2019_months <- PM25_1g_2019 %>%  
  group_by(month(ï.¿Data)) %>% 
  summarise(across(everything(), list(mean)), na.rm = TRUE)



PM25_1g_2020_months <- PM25_1g_2020 %>%  
  group_by(month(ï.¿Data)) %>% 
  summarise(across(everything(), list(mean)), na.rm = TRUE)



PM25_1g_2019_months_all_mean <-  PM25_1g_2019_months %>% 
  mutate(mean_25_2019 = rowMeans(select(PM25_1g_2019_months, starts_with("Ds")), na.rm = TRUE)) %>% 
  select(`month(ï.¿Data)`, mean_25_2019)



PM25_1g_2020_months_all_mean <-  PM25_1g_2020_months %>% 
  mutate(mean_25_2020 = rowMeans(select(PM25_1g_2020_months, starts_with("Ds")), na.rm = TRUE)) %>% 
  select(`month(ï.¿Data)`, mean_25_2020)




PM25_2019_2020 <- merge(PM25_1g_2019_months_all_mean, PM25_1g_2020_months_all_mean, by="month(ï.¿Data)")

PM25_2019_2020

colnames(PM25_2019_2020) <- c("Month", "PM25_2019", "PM25_2020")

p1 <- ggplot(PM25_2019_2020) + 
  geom_line(data=PM25_2019_2020, aes(x = Month, y = PM25_2019, color = "#2b8cbe"), size = 1.2) + 
  geom_line(data=PM25_2019_2020, aes(x = Month, y = PM25_2020, color = "#fd8d3c"), size = 1.2) + 
  geom_point(aes(x = Month, y = PM25_2019), data = PM25_2019_2020, size = 1.5) + 
  geom_point(aes(x = Month, y = PM25_2020), data = PM25_2019_2020, size = 1.5) + 
  labs(title = "Porównanie poziomu PM 2.5 
       w 2019 i 2020 roku", 
       x = element_blank(), 
       y = element_blank()) + 
  scale_x_discrete(limits=c("Styczeñ", "Luty", "Marzec", "Kwiecieñ", "Maj", "Czerwiec", "Lipiec", "Sierpieñ", "Wrzesieñ", "PaŸdziernik", "Listopad", "Grudzieñ")) + 
  scale_colour_manual(name = "Rok",
                      values = c("#2b8cbe","#fd8d3c"), 
                      labels = c("2019", "2020"), 
                      guide = "legend") + 
  theme(plot.title = element_text(size = 25,colour = "#403f3f"), 
        axis.title.x = element_text(size = 17, colour = "#403f3f"), 
        axis.title.y = element_text(size = 17, colour = "#403f3f"), 
        axis.text.x = element_text(size = 15, angle = 30, colour = "#403f3f"), 
        axis.text.y = element_text(size = 15, colour = "#403f3f"), 
        legend.position = c(0.9, 0.2),
        legend.background = element_rect(fill = "#cccccc"),
        legend.title = element_text(size = 20, colour = "#403f3f"), 
        legend.text = element_text(size = 18, colour = "#403f3f"),
        legend.key = element_rect(fill = "#cccccc"),
        panel.background = element_blank(),
        plot.background = element_blank()) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  scale_y_continuous(breaks = seq(7,34,1),
                     labels = c('','8','','10','','12','','14','','16','','18','','20','','22','','24','','26','','28','','30','','32','','34'))

p1
ggsave("Wykres.png", p1, height = 7, width = 10.5)
