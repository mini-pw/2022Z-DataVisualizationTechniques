library(ggplot2)
library(RColorBrewer)
library(rgdal)
library(dplyr)
library(stringr)

################################################################################
################################## DANE ########################################
setwd("C:/Users/laura/Desktop/TECHNIKI WIZUALIZACJI DANYCH/projekt_1/Grunas_Hoang_Kajka")
PM10_1g_2019 <- read.csv("./data/2019_PM10_1g.csv", sep=";", stringsAsFactors=TRUE)
PM25_1g_2019 <- read.csv("./data/2019_PM25_1g.csv", sep=";", stringsAsFactors=TRUE)
kody <- read.csv("./Kopia Kody_stacji_pomiarowych.csv", sep=";", stringsAsFactors=TRUE)

head(PM10_1g_2019)
head(PM25_1g_2019)
head(kody)

############################ (przetwarzanie) ###################################
for (variable in c(2:140)) {
  PM10_1g_2019[,variable] <- sub(",", ".", PM10_1g_2019[,variable])
  PM10_1g_2019[,variable] <- sub("^$", 0, PM10_1g_2019[,variable])
  PM10_1g_2019[,variable] <- as.numeric(PM10_1g_2019[,variable])
}
for (variable in c(2:140)) {
  PM25_1g_2019[,variable] <- sub(",", ".", PM25_1g_2019[,variable])
  PM25_1g_2019[,variable] <- sub("^$", 0, PM25_1g_2019[,variable])
  PM25_1g_2019[,variable] <- as.numeric(PM25_1g_2019[,variable])
}

PM25_1g_2019_mean <- PM25_1g_2019[,-c(1)] %>%  
  summarise(across(everything(), list(mean)))

df <- data.frame(colnames(PM25_1g_2019_mean),t(PM25_1g_2019_mean),row.names = 1:length(PM25_1g_2019_mean))
colnames(df) <- c("stacja","mean")
df <- df %>%
  mutate(stacja = substr(stacja,0,nchar(stacja)-2))
head(df) ################### STACJA-ŚREDNIA(PM25) ##############################

###################### WOJEWODZTWO-ŚREDNIA(PM25) ###############################
df2 <- inner_join(df,kody,by = c("stacja"="KOD.NOWY")) %>%
  group_by(WOJEWÓDZTWO) %>%
  summarise(ŚREDNIA = mean(mean, na.rm=TRUE))
df2

################################################################################
############################# WYKRESY ##########################################

################################## MAPA ########################################
############################## DANE DO MAPY ####################################
regiony <- readOGR(dsn="C:/Users/laura/Desktop/TECHNIKI WIZUALIZACJI DANYCH/projekt_1/poland_administrative_boundaries_province_polygon/poland_administrative_boundaries_province_polygon.shp",encoding = "UTF-8")

arrange(df2,df2$WOJEWÓDZTWO)
df3 <- data.frame(
  WOJEWÓDZTWO=df2$WOJEWÓDZTWO,
  ŚREDNIA=df2$ŚREDNIA,
  ref=c("DS","KP","LU","LB","LD","MP","MA","OP","PK","PD","PM","SL","SK","WM","WP","ZP")
)
df3 ################# WOJEWODZTWO-ŚREDNIA(PM25)-ref ############################

############################# (fortifying) #####################################
newregiony <- fortify( regiony, region="ref" ) # mamy teraz long i lat

head(newregiony)
head(regiony@data)

regiony_fortified = newregiony %>%
  left_join(. , df3, by=c("id"="ref"))
head(regiony_fortified)

############################ WYKRES - MAPA #####################################
mapa <- ggplot() +
  geom_polygon(data = regiony_fortified, aes(fill = ŚREDNIA, x = long, y = lat, group = group), color="black") +
  theme_void() +
  scale_fill_gradient(low="#9ECAE1", high="#08306B") +
  labs(
    title = "Średni pomiar PM(2,5) w poszczególnych województwach w 2019 roku",
    fill = "PM(2,5) [mg]"
  ) +
  theme(
    title = element_text(size=20, face='bold'),
    legend.title = element_text(size=20),
    legend.text = element_text(size=16),
    plot.background = element_blank()
    ) +
  coord_map() #+
  # theme(# panel.background = element_rect(fill="#cccccc"),
  #       # panel.border = element_rect(fill="#cccccc"),
  #       plot.background = element_rect(fill="#cccccc"))
mapa

ggsave("mapa2.png", plot=mapa, scale=2, width=2000, height=1600, units="px")

#### kolory
# display.brewer.all()
# display.brewer.pal(9, "Blues")
# brewer.pal(9, "Blues")

















