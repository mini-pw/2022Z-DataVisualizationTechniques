library(dplyr)
library(ggplot2)
options(scipen=999)
# library(png)
# library(emojifont)
# df <- read.table(file = "clipboard", sep = "\t", header = FALSE)
# bee_population <- t(df)



# colnames(bee_population) <- c("Year", "Hives_worldwide")
# bee_population <- bee_population[-1,]
# rownames(bee_population) <- NULL
# bee_population2 <- copy(bee_population)
# View(bee_population2)
# bee_population <- copy(bee_population2)
# 
# bee_population <- bee_population %>% 
#   transform(Year = as.character(Year)) %>% 
#   mutate(year = strtrim(Year,4)) %>% 
#   transform(year = as.numeric(year), Hives_worldwide = as.numeric(Hives_worldwide))
# bee_population <- bee_population[-1]
# colnames(bee_population)[2] <- 'Year'
# # bee_population %>% add_row(year = 2018, Hives_worldwide = 1)
# saveRDS(bee_population,file ="bee_population.Rda")

hp <- readRDS("human_population.Rda")
# 'https://www.worldometers.info/world-population/world-population-by-year/'
# View(hp)

bp <- readRDS("bee_population.Rda")
bp <- data.frame(bp)
# View(bp)


bhp <- inner_join(bp,hp,by="Year")
bhp <- add_row(bhp,Year = 2019, Hives_worldwide =102500000 )
# 
hp2 <- bhp %>%
  mutate(Hives_per_people =Hives_worldwide/(World.Population/100))
# # Hives_per_people to na 100 osob
# View(hp2)
hp2 <- hp2[-c(1)]

hp3 <- hp2
colnames(hp3)[4] <- "Hives_per_people"
hp3$Hives_per_people <- hp3$Hives_per_people/25000000
bhp2 <- inner_join(bp[-4],hp3[-3])


wykres_punktowy <- hp2 %>% 
  ggplot(aes(x =Year))+
  theme_minimal()+
  theme(panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major = element_line(colour = "grey"),
        rect = element_rect(fill = "#F7CE98"),
        legend.title = element_blank(),
        text =element_text( size = 15))+
  geom_point(aes(y=Hives_per_people), color = "#FFBD59",shape = 1,size=2, stroke=2.5)+
  # geom_image(aes(y=Hives_worldwide,image = Image), size = 0.07)+
  # labs(x ="Year", y = "Number of beehives per 100 people")+
  labs(x ="", y = "")+
  scale_x_continuous(guide = guide_axis(n.dodge = 1, angle = 80),
                     breaks = seq(1961,2017,2))+
  scale_y_continuous(
                     limits = c(0,1.8), 
                     breaks = seq(0,1.8,0.1),
                     expand = c(0,0))
wykres_punktowy

png('bee.plot.png',width=708,height=580,units="px",bg = "transparent")
wykres_punktowy
dev.off()
  


  
  
  
  
  
  
  
  
  
  
  
  