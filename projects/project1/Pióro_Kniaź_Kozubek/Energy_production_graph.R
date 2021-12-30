library(ggplot2)
library(dplyr)
file1 <- "D:\\Hubert\\Dokumenty\\Politechnika Warszawska\\IIAD\\Semestr III\\TWD\\Projekt\\Projekt 1 od zera\\electricity-prod-source-stacked.csv"

data <- read.csv(file = file1)

data %>% 
  select(3:11) %>% 
  group_by(Year) %>%
  summarise(s1 = sum(Electricity.from.coal..TWh., na.rm=TRUE), s2 = sum(Electricity.from.gas..TWh., na.rm=TRUE), s3 = sum(Electricity.from.hydro..TWh., na.rm=TRUE), s4 = sum(Electricity.from.other.renewables..TWh., na.rm=TRUE), s5 = sum(Electricity.from.solar..TWh., na.rm=TRUE), s6 = sum(Electricity.from.oil..TWh., na.rm=TRUE), s7 = sum(Electricity.from.wind..TWh., na.rm=TRUE), s8 = sum(Electricity.from.nuclear..TWh., na.rm=TRUE)) %>% 
  group_by(Year) %>%
  mutate(s10 = s1+s2+s3+s4+s5+s6+s7+s8, p1=s1/s10,p2=s2/s10, p3=s3/s10, p4=s4/s10, p5=s5/s10, p6=s6/s10, p7=s7/s10, p8=s8/s10) ->d4

yer <- rep(1985:2020, each=10)
x <- numeric(36*10)
y <- rep(c("Coal","Biofuels", "Gas", "Hydro","Other_renewables", "Solar", "Oil", "Wind", "Nuclear","TB" ), times = 36)
j <- 0
for(year in d4$Year){
  
  for(i in 1:10){
    z <- d4[year==d4$Year,]
    if(i == 1){
      x[j*10+i] <- z$p1*100
    }
    if(i == 2){
      x[j*10+i] <- 0
    }
    if(i == 3){
      x[j*10+i] <- z$p2*100
    }
    if(i == 4){
      x[j*10+i] <- z$p3*100
    }
    if(i == 5){
      x[j*10+i] <- z$p4*100
    }
    if(i == 6){
      x[j*10+i] <- z$p5*100
    }
    if(i == 7){
      x[j*10+i] <- z$p6*100
    }
    if(i == 8){
      x[j*10+i] <- z$p7*100
    }
    if(i == 9){
      x[j*10+i] <- z$p8*100
    }
    if(i == 10){
      x[j*10+i] <- 0
    }
    
  }
  j=j+1
}

ord <- c("Other_renewables","Biofuels", "Solar", "Wind", "Hydro","Nuclear", "Gas", "Oil" ,"Coal", "TB")
d <- data.frame(yer,x, y, stringsAsFactors=FALSE)
colnames(d) <- c("Year", "Percentage", "Group")
ggplot(d, aes(x = Year, y = Percentage, group = factor(Group,ord), fill = factor(Group,ord))) +
  geom_bar(position="stack", stat="identity")+ ggtitle("Electricity production by source, World") +
  ylab("Percentage[%]") +
  scale_fill_viridis_d(name = "Source") + 
  theme_dark()  + 
  theme(panel.grid.minor = element_blank(), panel.grid =element_line(color = "white",
                                                                       size = 0.2,
                                                                       linetype = 1)) -> plakat
plakat
ggsave(file="DiffColors2.svg", plot=plakat, width=10, height=8)




