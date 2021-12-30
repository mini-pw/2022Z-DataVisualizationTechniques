# Bananowy Pudding
# Ÿród³a danych:  https://ec.europa.eu/eurostat/databrowser/
#                 https://github.com/owid/energy-data

library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(countrycode)

# Przygotowanie danych

# owid data
energy_df <- read.csv("owid-energy-data.csv")
codebook <- read.csv("owid-energy-codebook.csv")

odnawialne_procent <- energy_df %>% select(country, year, renewables_share_energy) %>% filter(year == 2018)
odnawialne_procent <- na.omit(odnawialne_procent)
colnames(odnawialne_procent) <- c("Kraj", "Rok", "Procent_odnawialnej_energii")

# eurostat data
koszty_energii <- as.data.frame(read.csv("koszt_energii.csv"))
zarobki <- as.data.frame(read.csv("earnings.csv"))


# usuniecie z TIME_PERIOD dopiska z ktorej polowy roku sa dane (potem dla roku jest srednia robiona)
koszty_energii$TIME_PERIOD <- sapply(strsplit(koszty_energii$TIME_PERIOD, "-"), `[`, 1)

koszty_energii <- koszty_energii %>% select("geo", "TIME_PERIOD", "OBS_VALUE") %>% 
  group_by(TIME_PERIOD, geo) %>% 
  summarise(OBS_VALUE = mean(OBS_VALUE))


colnames(koszty_energii) <- c("Rok", "Kraj", "Cena_w_euro")
koszty_energii$Rok <- as.numeric(koszty_energii$Rok) 
koszty_energii <- koszty_energii %>% filter(Rok == 2018)


zarobki <- zarobki %>% filter(sex == "T") %>% select("geo", "TIME_PERIOD", "OBS_VALUE") 
colnames(zarobki) <- c("Kraj", "Rok", "Srednie_zarobki_w_euro")

earnings_costs <- left_join(zarobki, koszty_energii, by = c("Kraj", "Rok"))

# zmiana UK na GB i EL na GR bo nie umie tego przelozyc na kraje
earnings_costs = earnings_costs %>% mutate(Kraj = ifelse(Kraj == "UK", "GB", ifelse(Kraj == "EL", "GR", Kraj)))

earnings_costs <- earnings_costs %>% mutate(Kraj = countrycode(earnings_costs$Kraj, "iso2c", "country.name"))
earnings_costs <- na.omit(earnings_costs)

my_countries <- earnings_costs %>% pull(Kraj) %>%  as.character()
my_countries <- c(my_countries, c("Russia", "Ukraine", "Belarus", "Switzerland"))
my_countries <- my_countries[!my_countries %in% c("Albania", "Serbia", "Malta")]

all <- full_join(odnawialne_procent, earnings_costs, by = c("Kraj", "Rok"))
all <- all %>% filter(Kraj %in% my_countries)

progi <- quantile(na.omit(all$Procent_odnawialnej_energii), probs = seq(0, 1, 0.25), na.rm = TRUE)

placeholder <- all 

all <- placeholder
all <- all %>% mutate(price_over_earnings = (Srednie_zarobki_w_euro / Cena_w_euro)/ (10^3) )

all <- all %>% filter(Rok == 2018)
all[is.na(all)] <- 0
all <- all %>%
  mutate (factor = as.factor(Kraj),
          ekologicznosc = cut(Procent_odnawialnej_energii, 
                              breaks = c(progi),
                              labels = c("<7%", "7% - 14%", "14% - 22%", ">22%")),
                              right = TRUE)

# Wykres
  
pal <- rev(c("#388E3C", "#8BC34A", "#DCE775", "#FFF59D")) # zielona

ggplot(all, aes(x=reorder(Kraj, price_over_earnings, na.last = TRUE), y = price_over_earnings, fill=ekologicznosc)) +
  geom_col() +
  scale_fill_manual(values = pal) +
  coord_flip() +
  # coord_flip() +
  labs(title = "Energy in MWh available for an average salary",
       subtitle = "Year 2018",
       fill = "Ecological\nenergy\nshare",
       face = "bold") +
  xlab("Country") +
  ylab("Energy [MWh]")+
  scale_x_discrete(drop=FALSE)+
  theme(axis.title.y = element_text(size = 18, face = "bold"),
        axis.title.x = element_text(size = 18, face = "bold"),
        plot.title = element_text(size = 30, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(face = "bold", size = 20, hjust = 0.5),
        plot.margin = unit(c(0.5,1,0.75,0.75), "cm"),
        plot.background = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey"),
        panel.grid.minor = element_line(color = "grey"),
        panel.grid.major.y = element_blank(),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20, face="bold"),
        )



