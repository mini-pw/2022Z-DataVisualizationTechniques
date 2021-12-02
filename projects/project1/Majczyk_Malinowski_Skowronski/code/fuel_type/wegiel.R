library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
library(stringr)
library(readr)

cum_EJ <-  270.34*0.0036

export2 <- read_csv("export2.csv", col_types = cols(`monthly_hashrate_%` = col_number(), 
                                                    `monthly_absolute_hashrate_EH/S` = col_skip()))
export2

btc <- 
  export2 %>% 
  filter(date >= as.Date("2020/01/01"), date<as.Date("2021/01/01")) %>% 
  mutate(`monthly_hashrate_%` = `monthly_hashrate_%`/100) %>% 
  select(-date) %>% 
  group_by(country) %>% 
  summarise(sum_EJ = sum(`monthly_hashrate_%`)/12 * cum_EJ)

btc[6,1] <-  "China"
btc[10,1] <-  "US"
btc[3,1] <-  "Iran"
btc <-  btc[-c(8,4),]
  



bp <- read_csv("bp_fuel_type.csv", 
                         col_types = cols(Coal = col_double(), 
                                          `Nuclear energy` = col_double(), 
                                          `Hydro electric` = col_double(), 
                                          Renewables = col_double()))  
  



joined <-  left_join(btc,bp) %>% 
  mutate(Oil_per = Oil/Total*sum_EJ/10,#bo tylko 10% jest wykorzystywane w power plantach a reszta w srodkach transportu
         Natural_gas_per = `Natural Gas`/Total*sum_EJ,
         Coal_per = Coal/Total*sum_EJ,
         Nuclear_energy_per = `Nuclear energy`/Total*sum_EJ,
         Hydro_electric_per = `Hydro electric`/Total*sum_EJ,
         Renewables_per = Renewables/Total*sum_EJ)

energy_by_fuel <- as.data.frame(colSums(joined[,-c(1:9)] /0.0036)) #w TWh
#energy_by_fuel <- cbind(c("Oil","Natural gas", "Coal", "Nuclear energy", "Hydro electric","Renewables"),energy_by_fuel)
energy_by_fuel <- cbind(c("Olej","Gaz naturalny", "WÍgiel", "Energia jπdrowa", "Elektrownia wodna","Odnawialne"),energy_by_fuel)
rownames(energy_by_fuel) <- NULL
colnames(energy_by_fuel) <- c("Source","Energy")

total <- sum(energy_by_fuel$Energy)

energy_by_fuel <- energy_by_fuel %>% 
  mutate(Energy_percent = Energy/total) %>% 
  mutate(Source = forcats::fct_reorder(Source,Energy))

p1 <- energy_by_fuel %>% 
  ggplot(aes(x = "", y = Energy_percent, fill = Source)) + 
  geom_bar(stat="identity", width=0.5) +
  scale_y_continuous(breaks = seq(0,1,0.1),labels = scales::percent_format(accuracy = 1L),expand = c(0, 0)) +
  scale_x_discrete(expand = c(0,0)) +
  theme(axis.title.y = element_blank()) +
  labs(y = "Procent energii", title = "Paliwa wykorzystywane do produkcji energii do kopania BTC", fill="èrÛd≥a")+
  coord_flip() 
p1


### wartosci trzeba jeszcze podzielic przez 0,4 poniewaz takie mniej wiecej jest efficiency

  


