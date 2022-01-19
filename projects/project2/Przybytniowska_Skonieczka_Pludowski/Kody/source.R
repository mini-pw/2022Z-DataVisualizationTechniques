library(dplyr)
nickD1 <- "Kacper Skonieczka"
nickD2 <- "Dawid Płudowski"
nickD3 <- "Julia Przybytnikowska"
color1 <- "#1E90FF"
color2 <- "#fa3c4c"
color3 <- '#EE82EE'
colorMain <- "gray"


z<- 1

d1 <- 0.2 * z
d2<- 1 * z
d3 <- 0.12 * z


dfA3 <- read.csv2("./Dane/dfA.csv")               
dfG <- read.csv2("./Dane/dfG.csv")  

dfD1DP <- read.csv2("./Dane/dfD1DP.csv") %>%  sample_frac(d1)   
dfD1JP <- readr::read_csv("./Dane/dfD1JP.csv") %>%  sample_frac(d1)   
dfD1KS <- read.csv2("./Dane/dfD1KS.csv")   %>%  sample_frac(d1)            
dfD1KS2 <- read.csv2("./Dane/dfD1KS2.csv")  %>%  sample_frac(d1)           

dfD2DP <- read.csv2("./Dane/dfD2DP.csv")  %>%  sample_frac(d2)                  
dfD2JP <- readr::read_csv("./Dane/dfD2JP.csv")  %>%  sample_frac(d2)             
dfD2KS <- read.csv2("./Dane/dfD2KS.csv")  %>%  sample_frac(d2)                  
dfD2KS2 <- read.csv2("./Dane/dfD2KS2.csv")   %>%  sample_frac(d2)            


dfD3DP <- read.csv2("./Dane/dfD3DP.csv")   %>%  sample_frac(d3)   
dfD3JP <- readr::read_csv("./Dane/dfD3JP.csv")    %>%  sample_frac(d3)  
dfD3KS <- read.csv2("./Dane/dfD3KS.csv")  %>%  sample_frac(d3)  
dfD3KS2 <- read.csv2("./Dane/dfD3KS2.csv")   %>%  sample_frac(d3)  


