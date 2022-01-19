library(dplyr)
names <- dir("./data");
for (name in names){
    data <- read.csv2(paste("./data/",name,"/ready_data.csv",sep=''),sep=',');
    #długość wiadomości dzień tygodnia
    datad <- data %>% mutate(dzien = strftime(date,"%A")) %>% group_by(dzien) %>% summarize(total = mean(word_count)) ;
    datad$dzien <- factor(datad$dzien, levels = c("poniedziałek","wtorek","środa","czwartek","piątek","sobota","niedziela"));
    write.csv2(datad,paste("./data/",name,"/dlugoscdzien.csv",sep=''),sep=',');
    #liczba wiadomości dizeń tygodnia
    datad <- data %>% mutate(dzien = strftime(date,"%A")) %>% group_by(dzien) %>% summarize(total = n()) ;
    datad$dzien <- factor(datad$dzien, levels = c("poniedziałek","wtorek","środa","czwartek","piątek","sobota","niedziela"));
    write.csv2(datad,paste("./data/",name,"/liczbadzien.csv",sep=''),sep=',');
    #liczba wiadomości godzina
    datag <- data %>% mutate(godz  = gsub(".* (\\d{2}):.*","\\1", date)) %>% filter(nchar(godz)==2);
    datag <- datag %>% group_by(godz) %>% summarise(count = n());
    write.csv2(datag,paste("./data/",name,"/liczbagodzina.csv",sep=''),sep=',');
    #długość wiadomości godizna
    datag <- data %>% mutate(godz  = gsub(".* (\\d{2}):.*","\\1", date)) %>% filter(nchar(godz)==2);
    datag <- datag %>% group_by(godz) %>% summarise(count = mean(word_count));
    write.csv2(datag,paste("./data/",name,"/dlugoscgodzina.csv",sep=''),sep=',');
    }
