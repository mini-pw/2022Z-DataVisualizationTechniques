# import libraries
library("dplyr")
library("readr")
library("stringr")

fileList <- list.files(path = "./output2/")

outputAllDF <- data.frame(word = character(),
                          year = character(),
                          me = logical(),
                          wordsNumber = numeric())

for(file in fileList){
  
  print(paste0("preproc2: ", file))
  wordsFile <- read.csv(paste0("./output2/",file), encoding = "UTF-8")
  outputAllDF <- rbind(outputAllDF, wordsFile)
  
}

colnames(outputAllDF)[1] <- "word"

outputAllDF <- outputAllDF %>% group_by(word, year, me) %>%
  summarise(count = sum(wordsNumber)) %>% 
  filter(!(word %in% c("zalacznik")))


outputFreqDF <- outputAllDF %>% group_by(word) %>% 
  summarise(count = sum(count)) %>% 
  arrange(desc(count))

outputFreqDF <- head(outputFreqDF, 300)
outputAllDF <- outputAllDF %>% filter(word %in% outputFreqDF$word)

outputYearsDF <- outputAllDF %>% group_by(word, year) %>% 
  summarise(count = sum(count))

MeDFtemp1 <- outputAllDF %>% filter(me) %>% 
  select(-me) %>% 
  group_by(word) %>% 
  summarise(mine = sum(count))

MeDFtemp2 <- outputAllDF %>% group_by(word) %>% 
  summarise(count = sum(count))

outputMeDF <- left_join(MeDFtemp2, MeDFtemp1, by = "word")

outputMeDF$mine[is.na(outputMeDF$mine)] <- 0

outputMeDF <- outputMeDF %>%
  mutate(count = count - mine)
colnames(outputMeDF)[2] <- "not mine"

write_excel_csv(outputYearsDF, "./outputYearsDF.csv")
write_excel_csv(outputMeDF, "./outputMeDF.csv")
write_excel_csv(outputFreqDF, "./outputFreqDF.csv")