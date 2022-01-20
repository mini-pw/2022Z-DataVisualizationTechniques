# import data
library("dplyr")
library("readr")
library("stringr")

words <- read.csv("./outputFreqDF.csv", encoding = "UTF-8")
words <- words[,1]

fileList <- list.files(path = "./output2/")

allConvs <- length(fileList)

outputConversationstDF <- data.frame(word = words,
                          convsWITH = rep(0, length(words)),
                          convsWITHOUT = rep(allConvs, length(words)))

for(file in fileList){
  
  print(paste0("preproc3: ", file))
  wordsFile <- read.csv(paste0("./output2/", file), encoding = "UTF-8")
  colnames(wordsFile)[1] <- "word"
  wordsFile <- wordsFile %>% filter(word %in% words)
  
  for(i in 1:length(words)){
    
    if(words[i] %in% wordsFile$word){
      outputConversationstDF[i, "convsWITH"] <- outputConversationstDF[i, "convsWITH"] + 1
    }
    
  }
  
}

outputConversationstDF <- outputConversationstDF %>%
  mutate(convsWITHOUT = convsWITHOUT - convsWITH)

write_excel_csv(outputConversationstDF, "./outputConversationstDF.csv")
