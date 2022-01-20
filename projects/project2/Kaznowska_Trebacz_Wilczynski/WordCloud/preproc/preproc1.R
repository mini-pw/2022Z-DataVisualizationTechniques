# import libraries
library("dplyr")
library("lubridate")
library("stringr")
library("readr")

fileList <- list.files(path = "./output/")

myName <- read.csv("./name.csv", encoding = "UTF-8")["name"][[1]]

for(file in fileList){
  
  print(paste0("preproc1: ", file))
  df <- data.frame(word = character(), year = character(), me = logical())
  messFile <- read.csv(paste0("./output/",file), encoding = "UTF-8")
  
  for(row in 1:dim(messFile)[1]){
    
    sentence <- str_replace_all(tolower(messFile[row, 4]), "[^(\\w )]", "")
    sentence <- str_replace_all(sentence, "[0-9()_]", "")
    
    words <- unlist(strsplit(sentence, " "))
    years <- rep(year(as.POSIXct(messFile[row, 3]/1000, origin="1970-01-01")), length(words))
    mes <- rep(messFile[row, 2] == myName, length(words))
    
    df <- rbind(df, data.frame(word = words, year = years, me = mes))
  }
  
  df <- df %>% filter(word != "") %>%
    group_by(word, year, me) %>%
    summarise(wordsNumber = n())
  
  write_excel_csv(df, paste0("./output2/", file))
  
}
