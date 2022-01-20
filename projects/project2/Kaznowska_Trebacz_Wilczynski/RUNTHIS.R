library(dplyr)
library(stringr)
library(stringi)
library(padr)

# folder z wygenerowanymi csv z jupytera
setwd("/home/kacper/Documents/study/IiAD/TWD/twd_proj2/WordCloud/output")
name <- "Kacper Trębacz"
mindata <- "2013-01-01"
# minimalna data jakiejkolwiek wiadomości (w formacie yyyy-mm-dd)
# od początku roku (np. 2016-01-01)

files <- list.files()
messenger <- data.frame()
for(i in files){
  df <- read.csv(i, encoding="UTF-8")
  df <- df %>% mutate(convo = i)
  messenger <- rbind(messenger, df)
  print(i)
}
messenger <- select(messenger, c(-1))

messenger %>% 
  mutate(time = format(as.POSIXct(timestamp_ms / 1000, origin = "1970-01-01"), "%Y-%m-%d %H:%M:%OS3")) %>% 
  mutate(year = format(as.Date(time,format="%Y-%m-%d"), format = "%Y"),
         month = format(as.Date(time,format="%Y-%m-%d"), format = "%m"),
         day = format(as.Date(time,format="%Y-%m-%d"), format = "%d"),
         dayname = weekdays(as.Date(time))) %>% 
  select(-c(2)) -> messtime

########## poniższego Piotrek NIE ##########

messtime %>% 
  filter(sender_name == name) %>% 
  group_by(year, month, day) %>% 
  summarise(count = n()) %>% 
  mutate(month = formatC(month, width = 2, format = "d", flag = "0"),
         date = as.Date(paste(year, month, day, sep = "-"), 
                        format = "%Y-%m-%d")) %>% 
  ungroup() %>% 
  select(c(5,4))-> heatmapp


heatmapp <- heatmapp %>% 
  pad(start_val = as.Date(mindata, "%Y-%m-%d"),
      end_val = as.Date("2021-12-31", "%Y-%m-%d")) %>% 
  fill_by_value(value = 0)

heatmapp <- heatmapp %>% 
  mutate(weekday = as.POSIXlt(date)$wday,
         weekdayf = factor(weekday,levels=rev(0:6),
                           labels=rev(c("Sun", "Mon","Tue","Wed","Thu","Fri","Sat")),
                           ordered=TRUE),
         monthf = factor(month(heatmapp$date),
                         levels=as.character(1:12),
                         labels=c("Jan","Feb","Mar","Apr","May","Jun",
                                  "Jul","Aug","Sep","Oct","Nov","Dec"),
                         ordered=TRUE),
         yearmonth = factor(as.yearmon(date)),
         week = as.numeric(format(heatmapp$date,"%W"))) %>% 
  mutate(yearmonthnum = as.numeric(format(date, "%Y%m")),
         year = year(date))

heatmapp <- transform(heatmapp, weekday = ifelse(weekday == 0, 7, weekday))

write.csv(heatmapp, "heatmapp.csv", row.names = FALSE)

####### od tego miejsca już Piotrek TAK ########

convos <- messtime %>% 
  mutate(me = ifelse(sender_name == name, "yes", "no")) %>% 
  group_by(year, month, day, convo, me) %>% 
  summarise(count = n()) %>% 
  mutate(month = formatC(month, width = 2, format = "d", flag = "0"),
         date = as.Date(paste(year, month, day, sep = "-"), 
                        format = "%Y-%m-%d")
  )


convos <- convos %>% 
  mutate(weekday = as.POSIXlt(date)$wday) %>% 
  mutate(week = as.numeric(format(date,"%W")))

convos <- transform(convos, weekday = ifelse(weekday == 0, 7, weekday))

newnames <- data.frame(cbind(unique(convos$convo), paste("convo", 1:length(unique(convos$convo)))))
names(newnames) <- c("convo", "convoname")

convos <- left_join(convos, newnames, by = "convo")
convos <- convos %>% select(-convo)

write.csv(convos, "convos.csv", row.names = FALSE)
