library(proton)
proton()
employees[(employees$name == "John" & employees$surname == "Insecure"),] # johnins
proton(action = "login", login="johnins") #123456
sapply(top1000passwords, FUN = function(x) { proton(action = "login", login="johnins", password=x) })
employees[employees$surname=="Pietraszko",]$login #slap
max(table(logs[logs$login=="slap",]$host)) # 112
cnts <- table(logs[logs$login=="slap",]$host)
cnts[cnts == 112] #194.29.178.16
proton(action = "server", host="194.29.178.16")