
library(proton)
proton()
employees[employees$name=="John",]

proton(action="login",login="johnins")

for(i in 1:1000){
  proton(action = "login", login="johnins", password=top1000passwords[i])
  
}
employees[employees$surname=="Pietraszko",]
table(logs[logs$login=="slap","host"])

proton(action="login",host="194.29.178.16")

split_bash_history<-strsplit(bash_history," ")

for(x in split_bash_history){
  comads<-c(comads,x[[1]])
}

for(comads in unique(comads)){
  proton(action="login")
}