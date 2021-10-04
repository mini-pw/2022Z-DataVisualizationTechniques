
for(i in 1:length(top1000passwords)){
  + proton(action = "login", login="johnins", password=top1000passwords[i])}

sort(table(logs[logs$login=="johnins", "host"]),decreasing=TRUE)[1]

split_bash_history <- strsplit(bash_history, " ")
