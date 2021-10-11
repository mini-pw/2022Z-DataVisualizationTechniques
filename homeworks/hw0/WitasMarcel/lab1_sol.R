data("employees")
employees[employees$name == "John", ] -> login
login[login$surname == "Insecure", "login"] -> login
proton(action = "login", login=login)

for(i in top1000passwords){
  proton(action = "login", login=login, password=i)
}


head(logs)

employees[employees$surname == "Pietraszko", "login"] -> login2
logs[logs$login == login2, "host"] -> hosts
hosts <- sort(hosts)
sort(table(hosts)) -> hosts
host <- row.names(as.data.frame(hosts[length(hosts)]))
proton(action = "server", host=host)
split_bash_history <- strsplit(bash_history, " ")

