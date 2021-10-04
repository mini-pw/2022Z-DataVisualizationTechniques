

install.packages("proton")
library(proton)
proton()

head(employees)
dim(employees)

employees[employees$name == "John",]
proton(action = "login", login="johnins")

for (i in 1:length(top1000passwords)) {
  proton(action = "login", login="johnins", password=top1000passwords[[i]])
}

# John Insecure
# 
# Well done! This is the right password!
#   Bit used John Insecure's account in order to log into the Proton server.
# It turns out that John has access to server logs.
# Now, Bit wants to check from which workstation Pietraszko is frequently logging into the Proton server. Bit hopes that there will be some useful data.  
# 
# Logs are in the `logs` dataset. 
# Consecutive columns contain information such as: who, when and from which computer logged into Proton.
# 
# Problem 3: Check from which server Pietraszko logs into the Proton server most often.
# 
# Use `proton(action = "server", host="XYZ")` command in order to learn more  about what can be found on the XYZ server.
# The biggest chance to find something interesting is to find a server from which Pietraszko logs in the most often


employees[employees$surname== "Pietraszko",]

x <- table(logs[logs$login == "slap", "host"])
df <- data.frame(x)

proton(action = "server", host="194.29.178.16")


# It turns out that Pietraszko often uses the public workstation 194.29.178.16.
# What a carelessness.
# 
# Bit infiltrated this workstation easily. He downloaded `bash_history` file which contains a list of all commands that were entered into the server's console.
# The chances are that some time ago Pietraszko typed a password into the console by mistake thinking that he was logging into the Proton server.
# 
# Problem 4: Find the Pietraszko's password.
# 
# In the `bash_history` dataset you will find all commands and parameters which have ever been entered.
# Try to extract from this dataset only commands (only strings before space) and check whether one of them looks like a password.

for (i in 1:length(bash_history)) {
  tmp <- strsplit(bash_history[[i]], " ")
  if (length(tmp[[1]]) == 1) {
    print(tmp)
  }
}

proton(action = "login", login="slap", password="DHbb7QXppuHnaXGN")


