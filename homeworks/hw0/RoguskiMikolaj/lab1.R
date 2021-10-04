#proton
install.packages("proton")
library(proton)
proton()
head(employees)
a<-employees[employees$name=="John",]
b <- a[a$surname=="Insecure","login"]
proton(action="login",login=b)

for(i in c(1:1000)){
    proton(action="login",login=b,password=top1000passwords[i])
}

pietraszko <- employees[employees$surname=="Pietraszko",]
"slap"
c <- table(logs[logs$login=="slap","host"])

head(sort(c,decreasing=TRUE),1)

for(text in bash_history){
    a <- strsplit(text," ")
    
    if (length(a[[1]])==1) proton(action="login",login="slap",password=text)
    
}