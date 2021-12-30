# poprawienie wykresow
#https://omnil.fr/spip.php?article229
#https://omnil.fr/IMG/pdf/presentation_egt_v_publique_vf.pdf
# strona 10 

library(grid)
library(methods)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(scales)
library(grid)
cele<-c("dom-praca","praca-inne","dom-studia","dom-zakupy",
        "dom-rzeczy osobiste","dom-towarzyszace","dom-wypoczynek/wizyty",
        "inne")
czestosc<-c(17,13,11,12,6,11,17,13)
czas<-c(28,13,11,8,7,6,17,10)
kilometry<-c(38,13,9,7,6,5,15,7)
id<-c(1,2,3,4,5,6,7,8)
dane<- data.frame(cele,czestosc,czas,kilometry,id)
dane<- mutate(dane,cele=forcats::fct_reorder(cele,id))

plot1<-(ggplot(dane, aes(cele, czestosc)) 
        + geom_bar(stat='identity')
        + theme(axis.title.x = element_blank(),axis.title.y = element_blank())
        + labs(title = "Częstość")
        + scale_x_discrete(guide = guide_axis(angle = 45)))

plot2<-(ggplot(dane, aes(cele, czas))
        + geom_bar(stat='identity')
        + theme(axis.title.x = element_blank(),axis.title.y = element_blank())
        + labs(title = "Czas")
        + scale_x_discrete(guide = guide_axis(angle = 45)))
plot3<-(ggplot(dane, aes(cele, kilometry))
        + geom_bar(stat='identity')
        + theme(axis.title.x = element_blank(),axis.title.y = element_blank())
        + labs(title = "Kilometry")
        + scale_x_discrete(guide = guide_axis(angle = 45)))

plot<-grid.arrange(plot1, plot2,plot3, nrow=1,
             top = textGrob("Praca jako znaczący cel podróży",gp=gpar(fontsize=20,font=3)),
             left=textGrob("Procentowy udział",gp=gpar(fontsize=16), rot = 90),
             bottom=textGrob("Cele podróży",gp=gpar(fontsize=16)))

















































































