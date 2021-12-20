library(plotly)
library(dplyr)

#wczytanie dancy
dataraw <- read.csv("RoguskiMikolaj/complete.csv");
#zmienienie wyciągnięcie pogrupowanej po płci i kategori średniej nagrody (prizeAmountAdjusted)
data <- dataraw  %>%  group_by(category,gender) %>% summarise(meanprize = mean(prizeAmountAdjusted))
#utworzenie figury
fig <- plot_ly(type = "bar")
#narusowanie wykresu dla mężczyzn
fig <- fig %>% add_trace(
    data = data %>% filter(gender == 'male'),
    x = ~category,
    y = ~meanprize,
    name = "males",
    visible = T
)
#narysowanie wykresu dla kobiet (ukrytego)
fig <- fig %>% add_trace(
    data = data %>% filter(gender == 'female'),
                         x = ~category,
                         y = ~meanprize,
                         name = "females",
                         visible = F)


fig <- fig %>% 
    layout(
        #dropdown jest legendą więc nie potrzebuje już podpisu
        showlegend = F,
        title = "Mean Nobel prize amount awarded by gender",
        #największa wartość jest lekko poniżej 10M dlatego ustawiam tutaj stałą skalę do 10M
        yaxis = list(range = list(0,10000000), title = "Mean payout"),
        xaxis = list(title = "Category"),
        updatemenus = list(
            list(
                y = 0.8,
                buttons = list(
                    list(
                        #ten przycist pokazuje mężczyzn ukrywa kobiety
                        method = "restyle",
                        args = list("visible", list(F,T)),
                        label = "males"
                        ),
                    list(
                        #ten pokazuje kobiety ukrywa mężczyzn
                        method = "restyle",
                        args = list("visible", list(T,F)),
                        label = "females"
                    )
                )
            )
        )
    )
fig
