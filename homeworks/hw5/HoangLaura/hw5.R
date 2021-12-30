library(dplyr)
library(plotly)

setwd("C:/Users/laura/Desktop/TECHNIKI WIZUALIZACJI DANYCH/hw5")

complete <- read.csv("complete.csv")
# View(complete)
head(complete)

data <- complete[c("category","awardYear")] %>%
  mutate( decade = as.character(floor( awardYear/10 )), sep = "") %>%
  group_by(category,decade) %>%
  summarise(count = n())

tail(data)

fig <- plot_ly( data,
                x = ~decade,
                y = ~count,
                name = ~category,
                frame = ~category,
                type = "scatter",
                mode = "lines+markers",
                line = list(color = "#66C2A5", width = 2),
                marker = list(color = "#66C2A5") ) %>%
  layout( xaxis = list( title = "Decade", zeroline = F),
          yaxis = list( title = "Number of distributed awards", zeroline = F ),
          title = "Nobel Prize categories distribution in the previous 12 decades",
          legend = list(x = 0.1, y = 0.9, title=list(text="Category: ")),
          showlegend = TRUE
        ) %>%
  animation_opts( frame = 1000, transition = 0, redraw = TRUE ) %>%
  animation_slider( hide = FALSE )

fig

