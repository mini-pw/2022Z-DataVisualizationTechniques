###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 12           ###
###########################################

## tidycharts - IBCS International Business Communication Standards
# https://medium.com/responsibleml/a-way-of-creating-clear-transparent-and-unified-data-visualizations-f166a828c715

library(tidycharts)

#prepare the data frame
data <- data.frame(
  city = c("Berlin", "Munich", "Cologne", "London", "Vienna", "Paris", "Zurich"),
  Products = c(538, 250, 75, 301, 227, 100, 40),
  Services = c(621, 545, 302, 44, 39, 20, 34)
)
#generate 
barchart <- bar_chart(data, data$city, c("Products", "Services"), c("Products", "Services"))
barchart %>% SVGrenderer()


# Zadanie 1
#Przygotować 3 wykresy dotyczące zbioru danych titanic i połączyć ze sobą.

titanic <- read.csv("https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv")
titanic






## rpivotTable

library(SmarterPoland)
library(rpivotTable)

head(countries)
qnt <- quantile(countries$birth.rate, na.rm = TRUE)
countries$birth.rate.quant<- cut(countries$birth.rate, unique(qnt), include.lowest=TRUE)

rpivotTable(countries)


rpivotTable(countries, 
            cols = "continent", 
            aggregatorName = "Average", vals = "population", 
            rendererName = "Bar Chart")

rpivotTable(countries, cols = "continent",
            rows = "birth.rate.quant",
            aggregatorName = "Average", vals = "population", 
            rendererName = "Table Barchart")

# Zadanie 2
# Przygotować aplikację Shiny pozwalającą załadować własny zbiór danych i wygenerowanie dla niego pivotTable. (użyć funkcji `renderRpivotTable()` oraz `rpivotTableOutput()`)






## visNetwork

library(visNetwork)

nodes <- data.frame(id = 1:8, 
                    label = c("Białystok", "Warszawa",
                              "Radom", "Sosnowiec",
                              "Kraków", "Wrocław", 
                              "Gdańsk", "Lądek Zdrój"))

edges <- data.frame(from = c(2, 2, 2, 2, 3, 3, 3, 4, 4, 5, 2), 
                    to = c(6, 7, 3, 4, 4, 5, 6, 5, 6, 6, 8), 
                    color = "red",
                    label = "droga",
                    title = "Miasta")

net <- visNetwork(nodes, edges, height = 600, width = 1000) %>%
  visEdges(arrows = "from")  %>% 
  visLayout(randomSeed = 123) 

net

# Zadanie 3
# Stworzyć własny graf przedstawiający stacje metra w Warszawie (obie linie) i zapisać go do pliku html (funkcja visSave).




