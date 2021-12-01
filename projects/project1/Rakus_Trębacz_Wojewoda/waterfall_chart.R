library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(waterfalls)

load_data = function (name){
  print(paste("data/",name,".csv"))
  data = read.csv(paste("data/",name,".csv", sep=""), sep = "|",  col.names = c("DT", "Country", "Partner", "Industry", "Year", "Value"))
  data = data.table(data)
  data[Partner== "WLD" & Industry=="DTOTAL"][,c("DT","Partner", "Industry"):=NULL]
}

import = load_data("IMGR_TCO2")
export = load_data("EXGR_TCO2")
teritorial = load_data("PROD_CO2")


df <- merge(import, export, by=c("Country","Year"), all = TRUE)
colnames(df)[colnames(df) == 'Value.x'] <- 'Import'
colnames(df)[colnames(df) == 'Value.y'] <- 'Export'
df <- merge(df, teritorial, by=c("Country","Year"), all = TRUE)
colnames(df)[colnames(df) == 'Value'] <- 'Teritorial'

df <- df %>% mutate(Balance = Teritorial + Import - Export) 
df <- df %>% filter(!(Country %in% c("G20", "APEC", "NONOECD", "OECD", 
                                             "ZASI", "EASIA", "ZNAM", "ZOTH", 
                                             "ZEUR", "EU28", "EU15", "ROW", "EA19", 
                                             "EA12", "ASEAN", "ZSCA", "EU13", "WLD")))

#population dataframe
x <- read.csv("data/population.csv")
x <- x %>% select(c(1:2, 50:60))
x <- x %>%  pivot_longer(
  cols = starts_with("X"),
  names_to = "Year",
  names_prefix = "X",
  values_to = "Population",
  values_drop_na = TRUE)
x$Year = as.integer(x$Year)
#values per capita
df_all <- inner_join(df, x, by = c("Country"= "Country.Code", "Year" = "Year"))
df_all <- df_all %>% mutate(Teritorial_Pop = Teritorial/ (Population/1000000), 
                            Export_Pop = Export/ (Population/1000000), 
                            Import_Pop = Import/ (Population/1000000),
                            Balance_Pop = Balance/ (Population/1000000))


fun_waterfallPlot <- function(kraj, color1, color2, color3, nazwa){
  df_all %>% 
    mutate(Export_Pop = -Export_Pop) %>% 
    filter(Year == "2015", Country == kraj ) %>% 
    select(!c(Country, ď.żCountry.Name)) %>% 
    pivot_longer(everything()) %>% 
    slice(7:9) %>% 
    waterfall(tmp, rect_border = NA, 
              rect_width = 0.90,
              linetype = 1, 
              calc_total = TRUE,
              fill_by_sign = FALSE,
              fill_colours = c(color1, color2, color1),
              total_rect_color = color3,  
              rect_text_labels = c("", "", ""), 
              total_rect_text = "")+
    scale_x_discrete(limits = c("Teritorial_Pop", "Export_Pop","Import_Pop", "Balance_Pop"),
                     labels= c("Balance_Pop" = "balance",
                               "Teritorial_Pop" = "teritorial",
                               "Import_Pop" = "import",
                               "Export_Pop" = "export"))+
    labs(caption=nazwa) + 
    theme(panel.background = element_rect(fill="black"),
          plot.background = element_rect(fill="black"),
          legend.background = element_rect(fill = "black"),
          legend.position = "none",
          panel.grid = element_line(color = "#262626"),
          text = element_text(color = "#9E9E9E"),
          axis.text = element_text(color = "#9E9E9E", size = 16), 
          plot.caption = element_text(hjust=0.5, size=rel(2)))
}


South_Africa <- fun_waterfallPlot("ZAF", "white", "#656464", "#939393", "South Africa")
Australia <- fun_waterfallPlot("AUS", "#33E2FF", "#0092D8", "#00B9E3", "Australia")
China <- fun_waterfallPlot("CHN", "#4EFF84", "#015E1D", "#00BA38", "China")
Argentina <- fun_waterfallPlot("ARG", "#FFC2E8", "#DE008A", "#FF61C3", "Argentina")
Germany <- fun_waterfallPlot("DEU", "#FFF998", "#D2B800", "#F2E411", "Germany")
USA <- fun_waterfallPlot("USA", "#FF5B5B", "#A00000", "red", "USA")

#merged plots
(USA + Germany + Argentina + China + Australia + South_Africa + plot_layout(ncol = 6)) & 
  scale_y_continuous(limits = c(0, 18.5), 
                     breaks = seq(0, 18.5, by=2), 
                     expand = c(0, 0)) & 
  labs(y= NULL)&
  theme(axis.text.x = element_text(angle = -10, size = 16)) 

        