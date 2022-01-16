# FUNCTIONS USED TO GENERATE DATAREPORT_HOUSES.RMD

houses <- read.csv('houses.csv')

library(dplyr)
library(ggplot2)
library(dlookr)
library(kableExtra)

#----------CODE---------#

feature <- function(){
  
  df <- data.frame(
    "Feature" = c("Number of observations","Number of variables"),
    "Result" = c(nrow(houses),ncol(houses))
  )
  
  return(df)
  
}

create_stats_for_column <- function(column){
  
  uv <- unique(houses[[column]])
  uv <- uv[!is.na(uv)]
  
  out <- sapply(houses, summary) %>% 
    as.data.frame() %>% 
    select(column) %>% 
    rbind("unique values" = length(uv)) %>% 
    rbind("class" = class(houses[[column]]))
  
  return(out)
}

create_bar_plot_for_column <- function(column){
  
  houses %>% 
    ggplot(aes_string(x = column)) + 
    geom_histogram() + 
    theme_bw()
  
}

create_boxplots_searching_outliers <- function(columnx,columny){
  
  houses %>% 
    ggplot(aes_string(x = columnx, y = columny, group = columnx)) +
    geom_boxplot() + 
    theme_bw()
  
}

create_geom_bin2d_for_columns <- function(columnx,columny){
  
  houses %>% 
    ggplot(aes_string(x = columnx, y = columny)) + 
    geom_bin2d() +
    theme_bw()
  
}