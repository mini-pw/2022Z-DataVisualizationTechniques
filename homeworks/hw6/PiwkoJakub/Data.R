library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)

data <- read.csv("complete.csv") %>% 
  mutate(decade = awardYear - awardYear %% 10)
##___________________________________


df1 <- data %>% 
  filter(knownName != "") %>% 
  filter(birth_date != "1943-00-00")
##___________________________________


tmp1 <- data %>% 
  filter(ind_or_org == "Organization") %>% 
  mutate(Won_by = "Organization")
tmp2 <- data %>% 
  filter(portion == "1", ind_or_org == "Individual") %>% 
  mutate(Won_by = case_when(
    gender == "male" ~ "Man individually",
    gender == "female" ~ "Woman individually"
  )) 
tmp3 <- data %>% 
  filter(portion != "1", ind_or_org == "Individual") %>% 
  distinct(awardYear, category, .keep_all = TRUE) %>% 
  mutate(Won_by= "Group")

df2 <- rbind.data.frame(tmp1, tmp2, tmp3) %>% 
  group_by(Won_by, category, decade) %>% 
  summarise(count = n(), .groups = "drop") %>%
  complete(Won_by, category, nesting(decade), fill = list(count = 0)) 
##___________________________________

df3 <- data %>% 
  filter(prizeStatus == "received" &
           !(birth_continent == "" &
               org_founded_continent == "")) %>% 
  mutate(prize = case_when(
    portion == "1" ~ prizeAmountAdjusted,
    portion == "1/2" ~ as.integer(prizeAmountAdjusted/2),
    portion == "1/3" ~ as.integer(prizeAmountAdjusted/3),
    portion == "1/4" ~ as.integer(prizeAmountAdjusted/4))) %>%
  mutate(continent = case_when(
    birth_continent != "" ~ birth_continent,
    birth_continent == "" ~ org_founded_continent
  ))
