library(anytime)
library(rjson)
library(stringr)
library(ggplot2)
library(plotly)
library(lubridate)
library(data.table)
library(dplyr)
library(shiny)
library(sf)
library(leaflet)
library(geojsonsf)
library(RColorBrewer)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(reshape2)
library(heatmaply)
library(data.table)
library(hms)
library(rrapply)
library(tidyr)
library(dplyr)
library(tidyr)
library(rrapply)

#Asia
november <- rjson::fromJSON(file = "2021_NOVEMBER_JK.json")
january <- rjson::fromJSON(file = "2021_JANUARY_JK.json")
february <- rjson::fromJSON(file = "2021_FEBRUARY_JK.json")
march <- rjson::fromJSON(file = "2021_MARCH_JK.json")
may <- rjson::fromJSON(file = "2021_MAY_JK.json")
june <- rjson::fromJSON(file = "2021_JUNE_JK.json")
july <- rjson::fromJSON(file = "2021_JULY_JK.json")
august <- rjson::fromJSON(file = "2021_AUGUST_JK.json")
september <- rjson::fromJSON(file = "2021_SEPTEMBER_JK.json")
october <- rjson::fromJSON(file = "2021_OCTOBER_JK.json")
december <- rjson::fromJSON(file = "2021_DECEMBER_JK.json")
nov <- november[[1]]
jan <- january[[1]]
feb <- february[[1]]
mar <- march[[1]]
may <- may[[1]]
jun <- june[[1]]
jul <- july[[1]]
aug <- august[[1]]
sep <- september[[1]]
oct <- october[[1]]
dec <- december[[1]]


nov5 <- rrapply(nov, how = "melt") 
jan5 <- rrapply(jan, how = "melt") 
feb5 <- rrapply(feb, how = "melt") 
mar5 <- rrapply(mar, how = "melt") 
may5 <- rrapply(may, how = "melt") 
jun5 <- rrapply(jun, how = "melt") 
jul5 <- rrapply(jul, how = "melt") 
aug5 <- rrapply(aug, how = "melt") 
sep5 <- rrapply(sep, how = "melt") 
oct5 <- rrapply(oct, how = "melt") 
dec5 <- rrapply(dec, how = "melt") 


nov_activity <- nov5 %>% filter(L2 == "activitySegment") %>% select(!L2) %>% 
    pivot_wider(names_from = c(L3, L4, L5, L6, L7))
nov_placeVisit <- nov5 %>% filter(L2 == "placeVisit") %>% select(!L2) %>% 
    pivot_wider(names_from = c(L3, L4, L5, L6, L7))
colnames(nov_activity) <- colnames(nov_activity) %>% str_replace_all('_NA', "")
colnames(nov_placeVisit) <- colnames(nov_placeVisit) %>% str_replace_all('_NA', "")
nov_placeVisit <- nov_placeVisit[-c(60,73),]
nov_placeVisit_selected <- nov_placeVisit[,c('location_latitudeE7', 'location_longitudeE7', 'location_address', 'location_name', 'duration_startTimestampMs', 'duration_endTimestampMs')]

ms_to_date = function(ms, t0="1970-01-01") {
    sec = ms / 1000
    as.POSIXct(sec, origin=t0, tz="Europe/Warsaw")
}

change <- function(month) {
    month_activity <- month %>% filter(L2 == "activitySegment") %>% select(!L2) %>% 
        pivot_wider(names_from = c(L3, L4, L5, L6))
    month_placeVisit <- month %>% filter(L2 == "placeVisit") %>% select(!L2) %>% 
        pivot_wider(names_from = c(L3, L4, L5, L6))
    colnames(month_activity) <- colnames(month_activity) %>% str_replace_all('_NA', "")
    colnames(month_placeVisit) <- colnames(month_placeVisit) %>% str_replace_all('_NA', "")
    month_placeVisit_selected <- month_placeVisit[,c('location_latitudeE7', 'location_longitudeE7', 'location_address', 'location_name', 'duration_startTimestampMs', 'duration_endTimestampMs')]
    month_placeVisit_selected[, c(1,2,5,6)] <- sapply(month_placeVisit_selected[, c(1,2,5,6)], as.numeric)
    month_placeVisit_selected <- month_placeVisit_selected %>% mutate(start_date = ms_to_date(duration_startTimestampMs), end_date = ms_to_date(duration_endTimestampMs))
    month_placeVisit_selected$start_date <- ymd_hms(month_placeVisit_selected$start_date)
    month_placeVisit_selected$end_date <- ymd_hms(month_placeVisit_selected$end_date)
    month_placeVisit_selected <- month_placeVisit_selected %>% mutate(duration_in_sec = (duration_endTimestampMs - duration_startTimestampMs)/1000)
    month_most_visited <- month_placeVisit_selected %>% group_by(location_name) %>% summarise(amount = n(), full_duration = sum(duration_in_sec))
    month_placeVisit_selected <- month_placeVisit_selected %>% full_join(month_most_visited, by='location_name')
    return(month_placeVisit_selected)
}

jan_placeVisit_selected <- change(jan5)
feb_placeVisit_selected <- change(feb5)
mar_placeVisit_selected <- change(mar5)
may_placeVisit_selected <- change(may5)
jun_placeVisit_selected <- change(jun5)
jul_placeVisit_selected <- change(jul5)
aug_placeVisit_selected <- change(aug5)
sep_placeVisit_selected <- change(sep5)
oct_placeVisit_selected <- change(oct5)
dec_placeVisit_selected <- change(dec5)


nov_placeVisit_selected[, c(1,2,5,6)] <- sapply(nov_placeVisit_selected[, c(1,2,5,6)], as.numeric)
nov_placeVisit_selected <- nov_placeVisit_selected %>% mutate(start_date = ms_to_date(duration_startTimestampMs), end_date = ms_to_date(duration_endTimestampMs))
nov_placeVisit_selected$start_date <- ymd_hms(nov_placeVisit_selected$start_date)
nov_placeVisit_selected$end_date <- ymd_hms(nov_placeVisit_selected$end_date)
nov_placeVisit_selected <- nov_placeVisit_selected %>% mutate(duration_in_sec = (duration_endTimestampMs - duration_startTimestampMs)/1000)
nov_most_visited <- nov_placeVisit_selected %>% group_by(location_name) %>% summarise(amount = n(), full_duration = sum(duration_in_sec))
nov_placeVisit_selected <- nov_placeVisit_selected %>% full_join(nov_most_visited, by='location_name')


all <- rbind(jan_placeVisit_selected, feb_placeVisit_selected, mar_placeVisit_selected, may_placeVisit_selected, jun_placeVisit_selected,jul_placeVisit_selected, aug_placeVisit_selected, sep_placeVisit_selected, oct_placeVisit_selected, nov_placeVisit_selected, dec_placeVisit_selected)
most_visited <- all %>% group_by(location_name) %>% summarise(times = n(), total_time = sum(duration_in_sec))
all <- all %>% full_join(most_visited, by='location_name') %>% filter(location_name != "NA") %>% arrange(location_name) %>% transform()
all$location_latitudeE7 <- all$location_latitudeE7 / 10000000
all$location_longitudeE7 <- all$location_longitudeE7 / 10000000

#Wiktor
november_w <- rjson::fromJSON(file = "2021_NOVEMBER_WJ.json")
january_w <- rjson::fromJSON(file = "2021_JANUARY_WJ.json")
february_w <- rjson::fromJSON(file = "2021_FEBRUARY_WJ.json")
march_w <- rjson::fromJSON(file = "2021_MARCH_WJ.json")
may_w <- rjson::fromJSON(file = "2021_MAY_WJ.json")
june_w <- rjson::fromJSON(file = "2021_JUNE_WJ.json")
july_w <- rjson::fromJSON(file = "2021_JULY_WJ.json")
august_w <- rjson::fromJSON(file = "2021_AUGUST_WJ.json")
september_w <- rjson::fromJSON(file = "2021_SEPTEMBER_WJ.json")
october_w <- rjson::fromJSON(file = "2021_OCTOBER_WJ.json")
december_w <- rjson::fromJSON(file = "2021_DECEMBER_WJ.json")
april_w <- rjson::fromJSON(file = "2021_APRIL_WJ.json")
nov_w <- november_w[[1]]
jan_w <- january_w[[1]]
feb_w <- february_w[[1]]
mar_w <- march_w[[1]]
may_w <- may_w[[1]]
jun_w <- june_w[[1]]
jul_w <- july_w[[1]]
aug_w <- august_w[[1]]
sep_w <- september_w[[1]]
oct_w <- october_w[[1]]
dec_w <- december_w[[1]]
apr_w <- april_w[[1]]

nov5_w <- rrapply(nov_w, how = "melt") 
jan5_w <- rrapply(jan_w, how = "melt") 
feb5_w <- rrapply(feb_w, how = "melt") 
mar5_w <- rrapply(mar_w, how = "melt") 
may5_w <- rrapply(may_w, how = "melt") 
jun5_w <- rrapply(jun_w, how = "melt") 
jul5_w <- rrapply(jul_w, how = "melt") 
aug5_w <- rrapply(aug_w, how = "melt") 
sep5_w <- rrapply(sep_w, how = "melt") 
oct5_w <- rrapply(oct_w, how = "melt") 
apr5_w <- rrapply(apr_w, how = "melt")
dec5_w <- rrapply(dec_w, how = "melt") 

nov_activity_w <- nov5_w %>% filter(L2 == "activitySegment") %>% select(!L2) %>% 
    pivot_wider(names_from = c(L3, L4, L5, L6, L7))
nov_placeVisit_w <- nov5_w %>% filter(L2 == "placeVisit") %>% select(!L2) %>% 
    pivot_wider(names_from = c(L3, L4, L5, L6, L7))
colnames(nov_activity_w) <- colnames(nov_activity_w) %>% str_replace_all('_NA', "")
colnames(nov_placeVisit_w) <- colnames(nov_placeVisit_w) %>% str_replace_all('_NA', "")
nov_placeVisit_w <- nov_placeVisit_w[-c(60,73),]
nov_placeVisit_selected_w <- nov_placeVisit_w[,c('location_latitudeE7', 'location_longitudeE7', 'location_address', 'location_name', 'duration_startTimestampMs', 'duration_endTimestampMs')]

nov_placeVisit_selected_w[, c(1,2,5,6)] <- sapply(nov_placeVisit_selected_w[, c(1,2,5,6)], as.numeric)
nov_placeVisit_selected_w <- nov_placeVisit_selected_w %>% mutate(start_date = ms_to_date(duration_startTimestampMs), end_date = ms_to_date(duration_endTimestampMs))
nov_placeVisit_selected_w$start_date <- ymd_hms(nov_placeVisit_selected_w$start_date)
nov_placeVisit_selected_w$end_date <- ymd_hms(nov_placeVisit_selected_w$end_date)
nov_placeVisit_selected_w <- nov_placeVisit_selected_w %>% mutate(duration_in_sec = (duration_endTimestampMs - duration_startTimestampMs)/1000)
nov_most_visited_w <- nov_placeVisit_selected_w %>% group_by(location_name) %>% summarise(amount = n(), full_duration = sum(duration_in_sec))
nov_placeVisit_selected_w <- nov_placeVisit_selected_w %>% full_join(nov_most_visited_w, by='location_name')

jan_placeVisit_selected_w <- change(jan5_w)
feb_placeVisit_selected_w <- change(feb5_w)
mar_placeVisit_selected_w <- change(mar5_w)
apr_placeVisit_selected_w <- change(apr5_w)
may_placeVisit_selected_w <- change(may5_w)
jun_placeVisit_selected_w <- change(jun5_w)
jul_placeVisit_selected_w <- change(jul5_w)
aug_placeVisit_selected_w <- change(aug5_w)
sep_placeVisit_selected_w <- change(sep5_w)
oct_placeVisit_selected_w <- change(oct5_w)
dec_placeVisit_selected_w <- change(dec5_w)

all_w <- rbind(jan_placeVisit_selected_w, feb_placeVisit_selected_w, mar_placeVisit_selected_w, apr_placeVisit_selected_w, may_placeVisit_selected_w, jun_placeVisit_selected_w,jul_placeVisit_selected_w, aug_placeVisit_selected_w, sep_placeVisit_selected_w, oct_placeVisit_selected_w, nov_placeVisit_selected_w, dec_placeVisit_selected_w)
most_visited_w <- all_w %>% group_by(location_name) %>% summarise(times = n(), total_time = sum(duration_in_sec))
all_w <- all_w %>% full_join(most_visited_w, by='location_name') %>% filter(location_name != "NA") %>% arrange(end_date) 
all_w <- slice(all_w, 1:(n() - 1)) %>% arrange(location_name)
all_w$location_latitudeE7 <- all_w$location_latitudeE7 / 10000000
all_w$location_longitudeE7 <- all_w$location_longitudeE7 / 10000000

#Zuzia
november_z <- rjson::fromJSON(file = "2019_NOVEMBER.json")
january_z <- rjson::fromJSON(file = "2019_JANUARY.json")
february_z <- rjson::fromJSON(file = "2019_FEBRUARY.json")
march_z <- rjson::fromJSON(file = "2019_MARCH.json")
may_z <- rjson::fromJSON(file = "2019_MAY.json")
june_z <- rjson::fromJSON(file = "2019_JUNE.json")
july_z <- rjson::fromJSON(file = "2019_JULY.json")
august_z <- rjson::fromJSON(file = "2019_AUGUST.json")
september_z <- rjson::fromJSON(file = "2019_SEPTEMBER.json")
october_z <- rjson::fromJSON(file = "2019_OCTOBER.json")
december_z <- rjson::fromJSON(file = "2019_DECEMBER.json")
april_z <- rjson::fromJSON(file = "2019_APRIL.json")

nov_z <- november_z[[1]]
jan_z <- january_z[[1]]
feb_z <- february_z[[1]]
mar_z <- march_z[[1]]
may_z <- may_z[[1]]
jun_z <- june_z[[1]]
jul_z <- july_z[[1]]
aug_z <- august_z[[1]]
sep_z <- september_z[[1]]
oct_z <- october_z[[1]]
dec_z <- december_z[[1]]
apr_z <- april_z[[1]]

nov5_z <- rrapply(nov_z, how = "melt") 
jan5_z <- rrapply(jan_z, how = "melt") 
feb5_z <- rrapply(feb_z, how = "melt") 
mar5_z <- rrapply(mar_z, how = "melt") 
may5_z <- rrapply(may_z, how = "melt") 
jun5_z <- rrapply(jun_z, how = "melt") 
jul5_z <- rrapply(jul_z, how = "melt") 
aug5_z <- rrapply(aug_z, how = "melt") 
sep5_z <- rrapply(sep_z, how = "melt") 
oct5_z <- rrapply(oct_z, how = "melt") 
apr5_z <- rrapply(apr_z, how = "melt")
dec5_z <- rrapply(dec_z, how = "melt") 

nov_activity_z <- nov5_z %>% filter(L2 == "activitySegment") %>% select(!L2) %>% 
    pivot_wider(names_from = c(L3, L4, L5, L6, L7))
nov_placeVisit_z <- nov5_z %>% filter(L2 == "placeVisit") %>% select(!L2) %>% 
    pivot_wider(names_from = c(L3, L4, L5, L6, L7))
colnames(nov_activity_z) <- colnames(nov_activity_z) %>% str_replace_all('_NA', "")
colnames(nov_placeVisit_z) <- colnames(nov_placeVisit_z) %>% str_replace_all('_NA', "")
nov_placeVisit_z <- nov_placeVisit_z[-c(60,73),]
nov_placeVisit_selected_z <- nov_placeVisit_z[,c('location_latitudeE7', 'location_longitudeE7', 'location_address', 'location_name', 'duration_startTimestampMs', 'duration_endTimestampMs')]

nov_placeVisit_selected_z[, c(1,2,5,6)] <- sapply(nov_placeVisit_selected_z[, c(1,2,5,6)], as.numeric)
nov_placeVisit_selected_z <- nov_placeVisit_selected_z %>% mutate(start_date = ms_to_date(duration_startTimestampMs), end_date = ms_to_date(duration_endTimestampMs))
nov_placeVisit_selected_z$start_date <- ymd_hms(nov_placeVisit_selected_z$start_date)
nov_placeVisit_selected_z$end_date <- ymd_hms(nov_placeVisit_selected_z$end_date)
nov_placeVisit_selected_z <- nov_placeVisit_selected_z %>% mutate(duration_in_sec = (duration_endTimestampMs - duration_startTimestampMs)/1000)
nov_most_visited_z <- nov_placeVisit_selected_z %>% group_by(location_name) %>% summarise(amount = n(), full_duration = sum(duration_in_sec))
nov_placeVisit_selected_z <- nov_placeVisit_selected_z %>% full_join(nov_most_visited_z, by='location_name')

jan_placeVisit_selected_z <- change(jan5_z)
feb_placeVisit_selected_z <- change(feb5_z)
mar_placeVisit_selected_z <- change(mar5_z)
apr_placeVisit_selected_z <- change(apr5_z)
may_placeVisit_selected_z <- change(may5_z)
jun_placeVisit_selected_z <- change(jun5_z)
jul_placeVisit_selected_z <- change(jul5_z)
aug_placeVisit_selected_z <- change(aug5_z)
sep_placeVisit_selected_z <- change(sep5_z)
oct_placeVisit_selected_z <- change(oct5_z)
dec_placeVisit_selected_z <- change(dec5_z)

all_z <- rbind(jan_placeVisit_selected_z, feb_placeVisit_selected_z, mar_placeVisit_selected_z, apr_placeVisit_selected_z, may_placeVisit_selected_z, jun_placeVisit_selected_z,jul_placeVisit_selected_z, aug_placeVisit_selected_z, sep_placeVisit_selected_z, oct_placeVisit_selected_z, nov_placeVisit_selected_z, dec_placeVisit_selected_z)
most_visited_z <- all_z %>% group_by(location_name) %>% summarise(times = n(), total_time = sum(duration_in_sec))
all_z <- all_z %>% full_join(most_visited_z, by='location_name') %>% filter(location_name != "NA") %>% arrange(end_date) 
all_z <- slice(all_z, 1:(n() - 1)) %>% arrange(location_name)
all_z$location_latitudeE7 <- all_z$location_latitudeE7 / 10000000
all_z$location_longitudeE7 <- all_z$location_longitudeE7 / 10000000

change_activity <- function(month) {
    month_activity <- month %>% filter(L2 == "activitySegment") %>% select(!L2) %>% 
        pivot_wider(names_from = c(L3, L4, L5, L6))
    colnames(month_activity) <- colnames(month_activity) %>% str_replace_all('_NA', "")
    month_activity <- month_activity %>% select(duration_startTimestampMs, duration_endTimestampMs, activityType)
    month_activity[, c(1,2)] <- sapply(month_activity[, c(1,2)], as.numeric)
    df_month <- month_activity %>% 
        mutate(duration_h = (duration_endTimestampMs - duration_startTimestampMs)/3600000) %>% 
        select(activityType, duration_h)
    return(df_month)
}

df_jan <- change_activity(jan5)
df_feb <- change_activity(feb5)
df_mar <- change_activity(mar5)
df_may <- change_activity(may5)
df_jun <- change_activity(jun5)
df_jul<- change_activity(jul5)
df_aug <- change_activity(aug5)
df_sep <- change_activity(sep5)
df_oct <- change_activity(oct5)
df_dec <- change_activity(dec5)
df_nov <- change_activity(nov5)

df_winter <- rbind(df_dec, df_jan, df_feb) %>%
    filter(activityType != "UNKNOWN_ACTIVITY_TYPE") %>% 
    group_by(activityType) %>%
    summarise(mean_duration = mean(duration_h))
df_winter$activityType <- gsub("_", " ", df_winter$activityType)

df_autumn <- rbind(df_sep, df_oct, df_nov) %>%
    filter(activityType != "UNKNOWN_ACTIVITY_TYPE" & activityType != "IN_FERRY") %>% 
    group_by(activityType) %>%
    summarise(mean_duration = mean(duration_h))
df_autumn$activityType <- gsub("_", " ", df_autumn$activityType)

df_spring <- rbind(df_mar, df_may) %>%
    filter(activityType != "UNKNOWN_ACTIVITY_TYPE") %>% 
    group_by(activityType) %>%
    summarise(mean_duration = mean(duration_h))
df_spring$activityType <- gsub("_", " ", df_spring$activityType)

df_summer <- rbind(df_jun, df_jul, df_aug) %>%
    filter(activityType != "UNKNOWN_ACTIVITY_TYPE" & activityType != "IN_FERRY") %>% 
    group_by(activityType) %>%
    summarise(mean_duration = mean(duration_h))
df_summer$activityType <- gsub("_", " ", df_summer$activityType)

df_jan_w <- change_activity(jan5_w)
df_feb_w <- change_activity(feb5_w)
df_mar_w <- change_activity(mar5_w)
df_apr_w <- change_activity(apr5_w)
df_may_w <- change_activity(may5_w)
df_jun_w <- change_activity(jun5_w)
df_jul_w <- change_activity(jul5_w)
df_aug_w <- change_activity(aug5_w)
df_sep_w <- change_activity(sep5_w)
df_oct_w <- change_activity(oct5_w)
df_dec_w <- change_activity(dec5_w)
df_nov_w <- change_activity(nov5_w)

df_winter_w <- rbind(df_dec_w, df_jan_w, df_feb_w) %>%
    filter(activityType != "UNKNOWN_ACTIVITY_TYPE") %>% 
    group_by(activityType) %>%
    summarise(mean_duration = mean(duration_h))
df_winter_w$activityType <- gsub("_", " ", df_winter_w$activityType)

df_autumn_w <- rbind(df_sep_w, df_oct_w, df_nov_w) %>%
    filter(activityType != "UNKNOWN_ACTIVITY_TYPE" & activityType != "IN_FERRY") %>% 
    group_by(activityType) %>%
    summarise(mean_duration = mean(duration_h))
df_autumn_w$activityType <- gsub("_", " ", df_autumn_w$activityType)

df_spring_w <- rbind(df_mar_w, df_apr_w, df_may_w) %>%
    filter(activityType != "UNKNOWN_ACTIVITY_TYPE") %>% 
    group_by(activityType) %>%
    summarise(mean_duration = mean(duration_h))
df_spring_w$activityType <- gsub("_", " ", df_spring_w$activityType)

df_summer_w <- rbind(df_jun_w, df_jul_w, df_aug_w) %>%
    filter(activityType != "UNKNOWN_ACTIVITY_TYPE" & activityType != "IN_FERRY") %>% 
    group_by(activityType) %>%
    summarise(mean_duration = mean(duration_h))
df_summer_w$activityType <- gsub("_", " ", df_summer_w$activityType)

df_jan_z <- change_activity(jan5_z)
df_feb_z <- change_activity(feb5_z)
df_mar_z <- change_activity(mar5_z)
df_apr_z <- change_activity(apr5_z)
df_may_z <- change_activity(may5_z)
df_jun_z <- change_activity(jun5_z)
df_jul_z <- change_activity(jul5_z)
df_aug_z <- change_activity(aug5_z)
df_sep_z <- change_activity(sep5_z)
df_oct_z <- change_activity(oct5_z)
df_dec_z <- change_activity(dec5_z)
df_nov_z <- change_activity(nov5_z)

df_winter_z <- rbind(df_dec_z, df_jan_z, df_feb_z) %>%
    filter(activityType != "UNKNOWN_ACTIVITY_TYPE") %>% 
    group_by(activityType) %>%
    summarise(mean_duration = mean(duration_h))
df_winter_z$activityType <- gsub("_", " ", df_winter_z$activityType)

df_autumn_z <- rbind(df_sep_z, df_oct_z, df_nov_z) %>%
    filter(activityType != "UNKNOWN_ACTIVITY_TYPE" & activityType != "IN_FERRY") %>% 
    group_by(activityType) %>%
    summarise(mean_duration = mean(duration_h))
df_autumn_z$activityType <- gsub("_", " ", df_autumn_z$activityType)

df_spring_z <- rbind(df_mar_z, df_apr_z, df_may_z) %>%
    filter(activityType != "UNKNOWN_ACTIVITY_TYPE") %>% 
    group_by(activityType) %>%
    summarise(mean_duration = mean(duration_h))
df_spring_z$activityType <- gsub("_", " ", df_spring_z$activityType)

df_summer_z <- rbind(df_jun_z, df_jul_z, df_aug_z) %>%
    filter(activityType != "UNKNOWN_ACTIVITY_TYPE" & activityType != "IN_FERRY") %>% 
    group_by(activityType) %>%
    summarise(mean_duration = mean(duration_h))
df_summer_z$activityType <- gsub("_", " ", df_summer_z$activityType)


library(shinydashboard)

convertMenuItem <- function(mi,tabName) {
    mi$children[[1]]$attribs['data-toggle']="tab"
    mi$children[[1]]$attribs['data-value'] = tabName
    if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
        mi$attribs$class=NULL
    }
    mi
}
dbHeader <- dashboardHeader(title = "Google maps - Data Analysis")
# dbHeader$children[[2]]$children <-  tags$a(href='https://www.google.pl/maps/preview',
#                                            tags$img(src='google-logo.png',height='30',width='30'))

ui <- dashboardPage(
   
    dbHeader,
    #skin = "black",
    dashboardSidebar(
        sidebarMenu(id = "tabs",
                    menuItem("Main Panel", tabName = "main", icon = icon("dashboard")),
                    convertMenuItem(menuItem("Visited Places", tabName= "places",icon = icon("bar-chart-o"),
                                             menuSubItem("Asia", "asiaplaces"),
                                             menuSubItem("Zuzia G.", "wiktorplaces"),
                                             menuSubItem("Zuzia K.", "zuziaplaces")),"places"),
                    
                    # selectInput("chart",
                    #             label = "Select chart:",
                    #             choices = c("VisitedPlaces", "Activities"))),
                    menuItem("Activities", tabName = "act",icon = icon("bar-chart-o"),
                             menuSubItem("Asia", "asiaact"),
                             menuSubItem("Zuzia G.", "wiktoract"),
                             menuSubItem("Zuzia K.", "zuziaact")))
    ),
    dashboardBody(
        
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        
        tabItems(
            # First tab content
            tabItem(tabName = "main",
                    HTML('<a href="https://www.google.pl/maps/preview"><center><img src="google-logo.png" width="400"></center></a>'),
                    h1("Google maps - Data Analysis", align = "center", style = "color:grey45"),
                    h3("Zuzanna Glinka", style = "color:grey73"),
                    h3("Joanna Kajka", style = "color:grey45"),
                    h3("Zuzanna Kotlińska", style = "color:grey45")),
            tabItem(tabName = "places",
                    # h2("plot dla asi")
                    fluidRow(
                        box(
                            title = "Pick location and date range:",
                            leafletOutput("mymap"),
                            dateRangeInput("dates", label = "data range:", min = min(ymd(as.character("2019-01-01"))), max = max(all_w$end_date)),
                            width = 4
                        ),
                        box(
                            title = "How much time did we spend in chosen locations per hour",
                            plotlyOutput("plot1"),
                            width = 7),
                        box(
                            title = "What part of the day did we spend in chosen locations",
                            plotlyOutput("plot2"),
                            width = 7))
            ),
            tabItem(tabName = "asiaact",
                    fluidRow(
                        
                        # Application title
                        box(checkboxGroupInput("chosenActivity1", "Select activities: ", sort(distinct(df_summer, activityType)$activityType)), width = 3),
                        
                        box(title = "Mean duaration [h] spent on each activity",
                            shinycssloaders::withSpinner(plotly::plotlyOutput("seasonPlot1", width = 900, height =540)), width = 7)
                        
                        #type = getOption("spinner.type", default = 2),
                        #color = getOption("spinner.color", default = '#D0C19F'),
                        #color.background = getOption("spinner.color.background", default ='white'))
                    )
            ),
            tabItem(tabName = "wiktoract",
                    fluidRow(
                        
                        # Application title
                        box(checkboxGroupInput("chosenActivity2", "Select activities: ", sort(distinct(df_summer_w, activityType)$activityType)), width = 3),
                        
                        box(title = "Mean duaration [h] spent on each activity",
                            shinycssloaders::withSpinner(plotly::plotlyOutput("seasonPlot2", width = 900, height =540)), width = 7)
                        
                        #type = getOption("spinner.type", default = 2),
                        #color = getOption("spinner.color", default = '#D0C19F'),
                        #color.background = getOption("spinner.color.background", default ='white'))
                    )),
            tabItem(tabName = "zuziaact",
                    fluidRow(
                        
                        # Application title
                        box(checkboxGroupInput("chosenActivity3", "Select activities: ", sort(distinct(df_summer_z, activityType)$activityType)), width = 3),
                        
                        box(title = "Mean duaration [h] spent on each activity",
                            shinycssloaders::withSpinner(plotly::plotlyOutput("seasonPlot3", width = 900, height =540)), width = 7)
                        
                        #type = getOption("spinner.type", default = 2),
                        #color = getOption("spinner.color", default = '#D0C19F'),
                        #color.background = getOption("spinner.color.background", default ='white'))
                    ))
        )))





server <- function(input, output, session) {
    
    output$seasonPlot2 <- renderPlotly({
        
        df_winter_w <- df_winter_w %>% filter(activityType %in% input$chosenActivity2)
        df_autumn_w <- df_autumn_w %>% filter(activityType %in% input$chosenActivity2)
        df_summer_w <- df_summer_w %>% filter(activityType %in% input$chosenActivity2)
        df_spring_w <- df_spring_w %>% filter(activityType %in% input$chosenActivity2)
        
        p_winter <- plot_ly(df_winter_w, type = "bar", x = ~mean_duration, y = ~reorder(activityType, +mean_duration),
                            orientation = "h", hovertemplate = paste('<br>meanDuration: %{x} [h]<br>', "<br>activity:%{y}<br>"),
                            color = I("#4285F4"))
        p_autumn <- plot_ly(df_autumn_w, type = "bar", x = ~mean_duration, y = ~reorder(activityType, +mean_duration),
                            orientation = "h", hovertemplate = paste('<br>meanDuration: %{x} [h]<br>', "<br>activity:%{y}<br>"),
                            color = I("#EA4335"))
        p_spring <- plot_ly(df_spring_w, type = "bar", x = ~mean_duration, y = ~reorder(activityType, +mean_duration),
                            orientation = "h", hovertemplate = paste('<br>meanDuration: %{x} [h]<br>', "<br>activity:%{y}<br>"),
                            color = I("#34A853"))
        p_summer <- plot_ly(df_summer_w, type = "bar", x = ~mean_duration, y = ~reorder(activityType, +mean_duration),
                            orientation = "h", hovertemplate = paste('<br>meanDuration: %{x} [h]<br>', "<br>activity:%{y}<br>"),
                            color = I("#FBBC05"))
        fig <- subplot(p_spring, p_summer, p_autumn, p_winter, nrows = 2, margin = c(0.15, 0.15, 0.05, 0.05))
        fig <- fig %>%layout(showlegend = F,
                             plot_bgcolor='#e5ecf6',
                             xaxis = list(
                                 zerolinecolor = '#ffff',
                                 zerolinewidth = 2,
                                 gridcolor = 'ffff',
                                 range = list(0, 3)),
                             xaxis2 = list(
                                 zerolinecolor = '#ffff',
                                 zerolinewidth = 2,
                                 gridcolor = 'ffff',
                                 range = list(0, 3)),
                             xaxis3 = list(
                                 zerolinecolor = '#ffff',
                                 zerolinewidth = 2,
                                 gridcolor = 'ffff',
                                 range = list(0, 3)),
                             xaxis4 = list(
                                 zerolinecolor = '#ffff',
                                 zerolinewidth = 2,
                                 gridcolor = 'ffff',
                                 range = list(0, 3)),
                             yaxis = list(
                                 zerolinecolor = '#ffff',
                                 zerolinewidth = 2,
                                 gridcolor = 'ffff'),
                             yaxis2 = list(
                                 zerolinecolor = '#ffff',
                                 zerolinewidth = 2,
                                 gridcolor = 'ffff'),
                             yaxis3 = list(
                                 zerolinecolor = '#ffff',
                                 zerolinewidth = 2,
                                 gridcolor = 'ffff'),
                             yaxis4 = list(
                                 zerolinecolor = '#ffff',
                                 zerolinewidth = 2,
                                 gridcolor = 'ffff'))
        
        annotations = list(
            list(
                x = 0.2,
                y = 1.0,
                text = "Spring",
                xref = "paper",
                yref = "paper",
                xanchor = "center",
                yanchor = "bottom",
                showarrow = FALSE
            ),
            list(
                x = 0.8,
                y = 1,
                text = "Summer",
                xref = "paper",
                yref = "paper",
                xanchor = "center",
                yanchor = "bottom",
                showarrow = FALSE
            ),
            list(
                x = 0.2,
                y = 0.45,
                text = "Autumn",
                xref = "paper",
                yref = "paper",
                xanchor = "center",
                yanchor = "bottom",
                showarrow = FALSE
            ),
            list(
                x = 0.8,
                y = 0.45,
                text = "Winter",
                xref = "paper",
                yref = "paper",
                xanchor = "center",
                yanchor = "bottom",
                showarrow = FALSE
            ))
        fig %>%layout(annotations = annotations)
        
        
    }) %>% bindCache(input$chosenActivity2)
    
    output$seasonPlot3 <- renderPlotly({
        
        df_winter_z <- df_winter_z %>% filter(activityType %in% input$chosenActivity3)
        df_autumn_z <- df_autumn_z %>% filter(activityType %in% input$chosenActivity3)
        df_summer_z <- df_summer_z %>% filter(activityType %in% input$chosenActivity3)
        df_spring_z <- df_spring_z %>% filter(activityType %in% input$chosenActivity3)
        
        p_winter <- plot_ly(df_winter_z, type = "bar", x = ~mean_duration, y = ~reorder(activityType, +mean_duration),
                            orientation = "h", hovertemplate = paste('<br>meanDuration: %{x} [h]<br>', "<br>activity:%{y}<br>"),
                            color = I("#4285F4"))
        p_autumn <- plot_ly(df_autumn_z, type = "bar", x = ~mean_duration, y = ~reorder(activityType, +mean_duration),
                            orientation = "h", hovertemplate = paste('<br>meanDuration: %{x} [h]<br>', "<br>activity:%{y}<br>"),
                            color = I("#EA4335"))
        p_spring <- plot_ly(df_spring_z, type = "bar", x = ~mean_duration, y = ~reorder(activityType, +mean_duration),
                            orientation = "h", hovertemplate = paste('<br>meanDuration: %{x} [h]<br>', "<br>activity:%{y}<br>"),
                            color = I("#34A853"))
        p_summer <- plot_ly(df_summer_z, type = "bar", x = ~mean_duration, y = ~reorder(activityType, +mean_duration),
                            orientation = "h", hovertemplate = paste('<br>meanDuration: %{x} [h]<br>', "<br>activity:%{y}<br>"),
                            color = I("#FBBC05"))
        fig <- subplot(p_spring, p_summer, p_autumn, p_winter, nrows = 2, margin = c(0.15, 0.15, 0.05, 0.05))
        fig <- fig %>%layout(showlegend = F,
                             plot_bgcolor='#e5ecf6',
                             xaxis = list(
                                 zerolinecolor = '#ffff',
                                 zerolinewidth = 2,
                                 gridcolor = 'ffff',
                                 range = list(0, 3.5)),
                             xaxis2 = list(
                                 zerolinecolor = '#ffff',
                                 zerolinewidth = 2,
                                 gridcolor = 'ffff',
                                 range = list(0, 3.5)),
                             xaxis3 = list(
                                 zerolinecolor = '#ffff',
                                 zerolinewidth = 2,
                                 gridcolor = 'ffff',
                                 range = list(0, 3.5)),
                             xaxis4 = list(
                                 zerolinecolor = '#ffff',
                                 zerolinewidth = 2,
                                 gridcolor = 'ffff',
                                 range = list(0, 3.5)),
                             yaxis = list(
                                 zerolinecolor = '#ffff',
                                 zerolinewidth = 2,
                                 gridcolor = 'ffff'),
                             yaxis2 = list(
                                 zerolinecolor = '#ffff',
                                 zerolinewidth = 2,
                                 gridcolor = 'ffff'),
                             yaxis3 = list(
                                 zerolinecolor = '#ffff',
                                 zerolinewidth = 2,
                                 gridcolor = 'ffff'),
                             yaxis4 = list(
                                 zerolinecolor = '#ffff',
                                 zerolinewidth = 2,
                                 gridcolor = 'ffff'))
        
        annotations = list(
            list(
                x = 0.2,
                y = 1.0,
                text = "Spring",
                xref = "paper",
                yref = "paper",
                xanchor = "center",
                yanchor = "bottom",
                showarrow = FALSE
            ),
            list(
                x = 0.8,
                y = 1,
                text = "Summer",
                xref = "paper",
                yref = "paper",
                xanchor = "center",
                yanchor = "bottom",
                showarrow = FALSE
            ),
            list(
                x = 0.2,
                y = 0.45,
                text = "Autumn",
                xref = "paper",
                yref = "paper",
                xanchor = "center",
                yanchor = "bottom",
                showarrow = FALSE
            ),
            list(
                x = 0.8,
                y = 0.45,
                text = "Winter",
                xref = "paper",
                yref = "paper",
                xanchor = "center",
                yanchor = "bottom",
                showarrow = FALSE
            ))
        fig %>%layout(annotations = annotations)
        
        
    }) %>% bindCache(input$chosenActivity3)
    
    output$seasonPlot1 <- renderPlotly({
        
        df_winter <- df_winter %>% filter(activityType %in% input$chosenActivity1)
        df_autumn <- df_autumn %>% filter(activityType %in% input$chosenActivity1)
        df_summer <- df_summer %>% filter(activityType %in% input$chosenActivity1)
        df_spring <- df_spring %>% filter(activityType %in% input$chosenActivity1)
        
        p_winter <- plot_ly(df_winter, type = "bar", x = ~mean_duration, y = ~reorder(activityType, +mean_duration),
                            orientation = "h", hovertemplate = paste('<br>meanDuration: %{x} [h]<br>', "<br>activity:%{y}<br>"),
                            color = I("#4285F4"))
        p_autumn <- plot_ly(df_autumn, type = "bar", x = ~mean_duration, y = ~reorder(activityType, +mean_duration),
                            orientation = "h", hovertemplate = paste('<br>meanDuration: %{x} [h]<br>', "<br>activity:%{y}<br>"),
                            color = I("#EA4335"))
        p_spring <- plot_ly(df_spring, type = "bar", x = ~mean_duration, y = ~reorder(activityType, +mean_duration),
                            orientation = "h", hovertemplate = paste('<br>meanDuration: %{x} [h]<br>', "<br>activity:%{y}<br>"),
                            color = I("#34A853"))
        p_summer <- plot_ly(df_summer, type = "bar", x = ~mean_duration, y = ~reorder(activityType, +mean_duration),
                            orientation = "h", hovertemplate = paste('<br>meanDuration: %{x} [h]<br>', "<br>activity:%{y}<br>"),
                            color = I("#FBBC05"))
        fig <- subplot(p_spring, p_summer, p_autumn, p_winter, nrows = 2, margin = c(0.15, 0.15, 0.05, 0.05))
        fig <- fig %>%layout(showlegend = F,
                             plot_bgcolor='#e5ecf6',
                             xaxis = list(
                                 zerolinecolor = '#ffff',
                                 zerolinewidth = 2,
                                 gridcolor = 'ffff',
                                 range = list(0, 3)),
                             xaxis2 = list(
                                 zerolinecolor = '#ffff',
                                 zerolinewidth = 2,
                                 gridcolor = 'ffff',
                                 range = list(0, 3)),
                             xaxis3 = list(
                                 zerolinecolor = '#ffff',
                                 zerolinewidth = 2,
                                 gridcolor = 'ffff',
                                 range = list(0, 3)),
                             xaxis4 = list(
                                 zerolinecolor = '#ffff',
                                 zerolinewidth = 2,
                                 gridcolor = 'ffff',
                                 range = list(0, 3)),
                             yaxis = list(
                                 zerolinecolor = '#ffff',
                                 zerolinewidth = 2,
                                 gridcolor = 'ffff'),
                             yaxis2 = list(
                                 zerolinecolor = '#ffff',
                                 zerolinewidth = 2,
                                 gridcolor = 'ffff'),
                             yaxis3 = list(
                                 zerolinecolor = '#ffff',
                                 zerolinewidth = 2,
                                 gridcolor = 'ffff'),
                             yaxis4 = list(
                                 zerolinecolor = '#ffff',
                                 zerolinewidth = 2,
                                 gridcolor = 'ffff'))
        
        annotations = list(
            list(
                x = 0.2,
                y = 1.0,
                text = "Spring",
                xref = "paper",
                yref = "paper",
                xanchor = "center",
                yanchor = "bottom",
                showarrow = FALSE
            ),
            list(
                x = 0.8,
                y = 1,
                text = "Summer",
                xref = "paper",
                yref = "paper",
                xanchor = "center",
                yanchor = "bottom",
                showarrow = FALSE
            ),
            list(
                x = 0.2,
                y = 0.45,
                text = "Autumn",
                xref = "paper",
                yref = "paper",
                xanchor = "center",
                yanchor = "bottom",
                showarrow = FALSE
            ),
            list(
                x = 0.8,
                y = 0.45,
                text = "Winter",
                xref = "paper",
                yref = "paper",
                xanchor = "center",
                yanchor = "bottom",
                showarrow = FALSE
            ))
        fig %>%layout(annotations = annotations)
        
        
    }) %>% bindCache(input$chosenActivity1)
    
    
    #https://groups.google.com/g/shiny-discuss/c/LWk4ZYNhsSc
    
    observeEvent(input$tabs,{
        if(input$tabs == "asiaplaces"){
            all <- all
        }else if(input$tabs == "wiktorplaces"){
            all <- all_w
        }else if(input$tabs == "zuziaplaces"){
            all <- all_z
        }else {
            all <- all
        }
        
        points <- all %>% select(location_latitudeE7, location_longitudeE7, location_address) %>%
            unique() %>%  rename(X = location_longitudeE7, Y = location_latitudeE7, id = location_address) %>% 
            drop_na(id)
        points$clicked <- FALSE
        RV <- reactiveValues(points = points)
        
        
        icons <- awesomeIcons(
            icon = 'ios-close',
            iconColor = 'white',
            library = 'ion',
            markerColor = "blue"
        )
        
        output$mymap <- renderLeaflet({
            leaflet() %>%
                #addTiles() %>%
                addProviderTiles("OpenStreetMap", group = "OSM") %>%
                addAwesomeMarkers(data = points, lng = ~X, lat = ~Y, layerId = ~id, icon = icons)
        })
        
        myLeafletProxy <- leafletProxy(mapId = "mymap", session)
        
        observeEvent(input$mymap_marker_click,{
            clicked_point <- input$mymap_marker_click
            RV$points[points$id==clicked_point$id,]$clicked <- !(RV$points[points$id==clicked_point$id,]$clicked)
            print(RV$points[points$id==clicked_point$id,]$clicked)
            removeMarker(map = myLeafletProxy, layerId = clicked_point$id)
            
            addAwesomeMarkers(map = myLeafletProxy,
                              lng = clicked_point$lng,
                              lat = clicked_point$lat,
                              layerId = clicked_point$id,
                              icon = awesomeIcons(
                                  icon = 'ios-close',
                                  iconColor = 'white',
                                  library = 'ion',
                                  markerColor = ifelse(RV$points[points$id==clicked_point$id,]$clicked, yes = "red", no = "blue")
                              ))
        })
        
        output$plot1 <- plotly::renderPlotly({
            
            x <- input$dates
            y <- c(x[1], x[2])
            y <- sapply(y, as.character)
            date1 <- ymd(y[1])
            date2 <- ymd(y[2])
            
            # if (input$tabs == "asia") {
            #   all <- all
            # } else {
            #   all <- all_w
            # }
            
            new_all <- all %>%  filter(location_address %in% RV$points$id[which(RV$points$clicked)]) %>% 
                filter((start_date >= date1 & start_date <= date2) 
                       | (end_date >= date1 & end_date <= date2)) %>% 
                mutate(end_date = replace(end_date, end_date <= date1, date1)) %>% 
                mutate(end_date = replace(end_date, end_date >= date2, date2)) %>% 
                mutate(start_date = replace(start_date, start_date <= date1, date1)) %>% 
                mutate(start_date = replace(start_date, start_date >= date2, date2))
            
            if (is.na((new_all)$location_address[1])) {
                hours <- data.frame(hour = c(1:24), per = c(rep(0, 24)))
            } else {
                whole_time <- 0
                hours <- data.frame(hour = c(1:24), time = c(rep(0, 24)))
                selected_place <- new_all  %>%
                    select(start_date,end_date) %>%
                    transmute(start_date = ymd_hms(start_date), end_date = ymd_hms(end_date)) %>%
                    mutate(start_hour = hour(start_date), end_hour = hour(end_date))
                d <- c()
                
                for (k in 1:nrow(selected_place)) {
                    if (month(selected_place$start_date[k]) == month(selected_place$end_date[k])) {
                        if (day(selected_place$start_date[k]) == day(selected_place$end_date[k])) {
                            if (hour(selected_place$start_date[k]) == hour(selected_place$end_date[k])) {
                                tmp <- selected_place$start_date[k]
                                tmp1 <- selected_place$end_date[k]
                                a1 <- (duration(minute(tmp), "minutes") + duration(second(tmp), "seconds"))
                                a2 <- (duration(minute(tmp1), "minutes") + duration(second(tmp1), "seconds"))
                                hours[hour(selected_place$start_date[k]), "time"] <- hours[hour(selected_place$start_date[k]), "time"] + (a2-a1)
                                whole_time <-whole_time + (a2-a1)
                            } else {
                                for (i in hour(selected_place$start_date[k]):hour(selected_place$end_date[k])) {
                                    if (i == hour(selected_place$start_date[k])) {
                                        tmp <- selected_place$start_date[k]
                                        a2 <- 3600
                                        a1 <- (duration(minute(tmp), "minutes") + duration(second(tmp), "seconds"))
                                        hours[i, "time"] <- hours[i, "time"] + (a2-a1)
                                        whole_time <-whole_time + (a2-a1)
                                    } else if (i == hour(selected_place$end_date[k])) {
                                        tmp1 <- selected_place$end_date[k]
                                        a2 <- (duration(minute(tmp1), "minutes") + duration(second(tmp1), "seconds"))
                                        hours[i, "time"] <- hours[i, "time"] + (a2)
                                        whole_time <-whole_time + (a2)
                                    } else {
                                        hours[i, "time"] <- hours[i, "time"] + 3600
                                        whole_time <-whole_time + 3600
                                    }
                                }
                            }
                        } else {
                            for (j in day(selected_place$start_date[k]):day(selected_place$end_date[k])) {
                                if (j == day(selected_place$start_date[k])){
                                    for (i in hour(selected_place$start_date[k]):24) {
                                        if(i == hour(selected_place$start_date[k])) {
                                            tmp <- selected_place$start_date[k]
                                            a2 <- 3600
                                            a1 <- (duration(minute(tmp), "minutes") + duration(second(tmp), "seconds"))
                                            hours[i, "time"] <- hours[i, "time"] + (a2-a1)
                                            whole_time <-whole_time + (a2-a1)
                                        } else {
                                            hours[i, "time"] <- hours[i, "time"] + 3600
                                            whole_time <-whole_time + 3600
                                        }
                                    }
                                } else if (j == day(selected_place$end_date[k])) {
                                    for (i in 1:hour(selected_place$end_date[k])) {
                                        if(i == hour(selected_place$end_date[k])) {
                                            tmp1 <- selected_place$end_date[k]
                                            a2 <- (duration(minute(tmp1), "minutes") + duration(second(tmp1), "seconds"))
                                            hours[i, "time"] <- hours[i, "time"] + (a2)
                                            whole_time <-whole_time + (a2)
                                        } else {
                                            hours[i, "time"] <- hours[i, "time"] + 3600
                                            whole_time <-whole_time + 3600
                                        }
                                    }
                                } else {
                                    for (i in 1:24) {
                                        hours[i, "time"] <- hours[i, "time"] + 3600
                                        whole_time <-whole_time + 3600
                                    }
                                }
                            }
                        }
                    }
                    
                }
                hours <- hours %>% mutate(per = time/as.numeric(whole_time)*100)
                
            }
            
            
            p <- ggplot(hours, aes(x = hour, y = per)) +
                geom_col(col = "white", fill = "lightblue") +
                scale_x_continuous(breaks = c(1:24),
                                   labels = c('1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24')) +
                expand_limits(x = c(1,24))+
                labs(
                     x = "[h]",
                     y = '[%]') +
                theme(panel.background = element_blank(),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank())
            
            
            
            plotly::ggplotly(p)
        })
        
        output$plot2 <- renderPlot({
            
            # if (input$tabs == "asia") {
            #   all <- all
            # } else {
            #   all <- all_w
            # }

            
            x <- input$dates
            y <- c(x[1], x[2])
            y <- sapply(y, as.character)
            date1 <- ymd(y[1])
            date2 <- ymd(y[2])
            
            new_all <- all %>%  filter(location_address %in% RV$points$id[which(RV$points$clicked)]) %>% 
                filter((start_date >= date1 & start_date <= date2) 
                       | (end_date >= date1 & end_date <= date2)) %>% 
                mutate(end_date = replace(end_date, end_date <= date1, date1)) %>% 
                mutate(end_date = replace(end_date, end_date >= date2, date2)) %>% 
                mutate(start_date = replace(start_date, start_date <= date1, date1)) %>% 
                mutate(start_date = replace(start_date, start_date >= date2, date2))
            
            data_plot2 <- new_all %>% 
                select(start_date, end_date) %>% 
                mutate(dates = ymd(end_date - start_date))
            
        })
        
        output$plot2 <- renderPlotly({
            
            # if (input$tabs == "asia") {
            #   all <- all
            # } else {
            #   all <- all_w
            # }

            data_plot2 <- all %>% 
                filter(location_address %in% RV$points$id[which(RV$points$clicked)])
            
            all_dates <- c()
            start <- min(all$start_date)
            end <- max(all$end_date)
            for (i in month(start):month(end)) { 
                if (i == month(start)) {
                    if (i == 1 | i ==3 | i ==5 | i ==7 | i ==8 | i ==10 | i ==12) {
                        for (j in day(start):31) {
                            all_dates <- append(all_dates, ymd(paste(as.character(year(start)), as.character(i) , as.character(j), sep = "-")))
                        }
                    } else if (i == 2) {
                        if (year(start) %% 4 == 0) {
                            for (j in day(start):29) {
                                all_dates <- append(all_dates, ymd(paste(as.character(year(start)), as.character(i) , as.character(j), sep = "-")))
                            }
                        } else {
                            for (j in day(start):28) {
                                all_dates <- append(all_dates,ymd(paste(as.character(year(start)), as.character(i) , as.character(j), sep = "-")))
                            }
                        }
                    } else {
                        for (j in day(start):30) {
                            all_dates <- append(all_dates, ymd(paste(as.character(year(start)), as.character(i) , as.character(j), sep = "-")))
                        }
                    }
                } else if( i != month(start) & i != month(end)) {
                    if (i == 1 | i ==3 | i ==5 | i ==7 | i ==8 | i ==10 | i ==12) {
                        for (j in 1:31) {
                            all_dates <- append(all_dates, ymd(paste(as.character(year(start)), as.character(i) , as.character(j), sep = "-")))
                        }
                    } else if (i == 2) {
                        if (year(start) %% 4 == 0) {
                            for (j in 1:29) {
                                all_dates <- append(all_dates, ymd(paste(as.character(year(start)), as.character(i) , as.character(j), sep = "-")))
                            }
                        } else {
                            for (j in 1:28) {
                                all_dates <- append(all_dates, ymd(paste(as.character(year(start)), as.character(i) , as.character(j), sep = "-")))
                            }
                        }
                    } else {
                        for (j in 1:30) {
                            all_dates <- append(all_dates, ymd(paste(as.character(year(start)), as.character(i) , as.character(j), sep = "-")))
                        }
                    }
                } else {
                    for (i in 1:day(end)) {
                        all_dates <- append(all_dates, ymd(paste(as.character(year(start)), as.character(month(end)) , as.character(i), sep = "-")))
                    }
                    
                }
            }
            day <- wday(all_dates)
            id = c(1:length(all_dates))
            len <- length(all_dates)%/%7
            all_dates
            t <- c()
            for (i in 1:len) {
                t <- append(t, rep(i, 7))
            }
            if(length(all_dates)%%7 == 1) {
                t <- append(t, rep(len+1, 1))
            } else if(length(all_dates)%%7 == 2) {
                t <- append(t, rep(len+1, 2))
            } else if(length(all_dates)%%7 == 3) {
                t <- append(t, rep(len+1, 3))
            } else if(length(all_dates)%%7 == 4) {
                t <- append(t, rep(len+1, 4))
            } else if(length(all_dates)%%7 == 5) {
                t <- append(t, rep(len+1, 5))
            } else if(length(all_dates)%%7 == 6) {
                t <- append(t, rep(len+1, 6))
            }
            # day <- append(day, c(5,6,7,1))
            # t <- c(rep(1, 7),rep(2,7),rep(3, 7),rep(4,7),rep(5, 7),rep(6,7),rep(7, 7),rep(8,7),rep(9, 7),rep(10,7),rep(11, 7),rep(12,7),rep(13, 7),rep(14,7),rep(15, 7),rep(16,7),rep(17, 7),rep(18,7),rep(19, 7),rep(20,7),rep(21, 7),rep(22,7),rep(23, 7),rep(24,7),rep(25, 7),rep(26,7),rep(27, 7),rep(28,7),rep(29, 7),rep(30,7),rep(31, 7),rep(32,7),rep(33, 7),rep(34,7),rep(35, 7),rep(36,7),rep(37, 7),rep(38,7),rep(39, 7),rep(40,7),rep(41, 7),rep(42,7),rep(43, 7),rep(44,7),rep(45, 7),rep(46,7),rep(47, 7),rep(48,7),rep(49, 7),rep(50,4))
            all_dates <- data.frame(all_dates,id = c(1:length(all_dates)), day, t) %>%  mutate(time = 0) %>% mutate(month = month(all_dates)) 
            
            
            for (i in 1:nrow(data_plot2)) {
                # time_spent <- appenCd(time_spent, "i")
                # time_spent <- append(time_spent, i)
                if(month(data_plot2$start_date[i]) == month(data_plot2$end_date[i]) & day(data_plot2$start_date[i]) == day(data_plot2$end_date[i])) {
                    tmp1 <- data_plot2$end_date[i]
                    tmp <- data_plot2$start_date[i]
                    a1 <- (duration(hour(tmp), "hours") + duration(minute(tmp), "minutes") + duration(second(tmp), "seconds"))
                    a2 <- (duration(hour(tmp1), "hours") + duration(minute(tmp1), "minutes") + duration(second(tmp1), "seconds"))
                    id_tmp <- (all_dates %>% filter(month(all_dates) == month(tmp) & day(all_dates) == day(tmp)))$id
                    all_dates[id_tmp, "time"] <- all_dates[id_tmp, "time"] + (a2-a1)
                    # time_spent <- append(time_spent, "if start_date == end_date")
                    # time_spent <- append(time_spent, i)
                    # time_spent <- append(time_spent, c(i, a2, a1, a2-a1))
                    # time_spent <- append(time_spent, "tmp1")
                    # time_spent <- append(time_spent, id_tmp)
                    # dodaj a2-a1 do odpowiedniego wektora
                } else {
                    if(month(data_plot2$start_date[i]) == month(data_plot2$end_date[i])) {
                        # time_spent <- append(time_spent, "if month_1 == month2")
                        # time_spent <- append(time_spent, i)
                        # month <- month(data_plot2$start_date[i])
                        # day <- j 
                        for (j in day(data_plot2$start_date[i]):day(data_plot2$end_date[i])) {
                            # time_spCent <- append(time_spent, "j")
                            # time_spent <- append(time_spent, j)
                            if(j == day(data_plot2$start_date[i])) {
                                tmp1 <- as_hms(ymd_hms("2021-01-01 23:59:59 UTC"))
                                tmp <- data_plot2$start_date[i]
                                a1 <- (duration(hour(tmp), "hours") + duration(minute(tmp), "minutes") + duration(second(tmp), "seconds"))
                                a2 <- (duration(hour(tmp1), "hours") + duration(minute(tmp1), "minutes") + duration(second(tmp1), "seconds"))
                                id_tmp <- (all_dates %>% filter(month(all_dates) == month(data_plot2$start_date[i]) & day(all_dates) == j))$id
                                # time_spent <- append(time_spent, "if  warunek 1")
                                # time_spent <- append(time_spent, i)
                                # time_spent <- append(time_spent, j)
                                # time_spent <- append(time_spent, c(i, j, a2, a1, a2-a1))
                                # time_spent <- append(time_spent, id_tmp)
                                all_dates[id_tmp, "time"] <- all_dates[id_tmp, "time"] + (a2-a1)
                            } else if (j != day(data_plot2$start_date[i]) & j != day(data_plot2$end_date[i])) {
                                tmp1 <- as_hms(ymd_hms("2021-01-01 23:59:59 UTC"))
                                tmp <- as_hms(ymd_hms("2021-01-01 00:00:01 UTC"))
                                a1 <- (duration(hour(tmp), "hours") + duration(minute(tmp), "minutes") + duration(second(tmp), "seconds"))
                                a2 <- (duration(hour(tmp1), "hours") + duration(minute(tmp1), "minutes") + duration(second(tmp1), "seconds"))
                                id_tmp <- (all_dates %>% filter(month(all_dates) == month(data_plot2$start_date[i]) & day(all_dates) == j))$id
                                # time_spent <- append(time_spent, c(i, j, a2))
                                # time_spent <- append(time_spent, j)
                                # time_spent <- append(time_spent, id_tmp)
                                all_dates[id_tmp, "time"] <- all_dates[id_tmp, "time"] + (a2)
                            } else {
                                tmp1 <- data_plot2$end_date[i]
                                tmp <- as_hms(ymd_hms("2021-01-01 00:00:01 UTC"))
                                a1 <- (duration(hour(tmp), "hours") + duration(minute(tmp), "minutes") + duration(second(tmp), "seconds"))
                                a2 <- (duration(hour(tmp1), "hours") + duration(minute(tmp1), "minutes") + duration(second(tmp1), "seconds"))
                                id_tmp <- (all_dates %>% filter(month(all_dates) == month(data_plot2$start_date[i]) & day(all_dates) == j))$id
                                # time_spent <- append(time_spent, c(i, j, a2))
                                # time_spent <- append(time_spent, i)
                                # time_spent <- append(time_spent, j)
                                # time_spent <- append(time_spent, id_tmp)
                                all_dates[id_tmp, "time"] <- all_dates[id_tmp, "time"] + (a2)
                            }
                        }
                    } else {
                        # time_spent <- append(time_spent, "else które nie robi nic")
                        # jeśli miesiące są inne
                    }
                }
            }
            all_dates <- all_dates %>% mutate(per = time/863.99)
            all_dates <- all_dates %>% select(all_dates, time, day, per, t) %>% mutate(week = ifelse(day == 2, "Monday", ifelse(day == 3, "Tuesday", ifelse(day == 4, "Wensday", ifelse(day == 5, "Thursday", ifelse(day == 6, "Friday", ifelse(day == 7, "Saturday", "Sunday")))))))
            
            matrix <- acast(all_dates, week~t, value.var="per")
            mat <- matrix[1,]
            matrix[1,] <- matrix[2,]
            matrix[2, ] <- matrix[6,]
            matrix[6, ] <- matrix[3,]
            matrix[3, ] <- matrix[7, ]
            matrix[7, ] <- matrix[4,]
            matrix[4, ] <- matrix[5, ]
            matrix[5, ] <- mat
            rownames(matrix) <- c("Monday", "Tuesday", "Wensday", "Thursday", "Friday", "Saturday", "Sunday")
            matrix_1 <- acast(all_dates, week~t, value.var="all_dates")
            mat_1 <- matrix_1[1,]
            matrix_1[1,] <- matrix_1[2,]
            matrix_1[2, ] <- matrix_1[6,]
            matrix_1[6, ] <- matrix_1[3,]
            matrix_1[3, ] <- matrix_1[7, ]
            matrix_1[7, ] <- matrix_1[4,]
            matrix_1[4, ] <- matrix_1[5, ]
            matrix_1[5, ] <- mat_1
            rownames(matrix_1) <- c("Monday", "Tuesday", "Wensday", "Thursday", "Friday", "Saturday", "Sunday")
            
            p_h <- heatmaply(matrix, 
                             dendrogram = "none",
                             xlab = "", ylab = "", 
                             main = "",
                             grid_color = "grey93",
                             grid_width = 0.00001,
                             titleX = FALSE,
                             hide_colorbar = FALSE,
                             na.value = "grey50",
                             trace = "none",
                             key = FALSE,
                             width = ncol(matrix)*unit(5, "mm"),
                             height = nrow(matrix)*unit(5, "mm"),
                             scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
                                 high = "#34A853"),
                             custom_hovertext=matrix_1,
                             label_names = c("day", "week of the year", "percentage"),
                             showticklabels = c(FALSE, TRUE))
            p_h
            
            
        })
    })
    
    output$plot1w <- plotly::renderPlotly({
        
        x <- input$dates
        y <- c(x[1], x[2])
        y <- sapply(y, as.character)
        date1 <- ymd(y[1])
        date2 <- ymd(y[2])
        

        new_all <- all %>%  filter(location_address %in% RV$points$id[which(RV$points$clicked)]) %>% 
            filter((start_date >= date1 & start_date <= date2) 
                   | (end_date >= date1 & end_date <= date2)) %>% 
            mutate(end_date = replace(end_date, end_date <= date1, date1)) %>% 
            mutate(end_date = replace(end_date, end_date >= date2, date2)) %>% 
            mutate(start_date = replace(start_date, start_date <= date1, date1)) %>% 
            mutate(start_date = replace(start_date, start_date >= date2, date2))
        
        if (is.na((new_all)$location_address[1])) {
            hours <- data.frame(hour = c(1:24), per = c(rep(0, 24)))
        } else {
            whole_time <- 0
            hours <- data.frame(hour = c(1:24), time = c(rep(0, 24)))
            selected_place <- new_all  %>%
                select(start_date,end_date) %>%
                transmute(start_date = ymd_hms(start_date), end_date = ymd_hms(end_date)) %>%
                mutate(start_hour = hour(start_date), end_hour = hour(end_date))
            d <- c()
            
            for (k in 1:nrow(selected_place)) {
                if (month(selected_place$start_date[k]) == month(selected_place$end_date[k])) {
                    if (day(selected_place$start_date[k]) == day(selected_place$end_date[k])) {
                        if (hour(selected_place$start_date[k]) == hour(selected_place$end_date[k])) {
                            tmp <- selected_place$start_date[k]
                            tmp1 <- selected_place$end_date[k]
                            a1 <- (duration(minute(tmp), "minutes") + duration(second(tmp), "seconds"))
                            a2 <- (duration(minute(tmp1), "minutes") + duration(second(tmp1), "seconds"))
                            hours[hour(selected_place$start_date[k]), "time"] <- hours[hour(selected_place$start_date[k]), "time"] + (a2-a1)
                            whole_time <-whole_time + (a2-a1)
                        } else {
                            for (i in hour(selected_place$start_date[k]):hour(selected_place$end_date[k])) {
                                if (i == hour(selected_place$start_date[k])) {
                                    tmp <- selected_place$start_date[k]
                                    a2 <- 3600
                                    a1 <- (duration(minute(tmp), "minutes") + duration(second(tmp), "seconds"))
                                    hours[i, "time"] <- hours[i, "time"] + (a2-a1)
                                    whole_time <-whole_time + (a2-a1)
                                } else if (i == hour(selected_place$end_date[k])) {
                                    tmp1 <- selected_place$end_date[k]
                                    a2 <- (duration(minute(tmp1), "minutes") + duration(second(tmp1), "seconds"))
                                    hours[i, "time"] <- hours[i, "time"] + (a2)
                                    whole_time <-whole_time + (a2)
                                } else {
                                    hours[i, "time"] <- hours[i, "time"] + 3600
                                    whole_time <-whole_time + 3600
                                }
                            }
                        }
                    } else {
                        for (j in day(selected_place$start_date[k]):day(selected_place$end_date[k])) {
                            if (j == day(selected_place$start_date[k])){
                                for (i in hour(selected_place$start_date[k]):24) {
                                    if(i == hour(selected_place$start_date[k])) {
                                        tmp <- selected_place$start_date[k]
                                        a2 <- 3600
                                        a1 <- (duration(minute(tmp), "minutes") + duration(second(tmp), "seconds"))
                                        hours[i, "time"] <- hours[i, "time"] + (a2-a1)
                                        whole_time <-whole_time + (a2-a1)
                                    } else {
                                        hours[i, "time"] <- hours[i, "time"] + 3600
                                        whole_time <-whole_time + 3600
                                    }
                                }
                            } else if (j == day(selected_place$end_date[k])) {
                                for (i in 1:hour(selected_place$end_date[k])) {
                                    if(i == hour(selected_place$end_date[k])) {
                                        tmp1 <- selected_place$end_date[k]
                                        a2 <- (duration(minute(tmp1), "minutes") + duration(second(tmp1), "seconds"))
                                        hours[i, "time"] <- hours[i, "time"] + (a2)
                                        whole_time <-whole_time + (a2)
                                    } else {
                                        hours[i, "time"] <- hours[i, "time"] + 3600
                                        whole_time <-whole_time + 3600
                                    }
                                }
                            } else {
                                for (i in 1:24) {
                                    hours[i, "time"] <- hours[i, "time"] + 3600
                                    whole_time <-whole_time + 3600
                                }
                            }
                        }
                    }
                }
                
            }
            hours <- hours %>% mutate(per = time/as.numeric(whole_time)*100)
            
        }
        
        
        p <- ggplot(hours, aes(x = hour, y = per)) +
            geom_col(col = "white", fill = "lightblue") +
            scale_x_continuous(breaks = c(1:24),
                               labels = c('1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24')) +
            expand_limits(x = c(1,24))+
            labs(title = "Plot???",
                 x = "[h]",
                 y = '[%]') +
            theme(panel.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())
        
        
        
        plotly::ggplotly(p)
    })
    
    output$plot2w <- renderPlot({
        
        # if (input$tabs == "asia") {
        #   all <- all
        # } else {
        #   all <- all_w
        # }
        
        x <- input$dates
        y <- c(x[1], x[2])
        y <- sapply(y, as.character)
        date1 <- ymd(y[1])
        date2 <- ymd(y[2])
        
        new_all <- all %>%  filter(location_address %in% RV$points$id[which(RV$points$clicked)]) %>% 
            filter((start_date >= date1 & start_date <= date2) 
                   | (end_date >= date1 & end_date <= date2)) %>% 
            mutate(end_date = replace(end_date, end_date <= date1, date1)) %>% 
            mutate(end_date = replace(end_date, end_date >= date2, date2)) %>% 
            mutate(start_date = replace(start_date, start_date <= date1, date1)) %>% 
            mutate(start_date = replace(start_date, start_date >= date2, date2))
        
        data_plot2 <- new_all %>% 
            select(start_date, end_date) %>% 
            mutate(dates = ymd(end_date - start_date))
        
    })
    
    output$plot2w <- renderPlotly({
        
        # if (input$tabs == "asia") {
        #   all <- all
        # } else {
        #   all <- all_w
        # }
        
        data_plot2 <- all %>% 
            filter(location_address %in% RV$points$id[which(RV$points$clicked)])
        
        all_dates <- c()
        start <- min(all$start_date)
        end <- max(all$end_date)
        for (i in month(start):month(end)) { 
            if (i == month(start)) {
                if (i == 1 | i ==3 | i ==5 | i ==7 | i ==8 | i ==10 | i ==12) {
                    for (j in day(start):31) {
                        all_dates <- append(all_dates, ymd(paste(as.character(year(start)), as.character(i) , as.character(j), sep = "-")))
                    }
                } else if (i == 2) {
                    if (year(start) %% 4 == 0) {
                        for (j in day(start):29) {
                            all_dates <- append(all_dates, ymd(paste(as.character(year(start)), as.character(i) , as.character(j), sep = "-")))
                        }
                    } else {
                        for (j in day(start):28) {
                            all_dates <- append(all_dates,ymd(paste(as.character(year(start)), as.character(i) , as.character(j), sep = "-")))
                        }
                    }
                } else {
                    for (j in day(start):30) {
                        all_dates <- append(all_dates, ymd(paste(as.character(year(start)), as.character(i) , as.character(j), sep = "-")))
                    }
                }
            } else if( i != month(start) & i != month(end)) {
                if (i == 1 | i ==3 | i ==5 | i ==7 | i ==8 | i ==10 | i ==12) {
                    for (j in 1:31) {
                        all_dates <- append(all_dates, ymd(paste(as.character(year(start)), as.character(i) , as.character(j), sep = "-")))
                    }
                } else if (i == 2) {
                    if (year(start) %% 4 == 0) {
                        for (j in 1:29) {
                            all_dates <- append(all_dates, ymd(paste(as.character(year(start)), as.character(i) , as.character(j), sep = "-")))
                        }
                    } else {
                        for (j in 1:28) {
                            all_dates <- append(all_dates, ymd(paste(as.character(year(start)), as.character(i) , as.character(j), sep = "-")))
                        }
                    }
                } else {
                    for (j in 1:30) {
                        all_dates <- append(all_dates, ymd(paste(as.character(year(start)), as.character(i) , as.character(j), sep = "-")))
                    }
                }
            } else {
                for (i in 1:day(end)) {
                    all_dates <- append(all_dates, ymd(paste(as.character(year(start)), as.character(month(end)) , as.character(i), sep = "-")))
                }
                
            }
        }
        day <- wday(all_dates)
        id = c(1:length(all_dates))
        len <- length(all_dates)%/%7
        all_dates
        t <- c()
        for (i in 1:len) {
            t <- append(t, rep(i, 7))
        }
        if(length(all_dates)%%7 == 1) {
            t <- append(t, rep(len+1, 1))
        } else if(length(all_dates)%%7 == 2) {
            t <- append(t, rep(len+1, 2))
        } else if(length(all_dates)%%7 == 3) {
            t <- append(t, rep(len+1, 3))
        } else if(length(all_dates)%%7 == 4) {
            t <- append(t, rep(len+1, 4))
        } else if(length(all_dates)%%7 == 5) {
            t <- append(t, rep(len+1, 5))
        } else if(length(all_dates)%%7 == 6) {
            t <- append(t, rep(len+1, 6))
        }
        # day <- append(day, c(5,6,7,1))
        # t <- c(rep(1, 7),rep(2,7),rep(3, 7),rep(4,7),rep(5, 7),rep(6,7),rep(7, 7),rep(8,7),rep(9, 7),rep(10,7),rep(11, 7),rep(12,7),rep(13, 7),rep(14,7),rep(15, 7),rep(16,7),rep(17, 7),rep(18,7),rep(19, 7),rep(20,7),rep(21, 7),rep(22,7),rep(23, 7),rep(24,7),rep(25, 7),rep(26,7),rep(27, 7),rep(28,7),rep(29, 7),rep(30,7),rep(31, 7),rep(32,7),rep(33, 7),rep(34,7),rep(35, 7),rep(36,7),rep(37, 7),rep(38,7),rep(39, 7),rep(40,7),rep(41, 7),rep(42,7),rep(43, 7),rep(44,7),rep(45, 7),rep(46,7),rep(47, 7),rep(48,7),rep(49, 7),rep(50,4))
        all_dates <- data.frame(all_dates,id = c(1:length(all_dates)), day, t) %>%  mutate(time = 0) %>% mutate(month = month(all_dates)) 
        
        
        for (i in 1:nrow(data_plot2)) {
            # time_spent <- appenCd(time_spent, "i")
            # time_spent <- append(time_spent, i)
            if(month(data_plot2$start_date[i]) == month(data_plot2$end_date[i]) & day(data_plot2$start_date[i]) == day(data_plot2$end_date[i])) {
                tmp1 <- data_plot2$end_date[i]
                tmp <- data_plot2$start_date[i]
                a1 <- (duration(hour(tmp), "hours") + duration(minute(tmp), "minutes") + duration(second(tmp), "seconds"))
                a2 <- (duration(hour(tmp1), "hours") + duration(minute(tmp1), "minutes") + duration(second(tmp1), "seconds"))
                id_tmp <- (all_dates %>% filter(month(all_dates) == month(tmp) & day(all_dates) == day(tmp)))$id
                all_dates[id_tmp, "time"] <- all_dates[id_tmp, "time"] + (a2-a1)
                # time_spent <- append(time_spent, "if start_date == end_date")
                # time_spent <- append(time_spent, i)
                # time_spent <- append(time_spent, c(i, a2, a1, a2-a1))
                # time_spent <- append(time_spent, "tmp1")
                # time_spent <- append(time_spent, id_tmp)
                # dodaj a2-a1 do odpowiedniego wektora
            } else {
                if(month(data_plot2$start_date[i]) == month(data_plot2$end_date[i])) {
                    # time_spent <- append(time_spent, "if month_1 == month2")
                    # time_spent <- append(time_spent, i)
                    # month <- month(data_plot2$start_date[i])
                    # day <- j 
                    for (j in day(data_plot2$start_date[i]):day(data_plot2$end_date[i])) {
                        # time_spCent <- append(time_spent, "j")
                        # time_spent <- append(time_spent, j)
                        if(j == day(data_plot2$start_date[i])) {
                            tmp1 <- as_hms(ymd_hms("2021-01-01 23:59:59 UTC"))
                            tmp <- data_plot2$start_date[i]
                            a1 <- (duration(hour(tmp), "hours") + duration(minute(tmp), "minutes") + duration(second(tmp), "seconds"))
                            a2 <- (duration(hour(tmp1), "hours") + duration(minute(tmp1), "minutes") + duration(second(tmp1), "seconds"))
                            id_tmp <- (all_dates %>% filter(month(all_dates) == month(data_plot2$start_date[i]) & day(all_dates) == j))$id
                            # time_spent <- append(time_spent, "if  warunek 1")
                            # time_spent <- append(time_spent, i)
                            # time_spent <- append(time_spent, j)
                            # time_spent <- append(time_spent, c(i, j, a2, a1, a2-a1))
                            # time_spent <- append(time_spent, id_tmp)
                            all_dates[id_tmp, "time"] <- all_dates[id_tmp, "time"] + (a2-a1)
                        } else if (j != day(data_plot2$start_date[i]) & j != day(data_plot2$end_date[i])) {
                            tmp1 <- as_hms(ymd_hms("2021-01-01 23:59:59 UTC"))
                            tmp <- as_hms(ymd_hms("2021-01-01 00:00:01 UTC"))
                            a1 <- (duration(hour(tmp), "hours") + duration(minute(tmp), "minutes") + duration(second(tmp), "seconds"))
                            a2 <- (duration(hour(tmp1), "hours") + duration(minute(tmp1), "minutes") + duration(second(tmp1), "seconds"))
                            id_tmp <- (all_dates %>% filter(month(all_dates) == month(data_plot2$start_date[i]) & day(all_dates) == j))$id
                            # time_spent <- append(time_spent, c(i, j, a2))
                            # time_spent <- append(time_spent, j)
                            # time_spent <- append(time_spent, id_tmp)
                            all_dates[id_tmp, "time"] <- all_dates[id_tmp, "time"] + (a2)
                        } else {
                            tmp1 <- data_plot2$end_date[i]
                            tmp <- as_hms(ymd_hms("2021-01-01 00:00:01 UTC"))
                            a1 <- (duration(hour(tmp), "hours") + duration(minute(tmp), "minutes") + duration(second(tmp), "seconds"))
                            a2 <- (duration(hour(tmp1), "hours") + duration(minute(tmp1), "minutes") + duration(second(tmp1), "seconds"))
                            id_tmp <- (all_dates %>% filter(month(all_dates) == month(data_plot2$start_date[i]) & day(all_dates) == j))$id
                            # time_spent <- append(time_spent, c(i, j, a2))
                            # time_spent <- append(time_spent, i)
                            # time_spent <- append(time_spent, j)
                            # time_spent <- append(time_spent, id_tmp)
                            all_dates[id_tmp, "time"] <- all_dates[id_tmp, "time"] + (a2)
                        }
                    }
                } else {
                    # time_spent <- append(time_spent, "else które nie robi nic")
                    #jeśli miesiące są inne
                }
            }
        }
        all_dates <- all_dates %>% mutate(per = time/863.99)
        all_dates <- all_dates %>% select(all_dates, time, day, per, t) %>% mutate(week = ifelse(day == 2, "Monday", ifelse(day == 3, "Tuesday", ifelse(day == 4, "Wensday", ifelse(day == 5, "Thursday", ifelse(day == 6, "Friday", ifelse(day == 7, "Saturday", "Sunday")))))))
        
        matrix <- acast(all_dates, week~t, value.var="per")
        mat <- matrix[1,]
        matrix[1,] <- matrix[2,]
        matrix[2, ] <- matrix[6,]
        matrix[6, ] <- matrix[3,]
        matrix[3, ] <- matrix[7, ]
        matrix[7, ] <- matrix[4,]
        matrix[4, ] <- matrix[5, ]
        matrix[5, ] <- mat
        rownames(matrix) <- c("Monday", "Tuesday", "Wensday", "Thursday", "Friday", "Saturday", "Sunday")
        matrix_1 <- acast(all_dates, week~t, value.var="all_dates")
        mat_1 <- matrix_1[1,]
        matrix_1[1,] <- matrix_1[2,]
        matrix_1[2, ] <- matrix_1[6,]
        matrix_1[6, ] <- matrix_1[3,]
        matrix_1[3, ] <- matrix_1[7, ]
        matrix_1[7, ] <- matrix_1[4,]
        matrix_1[4, ] <- matrix_1[5, ]
        matrix_1[5, ] <- mat_1
        rownames(matrix_1) <- c("Monday", "Tuesday", "Wensday", "Thursday", "Friday", "Saturday", "Sunday")
        
        p_h <- heatmaply(matrix, 
                         dendrogram = "none",
                         xlab = "", ylab = "", 
                         main = "",
                         grid_color = "grey93",
                         grid_width = 0.00001,
                         titleX = FALSE,
                         hide_colorbar = FALSE,
                         na.value = "grey50",
                         trace = "none",
                         key = FALSE,
                         width = ncol(matrix)*unit(5, "mm"),
                         height = nrow(matrix)*unit(5, "mm"),
                         scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
                             high = "#34A853"),
                         custom_hovertext=matrix_1,
                         label_names = c("day", "week of the year", "percentage"),
                         showticklabels = c(FALSE, TRUE))
        p_h
        
        
    })
    
    
    
    
    
    
}





shinyApp(ui, server)
