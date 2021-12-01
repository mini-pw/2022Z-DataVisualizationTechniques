
map_total <- function (interestingCountries){
  library(tidyverse)
  library(viridis)
  library(DT)
  options(knitr.table.format = "html")
  library(jpeg)
  library(maps)
  library(geosphere)
  library(grid)
  library(data.table)
  library(countrycode)
  
  population_gdp = data.table(read.csv("data/co2-emissions-vs-gdp.csv"))
  population_gdp$GDP = population_gdp$GDP.per.capita*population_gdp$Total.population..Gapminder..HYDE...UN.
  population_gdp = population_gdp[,Population:=Total.population..Gapminder..HYDE...UN.][!is.na(Code) & !is.na(Population) & !is.na(GDP),.(Code,Population,GDP, Year)]
  
  load_data = function (name){
    data = read.csv(paste("data/",name,".csv", sep=""), sep = "|",  col.names = c("DT", "Country", "Partner", "Industry", "Year", "Emissions"))
    data = data.table(data)
    data = data[Industry=="DTOTAL"][,c("DT","Industry"):=NULL]
    data = data[population_gdp, on = .(Country = Code, Year = Year), nomatch=0]
    data[,EmissionsPerCapita:=Emissions/Population]
    data[,EmissionsPerGDP:=Emissions/GDP]
    data
  }
  export_d = load_data("EXGR_TCO2")
  export = export_d[,.(Country,Partner,Emissions, Year)]
  
  prepareDataEdges <- function(data,year,topN=1000, countries=NULL){
    mapData <- data[Year == year][,c("Year"):=NULL]
    mapData$CountryName <- countrycode(mapData$Country, origin = 'iso3c', destination = 'country.name')
    mapData$PartnerName <- countrycode(mapData$Partner, origin = 'iso3c', destination = 'country.name')
    mapData = mapData[!is.na(CountryName) & !is.na(PartnerName)]
    mapData[CountryName == "United States"]$CountryName = "USA"
    mapData[PartnerName == "United States"]$PartnerName = "USA"
    mapData <- mapData[CountryName != PartnerName]
    if (!is.null(countries)){
      mapData <- mapData[CountryName %in% countries & PartnerName %in% countries] 
    }
    mapData <- mapData[order(mapData$Emissions, decreasing=TRUE)[1:topN],]
    
    mapData
    
  }
  
  prepareDataForPlot <- function(mapData, interestingCountries){
    data_for_connection=function( dep_lon, dep_lat, arr_lon, arr_lat, group, emissions, countryname){
      inter <- gcIntermediate(c(dep_lon, dep_lat), c(arr_lon, arr_lat), n=40, addStartEnd=TRUE, breakAtDateLine=F)             
      inter=data.table(inter)
      inter$group=0
      inter$size = emissions
      inter$countryname = countryname
      inter$dep_lon = dep_lon
      inter$arr_lon = arr_lon
      diff_of_lon=abs(dep_lon-arr_lon)
      if( dep_lon <= arr_lon){
        inter <- inter[order(lon)]
      }else{
        inter <- inter[order(lon, , decreasing = TRUE)]
      }
      
      if(diff_of_lon > 180){
        inter$group[ which(inter$lon>=0)]=paste(group, "A",sep="")
        inter$group[ which(inter$lon<0)]=paste(group, "B",sep="")
        inter <- inter[dim(inter)[1]:1,]
      }else{
        inter$group=group
      }
      return(inter)
    }
    worldMap<- data.table(map_data("world"))
    worldMap <- worldMap[,.(region, lat,long)]
    worldMap <- worldMap[region %in% interestingCountries]
    worldMap = worldMap[region!= "USA" | (long >= -130 & long <= 0)] #fix usa
    worldMap = worldMap[region!= "South Africa" | (long <= 32)]#fix rpa
    worldMap = worldMap[region!= "Australia" | (lat >= -45)]#fix rpa
    worldMap = worldMap[region!= "Argentina" | (lat >= -52)]#fix rpa
    CJ.table.1 <- function(X,Y)
      setkey(X[,c(k=1,.SD)],k)[Y[,c(k=1,.SD)],allow.cartesian=TRUE][,k:=NULL]
    
    centers = worldMap[,.(lat=mean(lat),long=mean(long)), by = region]
    closest <- CJ.table.1(centers,worldMap)
    setnames(closest,c("region","lat","long", "i.region", "i.lat", "i.long"), c("CountryName", "CountryLat", "CountryLong", "PartnerName", "PartnerLat", "PartnerLong"))
    dt.haversine <- function(lat_from, lon_from, lat_to, lon_to, r = 6378137){
      radians <- pi/180
      lat_to <- lat_to * radians
      lat_from <- lat_from * radians
      lon_to <- lon_to * radians
      lon_from <- lon_from * radians
      dLat <- (lat_to - lat_from)
      dLon <- (lon_to - lon_from)
      a <- (sin(dLat/2)^2) + (cos(lat_from) * cos(lat_to)) * (sin(dLon/2)^2)
      return(2 * atan2(sqrt(a), sqrt(1 - a)) * r)
    }
    closest[, dist := dt.haversine(CountryLat, CountryLong, PartnerLat, PartnerLong)]
    closest <- closest[ , .SD[which.min(dist)], by = .(CountryName,PartnerName)]
    print(closest)
    mapData <- mapData[closest, on=.(CountryName=CountryName,PartnerName=PartnerName), nomatch=0]
    data_ready_plot=data.table()
    for(i in c(1:nrow(mapData))){
      tmp=data_for_connection(mapData$CountryLon[i], mapData$CountryLat[i], mapData$PartnerLon[i], mapData$PartnerLat[i] , i, mapData$Emissions[i], mapData$CountryName[i])
      data_ready_plot=rbind(tmp, data_ready_plot)
    }
    data_ready_plot
  }
  
  dataForMap <-prepareDataForPlot(prepareDataEdges(export,2015,1000, interestingCountries), interestingCountries)
  
  dataForMap$size=dataForMap$size/max(dataForMap$size)
  worldMap<- data.table(map_data("world"))
  ggplot() +
    geom_map(data=worldMap, map=worldMap, aes(map_id=region), fill="gray", color="#7f7f7f", size=0.5) +
    geom_map(data=dataForMap,aes(map_id=countryname), fill="white",  map = worldMap, size=2,  alpha =0.35) +
    geom_map(data=dataForMap,aes(map_id=countryname, fill=countryname),  map = worldMap, size=2,  alpha =0.35) +
    geom_path(data=dataForMap, aes(x=lon, y=lat, group=group, color=countryname, size=log(1 +size), alpha = 1-log(1 +size)), arrow = arrow(length=unit(0.4,"cm"), type = "open")) +
    theme_void() +
    theme(
      legend.position="none",
      panel.background = element_rect(fill = "black", colour = "black"),
      panel.spacing=unit(c(0,0,0,0), "null"),
      plot.margin=grid::unit(c(0,0,0,0), "cm"),
    ) +
    guides(show.legend = FALSE)+
    xlim(-180,180) +
    ylim(-90,90) +
    expand_limits(x = worldMap$long, y = worldMap$lat) +
    scale_x_continuous(expand = c(0.006, 0.006)) +
    coord_equal() +
    scale_size_continuous(range = c(2,10)) +
    scale_alpha_continuous(range = c(0.7,1)) +
    scale_color_manual(values=c("#FF61C3", "#00B9E3", "#00BA38", "#F2E411", "white", "red")) +
    scale_fill_manual(values=c("#FF61C3", "#00B9E3", "#00BA38", "#F2E411", "white", "red"))
}