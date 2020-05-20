# Relationship between City of origin and satisfaction
travelCityDf <- raw_data[,c("Satisfaction","Orgin City","Destination City")]
travelCityDf <- na.omit(travelCityDf)
str(travelCityDf)
length(unique(travelCityDf$`Orgin City`))
length(unique(travelCityDf$`Destination City`))

satisfactionOriginCityList <-tapply(travelCityDf$Satisfaction, travelCityDf$`Orgin City`, mean)
originCities <- unlist(labels(satisfactionOriginCityList))
originCityListCount <-tapply(travelCityDf$Satisfaction, travelCityDf$`Orgin City`, length)
satisfactionOriginCityDf <- data.frame(originCities,satisfactionOriginCityList,originCityListCount)
colnames(satisfactionOriginCityDf) <- c("originCity","meanSatisfaction","observationCount")
sortedByMeanOriginCitySatisfaction <- satisfactionOriginCityDf[order(-satisfactionOriginCityDf$meanSatisfaction),]
# Best cities to fly from
head(sortedByMeanOriginCitySatisfaction,10)
# Worst cities to fly from
tail(sortedByMeanOriginCitySatisfaction,10)
str(satisfactionOriginCityDf)
ggplot(satisfactionOriginCityDf,aes(x=observationCount,y=meanSatisfaction)) + geom_point() + scale_x_log10() + ggtitle("Mean Satisfaction vs. Number of Recordings Per Departure City") + labs(y="Mean Satisfaction", x = "Number of Recordings Per City")
sortedByCountOriginCitySatisfaction <- satisfactionOriginCityDf[order(-satisfactionOriginCityDf$observationCount),]
sortedByCountOriginCitySatisfaction
mean(raw_data$Satisfaction,na.rm = TRUE)

library(ggmap)
library(RCurl)
library(RJSONIO)

makeGeoUrl <- function(address) {
  root <- 'https://us1.locationiq.com/v1/search.php?key=cf9b5809b5fa79&q='
  url <- paste(root,address,'&format=json',sep="")
  return(URLencode(url))
}

geocodeAddress <- function(address) {
  url <- makeGeoUrl(address)
  apiResult <- getURL(url)
  geoStruct <- fromJSON(apiResult,simplify = FALSE)
  lat <- NA
  lon <- NA
  try(lat <- geoStruct[[1]]$lat)
  try(lon <- geoStruct[[1]]$lon)
  return(c(lat,lon))
}

addressLat <- function(address) {
  Sys.sleep(1)
  url <- makeGeoUrl(address)
  apiResult <- getURL(url)
  geoStruct <- fromJSON(apiResult,simplify = FALSE)
  lat <- NA
  try(lat <- geoStruct[[1]]$lat)
  return(lat)
}
addressLon <- function(address) {
  Sys.sleep(1)
  url <- makeGeoUrl(address)
  apiResult <- getURL(url)
  geoStruct <- fromJSON(apiResult,simplify = FALSE)
  lon <- NA
  try(lon <- geoStruct[[1]]$lon)
  return(lon)
}

getStateAbb <- function(address) {
  address <- as.character(address)
  return(substr(address,nchar(address)-1,nchar(address)))
}
satisfactionOriginCityDf$stateAbb <- unlist(lapply(satisfactionOriginCityDf$originCity,getStateAbb))
# match(satisfactionOriginCityDf$stateAbb,state.abb)     
satisfactionOriginCityDf$stateName <- state.name[match(satisfactionOriginCityDf$stateAbb,state.abb)]
satisfactionOriginCityDf$stateName <- tolower(satisfactionOriginCityDf$stateName)

satisfactionOriginCityDf$verboseAddress <- paste(substr(satisfactionOriginCityDf$originCity,1,nchar(as.character(satisfactionOriginCityDf$originCity))-2), satisfactionOriginCityDf$stateName, sep="")

satisfactionOriginCityDf[is.na(satisfactionOriginCityDf$stateName),]

cityMapData <- satisfactionOriginCityDf[!is.na(satisfactionOriginCityDf$stateName),]

#cityMapData[cityMapData$lon > 0,]
#moreheadLatLon <- geocodeAddress("Morehead,Beaufort, NC")
#cityMapData[cityMapData$originCity == 'New Bern/Morehead/Beaufort, NC',]$lat <- as.numeric(moreheadLatLon[1])
#cityMapData[cityMapData$originCity == 'New Bern/Morehead/Beaufort, NC',]$lon <- as.numeric(moreheadLatLon[2])

cityMapData <- cityMapData[cityMapData$stateAbb != "AK" & cityMapData$stateAbb != "HI",]

lats <- lapply(cityMapData$verboseAddress, addressLat)
latsVector <- unlist(lats)
lons <- lapply(cityMapData$verboseAddress, addressLon)
lonsVector <- unlist(lons)

cityMapData$lat <- latsVector
cityMapData$lon <- lonsVector

cityMapData[cityMapData$verboseAddress == "Saginaw/Bay City/Midland, michigan",]$lat <- addressLat("Saginaw,Bay City,Midland, michigan")
cityMapData[cityMapData$verboseAddress == "Saginaw/Bay City/Midland, michigan",]$lon <- addressLon("Saginaw,Bay City,Midland, michigan")

cityMapData$lat <- as.numeric(cityMapData$lat)
cityMapData$lon <- as.numeric(cityMapData$lon)

#write.csv(cityMapData,"/Users/sean/code/sean-nik/Airline-Customer-Satisfaction-Project/data/cityMapDataFinal.csv", row.names = FALSE)

str(cityMapData)
us <- map_data("state")  
satisfactionCityMap <- ggplot(cityMapData, aes(map_id = stateName))
satisfactionCityMap <- satisfactionCityMap + geom_map(map = us, fill='white',color='black')
satisfactionCityMap <- satisfactionCityMap + expand_limits(x=us$long,y=us$lat) + coord_map()
satisfactionCityMap <- satisfactionCityMap + geom_point(data = cityMapData, aes(x=lon,y=lat,color=meanSatisfaction,size=observationCount)) 
satisfactionCityMap <- satisfactionCityMap + scale_colour_gradient(low="red", high="green") + scale_size(range = c(2,10))
satisfactionCityMap







