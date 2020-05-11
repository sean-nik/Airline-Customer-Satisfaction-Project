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