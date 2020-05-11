travelExpDf <- raw_data[,c("Satisfaction","Year of First Flight","No of Flights p.a.")]
any(is.na(travelExpDf$Satisfaction))
length(travelExpDf$Satisfaction[travelExpDf$Satisfaction=='NA'])
any(is.na(travelExpDf$`No of Flights p.a.`))
any(is.na(travelExpDf$`Year of First Flight`))
travelExpDf <- na.omit(travelExpDf)
travelExpDf$`Year of First Flight` <- as.Date(travelExpDf$`Year of First Flight`)

summary(travelExpDf)
str(travelExpDf)

# Relationship between year of first flight and satisfaction
satisfactionMeanByFirstFlight <- tapply(travelExpDf$Satisfaction,travelExpDf$`Year of First Flight`,mean)
yearName <- unlist(labels(satisfactionMeanByFirstFlight))
satisfactionFirstFlightDf <- data.frame(satisfactionMeanByFirstFlight,yearName)
colnames(satisfactionFirstFlightDf) <- c("meanSatisfaction", "year")
ggplot(satisfactionFirstFlightDf,aes(x=year,y=meanSatisfaction)) + geom_bar(stat="identity") + ggtitle("Mean Satisfaction vs. Year of First Flight") + labs(y="Mean Satisfaction", x = "Year of First Flight")
sd(satisfactionFirstFlightDf$meanSatisfaction)

# Relationship between No of Flights p.a. and satisfaction
satisfactionMeanByFlightsPa <- tapply(travelExpDf$Satisfaction,travelExpDf$`No of Flights p.a.`,mean)
flightsPa <- as.numeric(unlist(labels(satisfactionMeanByFlightsPa)))
satisfactionCountByFlightsPa <- tapply(travelExpDf$Satisfaction,travelExpDf$`No of Flights p.a.`,length)
satisfactionFlightsPaDf <- data.frame(flightsPa,satisfactionMeanByFlightsPa,satisfactionCountByFlightsPa)
colnames(satisfactionFlightsPaDf) <- c("flightsPa","meanSatisfaction","flightsPaCount")
ggplot(satisfactionFlightsPaDf,aes(x=flightsPa,y=meanSatisfaction)) + geom_point() + geom_smooth() + ggtitle("Mean Satisfaction vs. Flights p.a.") + labs(y="Mean Satisfaction", x = "Number of flights per annum")
ggplot(satisfactionFlightsPaDf,aes(x=flightsPa,y=meanSatisfaction)) + geom_point(aes(size=flightsPaCount)) + geom_smooth() + ggtitle("Mean Satisfaction vs. Flights p.a.") + labs(y="Mean Satisfaction", x = "Number of flights per annum", size = "Recordings")
