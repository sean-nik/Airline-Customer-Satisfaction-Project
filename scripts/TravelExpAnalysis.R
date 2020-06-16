travelExpDf <- raw_data[,c("Satisfaction","Year of First Flight","No of Flights p.a.")]
any(is.na(travelExpDf$Satisfaction))
any(is.na(travelExpDf$`No of Flights p.a.`))
any(is.na(travelExpDf$`Year of First Flight`))
travelExpDf <- na.omit(travelExpDf)
travelExpDf$`Year of First Flight` <- as.Date(travelExpDf$`Year of First Flight`)

summary(travelExpDf)
str(travelExpDf)

# Relationship between year of first flight and satisfaction
ggplot(travelExpDf, aes(x=`Year of First Flight`)) + geom_histogram(bins = length(unique(travelExpDf$`Year of First Flight`)),color="black",fill="white") + scale_x_continuous(breaks=seq(2003,2012,1)) + ggtitle("Histogram of Year of First Flight")

satisfactionMeanByFirstFlight <- tapply(travelExpDf$Satisfaction,travelExpDf$`Year of First Flight`,mean)
yearName <- unlist(labels(satisfactionMeanByFirstFlight))
satisfactionCountByFirstFlight <- tapply(travelExpDf$Satisfaction,travelExpDf$`Year of First Flight`,length)
satisfactionFirstFlightDf <- data.frame(satisfactionMeanByFirstFlight,yearName, satisfactionCountByFirstFlight)
colnames(satisfactionFirstFlightDf) <- c("meanSatisfaction", "year", "count")
ggplot(satisfactionFirstFlightDf,aes(x=year,y=meanSatisfaction)) + geom_bar(stat="identity") + ggtitle("Mean Satisfaction vs. Year of First Flight") + labs(y="Mean Satisfaction", x = "Year of First Flight")
ggplot(satisfactionFirstFlightDf,aes(x=year,y=count)) + geom_bar(stat="identity") + ggtitle("Number of recordings vs. Year of First Flight") + labs(y="Count", x = "Year of First Flight")
sd(satisfactionFirstFlightDf$meanSatisfaction)

# Relationship between No of Flights p.a. and satisfaction
ggplot(travelExpDf, aes(x=`No of Flights p.a.`)) + geom_histogram(binwidth = 5,color="black",fill="white") + ggtitle("Histogram of Number of Flights per annum")
mean(travelExpDf$`No of Flights p.a.`)
sd(travelExpDf$`No of Flights p.a.`)

satisfactionMeanByFlightsPa <- tapply(travelExpDf$Satisfaction,travelExpDf$`No of Flights p.a.`,mean)
flightsPa <- as.numeric(unlist(labels(satisfactionMeanByFlightsPa)))
satisfactionCountByFlightsPa <- tapply(travelExpDf$Satisfaction,travelExpDf$`No of Flights p.a.`,length)
satisfactionFlightsPaDf <- data.frame(flightsPa,satisfactionMeanByFlightsPa,satisfactionCountByFlightsPa)
colnames(satisfactionFlightsPaDf) <- c("flightsPa","meanSatisfaction","flightsPaCount")
ggplot(satisfactionFlightsPaDf,aes(x=flightsPa,y=meanSatisfaction)) + geom_point() + geom_smooth() + ggtitle("Mean Satisfaction vs. Flights p.a.") + labs(y="Mean Satisfaction", x = "Number of flights per annum")
ggplot(satisfactionFlightsPaDf,aes(x=flightsPa,y=meanSatisfaction)) + geom_point(aes(size=flightsPaCount)) + geom_smooth() + ggtitle("Mean Satisfaction vs. Flights p.a.") + labs(y="Mean Satisfaction", x = "Number of flights per annum", size = "Recordings")
