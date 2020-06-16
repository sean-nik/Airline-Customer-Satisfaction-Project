# Association Rules Mining
library(arules)
library(arulesViz)
library(readxl)


# satisfactionXL <- read_xlsx("/Users/derekjohnson/Airline-Customer-Satisfaction-Project/Satisfaction Survey(2).xlsx")
summary(satisfactionXL)
colnames(satisfactionXL)
b<- satisfactionXL
str(b)
colnames(b)
b<-na.omit(b)
summary(b)

# remove irrelevant data
b<- b[,c(-14:-16, -19, -21, -24, -25, -27)]

# convert data to factors
b$Satisfaction<- ifelse(b$Satisfaction>=4, "More Satisfied", "Less Satisfied")
b$Satisfaction<- as.factor(b$Satisfaction)
b$`Airline Status`<- as.factor(b$`Airline Status`)
b$Age<- cut(b$Age, breaks = seq (10, 90, by = 10),
                           labels = FALSE,  include.lowest = TRUE)
b$Age<- as.factor(b$Age)
b$Gender<- as.factor(b$Gender)
b$`Price Sensitivity`<- as.factor(b$`Price Sensitivity`)
b$`Year of First Flight`<- as.factor(b$`Year of First Flight`)
b$`No of Flights p.a.`<- as.factor(b$`No of Flights p.a.`)
b$`% of Flight with other Airlines`<- cut(b$`% of Flight with other Airlines`
                                          , breaks = seq (0, 100, by = 10),
            labels = FALSE,  include.lowest = TRUE)
b$`% of Flight with other Airlines`<- as.factor(b$`% of Flight with other Airlines`)
b$`Type of Travel`<- as.factor(b$`Type of Travel`)
b$`No. of other Loyalty Cards`<- as.factor(b$`No. of other Loyalty Cards`)
b$`Shopping Amount at Airport`<- as.factor(b$`Shopping Amount at Airport`)
b$`Eating and Drinking at Airport`<- as.factor(b$`Eating and Drinking at Airport`)
b$Class<- as.factor(b$Class)
b$`Airline Name`<- as.factor(b$`Airline Name`)
b$`Scheduled Departure Hour`<- as.factor(b$`Scheduled Departure Hour`)
b$`Departure Delay in Minutes`<- cut(b$`Departure Delay in Minutes`, breaks = seq (0, 1592, by = 15),
                          labels = FALSE,  include.lowest = TRUE)
b$`Departure Delay in Minutes`<- as.factor(b$`Departure Delay in Minutes`)
b$`Flight time in minutes`<- cut(b$`Flight time in minutes`, breaks = quantile(b$`Flight time in minutes`))
b$`Flight time in minutes`<- as.factor(b$ `Flight time in minutes`)
b$`Arrival Delay greater 5 Mins`<- as.factor(b$`Arrival Delay greater 5 Mins`)
b$`Orgin City`<- as.factor(b$`Orgin City`)
b$`Destination City`<- as.factor(b$`Destination City`)


# convert to sparse matrix
airTrans<- as(b,"transactions")
str(airTrans)

itemFrequencyPlot(airTrans, support=0.05, cex.names = 0.5)
itemFrequencyPlot(airTrans, support=0.15, cex.names = 0.5)

# first apriori
airRules <- apriori(airTrans
                    , parameter = list(support=0.05,confidence = 0.5)
                    ,appearance =list(default="lhs"
                                      ,rhs =("Satisfaction=More Satisfied")))

summary(airRules)
inspect(airRules)
plot(airRules)

#second apriori
airRules2 <- apriori(airTrans
                    , parameter = list(support=0.05,confidence = 0.75)
                    ,appearance =list(default="lhs"
                                      ,rhs =("Satisfaction=More Satisfied")))

summary(airRules2)
inspect(airRules2)
plot(airRules2)


#third apriori
airRules3 <- apriori(airTrans
                     , parameter = list(support=0.1,confidence = 0.75)
                     ,appearance =list(default="lhs"
                                       ,rhs =("Satisfaction=More Satisfied")))

summary(airRules3)
inspect(airRules3)
plot(airRules3)


airRules3Lift <- airRules3[quality(airRules3)$lift > 1.5]
summary(airRules3Lift)
inspect(airRules3Lift)
plot(airRules3Lift)
plot(airRules3Lift, method = "graph", control = list(type = "items"))

#fourth apriori
airRules4 <- apriori(airTrans
                     , parameter = list(support=0.07,confidence = 0.5)
                     ,appearance =list(default="lhs"
                                       ,rhs =("Satisfaction=Less Satisfied")))

summary(airRules4)
inspect(airRules4)
plot(airRules4)


#fifth apriori
airRules5 <- apriori(airTrans
                     , parameter = list(support=0.15,confidence = 0.85)
                     ,appearance =list(default="lhs"
                                       ,rhs =("Satisfaction=Less Satisfied")))
airRules5Lift <- airRules5[quality(airRules5)$lift > 1.85]

summary(airRules5)
inspect(airRules5)
plot(airRules5)
plot(airRules5, method = "graph", control = list(type = "items"))

summary(airRules5Lift)
inspect(airRules5Lift)
plot(airRules5Lift)
plot(airRules5Lift, method = "graph", control = list(type = "items"))


