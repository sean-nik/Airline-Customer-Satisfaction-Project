# Are people who spend more on food/drink more satisfied?
# How much impact does flight delay have on satisfaction?
  

#install.packages("sqldf")
#install.packages("gdata")
#install.packages("readxl")

library(readxl)
library(sqldf)
library(ggplot2)
library(gdata)
search()

satisfactionXL <- read_xlsx("/Users/derekjohnson/Airline-Customer-Satisfaction-Project/Satisfaction Survey(2).xlsx")
#satisfactionGd <- read.xls("/Users/derekjohnson/Airline-Customer-Satisfaction-Project/Satisfaction Survey(2).xlsx")

str(satisfactionXL)
#str(satisfactionGd)
a<- satisfactionXL
str(a)
summary(a)


delayNA<-is.na(a$`Departure Delay in Minutes`=='NA')
head(delayNA)
str(delayNA)

# Create new variables and remove irrelevant data
colnames(a)

# Delays Impact on Satisfaction
aDelay<- a[,c(1,23)] # Create new variable of Satisfaction and Departure Delay Minutes
aDelay<- na.omit(aDelay) # NA's omitted 
any(is.na(aDelay))
str(aDelay)
summary(aDelay)

# Breaking flights down into 2 data frames: with and without delays
noDelay<- sqldf("SELECT * FROM aDelay WHERE `Departure Delay in Minutes` = 0")
str(noDelay)
yeaDelay<- sqldf("SELECT * FROM aDelay WHERE `Departure Delay in Minutes` > 0")
str(yeaDelay)
summary(yeaDelay)

# Create new variable to cut Delay Minutes into increments of fives
yeaDelay$delaysbyfive<- cut(yeaDelay$`Departure Delay in Minutes`, breaks = seq (0, 1500, by = 5),
                            labels = FALSE,  include.lowest = TRUE)
table(yeaDelay$delaysbyfive)
head(yeaDelay)

# Run tapply to create new data frame of average satisfaction per 5 minute increments
avgSatPer5<- tapply(yeaDelay$Satisfaction, yeaDelay$delaysbyfive, mean)
avgSatPer5
minutes5<-unlist(labels(avgSatPer5))
minutes5
afs5<-data.frame(avgSatPer5,minutes5)
afs5
afs5<- afs5[1:48,]
afs5$minutes5<- as.numeric(afs5$minutes5)
afs5$minutes5<- afs5$minutes5*5
str(afs5)
head(afs5)

ggplot(afs5, aes(x=(minutes5), y=avgSatPer5))+
  geom_point()+
  geom_smooth()+
  labs(x="Minutes Delayed", y="Average Satisfaction")




# Amount Spent on Eating and Drinking Impact on Satisfaction
aEating<- a[,c(1,12)]
aEating<- na.omit(aEating)
any(is.na(aEating))
str(aEating)
summary(aEating)

noEatDrink<- sqldf("SELECT * FROM aEating WHERE `Eating and Drinking at Airport` = 0")
str(noEatDrink)

yeaEatDrink<- sqldf("SELECT * FROM aEating WHERE `Eating and Drinking at Airport` > 0")
str(yeaEatDrink)
summary(yeaEatDrink)

# Create new variable to cut dollars spent on food/drink  into increments of five
yeaEatDrink$eatingbyfive<- cut(yeaEatDrink$`Eating and Drinking at Airport`, breaks = seq (0, 1000, by = 5),
                            labels = FALSE,  include.lowest = TRUE)
table(yeaEatDrink$eatingbyfive)
head(yeaEatDrink)

# Run tapply to create new data frame of average satisfaction per 5 dollars spent
avgSatPer5drinks<- tapply(yeaEatDrink$Satisfaction, yeaEatDrink$eatingbyfive, mean)
avgSatPer5drinks
drinks5<-unlist(labels(avgSatPer5drinks))
drinks5
ads5<-data.frame(avgSatPer5drinks,drinks5)
ads5
ads5<- ads5[1:60,]
ads5$drinks5<- as.numeric(ads5$drinks5)
ads5$drinks5<- ads5$drinks5*5
str(ads5)
head(ads5)

ggplot(ads5, aes(x=(drinks5), y=avgSatPer5drinks))+
  geom_point()+
  geom_smooth()+
  labs(x="Food/Drinks in $$", y="Average Satisfaction")

