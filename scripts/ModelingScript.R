str(raw_data)
model_data <- raw_data
model_data <- na.omit(model_data)
model_data$`Price Sensitivity` <- as.factor(model_data$`Price Sensitivity`)
model_data$`Year of First Flight` <- as.factor(model_data$`Year of First Flight`)
model_data$`Scheduled Departure Hour` <- as.factor(model_data$`Scheduled Departure Hour`)
model_data$`No. of other Loyalty Cards` <- as.factor(model_data$`No. of other Loyalty Cards`)
a <- unlist(sapply(model_data,class))
char_cols <- names(which(a == "character"))
model_data[,char_cols] <- lapply(model_data[,char_cols],factor)
flight_cancelled_index <- grep("Flight cancelled", colnames(model_data))
model_data <- model_data[,-flight_cancelled_index]
colnames(model_data)
model_data <- model_data[,-18:-21] #remove origin and destination city and state from analysis
str(model_data)
summary(model_data)
model1 <- lm(formula = model_data$Satisfaction ~.,data= model_data)
summary(model1) 

step(model1,data=model_data,direction="backward")

revisedModel <- lm(formula = model_data$Satisfaction ~ `Airline Status` + Age + 
     Gender + `Price Sensitivity` + `Year of First Flight` + `No of Flights p.a.` + 
     `% of Flight with other Airlines` + `Type of Travel` + `No. of other Loyalty Cards` + 
     `Shopping Amount at Airport` + `Eating and Drinking at Airport` + 
     Class + `Departure Delay in Minutes` + `Scheduled Departure Hour` +
     `Arrival Delay greater 5 Mins`, data = model_data)
summary(revisedModel)

revisedModel2 <- lm(formula = model_data$Satisfaction ~ `Airline Status` + Age + 
        Gender + `Year of First Flight` + `No of Flights p.a.`+ `Type of Travel` + 
        `Shopping Amount at Airport` + 
        Class + `Arrival Delay greater 5 Mins`, data = model_data)
summary(revisedModel2)

randomIndex <- sample(1:dim(model_data)[1])
cutoffPoint <- floor(2*dim(model_data)[1]/3)
trainData <- model_data[randomIndex[1:cutoffPoint],]
testData <- model_data[randomIndex[(cutoffPoint+1):dim(model_data)[1]],]

lmModel <- lm(formula=trainData$Satisfaction ~ `Airline Status` + Age + 
                      Gender + `No of Flights p.a.`+ `Type of Travel` + 
                      `Shopping Amount at Airport` + 
                      Class +`Arrival Delay greater 5 Mins`, data = trainData)
summary(lmModel)
lmPred <- predict(lmModel,testData)

lmCompTable <- data.frame(testData[,1], lmPred)
colnames(lmCompTable) <- c("Test","Pred")
lmCompTable$error <- abs(lmCompTable$Test - lmCompTable$Pred)
head(lmCompTable,10)
sqrt(mean(lmCompTable$error^2))

library("e1071")

#c("Airline Status", "Age","Gender","No of Flights p.a.","Type of Travel", "Shopping Amount at Airport", "Class", "Arrival Delay greater 5 Mins")
#model_data[c("Airline Status", "Age","Gender","No of Flights p.a.","Type of Travel", "Shopping Amount at Airport", "Class", "Arrival Delay greater 5 Mins")]




trainData$goodSatisfaction <- ifelse(trainData$Satisfaction<4, 0,1)
testData$goodSatisfaction <- ifelse(testData$Satisfaction<4, 0,1)
trainData$goodSatisfaction <- as.factor(trainData$goodSatisfaction)
testData$goodSatisfaction <- as.factor(testData$goodSatisfaction)
svmGood <- svm(goodSatisfaction ~ `Airline Status` + Age + 
                             Gender + `No of Flights p.a.`+ `Type of Travel` + 
                             `Shopping Amount at Airport` + 
                             Class + 
                             `Arrival Delay greater 5 Mins`, data=trainData)
svmGood
svmPred <- predict(svmGood,testData)
head(svmPred)
compSVMGood <- data.frame(testData[,24], svmPred)
colnames(compSVMGood) <- c("test","Pred")
perc_svm <- length(which(compSVMGood$test==compSVMGood$Pred))/dim(compSVMGood)[1]
perc_svm
svmResults <- table(test=compSVMGood$test, pred=compSVMGood$Pred)
print(svmResults)
