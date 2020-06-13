# specify the packages of interest
packages = c("ggplot2",
             "ggcharts",
             "dplyr",
             "kernlab",
             "e1071",
             "gridExtra",
             "caret",
             "imputeTS",
             "grid",
             "lattice")

# use this function to check if each package is on the local machine
# if a package is installed, it will be loaded
# if any are not, the missing package(s) will be installed and loaded
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

# set save location for the graphs
pathGraphs = "./Plots"


# replace NA's with column means to preserve the rest of the data
df <- drop_na(raw_data)

# verify no NA's remain
any(is.na(df))

# convert to tibble and preserve data
df_nb <- as_tibble(df)

# convert to tibble
df <- as_tibble(df)

# remove destination/origin city
df <- df[,-18:-21]

# remove year of first flight
df <- df[,-6]

# # select specific variables
# df <- df %>% select("Satisfaction",
#                     "Airline Status",
#                     "No of Flights p.a.",
#                     "Type of Travel",
#                     "Shopping Amount at Airport",
#                     "Class",
#                     "Arrival Delay greater 5 Mins")

# # select numeric variables
# df <- df %>% select("Satisfaction",
#                     "No of Flights p.a.",
#                     "Shopping Amount at Airport",
#                     "Scheduled Departure Hour")
# 
# # select only demographics variables
# df <- df %>% select("Satisfaction",
#                     "Age",
#                     "Gender")
# 
# select sean's variables
df <- df %>% select("Satisfaction",
                    "Age",
                    "Gender",
                    "Airline Status",
                    "No of Flights p.a.",
                    "Type of Travel",
                    "Shopping Amount at Airport",
                    "Class",
                    "Arrival Delay greater 5 Mins")


# convert char cols to factor
df$`Gender` <- as.factor(df$`Gender`)
df$`Airline Status` <- as.factor(df$`Airline Status`)
df$`Type of Travel` <- as.factor(df$`Type of Travel`)
df$`Class` <- as.factor(df$`Class`)
df$`Arrival Delay greater 5 Mins` <- as.factor(df$`Arrival Delay greater 5 Mins`)

# verify changes
str(df)

## Model Prep
# build a randomized index
randIndex <- sample(1:dim(df)[1])

# check the summary
summary(randIndex)

# find the 2/3 cut point
cutPoint2_3 <- floor(2 * dim(df)[1]/3)

# split for the training set
trainData <- df[randIndex[1:cutPoint2_3],]

# split for the testing set
testData <- df[randIndex[(cutPoint2_3 + 1):dim(df)[1]],]


# create a new variable named "Satisfied" in train data set
# Satisfied = 0 if score is < 4
# Satisfied = 1 if score is => 4
trainData$Satisfied <- ifelse(trainData$Satisfaction < 4, 0, 1)

# do the same thing for test dataset
testData$Satisfied <- ifelse(testData$Satisfaction < 4, 0, 1)

# remove "Satisfaction" from train data
trainData <- trainData[,-1]

# remove "Satisfaction" from test data
testData <- testData[,-1]

# verify changes
tibble(trainData)
glimpse(trainData)
tibble(testData)
glimpse(testData)

# convert "Satisfied" in train data from numeric to factor
trainData$Satisfied <- as.factor(trainData$Satisfied)

# convert "Satisfied" in test data from numeric to factor
testData$Satisfied <- as.factor(testData$Satisfied)

time1 <- Sys.time()
# build a model using Naive Bayes function,and use all other variables to predict
nbGood <- naiveBayes(Satisfied~., 
                     data = trainData)
time2 <- Sys.time()
time2 - time1


# check the model
nbGood

# test the model
time1 <- Sys.time()
goodPred <- predict(nbGood, # use model "nbGood" to predict
                    testData # use testData to do the test
)
time2 <- Sys.time()
time2 - time1

# verify first 10 predictions
tibble(goodPred)

# create a dataframe that contains the exact "Satisfied" value and the predicted "Satisfied"
compGood1 <- data.frame(testData$Satisfied, goodPred)

# change column names
colnames(compGood1) <- c("test","Pred")

# verify first 10 with changed column names
tibble(compGood1)

# Compute the percentage of correct cases
perc_nb <- length(which(compGood1$test == compGood1$Pred))/dim(compGood1)[1]
perc_nb
# 0.7689876

# Calculate the percentage of incorrect cases
perc_wrong_nb <- length(which(compGood1$test != compGood1$Pred))/dim(compGood1)[1]
perc_wrong_nb
# 0.2310124

# Confusion Matrix
results <- table(test = compGood1$test, pred = compGood1$Pred)

# check results
results

# confusion matrix results
#           pred
#  test     0     1
#     0 13790  6727      #  read horizontal,  0 class, 13495 identified correctly, 7108 incorrectly
#     1  3064 18802      #                    1 class, 3133 identified incorrectly, 18647 correctly




time1 <- Sys.time()
# build a model using ksvm function,and use all other variables to predict
ksvmGood3 <- ksvm(Satisfied~., # set "Ozone" as target variable; "." means use all other variables to predict "Ozone"
                 data=trainData # specify the data to use in the analysis
                 # scaled = TRUE,
                 # kernel="rbfdot", # kernel function that projects the low dimensional problem into higher dimensional space
                 # kpar="automatic",# kpar refer to parameters that can be used to control the radial function kernel(rbfdot)
                 # C=5 # C refers to "Cost of Constrains"
)
time2 <- Sys.time()
time2-time1


time3 <- Sys.time()
# build a model using ksvm function,and use all other variables to predict
ksvmGood4 <- ksvm(Satisfied~., # set "Ozone" as target variable; "." means use all other variables to predict "Ozone"
                 data=trainData, # specify the data to use in the analysis
                 scaled = TRUE,
                 kernel="rbfdot", # kernel function that projects the low dimensional problem into higher dimensional space
                 kpar="automatic",# kpar refer to parameters that can be used to control the radial function kernel(rbfdot)
                 C=5 # C refers to "Cost of Constrains"
)
time4 <- Sys.time()
time4-time3



# check the model
ksvmGood3
ksvmGood4
summary(ksvmGood1)

# 2) Test the model
goodPred <- predict(ksvmGood4, # use model "svmGood" to predict
                    testData # use testData to do the test
)

# verify first 10 predictions
tibble(goodPred)

# create a dataframe that contains the exact "goodOzone" value and the predicted "goodOzone"
compGood1 <- data.frame(testData$Satisfied, goodPred)

# change column names
colnames(compGood1) <- c("test","Pred")

# verify first 10 with changed column names
tibble(compGood1)

# Compute the percentage of correct cases
perc_ksvm <- length(which(compGood1$test == compGood1$Pred))/dim(compGood1)[1]
perc_ksvm

# Calculate the percentage of incorrect cases
perc_wrong_ksvm <- length(which(compGood1$test != compGood1$Pred))/dim(compGood1)[1]
perc_wrong_ksvm

# Confusion Matrix
results <- table(test = compGood1$test, pred = compGood1$Pred)


# check results
results




